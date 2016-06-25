open Common
open Geometry
open Action

module Params = struct
  type t =
    { center : Vector.t
    (* We could store both angle and distance as delta between the vectors, but
       then we would have to recompute the values for the initial params all the
       time. *)
    ; angle : Angle.t
    ; distance : float
    }

  let of_one_pointer v =
    { center = v
    ; angle = Angle.zero
    ; distance = 1.
    }

  let frame_diff ~initial:t1 ~current:t2 =
    let rotation = Angle.(t2.angle - t1.angle) in
    let scale = t2.distance /. t1.distance in
    (* Why [sin] here? It just works. *)
    let scale_delta_x =
      (scale -. 1.) *. (abs_float (sin (Angle.to_radians t2.angle)))
    in
    let scale_delta_y =
      (scale -. 1.) *. (abs_float (cos (Angle.to_radians t2.angle)))
    in
    let scale_x = 1. +. scale_delta_x in
    let scale_y = 1. +. scale_delta_y in
    let move_by = Vector.(t2.center - t1.center) in
    Frame.(scale ~scale_x ~scale_y
           +> rotate rotation
           +> translate move_by)
end

(* One group of pointers per shape. *)
module Group = struct
  type t =
    { initial_frame : Frame.t
    ; initial_params : Params.t
    (* In order of appearance *)
    ; pointers : Pointer_id.t list
    }

  let params pointers frame ~get_position =
    let positions = List.map pointers ~f:get_position in
    match positions with
    | []    -> Params.of_one_pointer Vector.zero (* Should never happen *)
    | [ v ] -> Params.of_one_pointer v
    | v1 ::  v2 :: _ ->
      let inverse = Matrix.inv (Frame.matrix frame) in
      let center = v1 in
      let v1 = Matrix.apply inverse v1 in
      let v2 = Matrix.apply inverse v2 in
      let delta = Vector.(v2 - v1) in
      let distance = Vector.length delta in
      let angle = Vector.angle delta in
      { Params. center; distance; angle  }

  let create pointers frame ~get_position =
    { initial_frame = frame
    ; initial_params = params pointers frame ~get_position
    ; pointers
    }

  let shape t ~get_position =
    let current = params t.pointers t.initial_frame ~get_position in
    let frame_diff =
      Params.frame_diff ~initial:t.initial_params ~current
    in
    Frame.(t.initial_frame +> frame_diff)
end

type t =
  { groups : Group.t Shape_id.Table.t
  ; positions : Vector.t Pointer_id.Table.t
  }

let create () =
  let groups = Hashtbl.create () in
  let positions = Hashtbl.create () in
  { groups; positions }

let get_position t =
  Hashtbl.find t.positions

let is_touching t shape_id =
  Hashtbl.mem t.groups shape_id

let add t shape_id shape (pointer : Pointer.t) =
  let get_position = get_position t in
  Hashtbl.replace t.positions ~key:pointer.id ~data:pointer.position;
  let current_pointers =
    match Hashtbl.maybe_find t.groups shape_id with
    | None -> []
    | Some group -> group.pointers
  in
  let pointers =
    if List.length current_pointers >= 2
    then current_pointers
    else current_pointers @ [ pointer.id ]
  in
  let group = Group.create pointers shape ~get_position in
  Hashtbl.replace t.groups ~key:shape_id ~data:group

let shapes t  =
  let get_position = get_position t in
  let results = ref [] in
  Hashtbl.iter t.groups ~f:(fun ~key:shape_id ~data:group ->
    results := (shape_id, Group.shape group ~get_position) :: !results);
  !results

let move t pointers =
  List.iter pointers ~f:(fun (pointer : Pointer.t) ->
    Hashtbl.replace t.positions ~key:pointer.id ~data:pointer.position);
  shapes t

let remove t pointers =
  let get_position = get_position t in
  List.iter pointers ~f:(fun (pointer : Pointer.t) ->
    Hashtbl.remove t.positions pointer.id);
  let deleted_shapes = ref [] in
  Hashtbl.filter_map_inplace t.groups ~f:(fun ~key:shape_id ~data:group ->
    let pointers = List.filter group.pointers ~f:(Hashtbl.mem t.positions) in
    match pointers with
    | [] ->
      deleted_shapes := shape_id :: !deleted_shapes;
      None
    | pointers ->
      let shape = Group.shape group ~get_position in
      let group = Group.create pointers shape ~get_position in
      Some group);
  !deleted_shapes

