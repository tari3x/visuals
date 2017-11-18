(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Base
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
      (scale -. 1.) *. (Float.abs (Float.sin (Angle.to_radians t2.angle)))
    in
    let scale_delta_y =
      (scale -. 1.) *. (Float.abs (Float.cos (Angle.to_radians t2.angle)))
    in
    let sx = 1. +. scale_delta_x in
    let sy = 1. +. scale_delta_y in
    let move_by = Vector.(t2.center - t1.center) in
    Frame.(scale ~scale_x:sx ~scale_y:sy
           *> rotate rotation
           *> translate move_by)
end

(* One group of pointers per box. *)
module Group = struct
  type t =
    { initial_frame : Frame.t
    ; initial_params : Params.t
    (* In order of appearance *)
    ; pointers : Pointer_id.t list
    }

  let params pointers frame ~get_position_exn =
    let positions = List.map pointers ~f:get_position_exn in
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
      let angle = Vector.dir delta in
      { Params. center; distance; angle  }

  let create pointers frame ~get_position_exn =
    { initial_frame = frame
    ; initial_params = params pointers frame ~get_position_exn
    ; pointers
    }

  let frame t ~get_position_exn =
    let current = params t.pointers t.initial_frame ~get_position_exn in
    let frame_diff =
      Params.frame_diff ~initial:t.initial_params ~current
    in
    Frame.(t.initial_frame *> frame_diff)

  let positions t ~get_position_exn =
    List.map t.pointers ~f:get_position_exn
end

type t =
  { groups : Group.t Hashtbl.M(Box_id).t
  ; positions : Vector.t Hashtbl.M(Pointer_id).t
  }

let create () =
  let groups = Hashtbl.create (module Box_id) () in
  let positions = Hashtbl.create (module Pointer_id) () in
  { groups; positions }

let active t =
  Hashtbl.keys t.groups

let get_position_exn t =
  Hashtbl.find_exn t.positions

let is_touching t box_id =
  Hashtbl.mem t.groups box_id

module Update = struct
  module Single = struct
    type t =
      { frame : Frame.t
      ; touches : Vector.t list
      }

    let apply {frame; touches } box =
      box
      |> Box.set ~frame
      |> Box.set_touches ~touches ~coordinates:`canvas
  end

  type t = (Box_id.t * Single.t option) list
end

(* This does not include deleted boxs. *)
let update t =
  let get_position_exn = get_position_exn t in
  let results = ref [] in
  Hashtbl.iteri t.groups ~f:(fun ~key:box_id ~data:group ->
    let single =
      { Update.Single.
        frame = Group.frame group ~get_position_exn
      ; touches = Group.positions group ~get_position_exn
      }
    in
    results := (box_id, Some single) :: !results);
  !results

let add t box_id box (pointer : Pointer.t) =
  let get_position_exn = get_position_exn t in
  Hashtbl.set t.positions ~key:pointer.id ~data:pointer.position;
  let current_pointers =
    match Hashtbl.find t.groups box_id with
    | None -> []
    | Some group -> group.pointers
  in
  let pointers = current_pointers @ [ pointer.id ] in
  let group = Group.create pointers box ~get_position_exn in
  Hashtbl.set t.groups ~key:box_id ~data:group;
  update t

let move t pointers =
  List.iter pointers ~f:(fun (pointer : Pointer.t) ->
    Hashtbl.set t.positions ~key:pointer.id ~data:pointer.position);
  update t

let remove t pointers_to_remove =
  let pointers_to_remove = List.map pointers_to_remove ~f:Pointer.id in
  let get_position_exn = get_position_exn t in
  let deleted_boxs = ref [] in
  (* Don't delete the pointers yet, since we use the prior pointer state to
     recompute the boxs. *)
  Hashtbl.filter_mapi_inplace t.groups ~f:(fun ~key:box_id ~data:group ->
    let pointers =
      List.diff group.pointers pointers_to_remove ~equal:Pointer_id.equal
    in
    match pointers with
    | [] ->
      deleted_boxs := box_id :: !deleted_boxs;
      None
    | pointers ->
      let frame = Group.frame group ~get_position_exn in
      let group = Group.create pointers frame ~get_position_exn in
      Some group);
  List.iter pointers_to_remove ~f:(Hashtbl.remove t.positions);
  List.map !deleted_boxs ~f:(fun box_id -> (box_id, None))
