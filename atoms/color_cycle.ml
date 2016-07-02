open Common

type t =
  { colors : Color.t list
  ; length : float
  ; offset : float
  }

let const c =
  { colors = [ c ]
  ; length = 2.
  ; offset = 0.1
  }

let to_string { colors; length; offset } =
  let colors =
    List.map colors ~f:Color.to_string |> String.concat "; "
  in
  Printf.sprintf "{ \
     colors = %s \
   ; length = %f \
   ; offset = %f \
   }"
    colors length offset

let random () =
  let color1 = Color.random () in
  let color2 = Color.random () in
  let color3 = Color.random () in
  let color4 = Color.random () in
  { colors = [ color1; color2; color3; color4 ]
  ; length = 2.
  ; offset = 0.1
  }

let default =
  const Color.white

let current_color t ~time =
  (*
  debug "current_color %f, %s" time (to_string t);
  *)
  match t.colors with
  | [] -> Color.white
  | [ color ] -> color
  | colors ->
    (* Loop around smoothly. *)
    let colors = colors @ [ List.hd colors ] in
    let fraction_of_cycle =
      (mod_float (time -. t.offset *. t.length) t.length) /. t.length
    in
    let segment_length = 1. /. (float_of_int (List.length colors - 1)) in
    let fraction_of_segment =
      (mod_float fraction_of_cycle segment_length) /. segment_length
    in
    let segment_number =
      int_of_float (floor (fraction_of_cycle /. segment_length))
    in
    let c1 = List.nth colors segment_number in
    let c2 = List.nth colors (segment_number + 1) in
    Color.interpolate c1 c2 fraction_of_segment

let nth_defaulting_to_last_or_white t n =
  let rec loop n = function
    | [] -> Color.white
    | [ c ] -> c
    | c :: cs ->
      if n = 0 then c
      else loop (n - 1) cs
  in
  loop n t.colors

let set_alpha t ~alpha =
  { t with
    colors = List.map t.colors ~f:(Color.set_alpha ~alpha)
  }

