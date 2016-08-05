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
    List.map colors ~f:Color.to_string |> String.concat ~sep:"; "
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

let random_constant () =
  let color = Color.random () in
  debug "%s" (Color.to_string color);
  let color1 = color in
  let color2 = color in
  let color3 = color in
  let color4 = color in
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
  (* Loop around smoothly. *)
  let colors =
    match t.colors with
    | [] -> []
    | (c :: _) as cs -> cs @ [ c ]
  in
  let fraction =
    (mod_float (time -. t.offset *. t.length) t.length) /. t.length
  in
  Color.interpolate colors fraction

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

