
type t =
  { r : int
  ; g : int
  ; b : int
  ; a : float
  }

let create ~r ~g ~b ~a =
  { r; g; b; a }

let r t = t.r
let g t = t.g
let b t = t.b
let a t = t.a

let white =
  { r = 255
  ; g = 255
  ; b = 255
  ; a = 1.
  }

(*
let wave ~cycle time =
  ((sin (time *. cycle) +. 1.) /. 2.) *. 255. |> int

let of_time time =
  let r = wave ~cycle:1. time in
  let g = wave ~cycle:2. time in
  let b = wave ~cycle:3. time in
  let a = 0.7 in
  { r; g; b; a }
*)

let to_string { r; g; b; a } =
  Printf.sprintf "rgba(%d, %d, %d, %f)" r g b a

let of_hex8_string s =
  let get n =
    Printf.sprintf "0X%s" (String.sub s n 2) |> int_of_string
  in
  { r = get 3
  ; g = get 5
  ; b = get 7
  ; a = (float_of_int (get 1)) /. 255.
  }

let random () =
  let r = Random.int 255 in
  let g = Random.int 255 in
  let b = Random.int 255 in
  let a = 1. in
  { r; g; b; a }

let interpolate t1 t2 fraction =
  let i n1 n2 =
    (1. -. fraction) *. (float_of_int n1) +. fraction *. (float_of_int n2)
    |> int_of_float
  in
  { r = i t1.r t2.r
  ; g = i t1.g t2.g
  ; b = i t1.b t2.b
  ; a = (1. -. fraction) *. t1.a +. fraction *. t2.a
  }

let set_alpha t ~alpha =
  { t with a = alpha }

