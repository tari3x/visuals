open Core
open Bigarray
open Tgl4

let ok_exn = function
  | Error (`Msg e) -> failwith e
  | Ok x -> x
;;

let bigarray_create k len = Array1.create k C_layout len
let create_uint32_array n = Array1.create Int32 C_layout n

let get_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f ->
    f a;
    Int32.to_int_exn a.{0}
;;

(*
let set_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f i ->
    a.{0} <- Int32.of_int_exn i;
    f a
;;
*)

let check_errors desc =
  let e = Gl.get_error () in
  if e <> Gl.no_error
  then
    (* let s = Glu.error_string e in *)
    failwithf "OpenGL error in \"%s\" (%d)\n" desc e ()
;;

let get_log ~f =
  let log = Array1.create Char C_layout 10240 in
  let length = create_uint32_array 1 in
  f 10240 (Some length) log;
  for i = 0 to Array1.get length 0 |> Int32.to_int_exn do
    printf "%c" (Array1.get log i)
  done
;;

let get_shader_compile_log ~shader =
  get_log ~f:(Gl.get_shader_info_log shader)
;;

let get_program_info_log ~prog = get_log ~f:(Gl.get_program_info_log prog)
