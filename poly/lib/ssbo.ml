open Core
open Gl_common
open Bigarray
open Tgl4

module type T = sig
  type t

  val glsl_declaration : t -> name:string -> string
end

type t =
  { id : int
  ; binding : int
  ; vec4 : bool
  ; glsl_type : string
  ; dims : int array
  }

let last_binding = ref 0

let glsl_type (type a b) ?(vec4 = false) (kind : (a, b) Bigarray.kind) =
  if vec4
  then (
    (* There's no Float64 color. *)
    match kind with
    | Float32 -> "vec4"
    | Float64
    | Int8_signed
    | Int8_unsigned
    | Int16_signed
    | Int16_unsigned
    | Int32
    | Int64
    | Int
    | Nativeint
    | Complex32
    | Complex64
    | Char -> raise_s [%message "array kind for color not supported"])
  else (
    match kind with
    | Float32 -> "float"
    | Float64 -> "double"
    | Int8_signed
    | Int8_unsigned
    | Int16_signed
    | Int16_unsigned
    | Int32
    | Int64
    | Int
    | Nativeint
    | Complex32
    | Complex64
    | Char -> raise_s [%message "array kind not supported"])
;;

let update t data =
  let dims = Genarray.dims data in
  [%test_result: int array] dims ~expect:t.dims;
  let num_elts = Array.reduce_exn dims ~f:( * ) in
  let data_type = glsl_type ~vec4:t.vec4 (Genarray.kind data) in
  if String.(t.glsl_type <> data_type)
  then
    raise_s
      [%message
        "mismatching kinds" (t.glsl_type : string) (data_type : string)];
  let data = reshape data [| num_elts |] |> Bigarray.array1_of_genarray in
  Gl.named_buffer_data t.id (Array1.size_in_bytes data) data Gl.static_draw
;;

let create_empty ?(vec4 = false) kind ~dims =
  let binding = !last_binding in
  incr last_binding;
  let id = get_int (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.shader_storage_buffer id;
  Gl.bind_buffer_base Gl.shader_storage_buffer binding id;
  let glsl_type = glsl_type ~vec4 kind in
  { id; binding; glsl_type; dims; vec4 }
;;

let create ?vec4 data =
  let kind = Genarray.kind data in
  let dims = Genarray.dims data in
  let t = create_empty ?vec4 kind ~dims in
  update t data;
  t
;;

let glsl_declaration t ~name =
  let dims =
    Array.to_list t.dims
    |> List.map ~f:(sprintf "[%d]")
    |> String.concat ~sep:""
  in
  sprintf
    {|
          layout(std430, binding = %d) buffer %s
            {
%s %s%s;
            };
          |}
    t.binding
    (String.uppercase name)
    t.glsl_type
    name
    dims
;;

module Scalar = struct
  type nonrec t = t

  let create_empty kind = create_empty kind

  let update1 t data =
    let data = Bigarray.genarray_of_array1 data in
    update t data
  ;;

  let create1 data =
    let data = Bigarray.genarray_of_array1 data in
    create data
  ;;

  let update2 t data =
    let data = Bigarray.genarray_of_array2 data in
    update t data
  ;;

  let create2 data =
    let data = Bigarray.genarray_of_array2 data in
    create data
  ;;

  let update3 t data =
    let data = Bigarray.genarray_of_array3 data in
    update t data
  ;;

  let create3 data =
    let data = Bigarray.genarray_of_array3 data in
    create data
  ;;

  let glsl_declaration = glsl_declaration
end

module Color = struct
  type nonrec t = t

  let color_data (colors : Color.t array) =
    let num_colors = Array.length colors in
    let data = Array1.create Float32 C_layout (num_colors * 4) in
    for i = 0 to num_colors - 1 do
      let color = colors.(i) in
      let i = i * 4 in
      Array1.set data (i + 0) color.r;
      Array1.set data (i + 1) color.g;
      Array1.set data (i + 2) color.b;
      Array1.set data (i + 3) 1.
    done;
    Bigarray.genarray_of_array1 data
  ;;

  let create_empty ~length =
    create_empty ~vec4:true Float32 ~dims:[| length * 4 |]
  ;;

  let create colors =
    let data = color_data colors in
    create ~vec4:true data
  ;;

  let update t colors =
    let data = color_data colors in
    update t data
  ;;

  let glsl_declaration = glsl_declaration
end
