open Core

module V = Vector.Float

module Style = struct
  type t = [ `zeroes | `heat ]
  [@@deriving sexp]
end

type t =
  { domain_size : float * float
  ; image_size : float * float
  ; grid_size: int * int
  ; style : Style.t
  ; cbrange : float * float
  } [@@deriving sexp, fields]

let create
    ~grid_size
    ?(image_width = 500)
    ~cbrange
    ?(style = `heat)
    () =
  (* Point range is [0; n - 1] *)
  let grid_x, grid_y = grid_size in
  let domain_x = float (grid_x - 1) in
  let domain_y = float (grid_y - 1) in
  let domain_size = (domain_x, domain_y) in
  let image_x = float image_width in
  let image_y = Float.(image_x * (domain_y / domain_x)) in
  let image_size = image_x, image_y in
  { domain_size
  ; grid_size
  ; image_size
  ; style
  ; cbrange
  }

let image_size t =
  Vector.Int.of_float t.image_size

let domain_to_image t (x, y) =
  let open Float in
  let image_x, image_y = t.image_size in
  let domain_x, domain_y = t.domain_size in
  let i = image_x * (x / domain_x) in
  let j = image_y * (y / domain_y) in
  int_of_float i, int_of_float j

let image_to_domain t (i, j) =
  let open Float in
  let image_x, image_y = t.image_size in
  let domain_x, domain_y = t.domain_size in
  let i = float i in
  let j = float j in
  let x = domain_x * (i / image_x) in
  let y = domain_y * (j / image_y) in
  x, y

let domain_centre t =
  let open Float in
  let domain_x, domain_y = domain_size t in
  domain_x / 2., domain_y / 2.

let domain t =
  let domain_x, domain_y = domain_size t in
  (0., 0.), (domain_x, domain_y)

module For_tests = struct
  let t =
    create
      ~grid_size:(10, 10)
      ~image_width:500
      ~cbrange:(0., 0.)
      ()

  let%expect_test _ =
    printf !"%{sexp:t}" t;
    [%expect {|
      ((domain_size (9 9)) (image_size (500 500)) (grid_size (10 10)) (style heat)
       (cbrange (0 0))) |}]

  let%test_unit _ =
    let v1 = (134, 25) in
    let v2 =
      image_to_domain t v1
      |> domain_to_image t
    in
    [%test_result: Vector.Int.t] v2 ~expect:v1
end

