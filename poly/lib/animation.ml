open Core
open Async

module P = Polynomial
module E = Maxima.Expr
module D = E.Division_result
module V = Vector.Float

let fun_id = ref 0

let _make_fun_name () =
  incr fun_id;
  sprintf "f%d" !fun_id

module State = struct
  (* animated and static parts *)
  type t =
    { p : P.t
    ; ps : P.t list
    ; defs : (string * P.t) list
    ; dots : V.t list
    ; palette : Palette.Basis.t
    } [@@deriving sexp]

  let with_palette t ~palette =
    { t with palette }

  let of_poly ?(show_dots = []) p =
    { p; ps = []; dots = show_dots; defs = []
    ; palette = Palette.Basis.rgb
    }

  let product t =
    P.product (t.p :: t.ps)

  let collapse t =
    { p = product t
    ; ps = []
    ; dots = t.dots
    ; defs = t.defs
    ; palette = t.palette
    }

  let map t ~f =
    { t with p = f t.p }

  (* CR: make interpolation fast for camlimage. *)
  let interpolate_two t1 t2 =
    let num_steps = 110 in
    let p1 = product t1 in
    let p2 = product t2 in
    (* Offset it a bit so we are less likely to hit an exact zero in log *)
    let epsilon = P.const 0.0000001 in
    let p1 = P.(p1 + epsilon) in
    let p2 = P.(p2 + epsilon) in
    List.init num_steps ~f:(fun n ->
      let defs = [] in
      let alpha = float n /. float num_steps in
      let p = P.(scale p1 ~by:(1. -. alpha) + scale p2 ~by:alpha) in
      let ps =
        match t1.ps, t2.ps with
        | [], [] -> []
        | _ -> raise_s [%message "collapse before interpolating!"]
      in
      { p; ps; dots = t1.dots; defs
      ; palette = t1.palette
      })

  let rec interpolate = function
    | [] -> []
    | [t] -> [t]
    | t1 :: t2 :: ts ->
      interpolate_two t1 t2 @ interpolate (t2 :: ts)

  let interpolate ts =
    match ts with
    | [] | [_] -> ts
    | t :: _ -> t :: interpolate ts

  let emerge t (l : Line.t) =
    let open P in
    let { p;  ps; _ } = t in
    let l = Line.poly l in
    let%bind { P.Division_result. q; r } = P.divide p l in
    if not (P.is_roughly_zero r)
    then raise_s [%message "expect zero remainder" (r : t)];
    return { t with p = q; ps = l :: ps }

  let%expect_test _ =
    let z = P.zero_line_between_two_points in
    let l = (0., 1.), (1., 0.) in
    let t =
      P.product
        [ z (0., 0.) (1., 2.)
        ; z (0., 1.) (2., 2.)
        ; z (0., 2.) (3., 0.)
        ; z (3., 2.) (2., 0.)
        ; z (3., 1.) (1., 0.)
        ]
      |> Line.deform_to_zero l
      |> of_poly
    in
    let%bind t = emerge t l in
    ignore t;
    [%expect {| |}]

  let suspend_end ?(num_frames = 20) ts =
    let t = List.last_exn ts in
    ts @ (List.init num_frames ~f:(fun _ -> t))
end

type t =
  { config : Config.t
  ; states : State.t list
  } [@@deriving sexp]

let create ~config states  =
  { config; states }
