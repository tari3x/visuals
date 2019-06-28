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
    } [@@deriving sexp]

  let of_poly ?(show_dots = []) p =
    { p; ps = []; dots = show_dots; defs = []}

  let product t =
    P.product (t.p :: t.ps)

  let collapse t =
    { p = product t
    ; ps = []
    ; dots = t.dots
    ; defs = t.defs
    }

  (* CR: make interpolation fast for camlimage. *)
  let interpolate_two t1 t2 =
    let num_steps = 110 in
    let p1 = product t1 in
    let p2 = product t2 in
    (* Offset it a bit so we are less likely to hit an exact zero in log *)
    let epsilon = P.const 0.0000001 in
    let p1 = P.(p1 + epsilon) in
    let p2 = P.(p2 + epsilon) in
    (*
       let f1 = make_fun_name () in
       let f2 = make_fun_name () in
    *)
    List.init num_steps ~f:(fun n ->
      let defs = [] in
      (*
      let defs =
        if n > 0 then []
        else [ (f1, p1); (f2, p2) ]
      in
      *)
      let alpha = float n /. float num_steps in
      let p = P.(scale p1 ~by:(1. -. alpha) + scale p2 ~by:alpha) in
      (*
         let p = P.(scale (call f1) (1. -. alpha) + scale (call f2) alpha) in
      *)
      { p; ps = t1.ps; dots = t1.dots; defs })

  let rec interpolate = function
    | [] -> []
    | [t] -> [t]
    | t1 :: t2 :: ts ->
      interpolate_two t1 t2 @ interpolate (t2 :: ts)

  let emerge t l =
    let { p;  ps; _ } = t in
    let%bind { D. q; r = _ } = E.divide (P.to_maxima p) (P.to_maxima l) in
    let q = P.verbatim (E.to_string q) in
    { t with
      p = q
      ; ps = l :: ps
    }
    |> return
end

type t =
  { config : Config.t
  ; states : State.t list
  } [@@deriving sexp]

let create ~config states  =
  { config; states }

