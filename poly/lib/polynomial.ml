open Core
open Async
open Common
module E = Maxima.Expr
module V = Vector.Float
module Matrix = Maxima.Matrix

let _debug a = debug_s ~enabled:false a

(* CR-someday: this will become [Atom] which is either variable with index or
   arbitrary string. *)
module Var = struct
  include String

  let create n = E.(var n |> to_string)
  let x = create 1
  let y = create 2
end

(* CR-someday: massively inefficient. Just specialize to 2D. *)
module Args = struct
  type t = float Var.Map.t

  let create values : t =
    List.mapi values ~f:(fun i v -> Var.create Int.(i + 1), v)
    |> Var.Map.of_alist_exn
  ;;

  let find_exn = Map.find_exn
end

module Mono = struct
  module T = struct
    type t = (Var.t * int) list [@@deriving sexp, compare, hash]
  end

  include T

  let one : t = []
  let is_one = List.is_empty
  let var v : t = [ v, 1 ]

  let normalize t =
    Var.Map.of_alist_multi t
    |> Map.to_alist
    |> List.map ~f:(fun (var, powers) -> var, Int.sum powers)
  ;;

  (* Going via map keeps variables sorted. *)
  let ( * ) (t1 : t) (t2 : t) : t = normalize (t1 @ t2)

  let pow t n =
    if n = 0 then one else List.map t ~f:(fun (v, m) -> v, Int.(n * m))
  ;;

  let to_maxima t =
    List.map t ~f:(fun (v, n) -> E.(pow (of_string v) n)) |> E.product
  ;;

  let eval (t : t) (args : Args.t) =
    List.map t ~f:(fun (v, n) -> Float.int_pow (Args.find_exn args v) n)
    |> Float.product
  ;;

  let all_of_degree n =
    List.init
      Int.(n + 1)
      ~f:(fun i ->
        let j = Int.(n - i) in
        pow (var Var.x) i * pow (var Var.y) j)
  ;;

  let all ~degree:n =
    List.init
      Int.(n + 1)
      ~f:(fun i ->
        List.init
          Int.(n + 1)
          ~f:(fun j ->
            if Int.(i + j > n)
            then None
            else Some (pow (var Var.x) i * pow (var Var.y) j))
        |> List.filter_opt)
    |> List.concat
  ;;

  (*
    let all ~degree:n =
    List.init Int.(n + 1) ~f:all_of_degree
    |> List.concat
  *)

  let first n =
    let rec aux ~degree n =
      let ts = all_of_degree degree in
      let nd = List.length ts in
      if n <= nd
      then List.take ts ~n
      else ts @ aux ~degree:(degree + 1) (n - nd)
    in
    aux ~degree:0 n
  ;;

  include Comparable.Make (T)
  include Hashable.Make (T)
end

module T = struct
  type t = (Mono.t * float) list [@@deriving compare, sexp]
end

include T

let zero = []
let const x : t = [ Mono.one, x ]
let mono m = [ m, 1. ]
let var n = mono (Mono.var n)

let group_sum t =
  Mono.Map.of_alist_multi t
  |> Map.to_alist
  |> List.filter_map ~f:(fun (mono, coeffs) ->
         let c = Float.sum coeffs in
         if Float.(c = 0.) then None else Some (mono, c))
;;

let normalize (t : t) : t =
  List.map t ~f:(fun (m, c) -> Mono.normalize m, c) |> group_sum
;;

let monomials (t : t) =
  normalize t |> List.map ~f:(fun (m, c) -> mono m, c)
;;

let ( + ) (t1 : t) (t2 : t) : t = group_sum (t1 @ t2)

let ( * ) (t1 : t) (t2 : t) : t =
  List.cartesian_product t1 t2
  |> List.map ~f:(fun ((m1, c1), (m2, c2)) ->
         Mono.(m1 * m2), Float.(c1 * c2))
  |> group_sum
;;

let product = function
  | [] -> const 1.
  | ts -> List.reduce_exn ts ~f:( * )
;;

let sum = function
  | [] -> zero
  | ts -> List.reduce_exn ts ~f:( + )
;;

(* CR-someday: make efficient. *)
let pow t n = List.init n ~f:(fun _ -> t) |> product
let scale t ~by:x = product [ const x; t ]
let ( - ) t1 t2 = sum [ t1; scale t2 ~by:(-1.) ]
let var_x = var Var.x
let var_y = var Var.y
let x = var_x
let y = var_y

let subst t ~var ~by:t' =
  let mono_subst (m : Mono.t) =
    List.map m ~f:(fun (v, n) ->
        if Var.(v = var) then pow t' n else mono [ v, n ])
    |> product
  in
  List.map t ~f:(fun (mono, c) -> const c * mono_subst mono) |> sum
;;

let to_maxima (t : t) =
  List.map t ~f:(fun (m, c) ->
      if Mono.is_one m
      then E.const c
      else (
        let m = Mono.to_maxima m in
        if Float.(c = 1.) then m else E.(const c * m)))
  |> E.sum
;;

let to_string t = to_maxima t |> E.to_string

let zero_line_between_two_points (x1, y1) (x2, y2) =
  if Float.(x2 = x1)
  then var_x - const x1
  else (
    let slope = Float.((y2 - y1) / (x2 - x1)) in
    var_y - const y1 - (const slope * (var_x - const x1)))
;;

let eval (t : t) (args : float list) =
  let args = Args.create args in
  List.map t ~f:(fun (m, c) -> Float.(c * Mono.eval m args)) |> Float.sum
;;

let eval_point (t : t) (x, y) = eval t [ x; y ]

(* 2D only *)
let first_monomials n = Mono.first n |> List.map ~f:mono

let distance (t1 : t) (t2 : t) =
  List.map2_exn t1 t2 ~f:(fun (_, w1) (_, w2) -> Float.(abs (w1 - w2)))
  |> Float.sum
;;

let weighted_average t1 t2 ~w =
  scale t1 ~by:Float.(1. - w) + scale t2 ~by:w
;;

module Basis = struct
  type nonrec t = t list

  let mono ~degree =
    let monos = Mono.all ~degree |> List.map ~f:mono in
    const 500. :: List.drop monos 1
  ;;

  (* "The Multivariate Bernstein Basis Polynomials and Their Kernels" by
     K. Jetter. *)
  let bernstein ~degree:n ~domain:((x1, y1), (x2, y2)) =
    let choose n i j k =
      Int.(factorial n / (factorial i * factorial j * factorial k))
      |> float
    in
    let make i j k =
      const (choose n i j k)
      * pow (const 1. - var_x - var_y) i
      * pow var_x j
      * pow var_y k
    in
    List.init
      Int.(n + 1)
      ~f:(fun i ->
        List.init
          Int.(n + 1)
          ~f:(fun j ->
            List.init
              Int.(n + 1)
              ~f:(fun k ->
                if Int.(i + j + k <> n) then [] else [ make i j k ])
            |> List.concat)
        |> List.concat)
    |> List.concat
    |> List.map ~f:(fun t ->
           t
           |> subst
                ~var:(Var.create 1)
                ~by:(scale (var_x - const x1) ~by:Float.(1. / (x2 - x1)))
           |> subst
                ~var:(Var.create 2)
                ~by:(scale (var_y - const y1) ~by:Float.(1. / (y2 - y1))))
  ;;

  let odd_powers_only (t : t) =
    List.map t ~f:(fun t ->
        List.map t ~f:(fun (m, x) ->
            let m = List.filter m ~f:(fun (_, n) -> n % 2 = 1) in
            m, x)
        |> normalize)
  ;;
end

module Parse = struct
  open Angstrom

  (* CR-someday: there's also lisp output mode using [save] instead of [stringout]
     but it is full of extra trash that one would need to sanitize. *)

  let chainl1 op e =
    let rec go acc =
      lift2 (fun f x -> f acc x) op e >>= go <|> return acc
    in
    e >>= fun init -> go init
  ;;

  let parens p = char '(' *> p <* char ')'
  let add = char '+' *> return ( + )
  let sub = char '-' *> return ( - )
  let mul = char '*' *> return ( * )
  let power : unit t = char '^' *> return ()

  let integer =
    take_while1 (function
        | '0' .. '9' -> true
        | _ -> false)
    >>| Int.of_string
  ;;

  (* CR-someday: figure out how to turn off scientific notation in maxima *)
  let number =
    let float =
      take_while1 (function
          | '0' .. '9' | '.' -> true
          | _ -> false)
    in
    let exponent =
      take_while1 (function
          | 'E' -> true
          | _ -> false)
      >>= fun s1 ->
      take_while1 (function
          | '-' | '+' -> true
          | _ -> false)
      >>= fun s2 -> float >>= fun s3 -> return (s1 ^ s2 ^ s3)
    in
    float
    >>= fun s1 ->
    option "" exponent
    >>= fun s2 -> return (const (Float.of_string (s1 ^ s2)))
  ;;

  let variable =
    take_while Char.is_alpha
    >>= fun v ->
    if String.is_empty v then fail "empty var" else return (var v)
  ;;

  let power = power >>= fun () -> integer

  let pow_var =
    variable >>= fun v -> power <|> return 1 >>= fun n -> return (pow v n)
  ;;

  let neg expr =
    sub >>= fun _ -> expr >>= fun t -> return (scale t ~by:(-1.))
  ;;

  let expr =
    fix (fun expr ->
        let factor = parens expr <|> number <|> pow_var in
        let term = chainl1 mul factor in
        chainl1 (add <|> sub) (term <|> neg term))
    >>= fun result -> end_of_input >>= fun () -> return result
  ;;

  let parse_string_exn t s =
    match parse_string t s ~consume:All with
    | Ok v -> v
    | Error msg -> raise_s [%message s msg]
  ;;

  let%expect_test _ =
    let t = parse_string_exn expr "(-y^2)-(3E+1)*y*x^4+1" in
    printf !"%{sexp:t}" t;
    [%expect {| ((() 1) (((x 4) (y 1)) -30) (((y 2)) -1)) |}]
  ;;
end

let of_maxima s =
  let open Parse in
  parse_string_exn expr s
;;

(* quotient and remainder *)
module Division_result = struct
  type nonrec t =
    { q : t
    ; r : t
    }
end

let divide t1 t2 =
  let%bind { E.Division_result.q; r } =
    E.divide (to_maxima t1) (to_maxima t2)
  in
  let q = of_maxima (E.to_string q) in
  let r = of_maxima (E.to_string r) in
  return { Division_result.q; r }
;;

let is_roughly_zero t = List.for_all t ~f:(fun (_, x) -> Float.(x < 10E-9))

include Comparable.Make (T)
