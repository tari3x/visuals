open Lwt
open Js

module Html = Dom_html

let error f = Printf.ksprintf (fun s -> Firebug.console##(error (Js.string s))) f
let debug f = Printf.ksprintf (fun s -> Firebug.console##(log (Js.string s))) f
let alert f = Printf.ksprintf (fun s -> Html.window##(alert (Js.string s))) f
let failwithf f =
  Printf.ksprintf (fun s ->
    Firebug.console##(error (Js.string s));
    failwith s) f

let float = float_of_int
let int   = int_of_float

let lwt_wrap f =
  let (t, w) = Lwt.task () in
  let cont x = Lwt.wakeup w x in
  f cont;
  t

let pi =
  let pi = acos (- 1.0) in
  assert (pi >= 3.0 && pi <= 4.);
  pi

module Optdef = struct
  include Optdef

  let value_exn t =
    get t (fun () -> failwith "Optdef: undefined")
end

module Option = struct
  type 'a t = 'a option
  let iter t ~f =
    match t with
    | None -> ()
    | Some x -> f x
end

module List = struct
  include ListLabels

  let delete xs x =
    filter xs ~f:(fun x' -> x' <> x)

  let bring_to_front xs x =
    x :: delete xs x

  let maybe_find t ~f =
    try Some (find t ~f) with
    | Not_found -> None

  let init n ~f =
    let rec init i =
      if i = n then []
      else f i :: init (i + 1)
    in
    init 0

  let concat_map t ~f =
    map t ~f |> concat

  let mem xs x =
    mem ~set:xs x

  let filter_map t ~f =
    let rec loop = function
      | [] -> []
      | x :: xs ->
        match f x with
        | None -> loop xs
        | Some x -> x :: loop xs
    in
    loop t
end

module Array = struct
  include ArrayLabels
end

module type Id = sig
  type t
  val create : unit -> t
  val to_string : t -> string
end

module Hashtbl = struct
  include Hashtbl

  let create () =
    create 1000

  let iter t ~f =
    let f key data = f ~key ~data in
    iter f t

  let filter_map_inplace t ~f =
    let f key data = f ~key ~data in
    filter_map_inplace f t

      (*
  let find t key =
    try find t key with
    | Not_found ->
      let key : string = Obj.magic key in
      error "key %s not found" key;
      raise Not_found
      *)

  let maybe_find t key =
    try Some (find t key)
    with Not_found -> None

  let replace t ~key ~data =
    replace t key data

  let find_or_add t key ~default =
    match maybe_find t key with
    | Some data -> data
    | None ->
      let data = default () in
      replace t ~key ~data;
      data
end

module Id(M: sig val name : string end) : Id = struct
  type t = string

  let create () =
    Printf.sprintf "%s%d"
      M.name
      (Random.int 100_000_000)

  let to_string t = t
end

module Client_id = Id(struct let name = "Client_id" end)
module Shape_id  = Id(struct let name = "Shape_id" end)

module Fn = struct
  let flip f x y =
    f y x
end

(* CR: move this to [Dom_wrappers]. *)
let add_event_listener elt event ~f =
  Html.addEventListener elt event
    (Html.handler
       (fun ev ->
         begin
           try f ev with exn ->
             error "uncaught exn in handler: %s"
               (Printexc.to_string exn)
         end;
         Js._true))
    Js._true
  |> ignore

let get_element_by_id id coerce_to =
  Opt.get
    (Opt.bind ( Html.document##getElementById(string id) )
       coerce_to)
    (fun () -> failwithf "can't find element %s" id)

let load_image src =
  let img = Html.createImg Html.document in
  lwt_wrap
    (fun c ->
      img##.onload := Html.handler (fun _ -> c (); Js._false);
      img##.src := (string src))
  >>= fun () ->
  Lwt.return img
