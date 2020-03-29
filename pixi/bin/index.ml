(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Lwt
open Js_of_ocaml
open Visuals
open Pixi_lib

let main () =
  let module A = Application in
  let module G = Graphics in
  let module C = Container in
  let app = A.create () in
  let (_ : Dom.node Js.t) =
    Html.document##.body##appendChild (A.view app :> Dom.node Js.t)
  in
  let g = G.create () in
  C.add_child (A.stage app) (g :> DisplayObject.t);
  let color1 = Color.create 150 100 0 in
  let color2 = Color.create 0 100 200 in
  G.begin_fill g ~color:color2 ();
  G.line_style g ~color:color1 ~width:10. ();
  G.draw_circle g ~x:250. ~y:250. ~radius:100.;
  G.end_fill g;
  return ()
;;

let () = top_level (fun () -> main ())
