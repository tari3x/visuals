open Lwt
open Js
open Common

module P = Point

let get_markers ctx =
  let markers = ref [] in
  let set_marker ev =
    (* CR: my other code uses offsetX and offsetY. *)
    let x = ev##.clientX and y = ev##.clientY in
    let p = P.create x y in
    markers := !markers @ [ p ];
  in
  add_event_listener ctx##.canvas Html.Event.mousedown ~f:set_marker;
  let rec loop () =
    Lwt_js.sleep 0.2
    >>= fun () ->
    match !markers with
    | [ u0; u1; u2; u3 ] -> Lwt.return (u0, u1, u2, u3)
    | _ -> loop ()
  in
  loop ()

let calibrate video ctx =
  (* zero is top-left corner *)
  let v0 = P.create 400 200 in
  let v1 = P.create 400 400 in
  let v2 = P.create 600 200 in
  let v3 = P.create 600 400 in
  fill_all ctx "black";
  ctx##.fillStyle = (string "white");
  plot_point ctx v0 1.;
  plot_point ctx v1 1.;
  plot_point ctx v2 1.;
  plot_point ctx v3 1.;
  get_markers ctx
  >>= fun (u0, u1, u2, u3) ->
  let open Point in
  (* "Fundamentals of Texture Mapping and Image Warping",
     "Inferring Projective Mappings" *)
  let a =
    [| [| (x v0); (y v0); 1.; 0.; 0.; 0.; -(x v0) * (x u0); -(y v0) * (x u0) |];
       [| (x v1); (y v1); 1.; 0.; 0.; 0.; -(x v1) * (x u1); -(y v1) * (x u1) |];
       [| (x v2); (y v2); 1.; 0.; 0.; 0.; -(x v2) * (x u2); -(y v2) * (x u2) |];
       [| (x v3); (y v3); 1.; 0.; 0.; 0.; -(x v3) * (x u3); -(y v3) * (x u3) |];

       [| 0.; 0.; 0.; (x v0); (y v0); 1.; -(x v0) * (y u0); -(y v0) * (y u0) |];
       [| 0.; 0.; 0.; (x v1); (y v1); 1.; -(x v1) * (y u1); -(y v1) * (y u1) |];
       [| 0.; 0.; 0.; (x v2); (y v2); 1.; -(x v2) * (y u2); -(y v2) * (y u2) |];
       [| 0.; 0.; 0.; (x v3); (y v3); 1.; -(x v3) * (y u3); -(y v3) * (y u3) |]
    |]
  in
  let b = [| x u0; x u1; x u2; x u3; y u0; y u1; y u2; y u3 |] in
  let x = Math.flatten (Math.lusolve a b) in
  let m =
    [| [| x.(0); x.(3); x.(6) |];
       [| x.(1); x.(4); x.(7) |];
       [| x.(2); x.(5); 1 |]
    |]
  in
  Lwt.return (Math.inv m)
