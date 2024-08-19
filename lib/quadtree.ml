open Core
open Geometry
include Quadtree_lib

let create ?max_depth ?(slice_size = 5) rect =
  let open Rectangle in
  let rect = x1 rect, x2 rect, y1 rect, y2 rect in
  init ~rect ?depth:max_depth slice_size
;;

let insert t v a =
  let open Vector in
  insert_tailrec t (x v, y v) a
;;

let remove t v a =
  let open Vector in
  remove t (x v, y v) a
;;

let fold_rect t rect ~init ~f =
  let open Rectangle in
  let rect = x1 rect, x2 rect, y1 rect, y2 rect in
  let f b _rect a = f b a in
  fold t rect f init
;;

let of_list ?max_depth ?slice_size elts =
  let vs = List.map elts ~f:fst in
  let rect = Rectangle.create_bounding vs in
  let t = create ?max_depth ?slice_size rect in
  List.iter elts ~f:(fun (v, x) -> insert t v x);
  t
;;
