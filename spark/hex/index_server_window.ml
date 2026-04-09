(*
   Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
   This file is distributed under a BSD license.
   See LICENSE file for copyright notice.
*)

open Std_internal

let config =
  let open Config in
  { Local_config.config with crop_top = 256.; crop_bottom = 822. }
;;

let () = top_level (fun () -> Server.main config)
