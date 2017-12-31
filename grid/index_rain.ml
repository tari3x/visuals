(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Std_internal

;;

top_level (fun () ->
  Gui_client.main (Box.create ~kind:Grid.Ctl.Rain_control ()))
