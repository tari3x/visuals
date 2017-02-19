(*
  Copyright (c) Mihhail Aizatulin (avatar@hot.ee).
  This file is distributed under a BSD license.
  See LICENSE file for copyright notice.
*)

open Common

(* XCR: after a while it stops responding after reload and rejects new clients *)

(* CR: on some phones touch doesn't work *)

(* CR; on Chris' phone it keeps reloading in landscape mode.
   Have a look at
   http://stackoverflow.com/questions/1649086/detect-rotation-of-android-phone-in-the-browser-with-javascript
*)

(* CR; Chris said it would be nice to control the decay. *)

;;

top_level Gui_light.main
