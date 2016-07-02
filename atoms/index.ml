open Common

;;
Html.window##.onload := Html.handler (Main.go ~is_server:false)
