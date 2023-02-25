open Core

type t = All of Spark.Ctl.t | List of Spark.Ctl.t list
[@@deriving sexp, variants]
