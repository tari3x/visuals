type t =
  | All of Spark.Ctl.t
  | List of Spark.Ctl.t Spark.Id.Table.t
[@@deriving sexp, variants]
