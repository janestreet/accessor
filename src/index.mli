open! Base
open! Import

(** A stack of indices accumulated during traversal of a data structure. *)
type 'a t =
  | [] : unit t
  | ( :: ) : 'a * 'b t -> ('a * 'b) t
[@@deriving sexp_of]

val hd : ('hd * _) t -> 'hd
val tl : (_ * 'tl) t -> 'tl t
