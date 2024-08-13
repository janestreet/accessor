open! Base
open! Import

(** An [(a, x, y) t] is an applicative for defining [many] accessors. See the
    documentation of [Accessor.many] for more information.

    A value of type [(a, x, y) t] can make the following claim:
    I can give you some number of [x]es and if you tell me how to replace each of them
    with a [y], I will also give you an [a].
*)
type ('a, +'x, 'y) t

module Accessed : Monad.S_indexed with type ('x, 'a, 'y) t := ('a, 'x, 'y) t

(** [access a] "accesses" [a] and returns the value you are expected replace it with. *)
val access : 'a -> ('b, 'a, 'b) t

val of_nonempty : ('a, 'x, 'y) Nonempty.t -> ('a, 'x, 'y) t

module Of_applicative (A : sig
    type 'a t

    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
  end) : sig
  val of_many : ('bt, 'a, 'b) t -> access:('a -> 'b A.t) -> 'bt A.t
end

module Of_applicative2 (A : sig
    type ('a, 'e) t

    val return : 'a -> ('a, 'e) t
    val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
    val apply : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
  end) : sig
  val of_many : ('bt, 'a, 'b) t -> access:('a -> ('b, 'e) A.t) -> ('bt, 'e) A.t
end

module Of_applicative3 (A : sig
    type ('a, 'd, 'e) t

    val return : 'a -> ('a, _, _) t
    val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
    val apply : ('a -> 'b, 'd, 'e) t -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t
  end) : sig
  val of_many : ('bt, 'a, 'b) t -> access:('a -> ('b, 'd, 'e) A.t) -> ('bt, 'd, 'e) A.t
end

module Open_on_rhs_intf : sig
  module type S = sig
    val access : 'a -> ('b, 'a, 'b) t
  end
end

include Applicative.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

include
  Applicative.Let_syntax3
  with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  with module Open_on_rhs_intf := Open_on_rhs_intf
