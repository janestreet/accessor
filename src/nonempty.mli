open! Base
open! Import

(** An [(a, x, y) t] is an applicative for defining [nonempty] accessors. See the
    documentation of [Accessor.nonempty] for more information. *)
type ('a, +'x, 'y) t

include
  Applicative_without_return.S3_without_let_syntax
  with type ('a, 'x, 'y) t := ('a, 'x, 'y) t

module Let_syntax : sig
  val map : ('a, 'e, 'f) t -> f:('a -> 'b) -> ('b, 'e, 'f) t
  val both : ('a, 'e, 'f) t -> ('b, 'e, 'f) t -> ('a * 'b, 'e, 'f) t

  module Let_syntax : sig
    val map : ('a, 'e, 'f) t -> f:('a -> 'b) -> ('b, 'e, 'f) t
    val both : ('a, 'e, 'f) t -> ('b, 'e, 'f) t -> ('a * 'b, 'e, 'f) t

    module Open_on_rhs : sig
      val access : 'a -> ('b, 'a, 'b) t
    end
  end
end

module Accessed : Monad.S_indexed with type ('a, 'bt, 'b) t := ('bt, 'a, 'b) t

(** [access a] "accesses" [a] and returns the value you are expected replace it with. *)
val access : 'a -> ('b, 'a, 'b) t

module Of_applicative_without_return (A : sig
    type 'a t

    val map : 'a t -> f:('a -> 'b) -> 'b t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
  end) : sig
  val of_nonempty : ('bt, 'a, 'b) t -> access:('a -> 'b A.t) -> 'bt A.t
end

module Of_applicative_without_return2 (A : sig
    type ('a, 'e) t

    val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
    val apply : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
  end) : sig
  val of_nonempty : ('bt, 'a, 'b) t -> access:('a -> ('b, 'e) A.t) -> ('bt, 'e) A.t
end

module Of_applicative_without_return3 (A : sig
    type ('a, 'e, 'f) t

    val map : ('a, 'e, 'f) t -> f:('a -> 'b) -> ('b, 'e, 'f) t
    val apply : ('a -> 'b, 'e, 'f) t -> ('a, 'e, 'f) t -> ('b, 'e, 'f) t
  end) : sig
  val of_nonempty
    :  ('bt, 'a, 'b) t
    -> access:('a -> ('b, 'e, 'f) A.t)
    -> ('bt, 'e, 'f) A.t
end
