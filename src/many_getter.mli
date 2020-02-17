open! Base
open! Import

(** [t] is an appendable type for defining [many_getter] accessors. See the documentation
    of [Accessor.many_getter] for more information. *)
type +'a t

include Monad.S with type 'a t := 'a t

(** [access a] "accesses" [a]. *)
val access : 'a -> 'a t

(** Don't access anything. *)
val empty : _ t

(** [append a b] accesses everything [a] accesses, then accesses everything [b] accesses.
*)
val append : 'a t -> 'a t -> 'a t

(** Append all the [t]s in a list together. *)
val of_list : 'a t list -> 'a t

module O : sig
  val ( @ ) : 'a t -> 'a t -> 'a t
end

include module type of O

val map_reduce : 'a t -> empty:'r -> combine:('r -> 'r -> 'r) -> f:('a -> 'r) -> 'r
val of_many : (_, 'a, _) Many.t -> 'a t
val of_nonempty : (_, 'a, _) Nonempty.t -> 'a t
val of_nonempty_getter : 'a Nonempty_getter.t -> 'a t
