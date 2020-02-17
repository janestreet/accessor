open! Base
open! Import

(** [t] is an appendable type for defining [nonempty_getter] accessors. See the
    documentation of [Accessor.nonempty_getter] for more information. *)
type +'a t

include Monad.S with type 'a t := 'a t

(** [access a] "accesses" [a]. *)
val access : 'a -> 'a t

(** [append a b] accesses everything [a] accesses, then accesses everything [b] accesses.
*)
val append : 'a t -> 'a t -> 'a t

module O : sig
  val ( @ ) : 'a t -> 'a t -> 'a t
end

include module type of O

val map_reduce : 'a t -> combine:('r -> 'r -> 'r) -> f:('a -> 'r) -> 'r
val of_nonempty : (_, 'a, _) Nonempty.t -> 'a t
