open! Base
open! Import

(** An applicative used to convert a [many] into a [mapper]. *)
type 'a t = 'a

val of_many : ('bt, 'a, 'b) Many.t -> access:('a -> 'b t) -> 'bt t
val of_nonempty : ('bt, 'a, 'b) Nonempty.t -> access:('a -> 'b t) -> 'bt t
