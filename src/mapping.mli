open! Base
open! Import

(** An [(i -> a -> b, w) t] is some mapping from [i] and [a] to [b]. [w] determines what
    kind of mapping it is. *)
type (_, _) t

val with_hk
  :  (('a Index.t * 'b, 'c, 'd) Hk.t2 -> ('e Index.t * 'f, 'g, 'h) Hk.t2)
  -> ('a -> 'b -> 'c, 'd) t
  -> ('e -> 'f -> 'g, 'h) t

module Make4 (T : sig
    type ('a, 'b, 'c, 'd) t
  end) : sig
  include Hk.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) T.t

  val projected
    :  ('a, 'b, 'c, 'd) witness
    -> f:(('a, 'b, 'c, 'd) T.t -> ('e, 'f, 'g, 'h) T.t)
    -> ('e, 'f, 'g, 'h) witness

  val injected
    :  ('a Index.t * 'b, 'c, 'd, 'e) T.t
    -> f:(('a -> 'b -> 'c, ('d, 'e) witness2) t -> ('f -> 'g -> 'h, ('i, 'j) witness2) t)
    -> ('f Index.t * 'g, 'h, 'i, 'j) T.t
end
