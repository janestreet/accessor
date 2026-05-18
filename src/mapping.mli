open! Base
open! Import

(** An [(i -> a -> b, w) t] is some mapping from [i] and [a] to [b]. [w] determines what
    kind of mapping it is. *)
type (_, _) t

val with_hk
  :  (('a Index.t * 'b, 'c, 'd) Hk.t2 @ local -> ('e Index.t * 'f, 'g, 'h) Hk.t2 @ local)
     @ local
  -> ('a -> 'b -> 'c, 'd) t @ local
  -> ('e -> 'f -> 'g, 'h) t @ local

module Make4 (T : sig
    type ('a, 'b, 'c, 'd) t
  end) : sig
  include Hk.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) T.t

  val projected
    :  ('a, 'b, 'c, 'd, higher_kinded) Higher_kinded.t4 @ local
    -> f:(('a, 'b, 'c, 'd) T.t @ local -> ('e, 'f, 'g, 'h) T.t @ local) @ local
    -> ('e, 'f, 'g, 'h, higher_kinded) Higher_kinded.t4 @ local

  val injected
    :  ('a Index.t * 'b, 'c, 'd, 'e) T.t @ local
    -> f:
         (('a -> 'b -> 'c, 'd -> 'e -> higher_kinded) t @ local
          -> ('f -> 'g -> 'h, 'i -> 'j -> higher_kinded) t @ local)
       @ local
    -> ('f Index.t * 'g, 'h, 'i, 'j) T.t @ local
end
