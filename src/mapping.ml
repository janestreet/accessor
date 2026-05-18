open! Base
open! Import

type ('m, 'w) t = T : ('i Index.t * 'a, 'b, 'w) Hk.t2 -> ('i -> 'a -> 'b, 'w) t
[@@unboxed]

let with_hk (f : _ @ local -> _ @ local) (T t) = exclave_ T (f t)

module Make4 (T : sig
    type ('a, 'b, 'c, 'd) t
  end) =
struct
  include Hk.Make4 (T)

  let projected t ~f = exclave_ inject (f (project t))

  let injected t ~f = exclave_
    let (T t) = f (T (inject t)) [@nontail] in
    project t
  ;;
end
