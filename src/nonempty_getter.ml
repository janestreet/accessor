open! Base
open! Import

type 'a t = { f : 'r. combine:('r -> 'r -> 'r) -> f:('a -> 'r) -> 'r } [@@unboxed]

let access a = { f = (fun ~combine:_ ~f -> f a) }

include Monad.Make (struct
    type nonrec 'a t = 'a t

    let return = access
    let map t ~f = { f = (fun ~combine ~f:g -> t.f ~combine ~f:(fun a -> g (f a))) }

    let bind t ~f =
      { f = (fun ~combine ~f:g -> t.f ~combine ~f:(fun a -> (f a).f ~combine ~f:g)) }
    ;;

    let map = `Custom map
  end)

let map_reduce t = t.f

let append t1 t2 =
  { f = (fun ~combine ~f -> combine (t1.f ~combine ~f) (t2.f ~combine ~f)) }
;;

module O = struct
  let ( @ ) = append
end

include O

include Nonempty.Of_applicative_without_return2 (struct
    type nonrec (_, 'a) t = 'a t

    let map t ~f:_ = t
    let apply = append
  end)

let of_nonempty nonempty = of_nonempty nonempty ~access
