open! Base
open! Import

type 'a t = { f : 'r. empty:'r -> combine:('r -> 'r -> 'r) -> f:('a -> 'r) -> 'r }
[@@unboxed]

let access a = { f = (fun ~empty:_ ~combine:_ ~f -> f a) }

let of_list ts =
  { f =
      (fun ~empty ~combine ~f ->
         List.map ts ~f:(fun t -> t.f ~empty ~combine ~f)
         |> List.reduce ~f:combine
         |> Option.value ~default:empty)
  }
;;

include Monad.Make (struct
    type nonrec 'a t = 'a t

    let return = access

    let map t ~f =
      { f = (fun ~empty ~combine ~f:g -> t.f ~empty ~combine ~f:(fun a -> g (f a))) }
    ;;

    let bind t ~f =
      { f =
          (fun ~empty ~combine ~f:g ->
             t.f ~empty ~combine ~f:(fun a -> (f a).f ~empty ~combine ~f:g))
      }
    ;;

    let map = `Custom map
  end)

let empty = { f = (fun ~empty ~combine:_ ~f:_ -> empty) }
let map_reduce t = t.f

let append t1 t2 =
  { f =
      (fun ~empty ~combine ~f ->
         let a = t1.f ~empty ~combine ~f in
         let b = t2.f ~empty ~combine ~f in
         combine a b)
  }
;;

module O = struct
  let ( @ ) = append
end

include O

include Many.Of_applicative2 (struct
    type nonrec (_, 'a) t = 'a t

    let return _ = empty
    let map t ~f:_ = t
    let apply = append
  end)

let of_many many = of_many many ~access
let of_nonempty nonempty = of_many (Many.of_nonempty nonempty)

let of_nonempty_getter nonempty_getter =
  Nonempty_getter.map_reduce nonempty_getter ~combine:append ~f:access
;;
