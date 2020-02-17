open! Base
open! Import

module Applicative = struct
  type 'w t =
    { map : 'a 'b. ('a, 'w) Hk.t -> f:('a -> 'b) -> ('b, 'w) Hk.t
    ; apply : 'a 'b. ('a -> 'b, 'w) Hk.t -> ('a, 'w) Hk.t -> ('b, 'w) Hk.t
    }
end

type ('bt, 'a, 'b) t =
  { f : 'w. 'w Applicative.t -> access:('a -> ('b, 'w) Hk.t) -> ('bt, 'w) Hk.t }
[@@unboxed]

let access a = { f = (fun _ ~access -> access a) }

module Accessed = Monad.Make_indexed (struct
    type nonrec ('a, 'bt, 'b) t = ('bt, 'a, 'b) t

    let return = access

    let map t ~f =
      { f = (fun applicative ~access -> t.f applicative ~access:(fun a -> access (f a))) }
    ;;

    let bind t ~f =
      { f =
          (fun applicative ~access ->
             t.f applicative ~access:(fun a -> (f a).f applicative ~access))
      }
    ;;

    let map = `Custom map
  end)

module A1 = Applicative_without_return.Make3 (struct
    type nonrec ('bt, 'a, 'b) t = ('bt, 'a, 'b) t

    let map t ~f =
      { f = (fun applicative ~access -> applicative.map (t.f applicative ~access) ~f) }
    ;;

    let apply t1 t2 =
      { f =
          (fun applicative ~access ->
             applicative.apply (t1.f applicative ~access) (t2.f applicative ~access))
      }
    ;;
  end)

module A2 : module type of A1 with module Let_syntax := A1.Let_syntax = A1
include A2

module Let_syntax = struct
  include A2

  module Let_syntax = struct
    include A2

    module Open_on_rhs = struct
      let access = access
    end
  end
end

module Of_applicative_without_return3 (A : sig
    type ('a, 'e, 'f) t

    val map : ('a, 'e, 'f) t -> f:('a -> 'b) -> ('b, 'e, 'f) t
    val apply : ('a -> 'b, 'e, 'f) t -> ('a, 'e, 'f) t -> ('b, 'e, 'f) t
  end) =
struct
  module H = Hk.Make3 (A)

  let applicative =
    { Applicative.map = (fun a ~f -> H.inject (A.map (H.project a) ~f))
    ; apply = (fun a b -> H.inject (A.apply (H.project a) (H.project b)))
    }
  ;;

  let of_nonempty t ~access =
    H.project (t.f applicative ~access:(fun a -> H.inject (access a)))
  ;;
end

module Of_applicative_without_return2 (A : sig
    type ('a, 'e) t

    val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
    val apply : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
  end) =
  Of_applicative_without_return3 (struct
    type ('a, 'e, _) t = ('a, 'e) A.t

    include (A : module type of A with type ('a, 'e) t := ('a, 'e) A.t)
  end)

module Of_applicative_without_return (A : sig
    type 'a t

    val map : 'a t -> f:('a -> 'b) -> 'b t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
  end) =
  Of_applicative_without_return2 (struct
    type ('a, _) t = 'a A.t

    include (A : module type of A with type 'a t := 'a A.t)
  end)

include Of_applicative_without_return3 (struct
    type nonrec ('bt, 'a, 'b) t = ('bt, 'a, 'b) t

    let map = map
    let apply = apply
  end)
