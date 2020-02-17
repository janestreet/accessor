open! Base
open! Import

module Applicative = struct
  type 'w t =
    { return : 'a. 'a -> ('a, 'w) Hk.t
    ; map : 'a 'b. ('a, 'w) Hk.t -> f:('a -> 'b) -> ('b, 'w) Hk.t
    ; apply : 'a 'b. ('a -> 'b, 'w) Hk.t -> ('a, 'w) Hk.t -> ('b, 'w) Hk.t
    }
end

module T = struct
  type ('bt, 'a, 'b) t =
    { f : 'w. 'w Applicative.t -> access:('a -> ('b, 'w) Hk.t) -> ('bt, 'w) Hk.t }
  [@@unboxed]

  let access a = { f = (fun _ ~access -> access a) }

  include Base.Applicative.Make3 (struct
      type nonrec ('bt, 'a, 'b) t = ('bt, 'a, 'b) t

      let return a = { f = (fun applicative ~access:_ -> applicative.return a) }

      let map t ~f =
        { f = (fun applicative ~access -> applicative.map (t.f applicative ~access) ~f) }
      ;;

      let map = `Custom map

      let apply t1 t2 =
        { f =
            (fun applicative ~access ->
               applicative.apply (t1.f applicative ~access) (t2.f applicative ~access))
        }
      ;;
    end)
end

include T

module Open_on_rhs_intf = struct
  module type S = sig
    val access : 'a -> ('b, 'a, 'b) t
  end
end

include Base.Applicative.Make_let_syntax3 (T) (Open_on_rhs_intf)
    (struct
      let access = access
    end)

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

module Of_applicative3 (A : sig
    type ('a, 'd, 'e) t

    val return : 'a -> ('a, _, _) t
    val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
    val apply : ('a -> 'b, 'd, 'e) t -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t
  end) =
struct
  module H = Hk.Make3 (A)

  let applicative =
    { Applicative.return = (fun a -> H.inject (A.return a))
    ; map = (fun a ~f -> H.inject (A.map (H.project a) ~f))
    ; apply = (fun a b -> H.inject (A.apply (H.project a) (H.project b)))
    }
  ;;

  let of_many t ~access =
    H.project (t.f applicative ~access:(fun a -> H.inject (access a)))
  ;;
end

module Of_applicative2 (A : sig
    type ('a, 'e) t

    val return : 'a -> ('a, _) t
    val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
    val apply : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
  end) =
  Of_applicative3 (struct
    type ('a, _, 'e) t = ('a, 'e) A.t

    include (A : module type of A with type ('a, 'e) t := ('a, 'e) A.t)
  end)

module Of_applicative (A : sig
    type 'a t

    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
  end) =
  Of_applicative2 (struct
    type ('a, _) t = 'a A.t

    include (A : module type of A with type 'a t := 'a A.t)
  end)

include Nonempty.Of_applicative_without_return3 (T)

let of_nonempty nonempty = of_nonempty nonempty ~access
