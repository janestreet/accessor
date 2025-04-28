open! Core
open! Import

module Quickcheckable : sig
  module type S = sig
    type t [@@deriving quickcheck, sexp_of]
  end
end

module Testable : sig
  module type S = sig
    type t [@@deriving equal, quickcheck, sexp_of]
  end

  module Either (A : S) (B : S) : S with type t = (A.t, B.t) Either.t
  module Tuple (A : S) (B : S) : S with type t = A.t * B.t
  module Option (A : S) : S with type t = A.t option
  module List (A : S) : S with type t = A.t list
  module Bool_map (A : S) : S with type t = A.t Bool.Map.t
  module Bool_set : S with type t = Bool.Set.t
end

val mapper
  :  (module Testable.S with type t = 'a)
  -> (module Testable.S with type t = 'at)
  -> (module Quickcheckable.S with type t = 'env)
  -> ('env -> (unit, 'a, 'at, [> mapper ]) Accessor.t)
  -> unit

val many
  :  (module Testable.S with type t = 'a)
  -> (module Testable.S with type t = 'at)
  -> (module Quickcheckable.S with type t = 'env)
  -> ('env -> (unit, 'a, 'at, [> many ]) Accessor.t)
  -> unit

val nonempty
  :  (module Testable.S with type t = 'a)
  -> (module Testable.S with type t = 'at)
  -> (module Quickcheckable.S with type t = 'env)
  -> ('env -> (unit, 'a, 'at, [> nonempty ]) Accessor.t)
  -> unit

val optional
  :  (module Testable.S with type t = 'a)
  -> (module Testable.S with type t = 'at)
  -> (module Quickcheckable.S with type t = 'env)
  -> ('env -> (unit, 'a, 'at, [> optional ]) Accessor.t)
  -> unit

val field
  :  (module Testable.S with type t = 'a)
  -> (module Testable.S with type t = 'at)
  -> (module Quickcheckable.S with type t = 'env)
  -> ('env -> (unit, 'a, 'at, [> field ]) Accessor.t)
  -> unit

val variant
  :  (module Testable.S with type t = 'a)
  -> (module Testable.S with type t = 'at)
  -> (module Quickcheckable.S with type t = 'env)
  -> ('env -> (unit, 'a, 'at, [> variant ]) Accessor.t)
  -> unit

val isomorphism
  :  (module Testable.S with type t = 'a)
  -> (module Testable.S with type t = 'at)
  -> (module Quickcheckable.S with type t = 'env)
  -> ('env -> (unit, 'a, 'at, [> isomorphism ]) Accessor.t)
  -> unit
