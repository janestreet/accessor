open! Core
open! Import

module Quickcheckable : sig
  module type S = sig
    type t [@@deriving quickcheck, sexp_of]
  end
end

module Testable : sig
  module type%template [@mode m = (local, global)] S = sig
    type t [@@deriving (equal [@mode.explicit m]), quickcheck, sexp_of]
  end

  module%template [@mode m = (local, global)] Either (A : S [@mode m]) (B : S [@mode m]) :
    S [@mode m] with type t = (A.t, B.t) Either.t

  module%template [@mode m = (local, global)] Tuple (A : S [@mode m]) (B : S [@mode m]) :
    S [@mode m] with type t = A.t * B.t

  module%template [@mode m = (local, global)] Option (A : S [@mode m]) :
    S [@mode m] with type t = A.t option

  module%template [@mode m = (local, global)] List (A : S [@mode m]) :
    S [@mode m] with type t = A.t list

  module%template [@mode m = (local, global)] Bool_map (A : S [@mode m]) :
    S [@mode m] with type t = A.t Bool.Map.t

  module Bool_set : S [@mode local] with type t = Bool.Set.t
end

val%template mapper
  :  ((module Testable.S with type t = 'a)[@mode m])
  -> ((module Testable.S with type t = 'at)[@mode m])
  -> (module Quickcheckable.S with type t = 'env)
  -> ('env -> (unit, 'a, 'at, [> mapper ]) Accessor.t)
  -> unit
[@@mode m = (local, global)]

val%template many
  :  ((module Testable.S with type t = 'a)[@mode m])
  -> ((module Testable.S with type t = 'at)[@mode m])
  -> (module Quickcheckable.S with type t = 'env)
  -> ('env -> (unit, 'a, 'at, [> many ]) Accessor.t)
  -> unit
[@@mode m = (local, global)]

val%template nonempty
  :  ((module Testable.S with type t = 'a)[@mode m])
  -> ((module Testable.S with type t = 'at)[@mode m])
  -> (module Quickcheckable.S with type t = 'env)
  -> ('env -> (unit, 'a, 'at, [> nonempty ]) Accessor.t)
  -> unit
[@@mode m = (local, global)]

val%template optional
  :  ((module Testable.S with type t = 'a)[@mode m])
  -> ((module Testable.S with type t = 'at)[@mode m])
  -> (module Quickcheckable.S with type t = 'env)
  -> ('env -> (unit, 'a, 'at, [> optional ]) Accessor.t)
  -> unit
[@@mode m = (local, global)]

val%template field
  :  ((module Testable.S with type t = 'a)[@mode m])
  -> ((module Testable.S with type t = 'at)[@mode m])
  -> (module Quickcheckable.S with type t = 'env)
  -> ('env -> (unit, 'a, 'at, [> field ]) Accessor.t)
  -> unit
[@@mode m = (local, global)]

val%template variant
  :  ((module Testable.S with type t = 'a)[@mode m])
  -> ((module Testable.S with type t = 'at)[@mode m])
  -> (module Quickcheckable.S with type t = 'env)
  -> ('env -> (unit, 'a, 'at, [> variant ]) Accessor.t)
  -> unit
[@@mode m = (local, global)]

val%template isomorphism
  :  ((module Testable.S with type t = 'a)[@mode m])
  -> ((module Testable.S with type t = 'at)[@mode m])
  -> (module Quickcheckable.S with type t = 'env)
  -> ('env -> (unit, 'a, 'at, [> isomorphism ]) Accessor.t)
  -> unit
[@@mode m = (local, global)]
