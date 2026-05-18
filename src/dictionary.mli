open! Base
open! Import
open Subtyping

(** A [(c, w) t] explains how to construct a mapping from some specification. For example,
    an [(isomorphism, w) t] knows how to convert the specification containing [get] and
    [construct] functions into a function over mappings witnessed by [w]. *)
type ('c, 'w) t

module Create : sig
  module Isomorphism : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt.
          get:('at -> 'a)
          -> construct:('b -> 'bt)
          -> ('a, 'b, 'w) Hk.t2 @ local
          -> ('at, 'bt, 'w) Hk.t2 @ local
        @@ global
      }
    [@@unboxed]
  end

  module Field : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt.
          ('at -> 'a * ('b -> 'bt)) @ local
          -> ('a, 'b, 'w) Hk.t2 @ local
          -> ('at, 'bt, 'w) Hk.t2 @ local
        @@ global
      }
    [@@unboxed]
  end

  module Variant : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt.
          match_:('at -> ('a, 'bt) Either.t) @ local
          -> construct:('b -> 'bt) @ local
          -> ('a, 'b, 'w) Hk.t2 @ local
          -> ('at, 'bt, 'w) Hk.t2 @ local
        @@ global
      }
    [@@unboxed]
  end

  module Constructor : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt.
          ('b -> 'bt) @ local
          -> ('a, 'b, 'w) Hk.t2 @ local
          -> ('at, 'bt, 'w) Hk.t2 @ local
        @@ global
      }
    [@@unboxed]
  end

  module Getter : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt.
          ('at -> 'a) @ local
          -> ('a, 'b, 'w) Hk.t2 @ local
          -> ('at, 'bt, 'w) Hk.t2 @ local
        @@ global
      }
    [@@unboxed]
  end

  module Optional : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt.
          ('at -> ('a * ('b -> 'bt), 'bt) Either.t) @ local
          -> ('a, 'b, 'w) Hk.t2 @ local
          -> ('at, 'bt, 'w) Hk.t2 @ local
        @@ global
      }
    [@@unboxed]
  end

  module Optional_getter : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt.
          ('at -> 'a option) @ local
          -> ('a, 'b, 'w) Hk.t2 @ local
          -> ('at, 'bt, 'w) Hk.t2 @ local
        @@ global
      }
    [@@unboxed]
  end

  module Nonempty : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt.
          ('at -> ('bt, 'a, 'b) Nonempty.t) @ local
          -> ('a, 'b, 'w) Hk.t2 @ local
          -> ('at, 'bt, 'w) Hk.t2 @ local
        @@ global
      }
    [@@unboxed]
  end

  module Nonempty_getter : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt.
          ('at -> 'a Nonempty_getter.t) @ local
          -> ('a, 'b, 'w) Hk.t2 @ local
          -> ('at, 'bt, 'w) Hk.t2 @ local
        @@ global
      }
    [@@unboxed]
  end

  module Many : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt.
          ('at -> ('bt, 'a, 'b) Many.t) @ local
          -> ('a, 'b, 'w) Hk.t2 @ local
          -> ('at, 'bt, 'w) Hk.t2 @ local
        @@ global
      }
    [@@unboxed]
  end

  module Many_getter : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt.
          ('at -> 'a Many_getter.t) @ local
          -> ('a, 'b, 'w) Hk.t2 @ local
          -> ('at, 'bt, 'w) Hk.t2 @ local
        @@ global
      }
    [@@unboxed]
  end

  module Mapper : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt.
          ('at -> f:('a -> 'b) -> 'bt)
          -> ('a, 'b, 'w) Hk.t2 @ local
          -> ('at, 'bt, 'w) Hk.t2 @ local
        @@ global
      }
    [@@unboxed]
  end

  val equality : ([> equality ], _) t
  val isomorphism : 'w Isomorphism.t -> ([> isomorphism ], 'w) t @ local
  val field : 'w Field.t -> ([> field ], 'w) t @ local
  val variant : 'w Variant.t -> ([> variant ], 'w) t @ local
  val constructor : 'w Constructor.t -> ([> constructor ], 'w) t @ local
  val getter : 'w Getter.t -> ([> getter ], 'w) t @ local
  val optional : 'w Optional.t -> ([> optional ], 'w) t @ local
  val optional_getter : 'w Optional_getter.t -> ([> optional_getter ], 'w) t @ local
  val nonempty : 'w Nonempty.t -> ([> nonempty ], 'w) t @ local
  val nonempty_getter : 'w Nonempty_getter.t -> ([> nonempty_getter ], 'w) t @ local
  val many : 'w Many.t -> ([> many ], 'w) t @ local
  val many_getter : 'w Many_getter.t -> ([> many_getter ], 'w) t @ local
  val mapper : 'w Mapper.t -> ([> mapper ], 'w) t @ local
end

module Run : sig
  val equality
    :  (_, 'w) t @ local
    -> ('a, 'b, 'w) Hk.t2 @ local
    -> ('a, 'b, 'w) Hk.t2 @ local

  val constructor
    :  ([< constructor ], 'w) t @ local
    -> ('b -> 'bt)
    -> ('a, 'b, 'w) Hk.t2 @ local
    -> ('at, 'bt, 'w) Hk.t2 @ local

  val field
    :  ([< field ], 'w) t @ local
    -> ('at -> 'a * ('b -> 'bt))
    -> ('a, 'b, 'w) Hk.t2 @ local
    -> ('at, 'bt, 'w) Hk.t2 @ local

  val getter
    :  ([< getter ], 'w) t @ local
    -> ('at -> 'a)
    -> ('a, 'b, 'w) Hk.t2 @ local
    -> ('at, 'bt, 'w) Hk.t2 @ local

  val isomorphism
    :  ([< isomorphism ], 'w) t @ local
    -> get:('at -> 'a)
    -> construct:('b -> 'bt)
    -> ('a, 'b, 'w) Hk.t2 @ local
    -> ('at, 'bt, 'w) Hk.t2 @ local

  val mapper
    :  ([< mapper ], 'w) t @ local
    -> ('at -> f:('a -> 'b) -> 'bt)
    -> ('a, 'b, 'w) Hk.t2 @ local
    -> ('at, 'bt, 'w) Hk.t2 @ local

  val many
    :  ([< many ], 'w) t @ local
    -> ('at -> ('bt, 'a, 'b) Many.t)
    -> ('a, 'b, 'w) Hk.t2 @ local
    -> ('at, 'bt, 'w) Hk.t2 @ local

  val many_getter
    :  ([< many_getter ], 'w) t @ local
    -> ('at -> 'a Many_getter.t)
    -> ('a, 'b, 'w) Hk.t2 @ local
    -> ('at, 'bt, 'w) Hk.t2 @ local

  val nonempty
    :  ([< nonempty ], 'w) t @ local
    -> ('at -> ('bt, 'a, 'b) Nonempty.t)
    -> ('a, 'b, 'w) Hk.t2 @ local
    -> ('at, 'bt, 'w) Hk.t2 @ local

  val nonempty_getter
    :  ([< nonempty_getter ], 'w) t @ local
    -> ('at -> 'a Nonempty_getter.t)
    -> ('a, 'b, 'w) Hk.t2 @ local
    -> ('at, 'bt, 'w) Hk.t2 @ local

  val optional
    :  ([< optional ], 'w) t @ local
    -> ('at -> ('a * ('b -> 'bt), 'bt) Either.t)
    -> ('a, 'b, 'w) Hk.t2 @ local
    -> ('at, 'bt, 'w) Hk.t2 @ local

  val optional_getter
    :  ([< optional_getter ], 'w) t @ local
    -> ('at -> 'a option)
    -> ('a, 'b, 'w) Hk.t2 @ local
    -> ('at, 'bt, 'w) Hk.t2 @ local

  val variant
    :  ([< variant ], 'w) t @ local
    -> match_:('at -> ('a, 'bt) Either.t)
    -> construct:('b -> 'bt)
    -> ('a, 'b, 'w) Hk.t2 @ local
    -> ('at, 'bt, 'w) Hk.t2 @ local
end
