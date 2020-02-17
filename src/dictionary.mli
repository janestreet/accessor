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
          'a 'b 'at 'bt. get:('at -> 'a) -> construct:('b -> 'bt) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Field : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> 'a * ('b -> 'bt)) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Variant : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt. match_:('at -> ('a, 'bt) Either.t) -> construct:('b -> 'bt)
          -> ('a, 'b, 'w) Hk.t2 -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Constructor : sig
    type 'w t =
      { f : 'a 'b 'at 'bt. ('b -> 'bt) -> ('a, 'b, 'w) Hk.t2 -> ('at, 'bt, 'w) Hk.t2 }
    [@@unboxed]
  end

  module Getter : sig
    type 'w t =
      { f : 'a 'b 'at 'bt. ('at -> 'a) -> ('a, 'b, 'w) Hk.t2 -> ('at, 'bt, 'w) Hk.t2 }
    [@@unboxed]
  end

  module Optional : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> ('a * ('b -> 'bt), 'bt) Either.t) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Optional_getter : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> 'a option) -> ('a, 'b, 'w) Hk.t2 -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Nonempty : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> ('bt, 'a, 'b) Nonempty.t) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Nonempty_getter : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> 'a Nonempty_getter.t) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Many : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> ('bt, 'a, 'b) Many.t) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Many_getter : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> 'a Many_getter.t) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Mapper : sig
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> f:('a -> 'b) -> 'bt) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  val equality : ([> equality ], _) t
  val isomorphism : 'w Isomorphism.t -> ([> isomorphism ], 'w) t
  val field : 'w Field.t -> ([> field ], 'w) t
  val variant : 'w Variant.t -> ([> variant ], 'w) t
  val constructor : 'w Constructor.t -> ([> constructor ], 'w) t
  val getter : 'w Getter.t -> ([> getter ], 'w) t
  val optional : 'w Optional.t -> ([> optional ], 'w) t
  val optional_getter : 'w Optional_getter.t -> ([> optional_getter ], 'w) t
  val nonempty : 'w Nonempty.t -> ([> nonempty ], 'w) t
  val nonempty_getter : 'w Nonempty_getter.t -> ([> nonempty_getter ], 'w) t
  val many : 'w Many.t -> ([> many ], 'w) t
  val many_getter : 'w Many_getter.t -> ([> many_getter ], 'w) t
  val mapper : 'w Mapper.t -> ([> mapper ], 'w) t
end

module Run : sig
  val equality : (_, 'w) t -> ('a, 'b, 'w) Hk.t2 -> ('a, 'b, 'w) Hk.t2

  val constructor
    :  ([< constructor ], 'w) t
    -> ('b -> 'bt)
    -> ('a, 'b, 'w) Hk.t2
    -> ('at, 'bt, 'w) Hk.t2

  val field
    :  ([< field ], 'w) t
    -> ('at -> 'a * ('b -> 'bt))
    -> ('a, 'b, 'w) Hk.t2
    -> ('at, 'bt, 'w) Hk.t2

  val getter
    :  ([< getter ], 'w) t
    -> ('at -> 'a)
    -> ('a, 'b, 'w) Hk.t2
    -> ('at, 'bt, 'w) Hk.t2

  val isomorphism
    :  ([< isomorphism ], 'w) t
    -> get:('at -> 'a)
    -> construct:('b -> 'bt)
    -> ('a, 'b, 'w) Hk.t2
    -> ('at, 'bt, 'w) Hk.t2

  val mapper
    :  ([< mapper ], 'w) t
    -> ('at -> f:('a -> 'b) -> 'bt)
    -> ('a, 'b, 'w) Hk.t2
    -> ('at, 'bt, 'w) Hk.t2

  val many
    :  ([< many ], 'w) t
    -> ('at -> ('bt, 'a, 'b) Many.t)
    -> ('a, 'b, 'w) Hk.t2
    -> ('at, 'bt, 'w) Hk.t2

  val many_getter
    :  ([< many_getter ], 'w) t
    -> ('at -> 'a Many_getter.t)
    -> ('a, 'b, 'w) Hk.t2
    -> ('at, 'bt, 'w) Hk.t2

  val nonempty
    :  ([< nonempty ], 'w) t
    -> ('at -> ('bt, 'a, 'b) Nonempty.t)
    -> ('a, 'b, 'w) Hk.t2
    -> ('at, 'bt, 'w) Hk.t2

  val nonempty_getter
    :  ([< nonempty_getter ], 'w) t
    -> ('at -> 'a Nonempty_getter.t)
    -> ('a, 'b, 'w) Hk.t2
    -> ('at, 'bt, 'w) Hk.t2

  val optional
    :  ([< optional ], 'w) t
    -> ('at -> ('a * ('b -> 'bt), 'bt) Either.t)
    -> ('a, 'b, 'w) Hk.t2
    -> ('at, 'bt, 'w) Hk.t2

  val optional_getter
    :  ([< optional_getter ], 'w) t
    -> ('at -> 'a option)
    -> ('a, 'b, 'w) Hk.t2
    -> ('at, 'bt, 'w) Hk.t2

  val variant
    :  ([< variant ], 'w) t
    -> match_:('at -> ('a, 'bt) Either.t)
    -> construct:('b -> 'bt)
    -> ('a, 'b, 'w) Hk.t2
    -> ('at, 'bt, 'w) Hk.t2
end
