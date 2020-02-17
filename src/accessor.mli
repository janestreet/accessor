open! Base
open! Import
open Applicative_signatures_intf

(** An "accessor" describes how to access some data within a larger data structure. See
    the tutorial in doc/tutorial.mlt for an overview. *)

(** {1 Types} *)

(** Here is a summary of the type parameters in [(i -> a -> b, it -> at -> bt, c)
    Accessor.t]:

    - [i] is the output index type
    - [a] is the type of value that is read
    - [b] is the type of value that is written
    - [it] is the input index type
    - [at] is the type of value that is read from
    - [bt] is the type of value resulting from a write
    - [c] is the kind of accessor

    The representation is exposed, but not intended to be used directly. *)
type ('inner, 'outer, 'kind) t =
  { f : 'w. ('kind, 'w) Dictionary.t -> ('inner, 'w) Mapping.t -> ('outer, 'w) Mapping.t
  }
[@@unboxed]

(** Accessors are commonly not indexed and don't need to support polymorphic updates. In
    such cases, it may be easier to read and write types in terms of [Simple.t]. Here is
    an example where the improvement by using [Simple.t] is significant:

    {[
      val date_ofday
        :  Time.Zone.t
        -> ( 'i -> Date.t * Time.Ofday.t -> Date.t * Time.Ofday.t
           , 'i -> Time.t -> Time.t
           , [< isomorphism] )
             Accessor.t
    ]}

    It is more cleanly written like this:

    {[
      val date_ofday
        :  Time.Zone.t
        -> (_, Date.t * Time.Ofday.t, Time.t, [< isomorphism]) Accessor.Simple.t
    ]} *)
module Simple : sig
  type nonrec ('index, 'inner, 'outer, 'kind) t =
    ('index -> 'inner -> 'inner, 'index -> 'outer -> 'outer, 'kind) t
end

(** To use [Accessor] in your own code, it is recommended to add the following to your
    import.ml:

    {[
      module Accessor =
        (* Consider using something like [Accessor_base], [Accessor_core], or
           [Accessor_async], instead. *)
        Accessor

      include Accessor.O
    ]}

    The [O] module contains a few operators and type aliases that are unlikely to clash
    with anything else. *)
module O : sig
  include module type of Subtyping (** @inline *)

  (** [a @> b] is the composition of the two accessors [a] and [b]. From left to right, a
      chain of composed accessors goes from outermost to innermost values. The resulting
      accessor kind is determined by the least powerful argument. Here are a few examples:

      - An [isomorphism] composed with a [field] is a [field].
      - A [field] composed with a [variant] is an [optional].
      - A [getter] composed with a [variant] is an [optional_getter].

      It's normally more intuitive to think of the operations you need than to think of
      exactly which kind of accessor you are creating. For example, if you are trying to
      extract a value from a data structure using a [field], you would probably use
      [get]. However, if you compose the [field] with an [optional], [get] no longer
      makes sense; you must use something like [get_option], instead.

      The non-operator name is [Accessor.compose]. *)
  val ( @> )
    :  ('middle, 'outer, 'kind) t
    -> ('inner, 'middle, 'kind) t
    -> ('inner, 'outer, 'kind) t

  (** [x.@(t)] extracts a single value from [x] as identified by [t]. The non-operator
      name is [Accessor.get]. *)
  val ( .@() ) : 'at -> (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> getter ]) t -> 'a

  (** [x.@?(t)] extracts at most one value from [x] as identified by [t]. The
      non-operator name is [Accessor.get_option]. *)
  val ( .@?() )
    :  'at
    -> (unit -> 'a -> _, unit -> 'at -> _, [> optional_getter ]) t
    -> 'a option

  (** [x.@*(t)] extracts any number of values from [x] as identified by [t]. The
      non-operator name is [Accessor.to_list]. *)
  val ( .@*() )
    :  'at
    -> (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
    -> 'a list

  (** [x.@(t) <- y] replaces any number of values in [x] with [y], as identified by [t].
      The non-operator name is [Accessor.set]. *)
  val ( .@()<- ) : 'at -> (_ -> _ -> 'b, unit -> 'at -> 'bt, [> mapper ]) t -> 'b -> 'bt
end

include module type of O (** @inline *)

(** [id] can be used as any kind of accessor. It is also the only way to summon an
    [equality]. *)
val id : ('a, 'a, _) t

(** [compose] is the same as [( @> )]. See the documentation of [( @> )] for more
    information. *)
val compose
  :  ('middle, 'outer, 'kind) t
  -> ('inner, 'middle, 'kind) t
  -> ('inner, 'outer, 'kind) t

(** {1 Using accessors} *)

(** {2 Indices} *)

(** An [Index.t] is a heterogeneous stack of values intended to serve as "breadcrumbs"
    that show how you got to some value currently being accessed inside a composite data
    structure. For example, if on the way in you traversed a [Map.t], one component of
    the index might be the key of the data being accessed.

    [Index.t] is defined to overload list syntax. This can look a little weird, because
    you may not have ever seen a list with different types of elements before, such as
    [["foo"; 5; true; 3.14]]. The idea is that you will normally only ever be pattern
    matching on them, and in contexts where the compiler can infer that it is an
    [Index.t], not a [list]. You should be able to just pattern match with a lambda, no
    need for a [match] expression or a type annotation. This is a lot more convenient
    than the alternative of pattern matching on deeply nested tuples.

    One thing that might not be immediately intuitive is that the first element of an
    [Index.t] is the innermost index, not the outermost. [Index.t] is a list-like data
    structure, so it is much more efficient for the head, not the tail, to be the most
    frequently updated, as a stack.

    Here is an example demonstrating the use of an [Index.t] that demonstrates both the
    pattern matching syntax and the ordering:

    {[
      Accessor.mapi
        (Accessor.Map.eachi @> Accessor.Map.eachi)
        (Map.singleton "foo" (Map.singleton "bar" 1))
        ~f:(fun [inner; outer] data -> outer, inner, data)
      = (Map.singleton "foo" (Map.singleton "bar" ("foo", "bar", 1)))
    ]} *)
module Index = Index

(** The [Subtyping] module contains all the types used for accessor subtyping. You
    shouldn't have to use it, but it's here for the documentation. *)
module Subtyping = Subtyping

(** {2 Functions} *)

(** {3 Getting and Folding} *)

(** [get t at] reads a value from [at]. *)
val get : (unit -> 'a -> _, unit -> 'at -> _, [> getter ]) t -> 'at -> 'a

(** [geti t at] reads a value and its index from [at]. *)
val geti : ('i -> 'a -> _, unit -> 'at -> _, [> getter ]) t -> 'at -> 'i Index.t * 'a

(** [get_option t at] reads a value from [at], if present. *)
val get_option
  :  (unit -> 'a -> _, unit -> 'at -> _, [> optional_getter ]) t
  -> 'at
  -> 'a option

(** [get_optioni t at] reads a value and its index from [at], if present. *)
val get_optioni
  :  ('i -> 'a -> _, unit -> 'at -> _, [> optional_getter ]) t
  -> 'at
  -> ('i Index.t * 'a) option

(** [match_ t at] is like [get_option], but in the failure case it may be able to give you
    the input data structure with a different type. *)
val match_
  :  (unit -> 'a -> _, unit -> 'at -> 'bt, [> variant ]) t
  -> 'at
  -> ('a, 'bt) Either.t

(** An indexed version of [match_]. *)
val matchi
  :  ('i -> 'a -> _, unit -> 'at -> 'bt, [> variant ]) t
  -> 'at
  -> ('i Index.t * 'a, 'bt) Either.t

(** Extract all the values targetted by an accessor in some composite data structure
    into a list. *)
val to_list : (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t -> 'at -> 'a list

(** An indexed version of [to_list]. *)
val to_listi
  :  ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> ('i Index.t * 'a) list

(** Extract all the values targetted by an accessor in some composite data structure
    into an array. *)
val to_array : (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t -> 'at -> 'a array

(** An indexed version of [to_array]. *)
val to_arrayi
  :  ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> ('i Index.t * 'a) array

(** Fold across all the values targetted by an accessor with an accumulator. *)
val fold
  :  (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> init:'acc
  -> f:('acc -> 'a -> 'acc)
  -> 'acc

(** Indexed version of fold. *)
val foldi
  :  ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> init:'acc
  -> f:('i Index.t -> 'acc -> 'a -> 'acc)
  -> 'acc

(** Iterate over all the values targetted by an accessor, applying the function argument
    to each one. *)
val iter
  :  (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> f:('a -> unit)
  -> unit

(** An indexed version of [iter]. *)
val iteri
  :  ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> f:('i Index.t -> 'a -> unit)
  -> unit

(** [length t at] returns the number of targets in [at]. *)
val length : (unit -> _ -> _, unit -> 'at -> _, [> many_getter ]) t -> 'at -> int

(** [is_empty t at] is [true] iff there are no targets in [at]. *)
val is_empty : (unit -> _ -> _, unit -> 'at -> _, [> many_getter ]) t -> 'at -> bool

(** [sum (module Summable) t at ~f] returns the sum of [f a] for all targets [a] in
    [at]. *)
val sum
  :  (module Container.Summable with type t = 'sum)
  -> (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> f:('a -> 'sum)
  -> 'sum

(** [sumi] is the indexed version of [sum]. *)
val sumi
  :  (module Container.Summable with type t = 'sum)
  -> ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> f:('i Index.t -> 'a -> 'sum)
  -> 'sum

(** [count t at ~f] returns the number of targets in [at] for which [f] evaluates to
    true. *)
val count
  :  (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> f:('a -> bool)
  -> int

(** [counti] is the indexed version of [count]. *)
val counti
  :  ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> f:('i Index.t -> 'a -> bool)
  -> int

(** [exists t at ~f] returns [true] iff there is a target in [at] for which [f] returns
    [true]. This is a short-circuiting operation. *)
val exists
  :  (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> f:('a -> bool)
  -> bool

(** [existsi] is the indexed version of [exists]. *)
val existsi
  :  ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> f:('i Index.t -> 'a -> bool)
  -> bool

(** [for_all t at ~f] returns [true] iff [f] returns [true] for all targets in [at].
    This is a short-circuiting operation. *)
val for_all
  :  (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> f:('a -> bool)
  -> bool

(** [for_alli] is the indexed version of [for_all]. *)
val for_alli
  :  ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> f:('i Index.t -> 'a -> bool)
  -> bool

(** [find_map] returns the first evaluation of [f] that returns [Some]. *)
val find_map
  :  (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> f:('a -> 'b option)
  -> 'b option

(** [find_mapi] is the indexed version of [find_map]. *)
val find_mapi
  :  ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> f:('i Index.t -> 'a -> 'b option)
  -> 'b option

(** [find t at ~f] returns the first target in [at] for which the evaluation of [f]
    returns [true]. *)
val find
  :  (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> f:('a -> bool)
  -> 'a option

(** [findi] is the indexed version of [find]. *)
val findi
  :  ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> f:('i Index.t -> 'a -> bool)
  -> ('i Index.t * 'a) option

(** [min_elt t at ~compare] uses [compare] to compare each target in [at] and returns
    the first target with the smallest value. *)
val min_elt
  :  (unit -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) t
  -> 'at
  -> compare:('a -> 'a -> int)
  -> 'a

(** [min_elt_option t at ~compare] uses [compare] to compare each target in [at] and
    returns the first target with the smallest value, if any. *)
val min_elt_option
  :  (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> compare:('a -> 'a -> int)
  -> 'a option

(** [max_elt t at ~compare] uses [compare] to compare each target in [at] and returns
    the first target with the largest value. *)
val max_elt
  :  (unit -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) t
  -> 'at
  -> compare:('a -> 'a -> int)
  -> 'a

(** [max_elt_option t at ~compare] uses [compare] to compare each target in [at] and
    returns the first target with the largest value, if any. *)
val max_elt_option
  :  (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> compare:('a -> 'a -> int)
  -> 'a option

(** [hd t at] returns the first targetted element of [at]. *)
val hd : (unit -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) t -> 'at -> 'a

(** An indexed version of [hd]. *)
val hdi
  :  ('i -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) t
  -> 'at
  -> 'i Index.t * 'a

(** [hd_option t at] returns the first targetted element of [at], if any. *)
val hd_option
  :  (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> 'a option

(** An indexed version of [hd_option]. *)
val hd_optioni
  :  ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> ('i Index.t * 'a) option

(** [map_reduce t at ~empty ~combine ~f] applies [f] to each targetted value in [at] and
    combines the results using [combine]. The result is [empty] if there were no values.
    [empty] and [combine] are expected to satisfy the following properties:

    - [combine empty a = a]
    - [combine a empty = a]
    - [combine (combine a b) c = combine a (combine b c)] *)
val map_reduce
  :  (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> empty:'r
  -> combine:('r -> 'r -> 'r)
  -> f:('a -> 'r)
  -> 'r

(** An indexed version of [map_reduce]. *)
val map_reducei
  :  ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) t
  -> 'at
  -> empty:'r
  -> combine:('r -> 'r -> 'r)
  -> f:('i Index.t -> 'a -> 'r)
  -> 'r

(** [map_reduce_nonempty t at ~combine ~f] applies [f] to each targetted value in [at]
    and combines the results using [combine]. [combine] is expected to satisfy the
    property: [combine (combine a b) c = combine a (combine b c)]. *)
val map_reduce_nonempty
  :  (unit -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) t
  -> 'at
  -> combine:('r -> 'r -> 'r)
  -> f:('a -> 'r)
  -> 'r

(** An indexed version of [map_reduce_nonempty]. *)
val map_reduce_nonemptyi
  :  ('i -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) t
  -> 'at
  -> combine:('r -> 'r -> 'r)
  -> f:('i Index.t -> 'a -> 'r)
  -> 'r

(** {3 Modifying} *)

(** [map t at ~f] applies [f] to each targetted value inside [at], replacing it with the
    result. *)
val map
  :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> mapper ]) t
  -> 'at
  -> f:('a -> 'b)
  -> 'bt

(** [mapi] is the indexed version of [map]. *)
val mapi
  :  ('i -> 'a -> 'b, unit -> 'at -> 'bt, [> mapper ]) t
  -> 'at
  -> f:('i Index.t -> 'a -> 'b)
  -> 'bt

(** [folding_map] is a version of [map] that threads an accumulator through calls to
    [f]. *)
val folding_map
  :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> many ]) t
  -> 'at
  -> init:'acc
  -> f:('acc -> 'a -> 'acc * 'b)
  -> 'bt

(** [folding_mapi] is the indexed version of [folding_map]. *)
val folding_mapi
  :  ('i -> 'a -> 'b, unit -> 'at -> 'bt, [> many ]) t
  -> 'at
  -> init:'acc
  -> f:('i Index.t -> 'acc -> 'a -> 'acc * 'b)
  -> 'bt

(** [fold_map] is a combination of [fold] and [map] that threads an accumulator through
    calls to [f]. *)
val fold_map
  :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> many ]) t
  -> 'at
  -> init:'acc
  -> f:('acc -> 'a -> 'acc * 'b)
  -> 'acc * 'bt

(** [fold_mapi] is the indexed version of [fold_map]. *)
val fold_mapi
  :  ('i -> 'a -> 'b, unit -> 'at -> 'bt, [> many ]) t
  -> 'at
  -> init:'acc
  -> f:('i Index.t -> 'acc -> 'a -> 'acc * 'b)
  -> 'acc * 'bt

(** [set t at ~to_] replaces targetted values inside [at] with [to_]. *)
val set : (_ -> _ -> 'b, unit -> 'at -> 'bt, [> mapper ]) t -> 'at -> to_:'b -> 'bt

(** {3 Monadic and Applicative functions} *)

(** {4 Signatures} *)

(** There are a number of signatures and functors in this section, all for the purpose
    of providing a wide variety of applicative/monadic functionality. *)

module Functor : sig
  module type S =
    Functor_s with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t

  module type S2 =
    Functor_s2 with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t

  module type S3 =
    Functor_s3 with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t
end

module Applicative : sig
  module type S =
    Applicative_s with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t

  module type S2 =
    Applicative_s2
    with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t
end

module Applicative_without_return : sig
  module type S =
    Applicative_without_return_s
    with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t

  module type S2 =
    Applicative_without_return_s2
    with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t

  module type S3 =
    Applicative_without_return_s3
    with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t
end

(** The monad signatures differ from the applicative ones in that some of the functions
    have an optional [how] argument. They always default to [`Sequential], which is the
    behavior that interleaves side effects with monadic effects. If you override this
    argument to [`Parallel] then all the side effects are performed up front, and then
    the results are combined. *)
module Monad : sig
  module type S =
    Monad_s with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t

  module type S2 =
    Monad_s2 with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t
end

module Monad_without_return : sig
  module type S =
    Monad_without_return_s
    with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t

  module type S2 =
    Monad_without_return_s2
    with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t

  module type S3 =
    Monad_without_return_s3
    with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t
end

(** {4 Functors} *)

(** [Of_functor], [Of_functor2], and [Of_functor3] generate map-like functions that work
    under some "functor", which is like a monad or applicative, except that it only
    supports [map]. *)
module Of_functor (F : sig
    type 'a t

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end) : Functor.S with type 'a t := 'a F.t

module Of_functor2 (F : sig
    type ('a, 'd) t

    val map : ('a, 'd) t -> f:('a -> 'b) -> ('b, 'd) t
  end) : Functor.S2 with type ('a, 'd) t := ('a, 'd) F.t

module Of_functor3 (F : sig
    type ('a, 'd, 'e) t

    val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
  end) : Functor.S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) F.t

(** [Of_applicative] and [Of_applicative2] can be used to generate map-like functions
    that can use applicative effects. See also [Of_monad], which gives more control over
    the relationship between side effects and monadic effects. *)
module Of_applicative (A : sig
    type 'a t

    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
  end) : Applicative.S with type 'a t := 'a A.t

(** See [Of_applicative]. *)
module Of_applicative2 (A : sig
    type ('a, 'e) t

    val return : 'a -> ('a, _) t
    val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
    val apply : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
  end) : Applicative.S2 with type ('a, 'e) t := ('a, 'e) A.t

(** [Of_monad] is similar to [Of_applicative]. There are two differences.

    Many functions generated by [Of_monad] can distinguish between [`Sequential]
    traversals and [`Parallel] traversals. This matters when you want to use monadic
    sequencing to control side effects. The functions generated by [Of_applicative] are
    only able to use the [`Parallel] behavior. Here are a couple examples of when you
    might care about this:

    - When using [Or_error], [map ~how:`Sequential t at ~f] would terminate the traversal
      as soon as an error has occurs. [map ~how:`Parallel t at ~f] would apply [f] to
      everything before attempting to compose the results, and if there is more than one
      error they will be combined.
    - When using [Deferred], [map ~how:`Sequential t at ~f] would wait for each deferred
      to become determined before moving on to the next value. [map ~how:`Parallel at ~f]
      would apply [f] to everything and then wait for the results to become determined
      asynchronously. *)
module Of_monad (M : sig
    type 'a t

    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val apply : [ `Custom of ('a -> 'b) t -> 'a t -> 'b t | `Define_using_bind ]
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end) : Monad.S with type 'a t := 'a M.t

(** See [Of_monad]. *)
module Of_monad2 (M : sig
    type ('a, 'e) t

    val return : 'a -> ('a, _) t
    val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t

    val apply
      : [ `Custom of ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t | `Define_using_bind ]

    val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  end) : Monad.S2 with type ('a, 'e) t := ('a, 'e) M.t

(** Like [Of_applicative], but without [return]. *)
module Of_applicative_without_return (A : sig
    type 'a t

    val map : 'a t -> f:('a -> 'b) -> 'b t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
  end) : Applicative_without_return.S with type 'a t := 'a A.t

(** Like [Of_applicative2], but without [return]. *)
module Of_applicative_without_return2 (A : sig
    type ('a, 'e) t

    val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
    val apply : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
  end) : Applicative_without_return.S2 with type ('a, 'e) t := ('a, 'e) A.t

module Of_applicative_without_return3 (A : sig
    type ('a, 'd, 'e) t

    val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
    val apply : ('a -> 'b, 'd, 'e) t -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t
  end) : Applicative_without_return.S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) A.t

(** Like [Of_monad], but without [return]. *)
module Of_monad_without_return (A : sig
    type 'a t

    val map : 'a t -> f:('a -> 'b) -> 'b t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end) : Monad_without_return.S with type 'a t := 'a A.t

(** Like [Of_monad2], but without [return]. *)
module Of_monad_without_return2 (A : sig
    type ('a, 'e) t

    val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
    val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  end) : Monad_without_return.S2 with type ('a, 'e) t := ('a, 'e) A.t

module Of_monad_without_return3 (A : sig
    type ('a, 'd, 'e) t

    val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
    val bind : ('a, 'd, 'e) t -> f:('a -> ('b, 'd, 'e) t) -> ('b, 'd, 'e) t
  end) : Monad_without_return.S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) A.t

(** {3 Recursive update} *)

(** [transform t a ~f] applies [f] everywhere it can inside of [a] once. It operates from
    the bottom up in one pass. [t] is used to find the children at each level, where the
    children are expected to have the same type as their parent. *)
val transform
  :  (unit -> 'a -> 'b, unit -> 'a -> 'b, [> mapper ]) t
  -> 'a
  -> f:('b -> 'b)
  -> 'b

(** [rewrite t a ~f] applies the rewrite rule [f] everywhere it can inside of [a] until it
    cannot be applied anywhere else. It operates from the bottom up, retrying subtrees
    each time a rule is applied successfully. [t] is used to find the children at each
    level, where the children are expected to have the same type as their parent. *)
val rewrite
  :  (unit -> 'a -> 'b, unit -> 'a -> 'b, [> mapper ]) t
  -> 'a
  -> f:('b -> 'a option)
  -> 'b

(** {3 Type equality} *)

(** An [Identical.t] is similar to a [Type_equal.t], but it relates two pairs of types
    with each other instead of merely two types. It is a more natural way of using an
    equality accessor than [Type_equal.t] would be, since you only need to match on one
    constructor. *)
module Identical : sig
  type ('a, 'b, 'at, 'bt) t = T : ('a, 'b, 'a, 'b) t
end

(** An equality is more powerful even than an isomorphism. It can be used to prove that
    the types are equal using the [identical] function. *)
val identical
  :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> equality ]) t
  -> ('a, 'b, 'at, 'bt) Identical.t

(** {3 Construction} *)

(** [construct] goes the opposite way to most access patterns. It allows you to
    construct a composite data structure without reading from one. *)
val construct : (_ -> _ -> 'b, _ -> _ -> 'bt, [> constructor ]) t -> 'b -> 'bt

(** {1 Custom mappings} *)

(** Accessors transform a mapping over inner data to a mapping over outer data. You can
    use accessors to transform your own mapping types by using the functors in this
    section. Here is an example of using [Mapper.Make] to define functions equivalent to
    [Accessor.map] and [Accessor.mapi]:

    {[
      include Mapper.Make (struct
          type ('a, 'b) t = 'a -> 'b

          let mapper map t at = map at ~f:t
        end)

      let mapi t at ~f = access t (fun (i, a) -> f i a) ([], at)

      let map t at ~f = mapi t at ~f:(fun [] a -> f a)
    ]} *)

include
  Custom_mappings_intf.S
  with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t
(** @inline *)

(** {1 Creating accessors} *)

(** {2 Avoiding the value restriction} *)

(** The value restriction will apply frequently to accessors you define. Unfortunately,
    even if you are not defining an accessor supporting polymorphic update, polymorphism
    is still critical for the normal type checking of accessors, because without it
    there could be no subtyping. [Accessor] addresses this problem by exposing its
    representation as a function and offering a syntax extension, as part of
    ppx_accessor, to eta expand the definition for you. The upshot is that you can
    define accessors like this and not think about the value restriction anymore:

    {[
      let foo =
        [%accessor
          Accessor.field ~get:(fun t -> t.foo) ~set:(fun t foo -> { t with foo })]
    ]} *)

(** {2 Deriving accessors} *)

(** ppx_accessor adds the ability to derive accessors given a type definition. See the
    ppx_accessor README for more information. *)

(** {2 "Well behaved" accessors } *)

(** Many of the functions for creating accessors are documented with conditions that are
    necessary for an accessor to be "well behaved". For most accessors, these conditions
    are sufficiently restrictive that the type parameters are not quite freely
    independent of each other.

    The properties generally make intuitive sense, but it can still be useful sometimes
    to define an accessor that is not always well behaved. Even [Accessor] itself
    defines a few badly behaved accessors. If you define an accessor that isn't well
    behaved, it is recommended that you document the conditions under which it is not
    well behaved. *)

(** {2 Creation functions} *)

(** {3 Field accessors} *)

(** [field ~get ~set] creates a field accessor. A field accesses exactly one value
    within a composite data structure. For the field to be well behaved, [get] and [set]
    should satisfy the following properties:

    - [get (set at a) = a]
    - [set at (get at) = at]
    - [set (set at a) b = set at b] *)
val field
  :  get:('at -> 'a)
  -> set:('at -> 'b -> 'bt)
  -> ('i -> 'a -> 'b, 'i -> 'at -> 'bt, [< field ]) t

(** [field'] is the same as [field], just with a slightly different interface. [field]
    is usually more convenient to use, but [field'] can be useful to allow [get] and
    [set] to share the computation of finding the location to modify. *)
val field'
  :  ('at -> 'a * ('b -> 'bt))
  -> ('i -> 'a -> 'b, 'i -> 'at -> 'bt, [< field ]) t

(** A [Field.t] is sufficient to define a field accessor, but the resulting accessor
    might not be as polymorphic as it could have been if defined by hand or using
    [@@deriving accessor]. *)
val of_field
  :  ([> `Set_and_create ], 'r, 'a) Base.Field.t_with_perm
  -> ('i -> 'a -> 'a, 'i -> 'r -> 'r, [< field ]) t

(** [fieldi] is the indexed version of [field]. *)
val fieldi
  :  get:('at -> 'i * 'a)
  -> set:('at -> 'b -> 'bt)
  -> ('i * 'it -> 'a -> 'b, 'it -> 'at -> 'bt, [< field ]) t

(** [fieldi'] is the indexed version of [field']. *)
val fieldi'
  :  ('at -> 'i * 'a * ('b -> 'bt))
  -> ('i * 'it -> 'a -> 'b, 'it -> 'at -> 'bt, [< field ]) t

(** A [Field.t] is sufficient to define an indexed field accessor, where the index is
    the name of the field as a string. The resulting accessor might not be as
    polymorphic as it could have been if defined by hand or using [@@deriving accessor].
*)
val of_fieldi
  :  ([> `Set_and_create ], 'r, 'a) Base.Field.t_with_perm
  -> (string * 'it -> 'a -> 'a, 'it -> 'r -> 'r, [< field ]) t

(** {3 Variant accessors} *)

(** [variant ~match_ ~construct] creates a variant accessor. A variant accesses at most
    one value within a composite data structure, and if it does access a value then that
    value is representative of the entire data structure. A well behaved variant should
    satisfy the following properties:

    - [match_ (construct a) = First a]
    - if [match_ at = First a] then [construct a = at]
    - if [match_ at = Second bt] then [at = bt] *)
val variant
  :  match_:('at -> ('a, 'bt) Either.t)
  -> construct:('b -> 'bt)
  -> ('i -> 'a -> 'b, 'i -> 'at -> 'bt, [< variant ]) t

(** [varianti] is the indexed version of [variant]. *)
val varianti
  :  match_:('at -> ('i * 'a, 'bt) Either.t)
  -> construct:('b -> 'bt)
  -> ('i * 'it -> 'a -> 'b, 'it -> 'at -> 'bt, [< variant ]) t

(** {3 Optional accessors} *)

(** [optional ~match_ ~set] creates an optional accessor. An optional accesses at most
    one value within a composite data structure. A well behaved optional should satisfy
    the following properties:

    - [match_ (set at a) = Either.First.map (match_ at) ~f:(const a)]
    - if [match_ at = First a] then [set at a = at]
    - if [match_ at = Second bt] then [at = bt] and [set at b = at]
    - [set (set at a) b = set at b] *)
val optional
  :  match_:('at -> ('a, 'bt) Either.t)
  -> set:('at -> 'b -> 'bt)
  -> ('i -> 'a -> 'b, 'i -> 'at -> 'bt, [< optional ]) t

(** [optional'] is the same as [optional], just with a slightly different interface.
    [optional] is usually more convenient to use, but [optional'] can be useful to allow
    [match_] and [set] to share the computation of finding the location to modify. *)
val optional'
  :  ('at -> ('a * ('b -> 'bt), 'bt) Either.t)
  -> ('i -> 'a -> 'b, 'i -> 'at -> 'bt, [< optional ]) t

(** [optionali] is the indexed version of [optional]. *)
val optionali
  :  match_:('at -> ('i * 'a, 'bt) Either.t)
  -> set:('at -> 'b -> 'bt)
  -> ('i * 'it -> 'a -> 'b, 'it -> 'at -> 'bt, [< optional ]) t

(** [optionali'] is the indexed version of [optional']. *)
val optionali'
  :  ('at -> ('i * 'a * ('b -> 'bt), 'bt) Either.t)
  -> ('i * 'it -> 'a -> 'b, 'it -> 'at -> 'bt, [< optional ]) t

(** [filter_index predicate] accesses the entire value if its index satisfies
    [predicate], otherwise it accesses nothing. Compose it with a many accessor to
    access a subset of values. *)
val filter_index
  :  ('i Index.t -> bool)
  -> ('i -> 'a -> 'a, 'i -> 'a -> 'a, [< optional ]) t

(** [filter_map_index f] is like [filter_index], but it can also modify the indices. *)
val filter_map_index
  :  ('i Index.t -> 'j Index.t option)
  -> ('j -> 'a -> 'a, 'i -> 'a -> 'a, [< optional ]) t

(** {3 Isomorphism accessors} *)

(** [isomorphism ~get ~construct] creates an isomorphism accessor. An isomorphism
    accesses exactly one value which exactly represents the entire data structure. A
    well behaved isomorphism should satisfy the following properties:

    - [get (construct b) = b]
    - [construct (get at) = at] *)
val isomorphism
  :  get:('at -> 'a)
  -> construct:('b -> 'bt)
  -> ('i -> 'a -> 'b, 'i -> 'at -> 'bt, [< isomorphism ]) t

(** [isomorphismi] is the indexed version of [isomorphism]. *)
val isomorphismi
  :  get:('at -> 'i * 'a)
  -> construct:('b -> 'bt)
  -> ('i * 'it -> 'a -> 'b, 'it -> 'at -> 'bt, [< isomorphism ]) t

(** [map_index f] applies [f] to the the indices that pass through it in a chain of
    composed accessors. *)
val map_index
  :  ('i Index.t -> 'j Index.t)
  -> ('j -> 'a -> 'b, 'i -> 'a -> 'b, [< isomorphism ]) t

(** {3 Mapper accessors} *)

(** [mapper map] creates a mapper accessor. A mapper can modify values inside a
    composite data structure, but cannot read anything out. A well behaved mapper should
    satisfy the following properties:

    - [map at ~f:Fn.id = at]
    - [map at ~f:(Fn.compose f g) = map (map at ~f:g) ~f] *)
val mapper
  :  ('at -> f:('a -> 'b) -> 'bt)
  -> ('i -> 'a -> 'b, 'i -> 'at -> 'bt, [< mapper ]) t

(** [mapperi] is the indexed version of [mapper]. *)
val mapperi
  :  ('at -> f:('i -> 'a -> 'b) -> 'bt)
  -> ('i * 'it -> 'a -> 'b, 'it -> 'at -> 'bt, [< mapper ]) t

(** {3 Many accessors} *)

(** [many traverse] creates a [many] accessor. A [many] accesses any number of values
    within a composite data structure.

    To define a [many] accessor, you must use [Accessor.Many.t], which is an applicative.
    You should traverse the data structure as necessary, and each time you reach a value
    that should be accessed, apply [Accessor.Many.access] to it.

    Here is an example of using [many] to define an accessor that reaches all the elements
    of a list:

    {[
      Accessor.many (fun at -> Accessor.Many.all (List.map at ~f:Accessor.Many.access))
    ]}

    A well behaved many should satisfy the same properties as a well behaved mapper, but
    generalized for an applicative setting. The properties themselves are uselessly
    complicated when written out, but here they are anyway. [A] and [B] are assumed to
    have the [of_many] function generated by [Many.Of_applicative], and [Compose] is
    assumed to be some functor behaving like [Applicative.Compose] that also uses
    [Many.Of_applicative] to generate an [of_many] function.

    - [A.of_many (traverse at) ~access:A.return = A.return at]
    - {[
      Compose(A)(B).of_many (traverse at) ~access:(fun a -> A.map (g a) ~f)
      = A.map (A.of_many (traverse at) ~access:g) ~f:(fun at ->
        B.of_many (traverse at) ~access:f)
    ]} *)
val many
  :  ('at -> ('bt, 'a, 'b) Many.t)
  -> ('i -> 'a -> 'b, 'i -> 'at -> 'bt, [< many ]) t

(** [manyi] is the indexed version of [many]. *)
val manyi
  :  ('at -> ('bt, 'i * 'a, 'b) Many.t)
  -> ('i * 'it -> 'a -> 'b, 'it -> 'at -> 'bt, [< many ]) t

(** {3 Nonempty accessors} *)

(** Nonempty accessors are defined very similarly to how many accessors are defined. The
    only difference is that you should use [Accessor.Nonempty] instead of [Accessor.Many].
    The practical difference between the two is that [Accessor.Nonempty] does not have a
    [return] function. *)

(** [nonempty traverse] creates a nonempty accessor. A nonempty accesses a nonzero
    number of values within a composite data structure.

    To define a [nonempty] accessor, you must use [Accessor.Nonempty.t], which is an
    applicative lacking [return]. You should traverse the data structure as necessary, and
    each time you reach a value that should be accessed, apply [Accessor.Nonempty.access]
    to it.

    Here is an example of using [nonempty] to define an accessor that reaches both
    components of a tuple:

    {[
      Accessor.nonempty (fun (a, b) ->
        let open Accessor.Nonempty.Let_syntax in
        let%map_open a = access a
        and b = access b in
        a, b)
    ]}

    A well behaved nonempty should satisfy the second property of a well behaved mapper,
    but generalized for an applicative setting. The property itself is uselessly
    complicated when written out, but here it is anyway. [A] and [B] are assumed to have
    the [of_nonempty] function generated by [Nonempty.Of_applicative_without_return], and
    [Compose] is assumed to be some functor behaving like
    [Applicative_without_return.Compose] that also uses
    [Nonempty.Of_applicative_without_return] to generate an [of_nonempty] function.

    {[
      Compose(A)(B).of_nonempty (traverse at) ~access:(fun a -> A.map (g a) ~f)
      = A.map (A.of_nonempty (traverse at) ~access:g) ~f:(fun at ->
        B.of_nonempty (traverse at) ~access:f)
    ]} *)
val nonempty
  :  ('at -> ('bt, 'a, 'b) Nonempty.t)
  -> ('i -> 'a -> 'b, 'i -> 'at -> 'bt, [< nonempty ]) t

(** [nonemptyi] is the indexed version of [nonempty]. *)
val nonemptyi
  :  ('at -> ('bt, 'i * 'a, 'b) Nonempty.t)
  -> ('i * 'it -> 'a -> 'b, 'it -> 'at -> 'bt, [< nonempty ]) t

(** {3 Getter accessors} *)

(** [getter get] creates a getter accessor. A getter reads exactly one value from a
    composite data structure. There are no properties necessary for a getter to be well
    behaved. *)
val getter : ('at -> 'a) -> ('i -> 'a -> _, 'i -> 'at -> _, [< getter ]) t

(** [getteri] is the indexed version of [getter]. *)
val getteri : ('at -> 'i * 'a) -> ('i * 'it -> 'a -> _, 'it -> 'at -> _, [< getter ]) t

(** {3 Optional getter accessors} *)

(** [optional_getter get] creates an optional getter accessor. An optional getter reads
    at most one value from a composite data structure. There are no properties necessary
    for an optional getter to be well behaved. *)
val optional_getter
  :  ('at -> 'a option)
  -> ('i -> 'a -> _, 'i -> 'at -> _, [< optional_getter ]) t

(** [optional_getteri] is the indexed version of [optional_getter]. *)
val optional_getteri
  :  ('at -> ('i * 'a) option)
  -> ('i * 'it -> 'a -> _, 'it -> 'at -> _, [< optional_getter ]) t

(** {3 Many getter accessors} *)

(** [many_getter map_reduce] creates a many_getter accessor. A many getter reads any
    number of values from a composite data structure. There are no properties necessary
    for a many getter to be well behaved.

    To define a many_getter, you must use the [Many_getter] interface. Like [Many], it has
    an [access] function to designate which values to access. Unlike [Many], instead of an
    applicative interface, it has [empty] and [append] (or [( @ )]) functions.

    Here is an example of defining a getter that reads all the elements of a list:

    {[
      Accessor.many_getter (fun at ->
        Accessor.Many_getter.of_list (List.map at ~f:Accessor.Many_getter.access))
    ]} *)
val many_getter
  :  ('at -> 'a Many_getter.t)
  -> ('i -> 'a -> 'b, 'i -> 'at -> 'bt, [< many_getter ]) t

(** [many_getteri] is the indexed version of [many_getter]. *)
val many_getteri
  :  ('at -> ('i * 'a) Many_getter.t)
  -> ('i * 'it -> 'a -> 'b, 'it -> 'at -> 'bt, [< many_getter ]) t

(** {3 Nonempty getter accessors} *)

(** Nonempty getter accessors are defined very similarly to how many getter accessors are
    defined. The only difference is that it uses [Nonempty_getter] instead of
    [Many_getter]. [Nonempty_getter] does not have [empty]. *)

(** [nonempty_getter map_reduce] creates a nonempty getter accessor. A nonempty getter
    reads at least one value from a composite data structure. There are no properties
    necessary for a nonempty getter to be well behaved.

    To define a nonempty_getter, you must use the [Nonempty_getter] interface. Like
    [Nonempty], it has an [access] function to designate which values to access. Unlike
    [Nonempty], instead of an applicative style interface, it has an [append] (or [( @ )])
    function.

    Here is an example of defining a getter that reads both of the components of a tuple:

    {[
      Accessor.nonempty_getter (fun (a, b) ->
        Accessor.Nonempty_getter.(access a @ access b))
    ]}
*)
val nonempty_getter
  :  ('at -> 'a Nonempty_getter.t)
  -> ('i -> 'a -> 'b, 'i -> 'at -> 'bt, [< nonempty_getter ]) t

(** [nonempty_getteri] is the indexed version of [nonempty_getter]. *)
val nonempty_getteri
  :  ('at -> ('i * 'a) Nonempty_getter.t)
  -> ('i * 'it -> 'a -> 'b, 'it -> 'at -> 'bt, [< nonempty_getter ]) t

(** {3 Constructor accessors} *)

(** [constructor construct] creates a constructor accessor. A constructor creates a
    composite data structure from an argument. There are no properties necessary for a
    constructor to be well behaved. *)
val constructor : ('b -> 'bt) -> (_ -> _ -> 'b, _ -> _ -> 'bt, [< constructor ]) t

(** A [Variant.t] is not sufficient to define a variant accessor, but is at least
    sufficient to define a constructor accessor. *)
val of_variant
  :  ('b -> 'bt) Base.Variant.t
  -> (_ -> _ -> 'b, _ -> _ -> 'bt, [< constructor ]) t

(** {1 Transforming accessors} *)

(** Turn an isomorphism around. [invert (isomorphism ~get:f ~construct:g)] is
    [isomorphism ~get:g ~construct:f]. *)
val invert
  :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> isomorphism ]) t
  -> ('i -> 'bt -> 'at, 'i -> 'b -> 'a, [< isomorphism ]) t

(** Turn a getter into a constructor. [getter_to_constructor (getter f)] is [constructor
    f]. *)
val getter_to_constructor
  :  (unit -> 'a -> _, unit -> 'at -> _, [> getter ]) t
  -> (_ -> _ -> 'at, _ -> _ -> 'a, [< constructor ]) t

(** Turn a constructor into a getter. [constructor_to_getter (constructor f)] is [getter
    f]. *)
val constructor_to_getter
  :  (_ -> _ -> 'b, _ -> _ -> 'bt, [> constructor ]) t
  -> ('i -> 'bt -> _, 'i -> 'b -> _, [< getter ]) t

(** Given a [many] accessor, generate a [field] accessor that accesses all the elements
    that would be accessed by the [many] accessor in the form of a list. When replacing,
    if the list is too short then later elements in the data structure are left alone, and
    if the list is too long then extraneous elements are not used.

    The resulting accessor is only well-behaved if you preserve the length of the list
    across getting and setting. *)
val many_to_list_field
  :  (unit, 'a, 'at, [> many ]) Simple.t
  -> (_, 'a list, 'at, [< field ]) Simple.t
