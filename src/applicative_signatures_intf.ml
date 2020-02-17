open! Base
open! Import
open Subtyping

module type Applicative_general = sig
  type ('inner, 'outer, 'kind) accessor
  type 'a optional_args
  type ('a, 'e) t

  val map
    : ((unit -> 'a -> 'b, unit -> 'at -> 'bt, [> many ]) accessor
       -> 'at
       -> f:('a -> ('b, 'e) t)
       -> ('bt, 'e) t)
        optional_args

  val mapi
    : (('i -> 'a -> 'b, unit -> 'at -> 'bt, [> many ]) accessor
       -> 'at
       -> f:('i Index.t -> 'a -> ('b, 'e) t)
       -> ('bt, 'e) t)
        optional_args

  val all
    :  (unit -> ('a, 'e) t -> 'a, unit -> 'at -> 'bt, [> many ]) accessor
    -> 'at
    -> ('bt, 'e) t

  val all_unit
    :  (unit -> (unit, 'e) t -> _, unit -> 'at -> _, [> many_getter ]) accessor
    -> 'at
    -> (unit, 'e) t

  val iter
    : ((unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) accessor
       -> 'at
       -> f:('a -> (unit, 'e) t)
       -> (unit, 'e) t)
        optional_args

  val iteri
    : (('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) accessor
       -> 'at
       -> f:('i Index.t -> 'a -> (unit, 'e) t)
       -> (unit, 'e) t)
        optional_args

  val sum
    : ((module Container.Summable with type t = 'sum)
       -> (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) accessor
       -> 'at
       -> f:('a -> ('sum, 'e) t)
       -> ('sum, 'e) t)
        optional_args

  val sumi
    : ((module Container.Summable with type t = 'sum)
       -> ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) accessor
       -> 'at
       -> f:('i Index.t -> 'a -> ('sum, 'e) t)
       -> ('sum, 'e) t)
        optional_args

  val count
    : ((unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) accessor
       -> 'at
       -> f:('a -> (bool, 'e) t)
       -> (int, 'e) t)
        optional_args

  val counti
    : (('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) accessor
       -> 'at
       -> f:('i Index.t -> 'a -> (bool, 'e) t)
       -> (int, 'e) t)
        optional_args

  val map_reduce
    : ((unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) accessor
       -> 'at
       -> empty:'b
       -> combine:('b -> 'b -> 'b)
       -> f:('a -> ('b, 'e) t)
       -> ('b, 'e) t)
        optional_args

  val map_reducei
    : (('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) accessor
       -> 'at
       -> empty:'b
       -> combine:('b -> 'b -> 'b)
       -> f:('i Index.t -> 'a -> ('b, 'e) t)
       -> ('b, 'e) t)
        optional_args

  val map_reduce_nonempty
    : ((unit -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) accessor
       -> 'at
       -> combine:('b -> 'b -> 'b)
       -> f:('a -> ('b, 'e) t)
       -> ('b, 'e) t)
        optional_args

  val map_reduce_nonemptyi
    : (('i -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) accessor
       -> 'at
       -> combine:('b -> 'b -> 'b)
       -> f:('i Index.t -> 'a -> ('b, 'e) t)
       -> ('b, 'e) t)
        optional_args
end

module type Applicative_without_return_general = sig
  type ('inner, 'outer, 'kind) accessor
  type 'a optional_args
  type ('a, 'd, 'e) t

  val map
    : ((unit -> 'a -> 'b, unit -> 'at -> 'bt, [> nonempty ]) accessor
       -> 'at
       -> f:('a -> ('b, 'd, 'e) t)
       -> ('bt, 'd, 'e) t)
        optional_args

  val mapi
    : (('i -> 'a -> 'b, unit -> 'at -> 'bt, [> nonempty ]) accessor
       -> 'at
       -> f:('i Index.t -> 'a -> ('b, 'd, 'e) t)
       -> ('bt, 'd, 'e) t)
        optional_args

  val iter
    : ((unit -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) accessor
       -> 'at
       -> f:('a -> (unit, 'd, 'e) t)
       -> (unit, 'd, 'e) t)
        optional_args

  val iteri
    : (('i -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) accessor
       -> 'at
       -> f:('i Index.t -> 'a -> (unit, 'd, 'e) t)
       -> (unit, 'd, 'e) t)
        optional_args

  val sum
    : ((module Container.Summable with type t = 'sum)
       -> (unit -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) accessor
       -> 'at
       -> f:('a -> ('sum, 'd, 'e) t)
       -> ('sum, 'd, 'e) t)
        optional_args

  val sumi
    : ((module Container.Summable with type t = 'sum)
       -> ('i -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) accessor
       -> 'at
       -> f:('i Index.t -> 'a -> ('sum, 'd, 'e) t)
       -> ('sum, 'd, 'e) t)
        optional_args

  val count
    : ((unit -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) accessor
       -> 'at
       -> f:('a -> (bool, 'd, 'e) t)
       -> (int, 'd, 'e) t)
        optional_args

  val counti
    : (('i -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) accessor
       -> 'at
       -> f:('i Index.t -> 'a -> (bool, 'd, 'e) t)
       -> (int, 'd, 'e) t)
        optional_args

  val all
    :  (unit -> ('a, 'd, 'e) t -> 'a, unit -> 'at -> 'bt, [> nonempty ]) accessor
    -> 'at
    -> ('bt, 'd, 'e) t

  val all_unit
    :  (unit -> (unit, 'd, 'e) t -> _, unit -> 'at -> _, [> nonempty_getter ]) accessor
    -> 'at
    -> (unit, 'd, 'e) t

  val map_reduce_nonempty
    : ((unit -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) accessor
       -> 'at
       -> combine:('b -> 'b -> 'b)
       -> f:('a -> ('b, 'd, 'e) t)
       -> ('b, 'd, 'e) t)
        optional_args

  val map_reduce_nonemptyi
    : (('i -> 'a -> _, unit -> 'at -> _, [> nonempty_getter ]) accessor
       -> 'at
       -> combine:('b -> 'b -> 'b)
       -> f:('i Index.t -> 'a -> ('b, 'd, 'e) t)
       -> ('b, 'd, 'e) t)
        optional_args
end

module type Functor_s3 = sig
  type ('inner, 'outer, 'kind) accessor
  type ('a, 'd, 'e) t

  val map
    :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> field ]) accessor
    -> 'at
    -> f:('a -> ('b, 'd, 'e) t)
    -> ('bt, 'd, 'e) t

  val mapi
    :  ('i -> 'a -> 'b, unit -> 'at -> 'bt, [> field ]) accessor
    -> 'at
    -> f:('i Index.t -> 'a -> ('b, 'd, 'e) t)
    -> ('bt, 'd, 'e) t

  val all
    :  (unit -> ('a, 'd, 'e) t -> 'a, unit -> 'at -> 'bt, [> field ]) accessor
    -> 'at
    -> ('bt, 'd, 'e) t
end

module type Functor_s2 = sig
  type ('a, 'd) t

  include Functor_s3 with type ('a, 'd, _) t := ('a, 'd) t
end

module type Functor_s = sig
  type 'a t

  include Functor_s2 with type ('a, _) t := 'a t
end

module type Applicative_without_return_s3 =
  Applicative_without_return_general with type 'a optional_args := 'a

module type Applicative_s2 = Applicative_general with type 'a optional_args := 'a

module type Applicative_without_return_s2 = sig
  type ('a, 'e) t

  include Applicative_without_return_s3 with type ('a, _, 'e) t := ('a, 'e) t
end

module type Applicative_s = sig
  type 'a t

  include Applicative_s2 with type ('a, _) t := 'a t
end

module type Applicative_without_return_s = sig
  type 'a t

  include Applicative_without_return_s2 with type ('a, _) t := 'a t
end

module type Monad_without_return_s3 =
  Applicative_without_return_general
  with type 'a optional_args := ?how:[ `Parallel | `Sequential ] -> 'a

module type Monad_s2 =
  Applicative_general
  with type 'a optional_args := ?how:[ `Parallel | `Sequential ] -> 'a

module type Monad_without_return_s2 = sig
  type ('a, 'e) t

  include Monad_without_return_s3 with type ('a, _, 'e) t := ('a, 'e) t
end

module type Monad_s = sig
  type 'a t

  include Monad_s2 with type ('a, _) t := 'a t
end

module type Monad_without_return_s = sig
  type 'a t

  include Monad_without_return_s2 with type ('a, _) t := 'a t
end
