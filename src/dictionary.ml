open! Base
open! Import
open Subtyping

(* This module is inspired by the approach described in the paper "Profunctor Optics", by
   Matthew Pickering, Jeremy Gibbons, and Nicolas Wu. Although there are significant
   differences, it's probably worth at least taking a look at in order to understand the
   ideas behind this module. *)

module Create = struct
  (* These types stand in for type class dictionaries. For example, an [Isomorphism.t] is
     like an instance of the following type class as would be written in Haskell:

     {v
        class Isomorphism w where
          f : (at -> a) -> (b -> bt) -> w a b -> w at bt
      v} *)

  module Isomorphism = struct
    type 'w t =
      { f :
          'a 'b 'at 'bt. get:('at -> 'a) -> construct:('b -> 'bt) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Field = struct
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> 'a * ('b -> 'bt)) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Variant = struct
    type 'w t =
      { f :
          'a 'b 'at 'bt. match_:('at -> ('a, 'bt) Either.t) -> construct:('b -> 'bt)
          -> ('a, 'b, 'w) Hk.t2 -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Constructor = struct
    type 'w t =
      { f : 'a 'b 'at 'bt. ('b -> 'bt) -> ('a, 'b, 'w) Hk.t2 -> ('at, 'bt, 'w) Hk.t2 }
    [@@unboxed]
  end

  module Getter = struct
    type 'w t =
      { f : 'a 'b 'at 'bt. ('at -> 'a) -> ('a, 'b, 'w) Hk.t2 -> ('at, 'bt, 'w) Hk.t2 }
    [@@unboxed]
  end

  module Optional = struct
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> ('a * ('b -> 'bt), 'bt) Either.t) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Optional_getter = struct
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> 'a option) -> ('a, 'b, 'w) Hk.t2 -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Nonempty = struct
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> ('bt, 'a, 'b) Nonempty.t) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Nonempty_getter = struct
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> 'a Nonempty_getter.t) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Many = struct
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> ('bt, 'a, 'b) Many.t) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Many_getter = struct
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> 'a Many_getter.t) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  module Mapper = struct
    type 'w t =
      { f :
          'a 'b 'at 'bt. ('at -> f:('a -> 'b) -> 'bt) -> ('a, 'b, 'w) Hk.t2
          -> ('at, 'bt, 'w) Hk.t2
      }
    [@@unboxed]
  end

  (* The type classes above are related with this GADT, using a phantom polymorphic
     variant type index. *)
  type ('c, 'w) t =
    | Equality : ([> equality ], _) t
    | Isomorphism : 'w Isomorphism.t -> ([> isomorphism ], 'w) t
    | Field : 'w Field.t -> ([> field ], 'w) t
    | Variant : 'w Variant.t -> ([> variant ], 'w) t
    | Constructor : 'w Constructor.t -> ([> constructor ], 'w) t
    | Getter : 'w Getter.t -> ([> getter ], 'w) t
    | Optional : 'w Optional.t -> ([> optional ], 'w) t
    | Optional_getter : 'w Optional_getter.t -> ([> optional_getter ], 'w) t
    | Nonempty : 'w Nonempty.t -> ([> nonempty ], 'w) t
    | Nonempty_getter : 'w Nonempty_getter.t -> ([> nonempty_getter ], 'w) t
    | Many : 'w Many.t -> ([> many ], 'w) t
    | Many_getter : 'w Many_getter.t -> ([> many_getter ], 'w) t
    | Mapper : 'w Mapper.t -> ([> mapper ], 'w) t

  let equality = Equality
  let isomorphism isomorphism = Isomorphism isomorphism
  let field field = Field field
  let variant variant = Variant variant
  let constructor constructor = Constructor constructor
  let getter getter = Getter getter
  let optional optional = Optional optional
  let optional_getter optional_getter = Optional_getter optional_getter
  let nonempty nonempty = Nonempty nonempty
  let nonempty_getter nonempty_getter = Nonempty_getter nonempty_getter
  let many many = Many many
  let many_getter many_getter = Many_getter many_getter
  let mapper mapper = Mapper mapper
end

type nonrec ('c, 'w) t = ('c, 'w) Create.t

module Run = struct
  (* These functions define how the coercions between different kinds of accessors
     actually work. For example, an accessor that is defined as a field can be used as a
     getter, so the [field] function includes a case for [Getter], and in that case it
     only uses the [get] ability of the defined field. *)

  let equality (_ : _ t) = Fn.id

  type 'w constructor_hack =
    | Constructor_hack : ([< constructor ], 'w) t -> 'w constructor_hack
  [@@unboxed]

  let constructor t =
    let (Constructor_hack (Constructor { f })) = Constructor_hack t in
    f
  ;;

  type 'w field_hack = Field_hack : ([< field ], 'w) t -> 'w field_hack [@@unboxed]

  let field t field_f =
    let (Field_hack t) = Field_hack t in
    match t with
    | Field { f } -> f field_f
    | Getter { f } -> f (fun at -> fst (field_f at))
    | Optional { f } -> f (fun at -> First (field_f at))
    | Optional_getter { f } -> f (fun at -> Some (fst (field_f at)))
    | Nonempty { f } ->
      f (fun at ->
        let open Nonempty.Let_syntax in
        let a, construct = field_f at in
        let%map b = Nonempty.access a in
        construct b)
    | Nonempty_getter { f } -> f (fun at -> Nonempty_getter.access (fst (field_f at)))
    | Many { f } ->
      f (fun at ->
        let open Many.Let_syntax in
        let a, construct = field_f at in
        let%map b = Many.access a in
        construct b)
    | Many_getter { f } -> f (fun at -> Many_getter.access (fst (field_f at)))
    | Mapper { f } ->
      f (fun at ~f ->
        let a, construct = field_f at in
        construct (f a))
  ;;

  type 'w getter_hack = Getter_hack : ([< getter ], 'w) t -> 'w getter_hack [@@unboxed]

  let getter t get =
    let (Getter_hack t) = Getter_hack t in
    match t with
    | Getter { f } -> f get
    | Optional_getter { f } -> f (fun at -> Some (get at))
    | Nonempty_getter { f } -> f (fun at -> Nonempty_getter.access (get at))
    | Many_getter { f } -> f (fun at -> Many_getter.access (get at))
  ;;

  type 'w isomorphism_hack =
    | Isomorphism_hack : ([< isomorphism ], 'w) t -> 'w isomorphism_hack
  [@@unboxed]

  let isomorphism t ~get ~construct =
    let (Isomorphism_hack t) = Isomorphism_hack t in
    match t with
    | Isomorphism { f } -> f ~get ~construct
    | Field { f } -> f (fun at -> get at, construct)
    | Variant { f } -> f ~match_:(fun at -> First (get at)) ~construct
    | Constructor { f } -> f construct
    | Getter { f } -> f get
    | Optional { f } -> f (fun at -> First (get at, construct))
    | Optional_getter { f } -> f (fun at -> Some (get at))
    | Nonempty { f } ->
      f (fun at ->
        let open Nonempty.Let_syntax in
        let%map b = Nonempty.access (get at) in
        construct b)
    | Nonempty_getter { f } -> f (fun at -> Nonempty_getter.access (get at))
    | Many { f } ->
      f (fun at ->
        let open Many.Let_syntax in
        let%map b = Many.access (get at) in
        construct b)
    | Many_getter { f } -> f (fun at -> Many_getter.access (get at))
    | Mapper { f } -> f (fun at ~f -> construct (f (get at)))
  ;;

  type 'w mapper_hack = Mapper_hack : ([< mapper ], 'w) t -> 'w mapper_hack [@@unboxed]

  let mapper t =
    let (Mapper_hack (Mapper { f })) = Mapper_hack t in
    f
  ;;

  type 'w many_hack = Many_hack : ([< many ], 'w) t -> 'w many_hack [@@unboxed]

  let many t traverse =
    let (Many_hack t) = Many_hack t in
    match t with
    | Many { f } -> f traverse
    | Many_getter { f } -> f (fun at -> Many_getter.of_many (traverse at))
    | Mapper { f } -> f (fun at ~f -> Ident.of_many (traverse at) ~access:f)
  ;;

  type 'w many_getter_hack =
    | Many_getter_hack : ([< many_getter ], 'w) t -> 'w many_getter_hack
  [@@unboxed]

  let many_getter t =
    let (Many_getter_hack (Many_getter { f })) = Many_getter_hack t in
    f
  ;;

  type 'w nonempty_hack = Nonempty_hack : ([< nonempty ], 'w) t -> 'w nonempty_hack
  [@@unboxed]

  let nonempty t traverse =
    let (Nonempty_hack t) = Nonempty_hack t in
    match t with
    | Nonempty { f } -> f traverse
    | Nonempty_getter { f } -> f (fun at -> Nonempty_getter.of_nonempty (traverse at))
    | Many { f } -> f (fun at -> Many.of_nonempty (traverse at))
    | Many_getter { f } -> f (fun at -> Many_getter.of_nonempty (traverse at))
    | Mapper { f } -> f (fun at ~f -> Ident.of_nonempty (traverse at) ~access:f)
  ;;

  type 'w nonempty_getter_hack =
    | Nonempty_getter_hack : ([< nonempty_getter ], 'w) t -> 'w nonempty_getter_hack
  [@@unboxed]

  let nonempty_getter t traverse =
    let (Nonempty_getter_hack t) = Nonempty_getter_hack t in
    match t with
    | Nonempty_getter { f } -> f traverse
    | Many_getter { f } -> f (fun at -> Many_getter.of_nonempty_getter (traverse at))
  ;;

  type 'w optional_hack = Optional_hack : ([< optional ], 'w) t -> 'w optional_hack
  [@@unboxed]

  let optional t optional_f =
    let (Optional_hack t) = Optional_hack t in
    match t with
    | Optional { f } -> f optional_f
    | Optional_getter { f } ->
      f (fun at ->
        match optional_f at with
        | First (a, _) -> Some a
        | Second _ -> None)
    | Many { f } ->
      f (fun at ->
        let open Many.Let_syntax in
        match optional_f at with
        | Either.First (a, construct) -> Many.access a >>| construct
        | Second bt -> return bt)
    | Many_getter { f } ->
      f (fun at ->
        match optional_f at with
        | Either.First (a, _) -> Many_getter.access a
        | Second _ -> Many_getter.empty)
    | Mapper { f } ->
      f (fun at ~f ->
        match optional_f at with
        | Either.First (a, construct) -> construct (f a)
        | Second bt -> bt)
  ;;

  type 'w optional_getter_hack =
    | Optional_getter_hack : ([< optional_getter ], 'w) t -> 'w optional_getter_hack
  [@@unboxed]

  let optional_getter t get_option =
    let (Optional_getter_hack t) = Optional_getter_hack t in
    match t with
    | Optional_getter { f } -> f get_option
    | Many_getter { f } ->
      f (fun at ->
        match get_option at with
        | Some a -> Many_getter.access a
        | None -> Many_getter.empty)
  ;;

  type 'w variant_hack = Variant_hack : ([< variant ], 'w) t -> 'w variant_hack
  [@@unboxed]

  let variant t ~match_ ~construct =
    let (Variant_hack t) = Variant_hack t in
    match t with
    | Variant { f } -> f ~match_ ~construct
    | Constructor { f } -> f construct
    | Optional { f } ->
      f (fun at ->
        match match_ at with
        | First a -> First (a, construct)
        | Second _ as bt -> bt)
    | Optional_getter { f } ->
      f (fun at ->
        match match_ at with
        | First a -> Some a
        | Second _ -> None)
    | Many { f } ->
      f (fun at ->
        let open Many.Let_syntax in
        match match_ at with
        | Either.First a -> Many.access a >>| construct
        | Second bt -> return bt)
    | Many_getter { f } ->
      f (fun at ->
        match match_ at with
        | Either.First a -> Many_getter.access a
        | Second _ -> Many_getter.empty)
    | Mapper { f } ->
      f (fun at ~f ->
        match match_ at with
        | Either.First a -> construct (f a)
        | Second bt -> bt)
  ;;
end
