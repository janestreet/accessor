open! Core
open! Import

module Quickcheckable = struct
  module type S = sig
    type t [@@deriving quickcheck, sexp_of]
  end

  let tuple (type a b) (module A : S with type t = a) (module B : S with type t = b) =
    let module AB = struct
      type t = A.t * B.t [@@deriving quickcheck, sexp_of]
    end
    in
    (module AB : S with type t = a * b)
  ;;
end

module Testable = struct
  module type S = sig
    type t [@@deriving equal, quickcheck, sexp_of]
  end

  module Either (A : S) (B : S) = struct
    type t = (A.t, B.t) Either.t [@@deriving equal, quickcheck, sexp_of]
  end

  module Tuple (A : S) (B : S) = struct
    type t = A.t * B.t [@@deriving equal, quickcheck, sexp_of]
  end

  module Option (A : S) = struct
    type t = A.t option [@@deriving equal, quickcheck, sexp_of]
  end

  module List (A : S) = struct
    type t = A.t list [@@deriving equal, quickcheck, sexp_of]
  end

  module Bool_map (A : S) = struct
    type t = A.t Bool.Map.t [@@deriving equal, sexp_of]

    let quickcheck_generator = [%quickcheck.generator: (bool, A.t) Bool.Map.t]
    let quickcheck_shrinker = [%quickcheck.shrinker: (bool, A.t) Bool.Map.t]
    let quickcheck_observer = [%quickcheck.observer: (bool, A.t) Bool.Map.t]
  end

  module Bool_set = struct
    type t = Bool.Set.t [@@deriving equal, sexp_of]

    let quickcheck_generator = [%quickcheck.generator: bool Bool.Set.t]
    let quickcheck_shrinker = [%quickcheck.shrinker: bool Bool.Set.t]
    let quickcheck_observer = [%quickcheck.observer: bool Bool.Set.t]
  end
end

let test_eq (type t) equal sexp_of_t x y =
  (* Fake the availability of [compare], which ppx_assert requires. In practice, it only
     uses [compare] to test equality, so we can return any non-zero integer in the not
     equal case. We do this to avoid imposing the requirement of [deriving compare] on
     callers that may only support equality testing. *)
  let compare x y =
    match equal x y with
    | true -> 0
    | false -> 1
  in
  [%test_eq: t] x y
;;

let test
  (type e a)
  (module Env : Quickcheckable.S with type t = e)
  (module T : Quickcheckable.S with type t = a)
  accessor
  ~f
  =
  Quickcheck.test
    [%quickcheck.generator: Env.t * T.t]
    ~sexp_of:[%sexp_of: Env.t * T.t]
    ~shrinker:[%quickcheck.shrinker: Env.t * T.t]
    ~f:(fun (env, t) -> f (accessor env) t)
;;

let mapper
  (type a at)
  (module A : Testable.S with type t = a)
  (module At : Testable.S with type t = at)
  env
  accessor
  =
  test env (module At) accessor ~f:(fun accessor at ->
    test_eq [%equal: At.t] [%sexp_of: At.t] (Accessor.map accessor at ~f:Fn.id) at);
  test
    env
    (module struct
      type t = At.t * (A.t -> A.t) * (A.t -> A.t) [@@deriving quickcheck, sexp_of]
    end)
    accessor
    ~f:(fun accessor (at, f, g) ->
      test_eq
        [%equal: At.t]
        [%sexp_of: At.t]
        (Accessor.map accessor at ~f:(Fn.compose f g))
        (Accessor.map accessor (Accessor.map accessor at ~f:g) ~f))
;;

let many
  (type a at)
  (module A : Testable.S with type t = a)
  (module At : Testable.S with type t = at)
  env
  accessor
  =
  let module X : sig
    type 'a t [@@deriving equal, quickcheck, sexp_of]

    include Applicative.S with type 'a t := 'a t
    module Accessor : Accessor.Applicative.S with type 'a t := 'a t
  end = struct
    module T = struct
      type 'a t = A.t list * 'a [@@deriving equal, quickcheck, sexp_of]

      include Applicative.Make_using_map2 (struct
          type 'a t = A.t list * 'a

          let return a = [], a
          let map = `Define_using_map2
          let map2 (xs, a) (ys, b) ~f = xs @ ys, f a b
        end)
    end

    include T
    module Accessor = Accessor.Of_applicative (T)
  end
  in
  let module Y : module type of X = X in
  let module XY = struct
    module T = Applicative.Compose (X) (Y)
    include T
    module Accessor = Accessor.Of_applicative (T)
  end
  in
  test env (module At) accessor ~f:(fun accessor at ->
    test_eq
      [%equal: At.t X.t]
      [%sexp_of: At.t X.t]
      (X.Accessor.map accessor at ~f:X.return)
      (X.return at));
  test
    env
    (module struct
      type t = At.t * (A.t -> A.t X.t) * (A.t -> A.t Y.t) [@@deriving quickcheck, sexp_of]
    end)
    accessor
    ~f:(fun accessor (at, f, g) ->
      test_eq
        [%equal: At.t X.t Y.t]
        [%sexp_of: At.t X.t Y.t]
        (XY.Accessor.map accessor at ~f:(fun a -> Y.map (g a) ~f))
        (Y.map (Y.Accessor.map accessor at ~f:g) ~f:(fun at ->
           X.Accessor.map accessor at ~f)))
;;

let nonempty
  (type a at)
  (module A : Testable.S with type t = a)
  (module At : Testable.S with type t = at)
  env
  accessor
  =
  let module X : sig
    type 'a t [@@deriving equal, quickcheck, sexp_of]

    include Applicative_without_return.S with type 'a t := 'a t
    module Accessor : Accessor.Applicative_without_return.S with type 'a t := 'a t
  end = struct
    module T = struct
      type 'a t = A.t Nonempty_list.t * 'a [@@deriving equal, quickcheck, sexp_of]

      include Applicative_without_return.Make (struct
          type 'a t = A.t Nonempty_list.t * 'a

          let map (xs, a) ~f = xs, f a

          let apply (xs, f) (ys, a) =
            Nonempty_list.append xs (Nonempty_list.to_list ys), f a
          ;;
        end)
    end

    include T
    module Accessor = Accessor.Of_applicative_without_return (T)
  end
  in
  let module Y : module type of X = X in
  let module XY = struct
    module T = struct
      type 'a t = 'a X.t Y.t

      include Applicative_without_return.Make (struct
          type 'a t = 'a X.t Y.t

          let map t ~f = Y.map t ~f:(X.map ~f)
          let apply f a = Y.apply (Y.map f ~f:X.apply) a
        end)
    end

    include T
    module Accessor = Accessor.Of_applicative_without_return (T)
  end
  in
  test
    env
    (module struct
      type t = At.t * (A.t -> A.t X.t) * (A.t -> A.t Y.t) [@@deriving quickcheck, sexp_of]
    end)
    accessor
    ~f:(fun accessor (at, f, g) ->
      test_eq
        [%equal: At.t X.t Y.t]
        [%sexp_of: At.t X.t Y.t]
        (XY.Accessor.map accessor at ~f:(fun a -> Y.map (g a) ~f))
        (Y.map (Y.Accessor.map accessor at ~f:g) ~f:(fun at ->
           X.Accessor.map accessor at ~f)))
;;

let optional
  (type a at)
  (module A : Testable.S with type t = a)
  (module At : Testable.S with type t = at)
  env
  accessor
  =
  test
    env
    (Quickcheckable.tuple (module At) (module A))
    accessor
    ~f:(fun accessor (at, a) ->
      test_eq
        [%equal: (A.t, At.t) Either.t]
        [%sexp_of: (A.t, At.t) Either.t]
        (Accessor.match_ accessor (at.@(accessor) <- a))
        (Either.First.map (Accessor.match_ accessor at) ~f:(const a)));
  test env (module At) accessor ~f:(fun accessor at ->
    let bt =
      match Accessor.match_ accessor at with
      | First a -> at.@(accessor) <- a
      | Second bt -> bt
    in
    test_eq [%equal: At.t] [%sexp_of: At.t] at bt);
  test
    env
    (Quickcheckable.tuple (module At) (module A))
    accessor
    ~f:(fun accessor (at, a) ->
      test_eq
        [%equal: At.t]
        [%sexp_of: At.t]
        ((at.@(accessor) <- a).@(accessor) <- a)
        (at.@(accessor) <- a))
;;

let field
  (type a at)
  (module A : Testable.S with type t = a)
  (module At : Testable.S with type t = at)
  env
  accessor
  =
  test
    env
    (Quickcheckable.tuple (module At) (module A))
    accessor
    ~f:(fun accessor (at, a) ->
      test_eq [%equal: A.t] [%sexp_of: A.t] (at.@(accessor) <- a).@(accessor) a);
  test env (module At) accessor ~f:(fun accessor at ->
    test_eq [%equal: At.t] [%sexp_of: At.t] (at.@(accessor) <- at.@(accessor)) at);
  test
    env
    (Quickcheckable.tuple (module At) (module A))
    accessor
    ~f:(fun accessor (at, a) ->
      test_eq
        [%equal: At.t]
        [%sexp_of: At.t]
        ((at.@(accessor) <- a).@(accessor) <- a)
        (at.@(accessor) <- a))
;;

let variant
  (type a at)
  (module A : Testable.S with type t = a)
  (module At : Testable.S with type t = at)
  env
  accessor
  =
  test env (module A) accessor ~f:(fun accessor a ->
    test_eq
      [%equal: (A.t, At.t) Either.t]
      [%sexp_of: (A.t, At.t) Either.t]
      (Accessor.match_ accessor (Accessor.construct accessor a))
      (First a));
  test env (module At) accessor ~f:(fun accessor at ->
    let bt =
      match Accessor.match_ accessor at with
      | First a -> Accessor.construct accessor a
      | Second bt -> bt
    in
    test_eq [%equal: At.t] [%sexp_of: At.t] at bt)
;;

let isomorphism
  (type a at)
  (module A : Testable.S with type t = a)
  (module At : Testable.S with type t = at)
  env
  accessor
  =
  test env (module A) accessor ~f:(fun accessor a ->
    test_eq [%equal: A.t] [%sexp_of: A.t] (Accessor.construct accessor a).@(accessor) a);
  test env (module At) accessor ~f:(fun accessor at ->
    test_eq
      [%equal: At.t]
      [%sexp_of: At.t]
      (Accessor.construct accessor at.@(accessor))
      at)
;;
