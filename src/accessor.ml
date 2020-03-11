open! Base
open! Import
open Applicative_signatures_intf
open With_return
module Index = Index
module Subtyping = Subtyping

type ('inner, 'outer, 'kind) t =
  { f : 'w. ('kind, 'w) Dictionary.t -> ('inner, 'w) Mapping.t -> ('outer, 'w) Mapping.t
  }
[@@unboxed]

module Simple = struct
  type nonrec ('index, 'inner, 'outer, 'kind) t =
    ('index -> 'inner -> 'inner, 'index -> 'outer -> 'outer, 'kind) t
end

module Prim = struct
  let id = { f = (fun _ mapping -> mapping) }

  let compose t1 t2 =
    { f = (fun dictionary mapping -> t1.f dictionary (t2.f dictionary mapping)) }
  ;;

  module Equality = struct
    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t
      end) =
    struct
      module M = Mapping.Make4 (T)

      let access { f } = M.injected ~f:(f Dictionary.Create.equality)
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t
      end) =
      Make_access4 (struct
        type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c) T.t
      end)

    module Make_access (T : sig
        type ('a, 'b) t
      end) =
      Make_access3 (struct
        type ('a, 'b, 'c) t = ('a, 'b) T.t
      end)
  end

  module Isomorphism = struct
    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val isomorphism
          :  get:('at -> 'a)
          -> construct:('b -> 'bt)
          -> ('a, 'b, 'c, 'd) t
          -> ('at, 'bt, 'c, 'd) t
      end) =
    struct
      module M = Mapping.Make4 (T)

      let isomorphism =
        { Dictionary.Create.Isomorphism.f =
            (fun ~get ~construct -> M.projected ~f:(T.isomorphism ~get ~construct))
        }
      ;;

      let access { f } = M.injected ~f:(f (Dictionary.Create.isomorphism isomorphism))
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val isomorphism
          :  get:('at -> 'a)
          -> construct:('b -> 'bt)
          -> ('a, 'b, 'c) t
          -> ('at, 'bt, 'c) t
      end) =
      Make_access4 (struct
        type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c) T.t

        let isomorphism = T.isomorphism
      end)

    module Make_access (T : sig
        type ('a, 'b) t

        val isomorphism
          :  get:('at -> 'a)
          -> construct:('b -> 'bt)
          -> ('a, 'b) t
          -> ('at, 'bt) t
      end) =
      Make_access3 (struct
        type ('a, 'b, 'c) t = ('a, 'b) T.t

        let isomorphism = T.isomorphism
      end)
  end

  module Field = struct
    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val field : ('at -> 'a * ('b -> 'bt)) -> ('a, 'b, 'c, 'd) t -> ('at, 'bt, 'c, 'd) t
      end) =
    struct
      module M = Mapping.Make4 (T)

      let field = { Dictionary.Create.Field.f = (fun f -> M.projected ~f:(T.field f)) }
      let access { f } = M.injected ~f:(f (Dictionary.Create.field field))
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val field : ('at -> 'a * ('b -> 'bt)) -> ('a, 'b, 'c) t -> ('at, 'bt, 'c) t
      end) =
      Make_access4 (struct
        type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c) T.t

        let field = T.field
      end)

    module Make_access (T : sig
        type ('a, 'b) t

        val field : ('at -> 'a * ('b -> 'bt)) -> ('a, 'b) t -> ('at, 'bt) t
      end) =
      Make_access3 (struct
        type ('a, 'b, 'c) t = ('a, 'b) T.t

        let field = T.field
      end)
  end

  module Variant = struct
    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val variant
          :  match_:('at -> ('a, 'bt) Either.t)
          -> construct:('b -> 'bt)
          -> ('a, 'b, 'c, 'd) t
          -> ('at, 'bt, 'c, 'd) t
      end) =
    struct
      module M = Mapping.Make4 (T)

      let variant =
        { Dictionary.Create.Variant.f =
            (fun ~match_ ~construct -> M.projected ~f:(T.variant ~match_ ~construct))
        }
      ;;

      let access { f } = M.injected ~f:(f (Dictionary.Create.variant variant))
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val variant
          :  match_:('at -> ('a, 'bt) Either.t)
          -> construct:('b -> 'bt)
          -> ('a, 'b, 'c) t
          -> ('at, 'bt, 'c) t
      end) =
      Make_access4 (struct
        type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c) T.t

        let variant = T.variant
      end)

    module Make_access (T : sig
        type ('a, 'b) t

        val variant
          :  match_:('at -> ('a, 'bt) Either.t)
          -> construct:('b -> 'bt)
          -> ('a, 'b) t
          -> ('at, 'bt) t
      end) =
      Make_access3 (struct
        type ('a, 'b, 'c) t = ('a, 'b) T.t

        let variant = T.variant
      end)
  end

  module Constructor = struct
    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val constructor : ('b -> 'bt) -> ('a, 'b, 'c, 'd) t -> ('at, 'bt, 'c, 'd) t
      end) =
    struct
      module M = Mapping.Make4 (T)

      let constructor =
        { Dictionary.Create.Constructor.f =
            (fun construct -> M.projected ~f:(T.constructor construct))
        }
      ;;

      let access { f } = M.injected ~f:(f (Dictionary.Create.constructor constructor))
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val constructor : ('b -> 'bt) -> ('a, 'b, 'c) t -> ('at, 'bt, 'c) t
      end) =
      Make_access4 (struct
        type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c) T.t

        let constructor = T.constructor
      end)

    module Make_access (T : sig
        type ('a, 'b) t

        val constructor : ('b -> 'bt) -> ('a, 'b) t -> ('at, 'bt) t
      end) =
      Make_access3 (struct
        type ('a, 'b, 'c) t = ('a, 'b) T.t

        let constructor = T.constructor
      end)
  end

  module Getter = struct
    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val getter : ('at -> 'a) -> ('a, 'b, 'c, 'd) t -> ('at, 'bt, 'c, 'd) t
      end) =
    struct
      module M = Mapping.Make4 (T)

      let getter =
        { Dictionary.Create.Getter.f = (fun get -> M.projected ~f:(T.getter get)) }
      ;;

      let access { f } = M.injected ~f:(f (Dictionary.Create.getter getter))
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val getter : ('at -> 'a) -> ('a, 'b, 'c) t -> ('at, 'bt, 'c) t
      end) =
      Make_access4 (struct
        type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c) T.t

        let getter = T.getter
      end)

    module Make_access (T : sig
        type ('a, 'b) t

        val getter : ('at -> 'a) -> ('a, 'b) t -> ('at, 'bt) t
      end) =
      Make_access3 (struct
        type ('a, 'b, 'c) t = ('a, 'b) T.t

        let getter = T.getter
      end)
  end

  module Optional = struct
    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val optional
          :  ('at -> ('a * ('b -> 'bt), 'bt) Either.t)
          -> ('a, 'b, 'c, 'd) t
          -> ('at, 'bt, 'c, 'd) t
      end) =
    struct
      module M = Mapping.Make4 (T)

      let optional =
        { Dictionary.Create.Optional.f = (fun f -> M.projected ~f:(T.optional f)) }
      ;;

      let access { f } = M.injected ~f:(f (Dictionary.Create.optional optional))
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val optional
          :  ('at -> ('a * ('b -> 'bt), 'bt) Either.t)
          -> ('a, 'b, 'c) t
          -> ('at, 'bt, 'c) t
      end) =
      Make_access4 (struct
        type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c) T.t

        let optional = T.optional
      end)

    module Make_access (T : sig
        type ('a, 'b) t

        val optional
          :  ('at -> ('a * ('b -> 'bt), 'bt) Either.t)
          -> ('a, 'b) t
          -> ('at, 'bt) t
      end) =
      Make_access3 (struct
        type ('a, 'b, 'c) t = ('a, 'b) T.t

        let optional = T.optional
      end)
  end

  module Optional_getter = struct
    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val optional_getter
          :  ('at -> 'a option)
          -> ('a, 'b, 'c, 'd) t
          -> ('at, 'bt, 'c, 'd) t
      end) =
    struct
      module M = Mapping.Make4 (T)

      let optional_getter =
        { Dictionary.Create.Optional_getter.f =
            (fun get -> M.projected ~f:(T.optional_getter get))
        }
      ;;

      let access { f } =
        M.injected ~f:(f (Dictionary.Create.optional_getter optional_getter))
      ;;
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val optional_getter : ('at -> 'a option) -> ('a, 'b, 'c) t -> ('at, 'bt, 'c) t
      end) =
      Make_access4 (struct
        type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c) T.t

        let optional_getter = T.optional_getter
      end)

    module Make_access (T : sig
        type ('a, 'b) t

        val optional_getter : ('at -> 'a option) -> ('a, 'b) t -> ('at, 'bt) t
      end) =
      Make_access3 (struct
        type ('a, 'b, 'c) t = ('a, 'b) T.t

        let optional_getter = T.optional_getter
      end)
  end

  module Nonempty = struct
    include Nonempty

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val nonempty
          :  ('at -> ('bt, 'a, 'b) Nonempty.t)
          -> ('a, 'b, 'c, 'd) t
          -> ('at, 'bt, 'c, 'd) t
      end) =
    struct
      module M = Mapping.Make4 (T)

      let nonempty =
        { Dictionary.Create.Nonempty.f =
            (fun traverse -> M.projected ~f:(T.nonempty traverse))
        }
      ;;

      let access { f } = M.injected ~f:(f (Dictionary.Create.nonempty nonempty))
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val nonempty
          :  ('at -> ('bt, 'a, 'b) Nonempty.t)
          -> ('a, 'b, 'c) t
          -> ('at, 'bt, 'c) t
      end) =
      Make_access4 (struct
        type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c) T.t

        let nonempty = T.nonempty
      end)

    module Make_access (T : sig
        type ('a, 'b) t

        val nonempty : ('at -> ('bt, 'a, 'b) Nonempty.t) -> ('a, 'b) t -> ('at, 'bt) t
      end) =
      Make_access3 (struct
        type ('a, 'b, 'c) t = ('a, 'b) T.t

        let nonempty = T.nonempty
      end)
  end

  module Nonempty_getter = struct
    include Nonempty_getter

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val nonempty_getter
          :  ('at -> 'a Nonempty_getter.t)
          -> ('a, 'b, 'c, 'd) t
          -> ('at, 'bt, 'c, 'd) t
      end) =
    struct
      module M = Mapping.Make4 (T)

      let nonempty_getter =
        { Dictionary.Create.Nonempty_getter.f =
            (fun traverse -> M.projected ~f:(T.nonempty_getter traverse))
        }
      ;;

      let access { f } =
        M.injected ~f:(f (Dictionary.Create.nonempty_getter nonempty_getter))
      ;;
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val nonempty_getter
          :  ('at -> 'a Nonempty_getter.t)
          -> ('a, 'b, 'c) t
          -> ('at, 'bt, 'c) t
      end) =
      Make_access4 (struct
        type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c) T.t

        let nonempty_getter = T.nonempty_getter
      end)

    module Make_access (T : sig
        type ('a, 'b) t

        val nonempty_getter : ('at -> 'a Nonempty_getter.t) -> ('a, 'b) t -> ('at, 'bt) t
      end) =
      Make_access3 (struct
        type ('a, 'b, 'c) t = ('a, 'b) T.t

        let nonempty_getter = T.nonempty_getter
      end)
  end

  module Many = struct
    include Many

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val many
          :  ('at -> ('bt, 'a, 'b) Many.t)
          -> ('a, 'b, 'c, 'd) t
          -> ('at, 'bt, 'c, 'd) t
      end) =
    struct
      module M = Mapping.Make4 (T)

      let many =
        { Dictionary.Create.Many.f = (fun traverse -> M.projected ~f:(T.many traverse)) }
      ;;

      let access { f } = M.injected ~f:(f (Dictionary.Create.many many))
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val many : ('at -> ('bt, 'a, 'b) Many.t) -> ('a, 'b, 'c) t -> ('at, 'bt, 'c) t
      end) =
      Make_access4 (struct
        type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c) T.t

        let many = T.many
      end)

    module Make_access (T : sig
        type ('a, 'b) t

        val many : ('at -> ('bt, 'a, 'b) Many.t) -> ('a, 'b) t -> ('at, 'bt) t
      end) =
      Make_access3 (struct
        type ('a, 'b, 'c) t = ('a, 'b) T.t

        let many = T.many
      end)
  end

  module Many_getter = struct
    include Many_getter

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val many_getter
          :  ('at -> 'a Many_getter.t)
          -> ('a, 'b, 'c, 'd) t
          -> ('at, 'bt, 'c, 'd) t
      end) =
    struct
      module M = Mapping.Make4 (T)

      let many_getter =
        { Dictionary.Create.Many_getter.f =
            (fun traverse -> M.projected ~f:(T.many_getter traverse))
        }
      ;;

      let access { f } = M.injected ~f:(f (Dictionary.Create.many_getter many_getter))
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val many_getter : ('at -> 'a Many_getter.t) -> ('a, 'b, 'c) t -> ('at, 'bt, 'c) t
      end) =
      Make_access4 (struct
        type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c) T.t

        let many_getter = T.many_getter
      end)

    module Make_access (T : sig
        type ('a, 'b) t

        val many_getter : ('at -> 'a Many_getter.t) -> ('a, 'b) t -> ('at, 'bt) t
      end) =
      Make_access3 (struct
        type ('a, 'b, 'c) t = ('a, 'b) T.t

        let many_getter = T.many_getter
      end)
  end

  module Mapper = struct
    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val mapper
          :  ('at -> f:('a -> 'b) -> 'bt)
          -> ('a, 'b, 'c, 'd) t
          -> ('at, 'bt, 'c, 'd) t
      end) =
    struct
      module M = Mapping.Make4 (T)

      let mapper =
        { Dictionary.Create.Mapper.f = (fun map -> M.projected ~f:(T.mapper map)) }
      ;;

      let access { f } = M.injected ~f:(f (Dictionary.Create.mapper mapper))
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val mapper : ('at -> f:('a -> 'b) -> 'bt) -> ('a, 'b, 'c) t -> ('at, 'bt, 'c) t
      end) =
      Make_access4 (struct
        type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c) T.t

        let mapper = T.mapper
      end)

    module Make_access (T : sig
        type ('a, 'b) t

        val mapper : ('at -> f:('a -> 'b) -> 'bt) -> ('a, 'b) t -> ('at, 'bt) t
      end) =
      Make_access3 (struct
        type ('a, 'b, 'c) t = ('a, 'b) T.t

        let mapper = T.mapper
      end)
  end

  module Construct = struct
    module T = struct
      type (_, 'b) t = 'b

      let constructor construct t = construct t
    end

    include T
    include Constructor.Make_access (T)
  end

  let construct = Construct.access

  module Identical = struct
    module T = struct
      type ('a, 'b, 'at, 'bt) t = T : ('a, 'b, 'a, 'b) t
    end

    include T
    include Equality.Make_access4 (T)
  end

  let identical (type a b at bt) (t : (unit -> a -> b, unit -> at -> bt, _) t)
    : (a, b, at, bt) Identical.t
    =
    let T = Identical.access t T in
    T
  ;;

  let map_reduce_nonemptyi (type r) t at ~combine ~f =
    let module T = struct
      include Nonempty_getter.Make_access (struct
          type ('a, 'b) t = 'a -> r

          let nonempty_getter traverse t at =
            Nonempty_getter.map_reduce (traverse at) ~combine ~f:t
          ;;
        end)
    end
    in
    T.access t (fun (i, a) -> f i a) ([], at)
  ;;

  let map_reducei (type r) t at ~empty ~combine ~f =
    let module T = struct
      include Many_getter.Make_access (struct
          type ('a, 'b) t = 'a -> r

          let many_getter traverse t at =
            Many_getter.map_reduce (traverse at) ~empty ~combine ~f:t
          ;;
        end)
    end
    in
    T.access t (fun (i, a) -> f i a) ([], at)
  ;;

  module Map = Mapper.Make_access (struct
      type ('a, 'b) t = 'a -> 'b

      let mapper map t at = map at ~f:t
    end)

  let mapi t at ~f = Map.access t (fun (i, a) -> f i a) ([], at)

  let map_reduce_nonempty t at ~combine ~f =
    map_reduce_nonemptyi t at ~combine ~f:(fun [] a -> f a)
  ;;

  let map_reduce t at ~empty ~combine ~f =
    map_reducei t at ~empty ~combine ~f:(fun [] a -> f a)
  ;;

  let map t at ~f = mapi t at ~f:(fun [] a -> f a)

  let constructor construct =
    { f =
        (fun dictionary ->
           Mapping.with_hk (Dictionary.Run.constructor dictionary construct))
    }
  ;;

  let field' f =
    { f =
        (fun dictionary ->
           Dictionary.Run.field dictionary (fun (it, at) ->
             let a, construct = f at in
             (it, a), construct)
           |> Mapping.with_hk)
    }
  ;;

  let fieldi' f =
    { f =
        (fun dictionary ->
           Dictionary.Run.field dictionary (fun (it, at) ->
             let i, a, construct = f at in
             (Index.(i :: it), a), construct)
           |> Mapping.with_hk)
    }
  ;;

  let getter get =
    { f =
        (fun dictionary ->
           Mapping.with_hk (Dictionary.Run.getter dictionary (fun (it, at) -> it, get at)))
    }
  ;;

  let getteri get =
    { f =
        (fun dictionary ->
           Dictionary.Run.getter dictionary (fun (it, at) ->
             let i, a = get at in
             Index.(i :: it), a)
           |> Mapping.with_hk)
    }
  ;;

  let isomorphism ~get ~construct =
    { f =
        (fun dictionary ->
           Dictionary.Run.isomorphism
             dictionary
             ~get:(fun (it, at) -> it, get at)
             ~construct
           |> Mapping.with_hk)
    }
  ;;

  let isomorphismi ~get ~construct =
    { f =
        (fun dictionary ->
           Dictionary.Run.isomorphism
             dictionary
             ~get:(fun (it, at) ->
               let i, a = get at in
               Index.(i :: it), a)
             ~construct
           |> Mapping.with_hk)
    }
  ;;

  let optional' f =
    { f =
        (fun dictionary ->
           Dictionary.Run.optional dictionary (fun (it, at) ->
             match f at with
             | Either.First (a, construct) -> First ((it, a), construct)
             | Second _ as bt -> bt)
           |> Mapping.with_hk)
    }
  ;;

  let optionali' f =
    { f =
        (fun dictionary ->
           Dictionary.Run.optional dictionary (fun (it, at) ->
             match f at with
             | Either.First (i, a, construct) -> First ((Index.(i :: it), a), construct)
             | Second _ as bt -> bt)
           |> Mapping.with_hk)
    }
  ;;

  let map_index f =
    { f =
        (fun dictionary ->
           Dictionary.Run.isomorphism
             dictionary
             ~get:(fun (i, a) -> f i, a)
             ~construct:Fn.id
           |> Mapping.with_hk)
    }
  ;;

  let filter_index f =
    { f =
        (fun dictionary ->
           Dictionary.Run.optional dictionary (fun ((i, a) as ia) ->
             if f i then First (ia, Fn.id) else Second a)
           |> Mapping.with_hk)
    }
  ;;

  let filter_map_index f =
    { f =
        (fun dictionary ->
           Dictionary.Run.optional dictionary (fun (i, a) ->
             match f i with
             | None -> Second a
             | Some i -> First ((i, a), Fn.id))
           |> Mapping.with_hk)
    }
  ;;

  let optional_getter get_option =
    { f =
        (fun dictionary ->
           Dictionary.Run.optional_getter dictionary (fun (it, at) ->
             Option.map (get_option at) ~f:(fun a -> it, a))
           |> Mapping.with_hk)
    }
  ;;

  let optional_getteri get_option =
    { f =
        (fun dictionary ->
           Dictionary.Run.optional_getter dictionary (fun (it, at) ->
             Option.map (get_option at) ~f:(fun (i, a) -> Index.(i :: it), a))
           |> Mapping.with_hk)
    }
  ;;

  let nonempty traverse =
    { f =
        (fun dictionary ->
           Dictionary.Run.nonempty dictionary (fun (it, at) ->
             Nonempty.Accessed.bind (traverse at) ~f:(fun a -> Nonempty.access (it, a)))
           |> Mapping.with_hk)
    }
  ;;

  let nonemptyi traverse =
    { f =
        (fun dictionary ->
           Dictionary.Run.nonempty dictionary (fun (it, at) ->
             Nonempty.Accessed.bind (traverse at) ~f:(fun (i, a) ->
               Nonempty.access (Index.(i :: it), a)))
           |> Mapping.with_hk)
    }
  ;;

  let nonempty_getter traverse =
    { f =
        (fun dictionary ->
           Dictionary.Run.nonempty_getter dictionary (fun (it, at) ->
             Nonempty_getter.map (traverse at) ~f:(fun a -> it, a))
           |> Mapping.with_hk)
    }
  ;;

  let nonempty_getteri traverse =
    { f =
        (fun dictionary ->
           Dictionary.Run.nonempty_getter dictionary (fun (it, at) ->
             Nonempty_getter.map (traverse at) ~f:(fun (i, a) -> Index.(i :: it), a))
           |> Mapping.with_hk)
    }
  ;;

  let many traverse =
    { f =
        (fun dictionary ->
           Dictionary.Run.many dictionary (fun (it, at) ->
             Many.Accessed.map (traverse at) ~f:(fun a -> it, a))
           |> Mapping.with_hk)
    }
  ;;

  let manyi traverse =
    { f =
        (fun dictionary ->
           Dictionary.Run.many dictionary (fun (it, at) ->
             Many.Accessed.map (traverse at) ~f:(fun (i, a) -> Index.(i :: it), a))
           |> Mapping.with_hk)
    }
  ;;

  let many_getter traverse =
    { f =
        (fun dictionary ->
           Dictionary.Run.many_getter dictionary (fun (it, at) ->
             Many_getter.map (traverse at) ~f:(fun a -> it, a))
           |> Mapping.with_hk)
    }
  ;;

  let many_getteri traverse =
    { f =
        (fun dictionary ->
           Dictionary.Run.many_getter dictionary (fun (it, at) ->
             Many_getter.map (traverse at) ~f:(fun (i, a) -> Index.(i :: it), a))
           |> Mapping.with_hk)
    }
  ;;

  let mapper map =
    { f =
        (fun dictionary ->
           Dictionary.Run.mapper dictionary (fun (it, at) ~f ->
             map at ~f:(fun a -> f (it, a)))
           |> Mapping.with_hk)
    }
  ;;

  let mapperi map =
    { f =
        (fun dictionary ->
           Dictionary.Run.mapper dictionary (fun (it, at) ~f ->
             map at ~f:(fun i a -> f (Index.(i :: it), a)))
           |> Mapping.with_hk)
    }
  ;;

  let variant ~match_ ~construct =
    { f =
        (fun dictionary ->
           Dictionary.Run.variant
             dictionary
             ~match_:(fun (it, at) ->
               match match_ at with
               | Either.First a -> First (it, a)
               | Second _ as bt -> bt)
             ~construct
           |> Mapping.with_hk)
    }
  ;;

  let varianti ~match_ ~construct =
    { f =
        (fun dictionary ->
           Dictionary.Run.variant
             dictionary
             ~match_:(fun (it, at) ->
               match match_ at with
               | Either.First (i, a) -> First (Index.(i :: it), a)
               | Second _ as bt -> bt)
             ~construct
           |> Mapping.with_hk)
    }
  ;;
end

include Prim

module Functor = struct
  module type S =
    Functor_s with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t

  module type S2 =
    Functor_s2 with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t

  module type S3 =
    Functor_s3 with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t
end

module Applicative = struct
  module type S =
    Applicative_s with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t

  module type S2 =
    Applicative_s2
    with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t
end

module Applicative_without_return = struct
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

module Monad = struct
  module type S =
    Monad_s with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t

  module type S2 =
    Monad_s2 with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) t
end

module Monad_without_return = struct
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

module Of_applicative2 (A : sig
    type ('a, 'e) t

    val return : 'a -> ('a, _) t
    val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
    val apply : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
  end) : Applicative.S2 with type ('a, 'e) t := ('a, 'e) A.t = struct
  include Many.Of_applicative2 (A)

  module Kleisli = Many.Make_access3 (struct
      type ('a, 'b, 'e) t = 'a -> ('b, 'e) A.t

      let many traverse f at = of_many (traverse at) ~access:f
    end)

  let mapi t at ~f = Kleisli.access t (fun (i, a) -> f i a) ([], at)
  let map t at ~f = mapi t at ~f:(fun [] a -> f a)

  let map_reduce t at ~empty ~combine ~f =
    map_reduce
      t
      at
      ~empty:(A.return empty)
      ~combine:(fun a b -> A.apply (A.map a ~f:combine) b)
      ~f
  ;;

  let map_reducei t at ~empty ~combine ~f =
    map_reducei
      t
      at
      ~empty:(A.return empty)
      ~combine:(fun a b -> A.apply (A.map a ~f:combine) b)
      ~f
  ;;

  let iter t at ~f = map_reduce t at ~empty:() ~combine:(fun () () -> ()) ~f
  let iteri t at ~f = map_reducei t at ~empty:() ~combine:(fun () () -> ()) ~f

  let sum (type a) (module M : Container.Summable with type t = a) t at ~f =
    map_reduce t at ~empty:M.zero ~combine:M.( + ) ~f
  ;;

  let sumi (type a) (module M : Container.Summable with type t = a) t at ~f =
    map_reducei t at ~empty:M.zero ~combine:M.( + ) ~f
  ;;

  module Let_syntax = struct
    let map = A.map
  end

  let count t at ~f =
    map_reduce t at ~empty:0 ~combine:( + ) ~f:(fun a -> if%map f a then 1 else 0)
  ;;

  let counti t at ~f =
    map_reducei t at ~empty:0 ~combine:( + ) ~f:(fun i a -> if%map f i a then 1 else 0)
  ;;

  let all_unit t = iter t ~f:Fn.id

  let map_reduce_nonempty t at ~combine ~f =
    map_reduce_nonempty t at ~combine:(fun a b -> A.apply (A.map a ~f:combine) b) ~f
  ;;

  let map_reduce_nonemptyi t at ~combine ~f =
    map_reduce_nonemptyi t at ~combine:(fun a b -> A.apply (A.map a ~f:combine) b) ~f
  ;;

  let all t at = map t at ~f:Fn.id
end

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

module Of_monad2 (M : sig
    type ('a, 'e) t

    val return : 'a -> ('a, _) t
    val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t

    val apply
      : [ `Custom of ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t | `Define_using_bind ]

    val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  end) : Monad.S2 with type ('a, 'e) t := ('a, 'e) M.t = struct
  module Sequential = struct
    include Of_applicative2 (struct
        type ('a, 'e) t = unit -> ('a, 'e) M.t

        let return a () = M.return a
        let map t ~f () = M.map (t ()) ~f
        let apply t1 t2 () = M.bind (t1 ()) ~f:(fun f -> M.map (t2 ()) ~f)
      end)

    let map t at ~f = map t at ~f:(fun a () -> f a) ()
    let mapi t at ~f = mapi t at ~f:(fun i a () -> f i a) ()

    let map_reduce t at ~empty ~combine ~f =
      Prim.map_reduce
        t
        at
        ~empty:(fun () -> M.return empty)
        ~combine:(fun a b () ->
          M.bind (a ()) ~f:(fun a -> M.map (b ()) ~f:(fun b -> combine a b)))
        ~f:(fun a () -> f a)
        ()
    ;;

    let map_reducei t at ~empty ~combine ~f =
      Prim.map_reducei
        t
        at
        ~empty:(fun () -> M.return empty)
        ~combine:(fun a b () ->
          M.bind (a ()) ~f:(fun a -> M.map (b ()) ~f:(fun b -> combine a b)))
        ~f:(fun i a () -> f i a)
        ()
    ;;

    let sum (type a) (module M : Container.Summable with type t = a) t at ~f =
      map_reduce t at ~empty:M.zero ~combine:M.( + ) ~f
    ;;

    let sumi (type a) (module M : Container.Summable with type t = a) t at ~f =
      map_reducei t at ~empty:M.zero ~combine:M.( + ) ~f
    ;;

    module Let_syntax = struct
      let map = M.map
    end

    let count t at ~f =
      map_reduce t at ~empty:0 ~combine:( + ) ~f:(fun a -> if%map f a then 1 else 0)
    ;;

    let counti t at ~f =
      map_reducei t at ~empty:0 ~combine:( + ) ~f:(fun i a -> if%map f i a then 1 else 0)
    ;;

    let iter t at ~f = map_reduce t at ~empty:() ~combine:(fun () () -> ()) ~f
    let iteri t at ~f = map_reducei t at ~empty:() ~combine:(fun () () -> ()) ~f

    let map_reduce_nonempty t at ~combine ~f =
      Prim.map_reduce_nonempty
        t
        at
        ~combine:(fun a b () ->
          M.bind (a ()) ~f:(fun a -> M.map (b ()) ~f:(fun b -> combine a b)))
        ~f:(fun a () -> f a)
        ()
    ;;

    let map_reduce_nonemptyi t at ~combine ~f =
      Prim.map_reduce_nonemptyi
        t
        at
        ~combine:(fun a b () ->
          M.bind (a ()) ~f:(fun a -> M.map (b ()) ~f:(fun b -> combine a b)))
        ~f:(fun i a () -> f i a)
        ()
    ;;
  end

  module Parallel = Of_applicative2 (struct
      type ('a, 'e) t = ('a, 'e) M.t

      let return = M.return
      let map = M.map

      let apply =
        match M.apply with
        | `Custom apply -> apply
        | `Define_using_bind -> fun t1 t2 -> M.bind t1 ~f:(fun f -> M.map t2 ~f)
      ;;
    end)

  let map ?(how = `Sequential) t at ~f =
    match how with
    | `Sequential -> Sequential.map t at ~f
    | `Parallel -> Parallel.map t at ~f
  ;;

  let mapi ?(how = `Sequential) t at ~f =
    match how with
    | `Sequential -> Sequential.mapi t at ~f
    | `Parallel -> Parallel.mapi t at ~f
  ;;

  let iter ?(how = `Sequential) t at ~f =
    match how with
    | `Sequential -> Sequential.iter t at ~f
    | `Parallel -> Parallel.iter t at ~f
  ;;

  let iteri ?(how = `Sequential) t at ~f =
    match how with
    | `Sequential -> Sequential.iteri t at ~f
    | `Parallel -> Parallel.iteri t at ~f
  ;;

  let sum ?(how = `Sequential) summable t at ~f =
    match how with
    | `Sequential -> Sequential.sum summable t at ~f
    | `Parallel -> Parallel.sum summable t at ~f
  ;;

  let sumi ?(how = `Sequential) summable t at ~f =
    match how with
    | `Sequential -> Sequential.sumi summable t at ~f
    | `Parallel -> Parallel.sumi summable t at ~f
  ;;

  let count ?(how = `Sequential) t at ~f =
    match how with
    | `Sequential -> Sequential.count t at ~f
    | `Parallel -> Parallel.count t at ~f
  ;;

  let counti ?(how = `Sequential) t at ~f =
    match how with
    | `Sequential -> Sequential.counti t at ~f
    | `Parallel -> Parallel.counti t at ~f
  ;;

  let map_reduce ?(how = `Sequential) t at ~empty ~combine ~f =
    match how with
    | `Sequential -> Sequential.map_reduce t at ~empty ~combine ~f
    | `Parallel -> Parallel.map_reduce t at ~empty ~combine ~f
  ;;

  let map_reducei ?(how = `Sequential) t at ~empty ~combine ~f =
    match how with
    | `Sequential -> Sequential.map_reducei t at ~empty ~combine ~f
    | `Parallel -> Parallel.map_reducei t at ~empty ~combine ~f
  ;;

  let map_reduce_nonempty ?(how = `Sequential) t at ~combine ~f =
    match how with
    | `Sequential -> Sequential.map_reduce_nonempty t at ~combine ~f
    | `Parallel -> Parallel.map_reduce_nonempty t at ~combine ~f
  ;;

  let map_reduce_nonemptyi ?(how = `Sequential) t at ~combine ~f =
    match how with
    | `Sequential -> Sequential.map_reduce_nonemptyi t at ~combine ~f
    | `Parallel -> Parallel.map_reduce_nonemptyi t at ~combine ~f
  ;;

  let all = Parallel.all
  let all_unit = Parallel.all_unit
end

module Of_monad (M : sig
    type 'a t

    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val apply : [ `Custom of ('a -> 'b) t -> 'a t -> 'b t | `Define_using_bind ]
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end) =
  Of_monad2 (struct
    type ('a, _) t = 'a M.t

    include (M : module type of M with type 'a t := 'a M.t)
  end)

module Of_functor3 (F : sig
    type ('a, 'd, 'e) t

    val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
  end) : Functor.S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) F.t = struct
  include Field.Make_access4 (struct
      type ('a, 'b, 'd, 'e) t = 'a -> ('b, 'd, 'e) F.t

      let field field_f t at =
        let a, construct = field_f at in
        F.map (t a) ~f:construct
      ;;
    end)

  let mapi t at ~f = access t (fun (i, a) -> f i a) ([], at)
  let map t at ~f = mapi t at ~f:(fun [] a -> f a)
  let all t at = map t at ~f:Fn.id
end

module Of_functor2 (F : sig
    type ('a, 'd) t

    val map : ('a, 'd) t -> f:('a -> 'b) -> ('b, 'd) t
  end) : Functor.S2 with type ('a, 'd) t := ('a, 'd) F.t = Of_functor3 (struct
    type ('a, 'd, _) t = ('a, 'd) F.t

    let map = F.map
  end)

module Of_functor (F : sig
    type 'a t

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end) : Functor.S with type 'a t := 'a F.t = Of_functor2 (struct
    type ('a, _) t = 'a F.t

    let map = F.map
  end)

module Of_applicative_without_return3 (A : sig
    type ('a, 'd, 'e) t

    val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
    val apply : ('a -> 'b, 'd, 'e) t -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t
  end) : Applicative_without_return.S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) A.t =
struct
  include Nonempty.Of_applicative_without_return3 (A)

  module Kleisli = Nonempty.Make_access4 (struct
      type ('a, 'b, 'd, 'e) t = 'a -> ('b, 'd, 'e) A.t

      let nonempty traverse f at = of_nonempty (traverse at) ~access:f
    end)

  let mapi t at ~f = Kleisli.access t (fun (i, a) -> f i a) ([], at)
  let map t at ~f = mapi t at ~f:(fun [] a -> f a)

  let map_reduce_nonempty t at ~combine ~f =
    map_reduce_nonempty t at ~combine:(fun a b -> A.apply (A.map a ~f:combine) b) ~f
  ;;

  let map_reduce_nonemptyi t at ~combine ~f =
    map_reduce_nonemptyi t at ~combine:(fun a b -> A.apply (A.map a ~f:combine) b) ~f
  ;;

  let sum (type a) (module M : Container.Summable with type t = a) t at ~f =
    map_reduce_nonempty t at ~combine:M.( + ) ~f
  ;;

  let sumi (type a) (module M : Container.Summable with type t = a) t at ~f =
    map_reduce_nonemptyi t at ~combine:M.( + ) ~f
  ;;

  module Let_syntax = struct
    let map = A.map
  end

  let count t at ~f =
    map_reduce_nonempty t at ~combine:( + ) ~f:(fun a -> if%map f a then 1 else 0)
  ;;

  let counti t at ~f =
    map_reduce_nonemptyi t at ~combine:( + ) ~f:(fun i a -> if%map f i a then 1 else 0)
  ;;

  let iter t at ~f = map_reduce_nonempty t at ~combine:(fun () () -> ()) ~f
  let iteri t at ~f = map_reduce_nonemptyi t at ~combine:(fun () () -> ()) ~f
  let all t at = map t at ~f:Fn.id
  let all_unit t = iter t ~f:Fn.id
end

module Of_applicative_without_return2 (A : sig
    type ('a, 'e) t

    val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
    val apply : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
  end) =
  Of_applicative_without_return3 (struct
    type ('a, _, 'e) t = ('a, 'e) A.t

    include (A : module type of A with type ('a, 'e) t := ('a, 'e) A.t)
  end)

module Of_applicative_without_return (A : sig
    type 'a t

    val map : 'a t -> f:('a -> 'b) -> 'b t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
  end) =
  Of_applicative_without_return2 (struct
    type ('a, 'i) t = 'a A.t

    include (A : module type of A with type 'a t := 'a A.t)
  end)

module Of_monad_without_return3 (M : sig
    type ('a, 'd, 'e) t

    val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
    val bind : ('a, 'd, 'e) t -> f:('a -> ('b, 'd, 'e) t) -> ('b, 'd, 'e) t
  end) : Monad_without_return.S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) M.t = struct
  module Sequential = struct
    include Of_applicative_without_return3 (struct
        type ('a, 'd, 'e) t = unit -> ('a, 'd, 'e) M.t

        let map t ~f () = M.map (t ()) ~f
        let apply t1 t2 () = M.bind (t1 ()) ~f:(fun f -> M.map (t2 ()) ~f)
      end)

    let map t at ~f = map t at ~f:(fun a () -> f a) ()
    let mapi t at ~f = mapi t at ~f:(fun i a () -> f i a) ()

    let map_reduce_nonempty t at ~combine ~f =
      Prim.map_reduce_nonempty
        t
        at
        ~combine:(fun a b () ->
          M.bind (a ()) ~f:(fun a -> M.map (b ()) ~f:(fun b -> combine a b)))
        ~f:(fun a () -> f a)
        ()
    ;;

    let map_reduce_nonemptyi t at ~combine ~f =
      Prim.map_reduce_nonemptyi
        t
        at
        ~combine:(fun a b () ->
          M.bind (a ()) ~f:(fun a -> M.map (b ()) ~f:(fun b -> combine a b)))
        ~f:(fun i a () -> f i a)
        ()
    ;;

    let sum (type a) (module M : Container.Summable with type t = a) t at ~f =
      map_reduce_nonempty t at ~combine:M.( + ) ~f
    ;;

    let sumi (type a) (module M : Container.Summable with type t = a) t at ~f =
      map_reduce_nonemptyi t at ~combine:M.( + ) ~f
    ;;

    module Let_syntax = struct
      let map = M.map
    end

    let count t at ~f =
      map_reduce_nonempty t at ~combine:( + ) ~f:(fun a -> if%map f a then 1 else 0)
    ;;

    let counti t at ~f =
      map_reduce_nonemptyi t at ~combine:( + ) ~f:(fun i a -> if%map f i a then 1 else 0)
    ;;

    let iter t at ~f = map_reduce_nonempty t at ~combine:(fun () () -> ()) ~f
    let iteri t at ~f = map_reduce_nonemptyi t at ~combine:(fun () () -> ()) ~f
  end

  module Parallel = Of_applicative_without_return3 (struct
      type ('a, 'd, 'e) t = ('a, 'd, 'e) M.t

      let map t ~f = M.map t ~f
      let apply t1 t2 = M.bind t1 ~f:(fun f -> M.map t2 ~f)
    end)

  let map ?(how = `Sequential) t at ~f =
    match how with
    | `Sequential -> Sequential.map t at ~f
    | `Parallel -> Parallel.map t at ~f
  ;;

  let mapi ?(how = `Sequential) t at ~f =
    match how with
    | `Sequential -> Sequential.mapi t at ~f
    | `Parallel -> Parallel.mapi t at ~f
  ;;

  let iter ?(how = `Sequential) t at ~f =
    match how with
    | `Sequential -> Sequential.iter t at ~f
    | `Parallel -> Parallel.iter t at ~f
  ;;

  let iteri ?(how = `Sequential) t at ~f =
    match how with
    | `Sequential -> Sequential.iteri t at ~f
    | `Parallel -> Parallel.iteri t at ~f
  ;;

  let sum ?(how = `Sequential) summable t at ~f =
    match how with
    | `Sequential -> Sequential.sum summable t at ~f
    | `Parallel -> Parallel.sum summable t at ~f
  ;;

  let sumi ?(how = `Sequential) summable t at ~f =
    match how with
    | `Sequential -> Sequential.sumi summable t at ~f
    | `Parallel -> Parallel.sumi summable t at ~f
  ;;

  let count ?(how = `Sequential) t at ~f =
    match how with
    | `Sequential -> Sequential.count t at ~f
    | `Parallel -> Parallel.count t at ~f
  ;;

  let counti ?(how = `Sequential) t at ~f =
    match how with
    | `Sequential -> Sequential.counti t at ~f
    | `Parallel -> Parallel.counti t at ~f
  ;;

  let map_reduce_nonempty ?(how = `Sequential) t at ~combine ~f =
    match how with
    | `Sequential -> Sequential.map_reduce_nonempty t at ~combine ~f
    | `Parallel -> Parallel.map_reduce_nonempty t at ~combine ~f
  ;;

  let map_reduce_nonemptyi ?(how = `Sequential) t at ~combine ~f =
    match how with
    | `Sequential -> Sequential.map_reduce_nonemptyi t at ~combine ~f
    | `Parallel -> Parallel.map_reduce_nonemptyi t at ~combine ~f
  ;;

  let all = Parallel.all
  let all_unit = Parallel.all_unit
end

module Of_monad_without_return2 (M : sig
    type ('a, 'e) t

    val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
    val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  end) =
  Of_monad_without_return3 (struct
    type ('a, _, 'e) t = ('a, 'e) M.t

    include (M : module type of M with type ('a, 'e) t := ('a, 'e) M.t)
  end)

module Of_monad_without_return (M : sig
    type 'a t

    val map : 'a t -> f:('a -> 'b) -> 'b t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end) =
  Of_monad_without_return2 (struct
    type ('a, _) t = 'a M.t

    include (M : module type of M with type 'a t := 'a M.t)
  end)

let set t at ~to_ = mapi t at ~f:(fun _ _ -> to_)

let fold t at ~init ~f =
  map_reduce
    t
    at
    ~empty:Fn.id
    ~combine:(fun l r acc -> r (l acc))
    ~f:(fun x acc -> f acc x)
    init
;;

let foldi t at ~init ~f =
  map_reducei
    t
    at
    ~empty:Fn.id
    ~combine:(fun l r acc -> r (l acc))
    ~f:(fun i x acc -> f i acc x)
    init
;;

let iter t at ~f = map_reduce t at ~empty:() ~combine:(fun () () -> ()) ~f
let iteri t at ~f = map_reducei t at ~empty:() ~combine:(fun () () -> ()) ~f
let length t at = fold t at ~init:0 ~f:(fun n _ -> Int.succ n)

let is_empty t at =
  with_return (fun { return } ->
    iter t at ~f:(fun _ -> return false);
    true)
;;

let sum (type a) (module M : Container.Summable with type t = a) t at ~f =
  map_reduce t at ~empty:M.zero ~combine:M.( + ) ~f
;;

let sumi (type a) (module M : Container.Summable with type t = a) t at ~f =
  map_reducei t at ~empty:M.zero ~combine:M.( + ) ~f
;;

let count t at ~f =
  map_reduce t at ~empty:0 ~combine:( + ) ~f:(fun a -> if f a then 1 else 0)
;;

let counti t at ~f =
  map_reducei t at ~empty:0 ~combine:( + ) ~f:(fun i a -> if f i a then 1 else 0)
;;

let exists t at ~f =
  with_return (fun { return } ->
    iter t at ~f:(fun a -> if f a then return true);
    false)
;;

let existsi t at ~f =
  with_return (fun { return } ->
    iteri t at ~f:(fun i a -> if f i a then return true);
    false)
;;

let for_alli t at ~f = not (existsi t at ~f:(fun i a -> not (f i a)))
let for_all t at ~f = for_alli t at ~f:(fun [] a -> f a)

let find_map t at ~f =
  with_return (fun { return } ->
    iter t at ~f:(fun a ->
      match f a with
      | None -> ()
      | Some _ as result -> return result);
    None)
;;

let find_mapi t at ~f =
  with_return (fun { return } ->
    iteri t at ~f:(fun i a ->
      match f i a with
      | None -> ()
      | Some _ as result -> return result);
    None)
;;

let find t at ~f =
  with_return (fun { return } ->
    iter t at ~f:(fun a -> if f a then return (Some a));
    None)
;;

let findi t at ~f =
  with_return (fun { return } ->
    iteri t at ~f:(fun i a -> if f i a then return (Some (i, a)));
    None)
;;

let min_elt_option t at ~compare =
  map_reduce
    t
    at
    ~empty:None
    ~combine:(fun l r ->
      match l, r with
      | None, None -> None
      | Some _, None -> l
      | None, Some _ -> r
      | Some a, Some b -> if compare a b <= 0 then l else r)
    ~f:Option.some
;;

let max_elt_option t at ~compare =
  map_reduce
    t
    at
    ~empty:None
    ~combine:(fun l r ->
      match l, r with
      | None, None -> None
      | Some _, None -> l
      | None, Some _ -> r
      | Some a, Some b -> if compare a b >= 0 then l else r)
    ~f:Option.some
;;

let min_elt t at ~compare =
  map_reduce_nonempty
    t
    at
    ~combine:(fun l r -> if compare l r <= 0 then l else r)
    ~f:Fn.id
;;

let max_elt t at ~compare =
  map_reduce_nonempty
    t
    at
    ~combine:(fun l r -> if compare l r >= 0 then l else r)
    ~f:Fn.id
;;

let to_list t at = List.rev (fold t at ~init:[] ~f:(fun acc x -> x :: acc))
let to_listi t at = List.rev (foldi t at ~init:[] ~f:(fun i acc x -> (i, x) :: acc))
let to_array t at = Array.of_list (to_list t at)
let to_arrayi t at = Array.of_list (to_listi t at)
let hd t at = map_reduce_nonempty t at ~combine:Fn.const ~f:Fn.id
let hdi t at = map_reduce_nonemptyi t at ~combine:Fn.const ~f:(fun i a -> i, a)

let hd_option t at =
  map_reduce t at ~empty:None ~combine:Option.first_some ~f:Option.return
;;

let hd_optioni t at =
  map_reducei t at ~empty:None ~combine:Option.first_some ~f:(fun i a -> Some (i, a))
;;

module With_accumulator = Of_monad2 (struct
    type ('a, 'acc) t = 'acc -> 'acc * 'a

    let return a acc = acc, a

    let map t ~f acc =
      let acc, a = t acc in
      acc, f a
    ;;

    let apply t1 t2 acc =
      let acc, f = t1 acc in
      let acc, a = t2 acc in
      acc, f a
    ;;

    let apply = `Custom apply

    let bind t ~f acc =
      let acc, a = t acc in
      f a acc
    ;;
  end)

let fold_map t at ~init ~f = With_accumulator.map t at ~f:(fun a acc -> f acc a) init

let fold_mapi t at ~init ~f =
  With_accumulator.mapi t at ~f:(fun i a acc -> f i acc a) init
;;

let folding_map t at ~init ~f = snd (fold_map t at ~init ~f)
let folding_mapi t at ~init ~f = snd (fold_mapi t at ~init ~f)
let field ~get ~set = field' (fun at -> get at, set at)

let fieldi ~get ~set =
  fieldi' (fun at ->
    let i, a = get at in
    i, a, set at)
;;

let of_field field_t = field ~get:(Base.Field.get field_t) ~set:(Base.Field.fset field_t)

let of_fieldi field_t =
  fieldi
    ~get:(fun at -> Base.Field.name field_t, Base.Field.get field_t at)
    ~set:(Base.Field.fset field_t)
;;

let optional ~match_ ~set =
  optional' (fun at ->
    match match_ at with
    | Either.First a -> First (a, set at)
    | Second _ as bt -> bt)
;;

let optionali ~match_ ~set =
  optionali' (fun at ->
    match match_ at with
    | Either.First (i, a) -> First (i, a, set at)
    | Second _ as bt -> bt)
;;

let of_variant variant_t = constructor variant_t.Base.Variant.constructor

module Concrete_getter = Getter.Make_access3 (struct
    type ('a, _, 'r) t = 'a -> 'r

    let getter get t at = t (get at)
  end)

let geti t at = Concrete_getter.access t Fn.id ([], at)

let get t at =
  let [], a = geti t at in
  a
;;

module Concrete_optional_getter = Optional_getter.Make_access3 (struct
    type ('a, _, 'r) t = 'a -> 'r option

    let optional_getter get t at = Option.bind (get at) ~f:t
  end)

let get_optioni t at = Concrete_optional_getter.access t Option.some ([], at)
let get_option t at = Option.map (get_optioni t at) ~f:(fun ([], a) -> a)

module Match_ = Variant.Make_access3 (struct
    type ('at, 'bt, 'a) t = 'at -> ('a, 'bt) Either.t

    let variant ~match_ ~construct t att =
      match match_ att with
      | Either.First at ->
        (match t at with
         | Either.First _ as a -> a
         | Second b -> Second (construct b))
      | Second _ as bt -> bt
    ;;
  end)

let matchi t at = Match_.access t Either.first ([], at)
let match_ t at = Either.First.map (matchi t at) ~f:(fun ([], a) -> a)

module Concrete_isomorphism = struct
  module T = struct
    type ('at, 'bt, 'a, 'b) t =
      { get : 'at -> 'a
      ; construct : 'b -> 'bt
      }

    let isomorphism ~get ~construct t =
      { get = Fn.compose t.get get; construct = Fn.compose construct t.construct }
    ;;
  end

  include T
  include Isomorphism.Make_access4 (T)

  let id = { get = Fn.id; construct = Fn.id }
end

let invert t =
  let { Concrete_isomorphism.get; construct } =
    Concrete_isomorphism.access t Concrete_isomorphism.id
  in
  isomorphism ~get:construct ~construct:(fun at ->
    let [], a = get ([], at) in
    a)
;;

let getter_to_constructor t = constructor (get t)
let constructor_to_getter t = getter (construct t)

module O = struct
  include Subtyping

  let ( @> ) = compose
  let ( .@() ) at t = get t at
  let ( .@?() ) at t = get_option t at
  let ( .@*() ) at t = to_list t at
  let ( .@()<- ) at t to_ = set t at ~to_
end

include O

let transform t at ~f =
  let rec loop at = f (map t at ~f:loop) in
  loop at
;;

let rewrite t at ~f =
  let rec loop at =
    transform t at ~f:(fun x ->
      match f x with
      | None -> x
      | Some at -> loop at)
  in
  loop at
;;

let many_to_list_field t =
  field ~get:(to_list t) ~set:(fun at bs ->
    folding_map t at ~init:bs ~f:(fun bs a ->
      match bs with
      | [] -> [], a
      | b :: bs -> bs, b))
;;
