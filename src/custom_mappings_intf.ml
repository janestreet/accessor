open! Base
open! Import
open Subtyping

module type S = sig
  type ('inner, 'outer, 'kind) accessor

  (** An [equality] can transform any mapping. There is no need for you to provide any
      functionality of your own. *)
  module Equality : sig
    module Make_access (T : sig
        type ('a, 'b) t
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> equality ]) accessor
        -> ('i Index.t * 'a, 'b) T.t @ local
        -> ('it Index.t * 'at, 'bt) T.t @ local
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> equality ]) accessor
        -> ('i Index.t * 'a, 'b, 'c) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c) T.t @ local
    end

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> equality ]) accessor
        -> ('i Index.t * 'a, 'b, 'c, 'd) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c, 'd) T.t @ local
    end
  end

  module Isomorphism : sig
    module Make_access (T : sig
        type ('a, 'b) t

        (** A legal implementation of this function must satisfy the following properties:

            {[
              isomorphism ~get:Fn.id ~construct:Fn.id = Fn.id
            ]}

            {[
              Fn.compose
                (isomorphism ~get:g1 ~construct:c1)
                (isomorphism ~get:g2 ~construct:c2)
              = isomorphism ~get:(Fn.compose g2 g1) ~construct:(Fn.compose c1 c2)
            ]} *)
        val isomorphism
          :  get:('at -> 'a)
          -> construct:('b -> 'bt)
          -> ('a, 'b) t @ local
          -> ('at, 'bt) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> isomorphism ]) accessor
        -> ('i Index.t * 'a, 'b) T.t @ local
        -> ('it Index.t * 'at, 'bt) T.t @ local
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val isomorphism
          :  get:('at -> 'a)
          -> construct:('b -> 'bt)
          -> ('a, 'b, 'c) t @ local
          -> ('at, 'bt, 'c) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> isomorphism ]) accessor
        -> ('i Index.t * 'a, 'b, 'c) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c) T.t @ local
    end

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val isomorphism
          :  get:('at -> 'a)
          -> construct:('b -> 'bt)
          -> ('a, 'b, 'c, 'd) t @ local
          -> ('at, 'bt, 'c, 'd) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> isomorphism ]) accessor
        -> ('i Index.t * 'a, 'b, 'c, 'd) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c, 'd) T.t @ local
    end
  end

  module Field : sig
    module Make_access (T : sig
        type ('a, 'b) t

        (** A legal implementation of this function must satisfy the following properties:

            {[
              field (fun a -> a, Fn.id) = Fn.id
            ]}

            {[
              Fn.compose (field f) (field g)
              = field (fun a ->
                let a, j = f a in
                let a, k = g a in
                a, Fn.compose j k)
            ]} *)
        val field
          :  ('at -> 'a * ('b -> 'bt)) @ local
          -> ('a, 'b) t @ local
          -> ('at, 'bt) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> field ]) accessor
        -> ('i Index.t * 'a, 'b) T.t @ local
        -> ('it Index.t * 'at, 'bt) T.t @ local
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val field
          :  ('at -> 'a * ('b -> 'bt)) @ local
          -> ('a, 'b, 'c) t @ local
          -> ('at, 'bt, 'c) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> field ]) accessor
        -> ('i Index.t * 'a, 'b, 'c) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c) T.t @ local
    end

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val field
          :  ('at -> 'a * ('b -> 'bt)) @ local
          -> ('a, 'b, 'c, 'd) t @ local
          -> ('at, 'bt, 'c, 'd) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> field ]) accessor
        -> ('i Index.t * 'a, 'b, 'c, 'd) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c, 'd) T.t @ local
    end
  end

  module Variant : sig
    module Make_access (T : sig
        type ('a, 'b) t

        (** A legal implementation of this function must satisfy the following properties:

            {[
              variant ~match_:Either.first ~construct:Fn.id = Fn.id
            ]}

            {[
              Fn.compose
                (variant ~match_:m1 ~construct:c1)
                (variant ~match_:m2 ~construct:c2)
              = variant
                  ~match_:(fun a ->
                    match m1 a with
                    | Second _ as a -> a
                    | First a ->
                      (match m2 a with
                       | First _ as a -> a
                       | Second a -> Second (c1 a)))
                  ~construct:(Fn.compose c1 c2)
            ]} *)
        val variant
          :  match_:('at -> ('a, 'bt) Either.t) @ local
          -> construct:('b -> 'bt) @ local
          -> ('a, 'b) t @ local
          -> ('at, 'bt) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> variant ]) accessor
        -> ('i Index.t * 'a, 'b) T.t @ local
        -> ('it Index.t * 'at, 'bt) T.t @ local
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val variant
          :  match_:('at -> ('a, 'bt) Either.t) @ local
          -> construct:('b -> 'bt) @ local
          -> ('a, 'b, 'c) t @ local
          -> ('at, 'bt, 'c) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> variant ]) accessor
        -> ('i Index.t * 'a, 'b, 'c) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c) T.t @ local
    end

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val variant
          :  match_:('at -> ('a, 'bt) Either.t) @ local
          -> construct:('b -> 'bt) @ local
          -> ('a, 'b, 'c, 'd) t @ local
          -> ('at, 'bt, 'c, 'd) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> variant ]) accessor
        -> ('i Index.t * 'a, 'b, 'c, 'd) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c, 'd) T.t @ local
    end
  end

  module Constructor : sig
    module Make_access (T : sig
        type ('a, 'b) t

        (** A legal implementation of this function must satisfy the following properties:

            {[
              constructor Fn.id = Fn.id
            ]}

            {[
              Fn.compose (construct f) (construct g) = construct (Fn.compose f g)
            ]} *)
        val constructor
          :  ('b -> 'bt) @ local
          -> ('a, 'b) t @ local
          -> ('at, 'bt) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> constructor ]) accessor
        -> ('i Index.t * 'a, 'b) T.t @ local
        -> ('it Index.t * 'at, 'bt) T.t @ local
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val constructor
          :  ('b -> 'bt) @ local
          -> ('a, 'b, 'c) t @ local
          -> ('at, 'bt, 'c) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> constructor ]) accessor
        -> ('i Index.t * 'a, 'b, 'c) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c) T.t @ local
    end

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val constructor
          :  ('b -> 'bt) @ local
          -> ('a, 'b, 'c, 'd) t @ local
          -> ('at, 'bt, 'c, 'd) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> constructor ]) accessor
        -> ('i Index.t * 'a, 'b, 'c, 'd) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c, 'd) T.t @ local
    end
  end

  module Getter : sig
    module Make_access (T : sig
        type ('a, 'b) t

        (** A legal implementation of this function must satisfy the following properties:

            {[
              getter Fn.id = Fn.id
            ]}

            {[
              Fn.compose (getter f) (getter g) = getter (Fn.compose g f)
            ]} *)
        val getter : ('at -> 'a) @ local -> ('a, 'b) t @ local -> ('at, 'bt) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> getter ]) accessor
        -> ('i Index.t * 'a, 'b) T.t @ local
        -> ('it Index.t * 'at, 'bt) T.t @ local
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val getter
          :  ('at -> 'a) @ local
          -> ('a, 'b, 'c) t @ local
          -> ('at, 'bt, 'c) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> getter ]) accessor
        -> ('i Index.t * 'a, 'b, 'c) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c) T.t @ local
    end

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val getter
          :  ('at -> 'a) @ local
          -> ('a, 'b, 'c, 'd) t @ local
          -> ('at, 'bt, 'c, 'd) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> getter ]) accessor
        -> ('i Index.t * 'a, 'b, 'c, 'd) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c, 'd) T.t @ local
    end
  end

  module Optional : sig
    module Make_access (T : sig
        type ('a, 'b) t

        (** A legal implementation of this function must satisfy the following properties:

            {[
              optional (fun a -> First (a, Fn.id)) = Fn.id
            ]}

            {[
              Fn.compose (optional f) (optional g)
              = optional (fun a ->
                match f a with
                | Second _ as a -> a
                | First (a, j) ->
                  (match g a with
                   | First (a, k) -> First (a, Fn.compose j k)
                   | Second a -> Second (j a)))
            ]} *)
        val optional
          :  ('at -> ('a * ('b -> 'bt), 'bt) Either.t) @ local
          -> ('a, 'b) t @ local
          -> ('at, 'bt) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> optional ]) accessor
        -> ('i Index.t * 'a, 'b) T.t @ local
        -> ('it Index.t * 'at, 'bt) T.t @ local
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val optional
          :  ('at -> ('a * ('b -> 'bt), 'bt) Either.t) @ local
          -> ('a, 'b, 'c) t @ local
          -> ('at, 'bt, 'c) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> optional ]) accessor
        -> ('i Index.t * 'a, 'b, 'c) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c) T.t @ local
    end

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val optional
          :  ('at -> ('a * ('b -> 'bt), 'bt) Either.t) @ local
          -> ('a, 'b, 'c, 'd) t @ local
          -> ('at, 'bt, 'c, 'd) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> optional ]) accessor
        -> ('i Index.t * 'a, 'b, 'c, 'd) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c, 'd) T.t @ local
    end
  end

  module Optional_getter : sig
    module Make_access (T : sig
        type ('a, 'b) t

        (** A legal implementation of this function must satisfy the following properties:

            {[
              optional_getter Option.some = Fn.id
            ]}

            {[
              Fn.compose (optional_getter f) (optional_getter g)
              = optional_getter (fun a -> Option.bind (f a) ~f:g)
            ]} *)
        val optional_getter
          :  ('at -> 'a option) @ local
          -> ('a, 'b) t @ local
          -> ('at, 'bt) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> optional_getter ]) accessor
        -> ('i Index.t * 'a, 'b) T.t @ local
        -> ('it Index.t * 'at, 'bt) T.t @ local
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val optional_getter
          :  ('at -> 'a option) @ local
          -> ('a, 'b, 'c) t @ local
          -> ('at, 'bt, 'c) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> optional_getter ]) accessor
        -> ('i Index.t * 'a, 'b, 'c) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c) T.t @ local
    end

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val optional_getter
          :  ('at -> 'a option) @ local
          -> ('a, 'b, 'c, 'd) t @ local
          -> ('at, 'bt, 'c, 'd) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> optional_getter ]) accessor
        -> ('i Index.t * 'a, 'b, 'c, 'd) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c, 'd) T.t @ local
    end
  end

  module Nonempty : sig
    include module type of Nonempty (** @inline *)

    (** Access everything that the given accessor accesses. *)
    val access_nonempty
      :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> nonempty ]) accessor
      -> 'at
      -> ('bt, 'a, 'b) t

    module Let_syntax : sig
      include module type of Nonempty.Let_syntax

      module Let_syntax : sig
        include module type of Nonempty.Let_syntax.Let_syntax

        module Open_on_rhs : sig
          include module type of Nonempty.Let_syntax.Let_syntax.Open_on_rhs

          val access_nonempty
            :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> nonempty ]) accessor
            -> 'at
            -> ('bt, 'a, 'b) t
        end
      end
    end

    module Accessor :
      Applicative_signatures_intf.Applicative_without_return_s3
      with type ('a, 'd, 'e) t := ('a, 'd, 'e) t
      with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) accessor

    module Make_access (T : sig
        type ('a, 'b) t

        (** A legal implementation of this function must satisfy the following properties:

            {[
              nonempty Nonempty.Accessed.return = Fn.id
            ]}

            {[
              Fn.compose (nonempty f) (nonempty g)
              = nonempty (fun at -> Nonempty.Accessed.bind (f at) ~f:g)
            ]} *)
        val nonempty
          :  ('at -> ('bt, 'a, 'b) Nonempty.t) @ local
          -> ('a, 'b) t @ local
          -> ('at, 'bt) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> nonempty ]) accessor
        -> ('i Index.t * 'a, 'b) T.t @ local
        -> ('it Index.t * 'at, 'bt) T.t @ local
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val nonempty
          :  ('at -> ('bt, 'a, 'b) Nonempty.t) @ local
          -> ('a, 'b, 'c) t @ local
          -> ('at, 'bt, 'c) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> nonempty ]) accessor
        -> ('i Index.t * 'a, 'b, 'c) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c) T.t @ local
    end

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val nonempty
          :  ('at -> ('bt, 'a, 'b) Nonempty.t) @ local
          -> ('a, 'b, 'c, 'd) t @ local
          -> ('at, 'bt, 'c, 'd) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> nonempty ]) accessor
        -> ('i Index.t * 'a, 'b, 'c, 'd) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c, 'd) T.t @ local
    end
  end

  module Nonempty_getter : sig
    include module type of Nonempty_getter (** @inline *)

    module Make_access (T : sig
        type ('a, 'b) t

        (** A legal implementation of this function must satisfy the following properties:

            {[
              nonempty_getter Nonempty_getter.return = Fn.id
            ]}

            {[
              Fn.compose (nonempty_getter f) (nonempty_getter g)
              = nonempty_getter (fun at -> Nonempty_getter.bind (f at) ~f:g)
            ]} *)
        val nonempty_getter
          :  ('at -> 'a Nonempty_getter.t) @ local
          -> ('a, 'b) t @ local
          -> ('at, 'bt) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> nonempty_getter ]) accessor
        -> ('i Index.t * 'a, 'b) T.t @ local
        -> ('it Index.t * 'at, 'bt) T.t @ local
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val nonempty_getter
          :  ('at -> 'a Nonempty_getter.t) @ local
          -> ('a, 'b, 'c) t @ local
          -> ('at, 'bt, 'c) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> nonempty_getter ]) accessor
        -> ('i Index.t * 'a, 'b, 'c) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c) T.t @ local
    end

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val nonempty_getter
          :  ('at -> 'a Nonempty_getter.t) @ local
          -> ('a, 'b, 'c, 'd) t @ local
          -> ('at, 'bt, 'c, 'd) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> nonempty_getter ]) accessor
        -> ('i Index.t * 'a, 'b, 'c, 'd) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c, 'd) T.t @ local
    end
  end

  module Many : sig
    include module type of Many (** @inline *)

    (** Access everything that the given accessor accesses. *)
    val access_many
      :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> many ]) accessor
      -> 'at
      -> ('bt, 'a, 'b) t

    module Let_syntax : sig
      include module type of Many.Let_syntax

      module Let_syntax : sig
        include module type of Many.Let_syntax.Let_syntax

        module Open_on_rhs : sig
          include module type of Many.Let_syntax.Let_syntax.Open_on_rhs

          val access_many
            :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> many ]) accessor
            -> 'at
            -> ('bt, 'a, 'b) t
        end
      end
    end

    module Accessor :
      Applicative_signatures_intf.Applicative_s3
      with type ('a, 'd, 'e) t := ('a, 'd, 'e) t
      with type ('inner, 'outer, 'kind) accessor := ('inner, 'outer, 'kind) accessor

    module Make_access (T : sig
        type ('a, 'b) t

        (** A legal implementation of this function must satisfy the following properties:

            {[
              many Many.Accessed.return = Fn.id
            ]}

            {[
              Fn.compose (many f) (many g)
              = many (fun at -> Many.Accessed.bind (f at) ~f:g)
            ]} *)
        val many
          :  ('at -> ('bt, 'a, 'b) Many.t) @ local
          -> ('a, 'b) t @ local
          -> ('at, 'bt) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> many ]) accessor
        -> ('i Index.t * 'a, 'b) T.t @ local
        -> ('it Index.t * 'at, 'bt) T.t @ local
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val many
          :  ('at -> ('bt, 'a, 'b) Many.t) @ local
          -> ('a, 'b, 'c) t @ local
          -> ('at, 'bt, 'c) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> many ]) accessor
        -> ('i Index.t * 'a, 'b, 'c) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c) T.t @ local
    end

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val many
          :  ('at -> ('bt, 'a, 'b) Many.t) @ local
          -> ('a, 'b, 'c, 'd) t @ local
          -> ('at, 'bt, 'c, 'd) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> many ]) accessor
        -> ('i Index.t * 'a, 'b, 'c, 'd) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c, 'd) T.t @ local
    end
  end

  module Many_getter : sig
    include module type of Many_getter (** @inline *)

    module Make_access (T : sig
        type ('a, 'b) t

        (** A legal implementation of this function must satisfy the following properties:

            {[
              many_getter Many_getter.return = Fn.id
            ]}

            {[
              Fn.compose (many_getter f) (many_getter g)
              = many_getter (fun at -> Many_getter.bind (f at) ~f:g)
            ]} *)
        val many_getter
          :  ('at -> 'a Many_getter.t) @ local
          -> ('a, 'b) t @ local
          -> ('at, 'bt) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> many_getter ]) accessor
        -> ('i Index.t * 'a, 'b) T.t @ local
        -> ('it Index.t * 'at, 'bt) T.t @ local
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val many_getter
          :  ('at -> 'a Many_getter.t) @ local
          -> ('a, 'b, 'c) t @ local
          -> ('at, 'bt, 'c) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> many_getter ]) accessor
        -> ('i Index.t * 'a, 'b, 'c) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c) T.t @ local
    end

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val many_getter
          :  ('at -> 'a Many_getter.t) @ local
          -> ('a, 'b, 'c, 'd) t @ local
          -> ('at, 'bt, 'c, 'd) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> many_getter ]) accessor
        -> ('i Index.t * 'a, 'b, 'c, 'd) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c, 'd) T.t @ local
    end
  end

  module Mapper : sig
    module Make_access (T : sig
        type ('a, 'b) t

        (** A legal implementation of this function must satisfy the following properties:

            {[
              mapper (fun a ~f -> f a) = Fn.id
            ]}

            {[
              Fn.compose (mapper f) (mapper g) = mapper (fun a ~f:h -> f a ~f:(g ~f:h))
            ]} *)
        val mapper
          :  ('at -> f:('a -> 'b) -> 'bt)
          -> ('a, 'b) t @ local
          -> ('at, 'bt) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> mapper ]) accessor
        -> ('i Index.t * 'a, 'b) T.t @ local
        -> ('it Index.t * 'at, 'bt) T.t @ local
    end

    module Make_access3 (T : sig
        type ('a, 'b, 'c) t

        val mapper
          :  ('at -> f:('a -> 'b) -> 'bt)
          -> ('a, 'b, 'c) t @ local
          -> ('at, 'bt, 'c) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> mapper ]) accessor
        -> ('i Index.t * 'a, 'b, 'c) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c) T.t @ local
    end

    module Make_access4 (T : sig
        type ('a, 'b, 'c, 'd) t

        val mapper
          :  ('at -> f:('a -> 'b) -> 'bt)
          -> ('a, 'b, 'c, 'd) t @ local
          -> ('at, 'bt, 'c, 'd) t @ local
      end) : sig
      val access
        :  ('i -> 'a -> 'b, 'it -> 'at -> 'bt, [> mapper ]) accessor
        -> ('i Index.t * 'a, 'b, 'c, 'd) T.t @ local
        -> ('it Index.t * 'at, 'bt, 'c, 'd) T.t @ local
    end
  end
end
