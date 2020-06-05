open! Base
open! Import

module type Basic3 = sig
  type ('a, 'd, 'e) t

  val apply : ('a -> 'b, 'd, 'e) t -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t
  val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
end

module type Basic2 = sig
  type ('a, 'e) t

  include Basic3 with type ('a, _, 'e) t := ('a, 'e) t
end

module type Basic = sig
  type 'a t

  include Basic2 with type ('a, _) t := 'a t
end

module type S3_without_let_syntax = sig
  type ('a, 'd, 'e) t

  val apply : ('a -> 'b, 'd, 'e) t -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t
  val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
  val map2 : ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'd, 'e) t

  val map3
    :  ('a, 'd, 'e) t
    -> ('b, 'd, 'e) t
    -> ('c, 'd, 'e) t
    -> f:('a -> 'b -> 'c -> 'f)
    -> ('f, 'd, 'e) t

  val both : ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> ('a * 'b, 'd, 'e) t

  module Applicative_infix : sig
    val ( <*> ) : ('a -> 'b, 'd, 'e) t -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t
    val ( >>| ) : ('a, 'd, 'e) t -> ('a -> 'b) -> ('b, 'd, 'e) t
  end

  include module type of Applicative_infix
end

module type S2_without_let_syntax = sig
  type ('a, 'e) t

  include S3_without_let_syntax with type ('a, _, 'e) t := ('a, 'e) t
end

module type S_without_let_syntax = sig
  type 'a t

  include S2_without_let_syntax with type ('a, _) t := 'a t
end

module type S3 = sig
  include S3_without_let_syntax

  module Let_syntax : sig
    include module type of Applicative_infix

    module Let_syntax : sig
      val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
      val both : ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> ('a * 'b, 'd, 'e) t

      module Open_on_rhs : sig end
    end
  end
end

module type S2 = sig
  type ('a, 'e) t

  include S3 with type ('a, _, 'e) t := ('a, 'e) t
end

module type S = sig
  type 'a t

  include S2 with type ('a, _) t := 'a t
end

module type Applicative_without_return = sig
  module type Basic = Basic
  module type Basic2 = Basic2
  module type Basic3 = Basic3
  module type S_without_let_syntax = S_without_let_syntax
  module type S2_without_let_syntax = S2_without_let_syntax
  module type S3_without_let_syntax = S3_without_let_syntax
  module type S = S
  module type S2 = S2
  module type S3 = S3

  module Make (B : Basic) : S with type 'a t := 'a B.t
  module Make2 (B : Basic2) : S2 with type ('a, 'e) t := ('a, 'e) B.t
  module Make3 (B : Basic3) : S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) B.t
end
