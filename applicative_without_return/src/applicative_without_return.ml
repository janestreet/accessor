open! Base
open! Import
include Applicative_without_return_intf

module Make3 (X : Basic3) : S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) X.t = struct
  include X

  let ( <*> ) = apply
  let ( >>| ) t f = map t ~f
  let map2 ta tb ~f = map ~f ta <*> tb
  let map3 ta tb tc ~f = map ~f ta <*> tb <*> tc
  let both ta tb = map2 ta tb ~f:(fun a b -> a, b)

  module Applicative_infix = struct
    let ( <*> ) = ( <*> )
    let ( >>| ) = ( >>| )
  end

  module Let_syntax = struct
    include Applicative_infix

    module Let_syntax = struct
      let map = map
      let both = both

      module Open_on_rhs = struct end
    end
  end
end

module Make2 (X : Basic2) : S2 with type ('a, 'e) t := ('a, 'e) X.t = Make3 (struct
    type ('a, _, 'e) t = ('a, 'e) X.t

    include (X : module type of X with type ('a, 'e) t := ('a, 'e) X.t)
  end)

module Make (X : Basic) : S with type 'a t := 'a X.t = Make2 (struct
    type ('a, _) t = 'a X.t

    include (X : module type of X with type 'a t := 'a X.t)
  end)
