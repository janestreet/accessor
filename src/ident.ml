open! Base
open! Import

module T = struct
  include Monad.Ident

  let apply t1 t2 = t1 t2
end

include T
include Many.Of_applicative (T)
include Nonempty.Of_applicative_without_return (T)
