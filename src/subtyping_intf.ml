open! Base
open! Import


module type Subtyping = sig
  (** The subtyping scheme for accessors involves the following "feature" types. Each kind
      of accessor is defined by the features it has. If an accessor A's features is a
      subset of another accessor B's features, then A is a supertype of B. *)

  type at_least_one = [ `at_least_one ]
  type at_most_one = [ `at_most_one ]
  type coerce = [ `coerce ]
  type construct = [ `construct ]
  type get = [ `get ]
  type map = [ `map ]
  type constructor = construct

  type equality =
    [ get
    | map
    | at_most_one
    | at_least_one
    | construct
    | coerce
    ]

  type field =
    [ get
    | map
    | at_most_one
    | at_least_one
    ]

  type getter =
    [ get
    | at_least_one
    | at_most_one
    ]

  type isomorphism =
    [ get
    | map
    | at_most_one
    | at_least_one
    | construct
    ]

  type many =
    [ get
    | map
    ]

  type many_getter = get
  type mapper = map

  type nonempty =
    [ get
    | map
    | at_least_one
    ]

  type nonempty_getter =
    [ get
    | at_least_one
    ]

  type optional =
    [ get
    | map
    | at_most_one
    ]

  type optional_getter =
    [ get
    | at_most_one
    ]

  type variant =
    [ get
    | map
    | at_most_one
    | construct
    ]
end
with type at_least_one := [ `at_least_one ]
 and type at_most_one := [ `at_most_one ]
 and type coerce := [ `coerce ]
 and type construct := [ `construct ]
 and type get := [ `get ]
 and type map := [ `map ]
