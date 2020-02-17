open! Core

module Color = struct
  module T = struct
    type t =
      | Red
      | Orange
      | Gold
      | Green
      | Blue
      | Indigo
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)

  let to_string t = String.lowercase (Sexp.to_string [%sexp (t : t)])
end

module Feature = struct
  module T = struct
    type t =
      [ `at_least_one
      | `at_most_one
      | `coerce
      | `construct
      | `get
      | `map
      ]
    [@@deriving compare, enumerate, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)

  let color : t -> Color.t = function
    | `at_least_one -> Red
    | `at_most_one -> Orange
    | `coerce -> Gold
    | `construct -> Green
    | `get -> Blue
    | `map -> Indigo
  ;;

  let color_set ts = Core.Set.map (module Color) ts ~f:color

  let to_string t =
    String.substr_replace_all (Sexp.to_string ([%sexp_of: t] t)) ~pattern:"_" ~with_:" "
  ;;

  let legend =
    let label =
      List.map all ~f:(fun t ->
        String.concat
          [ {|<font color="|}
          ; Color.to_string (color t)
          ; {|">|}
          ; to_string t
          ; {|</font><br align="left"/>|}
          ])
      |> String.concat
    in
    String.concat
      [ {|
  subgraph cluster_01 {
    node [shape=plaintext]
    label = "Legend";
    legend [label=<|}
      ; label
      ; {|>]
  }|}
      ]
  ;;
end

module Feature_set = struct
  module T = Feature.Set
  include T
  include Comparable.Make_plain (T)
end

module Subtyping = struct
  type constructor = [ `construct ] [@@deriving enumerate]

  let constructor_features = Feature.Set.of_list (all_of_constructor :> Feature.t list)

  type equality =
    [ `get
    | `map
    | `at_most_one
    | `at_least_one
    | `construct
    | `coerce
    ]
  [@@deriving enumerate]

  let equality_features = Feature.Set.of_list (all_of_equality :> Feature.t list)

  type field =
    [ `get
    | `map
    | `at_most_one
    | `at_least_one
    ]
  [@@deriving enumerate]

  let field_features = Feature.Set.of_list (all_of_field :> Feature.t list)

  type getter =
    [ `get
    | `at_least_one
    | `at_most_one
    ]
  [@@deriving enumerate]

  let getter_features = Feature.Set.of_list (all_of_getter :> Feature.t list)

  type isomorphism =
    [ `get
    | `map
    | `at_most_one
    | `at_least_one
    | `construct
    ]
  [@@deriving enumerate]

  let isomorphism_features = Feature.Set.of_list (all_of_isomorphism :> Feature.t list)

  type many =
    [ `get
    | `map
    ]
  [@@deriving enumerate]

  let many_features = Feature.Set.of_list (all_of_many :> Feature.t list)

  type many_getter = [ `get ] [@@deriving enumerate]

  let many_getter_features = Feature.Set.of_list (all_of_many_getter :> Feature.t list)

  type mapper = [ `map ] [@@deriving enumerate]

  let mapper_features = Feature.Set.of_list (all_of_mapper :> Feature.t list)

  type nonempty =
    [ `get
    | `map
    | `at_least_one
    ]
  [@@deriving enumerate]

  let nonempty_features = Feature.Set.of_list (all_of_nonempty :> Feature.t list)

  type nonempty_getter =
    [ `get
    | `at_least_one
    ]
  [@@deriving enumerate]

  let nonempty_getter_features =
    Feature.Set.of_list (all_of_nonempty_getter :> Feature.t list)
  ;;

  type optional =
    [ `get
    | `map
    | `at_most_one
    ]
  [@@deriving enumerate]

  let optional_features = Feature.Set.of_list (all_of_optional :> Feature.t list)

  type optional_getter =
    [ `get
    | `at_most_one
    ]
  [@@deriving enumerate]

  let optional_getter_features =
    Feature.Set.of_list (all_of_optional_getter :> Feature.t list)
  ;;

  type variant =
    [ `get
    | `map
    | `at_most_one
    | `construct
    ]
  [@@deriving enumerate]

  let variant_features = Feature.Set.of_list (all_of_variant :> Feature.t list)
end

open (
struct
  module Coverage_check : module type of Accessor.Subtyping = Subtyping
end :
sig end)

module Named_kind = struct
  type t =
    | Constructor
    | Equality
    | Field
    | Getter
    | Isomorphism
    | Many
    | Many_getter
    | Mapper
    | Nonempty
    | Nonempty_getter
    | Optional
    | Optional_getter
    | Variant
  [@@deriving enumerate, sexp_of]

  let name t = String.lowercase (Sexp.to_string [%sexp (t : t)])

  let features =
    let open Subtyping in
    function
    | Constructor -> constructor_features
    | Equality -> equality_features
    | Field -> field_features
    | Getter -> getter_features
    | Isomorphism -> isomorphism_features
    | Many -> many_features
    | Many_getter -> many_getter_features
    | Mapper -> mapper_features
    | Nonempty -> nonempty_features
    | Nonempty_getter -> nonempty_getter_features
    | Optional -> optional_features
    | Optional_getter -> optional_getter_features
    | Variant -> variant_features
  ;;

  let functions : t -> string list = function
    | Constructor -> [ "construct" ]
    | Equality -> [ "identical" ]
    | Field -> []
    | Getter -> [ "get" ]
    | Isomorphism -> [ "invert" ]
    | Many -> [ "Applicative.map" ]
    | Many_getter ->
      [ "fold"; "iter"; "to_list"; "map_reduce"; "Applicative.fold"; "Applicative.iter" ]
    | Mapper -> [ "map"; "set" ]
    | Nonempty -> []
    | Nonempty_getter -> [ "map_reduce_nonempty" ]
    | Optional -> []
    | Optional_getter -> [ "get_option" ]
    | Variant -> [ "match_" ]
  ;;

  let label t =
    let format string ~size =
      String.concat [ "<font point-size='"; Int.to_string size; "'>"; string; "</font>" ]
    in
    let name = format (name t) ~size:14 in
    match functions t with
    | [] -> name
    | functions ->
      String.concat
        ([ name; "|" ]
         @ List.map functions ~f:(fun f -> format f ~size:10 ^ "<br align=\"left\"/>"))
  ;;

  let node kind =
    printf
      "%s [shape=Mrecord, fontname=\"Monospace\", label=<{%s}>]\n"
      (name kind)
      (label kind)
  ;;
end

module Kind = struct
  type t =
    | Named of Named_kind.t
    | Unnamed of Feature.Set.t

  let unnamed_node_name ~features =
    String.concat [ "\""; Sexp.to_string_mach [%sexp (features : Feature.Set.t)]; "\"" ]
  ;;

  let name = function
    | Named kind -> Named_kind.name kind
    | Unnamed features -> unnamed_node_name ~features
  ;;

  let features = function
    | Named kind -> Named_kind.features kind
    | Unnamed features -> features
  ;;

  let colors t = Feature.color_set (features t)
  let is_subtype t ~of_ = Set.is_subset (features t) ~of_:(features of_)

  let node = function
    | Named kind -> Named_kind.node kind
    | Unnamed features -> print_endline (unnamed_node_name ~features ^ " [label=\"?\"]")
  ;;

  let all =
    let named = List.map Named_kind.all ~f:(fun kind -> Named kind) in
    let unnamed =
      let unnamed_feature_sets =
        let open List.Let_syntax in
        let named_feature_sets =
          List.map Named_kind.all ~f:Named_kind.features |> Feature_set.Set.of_list
        in
        let%bind a = Named_kind.all
        and b = Named_kind.all in
        let candidate = Set.inter (Named_kind.features a) (Named_kind.features b) in
        if Set.mem named_feature_sets candidate then [] else return candidate
      in
      unnamed_feature_sets
      |> List.filter ~f:(fun features -> not (Set.is_empty features))
      |> List.dedup_and_sort ~compare:Feature_set.compare
      |> List.map ~f:(fun features -> Unnamed features)
    in
    named @ unnamed
  ;;
end

module Edge = struct
  type t =
    { source : Kind.t
    ; target : Kind.t
    }

  let to_string { source; target } =
    sprintf
      "%s -> %s [color=\"%s\", arrowhead=none, penwidth=2]"
      (Kind.name source)
      (Kind.name target)
      (Set.diff (Kind.colors source) (Kind.colors target)
       |> Set.to_list
       |> List.map ~f:Color.to_string
       |> String.concat ~sep:":invis:")
  ;;
end

let edges =
  let open List.Let_syntax in
  let%bind source = Kind.all
  and target = Kind.all in
  let%map () = if Kind.is_subtype target ~of_:source then return () else [] in
  { Edge.source; target }
;;

let () =
  print_endline "digraph G {";
  List.iter Kind.all ~f:Kind.node;
  List.iter edges ~f:(fun edge -> print_endline (Edge.to_string edge));
  print_endline Feature.legend;
  print_endline "}"
;;
