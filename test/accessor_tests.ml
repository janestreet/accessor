open! Core
open! Import

let%expect_test "Index.sexp_of_t" =
  print_s [%sexp ([] : unit Accessor.Index.t)];
  [%expect {| () |}];
  print_s [%sexp ([ "a", "b" ] : ((string * string) * unit) Accessor.Index.t)];
  [%expect {| ((a b)) |}];
  print_s
    [%sexp ([ "a"; "b"; "c" ] : (string * (string * (string * unit))) Accessor.Index.t)];
  [%expect {| (a b c) |}]
;;

let%expect_test "invert" =
  let accessor =
    Accessor.isomorphism ~get:(fun at -> `get at) ~construct:(fun b -> `construct b)
  in
  print_s [%sexp (Accessor.get accessor () : [ `get of unit ])];
  [%expect {| (get ()) |}];
  print_s [%sexp (Accessor.construct accessor () : [ `construct of unit ])];
  [%expect {| (construct ()) |}];
  let inverted_accessor = Accessor.invert accessor in
  print_s [%sexp (Accessor.get inverted_accessor () : [ `construct of unit ])];
  [%expect {| (construct ()) |}];
  print_s [%sexp (Accessor.construct inverted_accessor () : [ `get of unit ])];
  [%expect {| (get ()) |}]
;;

let%expect_test "match_" =
  let module Foo = struct
    type 'a t =
      | A of 'a
      | B
    [@@deriving accessors, sexp_of]
  end
  in
  let a : int Foo.t = A 42 in
  print_s [%sexp (Accessor.match_ Foo.a a : (int, Nothing.t Foo.t) Either.t)];
  [%expect {| (First 42) |}];
  let b : int Foo.t = B in
  print_s [%sexp (Accessor.match_ Foo.a b : (int, Nothing.t Foo.t) Either.t)];
  [%expect {| (Second B) |}]
;;

let%expect_test "disjoint_field_product" =
  let module Foo = struct
    type t =
      { x : int
      ; y : int
      }
    [@@deriving accessors, sexp_of]
  end
  in
  let tupled = [%accessor Accessor.disjoint_field_product Foo.x Foo.y] in
  let a = { Foo.x = 42; y = 1337 } in
  print_s [%sexp (a.@(tupled) : int * int)];
  [%expect {| (42 1337) |}];
  print_s [%sexp (a.@(tupled) <- 0, 1 : Foo.t)];
  [%expect {| ((x 0) (y 1)) |}]
;;

let%expect_test "disjoint_merge" =
  let module Foo = struct
    type t =
      { x : int
      ; y : int
      }
    [@@deriving accessors, sexp_of]
  end
  in
  let each = [%accessor Accessor.disjoint_merge Foo.x Foo.y] in
  let a = { Foo.x = 42; y = 1337 } in
  print_s [%sexp (a.@*(each) : int list)];
  [%expect {| (42 1337) |}];
  print_s [%sexp (Accessor.map each a ~f:succ : Foo.t)];
  [%expect {| ((x 43) (y 1338)) |}]
;;

let%expect_test "add_to_index" =
  let x = 42 in
  Accessor.iteri Accessor.add_to_index x ~f:(fun [ index ] value ->
    print_s [%message "" (index : int) (value : int)]);
  [%expect {| ((index 42) (value 42)) |}];
  Accessor.mapi Accessor.add_to_index x ~f:(fun [ index ] value ->
    [%message "" (index : int) (value : int)])
  |> print_s;
  [%expect {| ((index 42) (value 42)) |}]
;;
