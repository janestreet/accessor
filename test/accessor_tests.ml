open! Core_kernel
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
