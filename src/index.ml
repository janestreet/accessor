open! Base
open! Import

type 'a t =
  | [] : unit t
  | ( :: ) : 'a * 'b t -> ('a * 'b) t

let hd (hd :: _) = hd
let tl (_ :: tl) = tl

let rec tail_recursive_with_tuple : type a r. a t -> f:(a -> r) -> r =
  fun t ~f ->
  match t with
  | [] -> f ()
  | hd :: tl -> tail_recursive_with_tuple tl ~f:(fun tl -> f (hd, tl))
;;

let to_tuple t = tail_recursive_with_tuple t ~f:Fn.id

(* renders like a list *)
let sexp_of_t sexp_of_tuple =
  let rec loop acc = function
    | Sexp.List [] -> Sexp.List (List.rev acc)
    | Sexp.List [ sexp; sexps ] -> loop (sexp :: acc) sexps
    | (Sexp.Atom _ | Sexp.List [ _ ] | Sexp.List (_ :: _ :: _)) as sexp ->
      raise_s
        [%message
          "Bug in Index.sexp_of_t: unexpected nested sexp for nested tuple"
            (sexp : Sexp.t)]
  in
  fun t -> loop [] (sexp_of_tuple (to_tuple t))
;;
