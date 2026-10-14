(* Generators shared by the property tests. *)

open! Core
open Service_kit

let name =
  let open Quickcheck.Generator.Let_syntax in
  let%bind first = Char.gen_lowercase in
  let%map rest =
    String.gen_with_length
      (* up to 63 more characters keeps the whole name within the 64 limit *)
      (Int.min 63 5)
      (Quickcheck.Generator.union [ Char.gen_lowercase; Char.gen_digit; return '_' ])
  in
  String.of_char first ^ rest
;;

let time =
  Quickcheck.Generator.map
    (Int.gen_incl 0 4_102_444_800_000_000_000 (* up to 2100 *))
    ~f:(fun ns -> Time_ns.of_int_ns_since_epoch ns)
;;

let decimal =
  let open Quickcheck.Generator.Let_syntax in
  let%map whole = Int.gen_incl (-1_000_000) 1_000_000
  and frac = Int.gen_incl 0 99 in
  Printf.sprintf "%d.%02d" whole frac
;;

let param_of_kind =
  let open Quickcheck.Generator.Let_syntax in
  Quickcheck.Generator.fixed_point (fun self ->
    Quickcheck.Generator.weighted_union
      [ (3., String.quickcheck_generator >>| fun s -> Param.String s)
      ; (3., Int.quickcheck_generator >>| fun i -> Param.Int i)
      ; (1., Bool.quickcheck_generator >>| fun b -> Param.Bool b)
      ; (2., decimal >>| fun d -> Param.Decimal d)
      ; (2., time >>| fun t -> Param.Time t)
      ; (1., List.gen_with_length 2 self >>| fun l -> Param.List l)
      ])
;;

(* Values that carry their kind, so JSON round trips know what to decode. *)
let rec kind_of (p : Param.t) : Param.Kind.t =
  match p with
  | String _ -> String
  | Int _ -> Int
  | Bool _ -> Bool
  | Decimal _ -> Decimal
  | Time _ -> Time
  | List [] -> List String
  | List (x :: _) -> List (kind_of x)
;;

let homogeneous =
  Quickcheck.Generator.filter param_of_kind ~f:(fun p -> Param.has_kind p (kind_of p))
;;
