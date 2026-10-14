open! Core

module Stable = struct
  module V1 = struct
    type t =
      | String of string
      | Int of int
      | Bool of bool
      | Decimal of string
      | Time of Time_ns.Stable.V1.t
      | List of t list
    [@@deriving bin_io, sexp, compare, equal]
  end

  module Kind = struct
    module V1 = struct
      type t =
        | String
        | Int
        | Bool
        | Decimal
        | Time
        | List of t
      [@@deriving bin_io, sexp, compare, equal]
    end
  end
end

(* Alternate_sexp prints times in UTC whatever the machine's zone. *)
type t = Stable.V1.t =
  | String of string
  | Int of int
  | Bool of bool
  | Decimal of string
  | Time of Time_ns.Alternate_sexp.t
  | List of t list
[@@deriving sexp, compare, equal]

module Kind = struct
  type t = Stable.Kind.V1.t =
    | String
    | Int
    | Bool
    | Decimal
    | Time
    | List of t
  [@@deriving sexp, compare, equal]

  let rec to_string = function
    | String -> "string"
    | Int -> "int"
    | Bool -> "bool"
    | Decimal -> "decimal"
    | Time -> "time"
    | List k -> "list(" ^ to_string k ^ ")"
  ;;
end

let is_decimal s =
  let digits s = (not (String.is_empty s)) && String.for_all s ~f:Char.is_digit in
  let s = Option.value (String.chop_prefix s ~prefix:"-") ~default:s in
  match String.lsplit2 s ~on:'.' with
  | None -> digits s
  | Some (whole, frac) -> digits whole && digits frac
;;

let decimal s =
  if is_decimal s
  then Ok (Decimal s)
  else Or_error.error_s [%message "Param.decimal: not a decimal" (s : string)]
;;

let rec has_kind t (kind : Kind.t) =
  match t, kind with
  | String _, String | Int _, Int | Bool _, Bool | Decimal _, Decimal | Time _, Time ->
    true
  | List l, List k -> List.for_all l ~f:(fun v -> has_kind v k)
  | (String _ | Int _ | Bool _ | Decimal _ | Time _ | List _), _ -> false
;;

let time_to_string time = Time_ns.to_string_iso8601_basic time ~zone:Time_float.Zone.utc

let time_of_string s =
  Or_error.try_with (fun () ->
    Time_ns.of_string_with_utc_offset (String.tr s ~target:'T' ~replacement:' '))
;;

let rec to_yojson : t -> Yojson.Safe.t = function
  | String s -> `String s
  | Int i -> `Int i
  | Bool b -> `Bool b
  | Decimal d -> `String d
  | Time time -> `String (time_to_string time)
  | List l -> `List (List.map l ~f:to_yojson)
;;

let rec of_yojson (kind : Kind.t) (json : Yojson.Safe.t) : t Or_error.t =
  match kind, json with
  | String, `String s -> Ok (String s)
  | Int, `Int i -> Ok (Int i)
  | Bool, `Bool b -> Ok (Bool b)
  | Decimal, `String s -> decimal s
  | Time, `String s -> Or_error.map (time_of_string s) ~f:(fun t -> Time t)
  | List k, `List l ->
    Or_error.map (Or_error.all (List.map l ~f:(of_yojson k))) ~f:(fun l -> List l)
  | _ ->
    Or_error.error_s
      [%message
        "Param.of_yojson: kind mismatch"
          ~expected:(Kind.to_string kind)
          ~got:(Yojson.Safe.to_string json)]
;;

let params_to_yojson params =
  `Assoc (List.map params ~f:(fun (name, v) -> name, to_yojson v))
;;

let is_valid_name s =
  let n = String.length s in
  n >= 1
  && n <= 64
  && Char.is_lowercase s.[0]
  && String.for_all s ~f:(fun c ->
    Char.is_lowercase c || Char.is_digit c || Char.equal c '_')
;;
