open! Core

(* Cohttp derives only sexp for statuses; compare them by number. *)
module Status = struct
  type t = Cohttp.Code.status_code [@@deriving sexp_of]

  let compare a b =
    Int.compare (Cohttp.Code.code_of_status a) (Cohttp.Code.code_of_status b)
  ;;

  let equal a b = compare a b = 0
end

type t =
  { status : Status.t
  ; code : string
  ; params : (string * Param.t) list
  ; instance : string option
  ; headers : (string * string) list
  }
[@@deriving sexp_of, compare, equal]

let content_type = "application/problem+json"

let check_names ~what code params =
  if not (Param.is_valid_name code)
  then raise_s [%message (what ^ ": invalid code") (code : string)];
  List.iter params ~f:(fun (name, _) ->
    if not (Param.is_valid_name name)
    then
      raise_s [%message (what ^ ": invalid param name") (code : string) (name : string)]);
  match List.find_a_dup params ~compare:(fun (a, _) (b, _) -> String.compare a b) with
  | Some (name, _) ->
    raise_s [%message (what ^ ": repeated param") (code : string) (name : string)]
  | None -> ()
;;

let create ?(params = []) ?instance ?(headers = []) status code =
  check_names ~what:"Problem.create" code params;
  { status; code; params; instance; headers }
;;

let bad_request ?params code = create ?params `Bad_request code
let unauthorized ?params code = create ?params `Unauthorized code
let forbidden ?params code = create ?params `Forbidden code
let not_found ?params code = create ?params `Not_found code
let conflict ?params code = create ?params `Conflict code
let unprocessable ?params code = create ?params `Unprocessable_entity code
let service_unavailable ?params code = create ?params `Service_unavailable code

let method_not_allowed ~allowed =
  let allow =
    List.map allowed ~f:Cohttp.Code.string_of_method |> String.concat ~sep:", "
  in
  create `Method_not_allowed "method_not_allowed" ~headers:[ "allow", allow ]
;;

let payload_too_large ~max_bytes =
  create
    `Request_entity_too_large
    "payload_too_large"
    ~params:[ "max_bytes", Int max_bytes ]
;;

let too_many_requests ~retry_after =
  let seconds =
    Int.max 1 (Float.to_int (Float.round_up (Time_ns.Span.to_sec retry_after)))
  in
  create
    `Too_many_requests
    "rate_limited"
    ~params:[ "retry_after_seconds", Int seconds ]
    ~headers:[ "retry-after", Int.to_string seconds ]
;;

let internal = create `Internal_server_error "internal_error"
let status_code t = Cohttp.Code.code_of_status t.status
let title t = Cohttp.Code.reason_phrase_of_code (status_code t)
let with_instance t instance = { t with instance = Some instance }

let to_yojson ?type_base t : Yojson.Safe.t =
  let type_ =
    match type_base with
    | Some base -> base ^ t.code
    | None -> "about:blank"
  in
  `Assoc
    ([ "type", `String type_
     ; "title", `String (title t)
     ; "status", `Int (status_code t)
     ; "code", `String t.code
     ]
     @ (if List.is_empty t.params
        then []
        else [ "params", Param.params_to_yojson t.params ])
     @ Option.value_map t.instance ~default:[] ~f:(fun i -> [ "instance", `String i ]))
;;

let rec untyped_param (json : Yojson.Safe.t) : Param.t Or_error.t =
  match json with
  | `String s -> Ok (String s)
  | `Int i -> Ok (Int i)
  | `Bool b -> Ok (Bool b)
  | `List l ->
    Or_error.map (Or_error.all (List.map l ~f:untyped_param)) ~f:(fun l -> Param.List l)
  | other ->
    Or_error.error_s
      [%message
        "Problem.of_yojson: unsupported param" ~json:(Yojson.Safe.to_string other)]
;;

let of_yojson (json : Yojson.Safe.t) =
  let open Or_error.Let_syntax in
  match json with
  | `Assoc fields ->
    let find name = List.Assoc.find fields name ~equal:String.equal in
    let%bind status =
      match find "status" with
      | Some (`Int n) -> Ok (Cohttp.Code.status_of_code n)
      | _ -> Or_error.error_string "Problem.of_yojson: missing status"
    in
    let%bind code =
      match find "code" with
      | Some (`String c) when Param.is_valid_name c -> Ok c
      | _ -> Or_error.error_string "Problem.of_yojson: missing or invalid code"
    in
    let%bind params =
      match find "params" with
      | None -> Ok []
      | Some (`Assoc ps) ->
        List.map ps ~f:(fun (name, v) ->
          let%map v = untyped_param v in
          name, v)
        |> Or_error.all
      | Some _ -> Or_error.error_string "Problem.of_yojson: params is not an object"
    in
    let instance =
      match find "instance" with
      | Some (`String i) -> Some i
      | _ -> None
    in
    Ok { status; code; params; instance; headers = [] }
  | _ -> Or_error.error_string "Problem.of_yojson: not an object"
;;
