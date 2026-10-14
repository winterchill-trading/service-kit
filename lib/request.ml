open! Core

type t =
  { meth : Cohttp.Code.meth
  ; uri : Uri.t
  ; path : string list
  ; headers : Http.Header.t
  ; body : string
  ; remote : string option
  ; route : string
  ; request_id : string
  ; context : Univ_map.t
  }

let split_path path =
  String.split path ~on:'/'
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:Uri.pct_decode
;;

let create ?(headers = []) ?(body = "") ?remote meth uri =
  let uri = Uri.of_string uri in
  { meth
  ; uri
  ; path = split_path (Uri.path uri)
  ; headers = Http.Header.of_list headers
  ; body
  ; remote
  ; route = "unmatched"
  ; request_id = ""
  ; context = Univ_map.empty
  }
;;

let header t name = Http.Header.get t.headers name
let query t name = Uri.get_query_param t.uri name

let json t =
  if String.is_empty (String.strip t.body)
  then Ok (`Assoc [])
  else (
    try Ok (Yojson.Safe.from_string t.body) with
    | Yojson.Json_error _ -> Error (Problem.bad_request "invalid_json"))
;;

let decode t of_yojson =
  Result.bind (json t) ~f:(fun j ->
    try Ok (of_yojson j) with
    | _ -> Error (Problem.bad_request "invalid_body"))
;;

let set t key value = { t with context = Univ_map.set t.context ~key ~data:value }
let find t key = Univ_map.find t.context key
