open! Core

module Entry = struct
  type t =
    { request_id : string
    ; meth : string
    ; route : string
    ; target : string
    ; status : int
    ; duration : Time_ns.Span.t
    ; remote : string option
    ; headers : (string * string) list
    ; fields : (string * string) list
    }
  [@@deriving sexp_of]

  let to_yojson t : Yojson.Safe.t =
    let strings l = `Assoc (List.map l ~f:(fun (k, v) -> k, `String v)) in
    `Assoc
      ([ "request_id", `String t.request_id
       ; "method", `String t.meth
       ; "route", `String t.route
       ; "target", `String t.target
       ; "status", `Int t.status
       ; "duration_ms", `Float (Time_ns.Span.to_ms t.duration)
       ]
       @ Option.value_map t.remote ~default:[] ~f:(fun r -> [ "remote", `String r ])
       @ (if List.is_empty t.headers then [] else [ "headers", strings t.headers ])
       @ if List.is_empty t.fields then [] else [ "fields", strings t.fields ])
  ;;
end

(* Plain letters, so it reads the same inside a percent-encoded query. *)
let redacted = "REDACTED"

module Redact = struct
  type t =
    { headers : String.Caseless.Set.t
    ; query : String.Set.t
    ; hook : name:string -> string -> string
    }

  let default_headers =
    [ "authorization"; "cookie"; "set-cookie"; "proxy-authorization"; "x-api-key" ]
  ;;

  let default_query =
    [ "token"
    ; "access_token"
    ; "refresh_token"
    ; "code"
    ; "password"
    ; "secret"
    ; "key"
    ; "signature"
    ]
  ;;

  let create
    ?(replace_defaults = false)
    ?(headers = [])
    ?(query = [])
    ?(hook = fun ~name:_ v -> v)
    ()
    =
    let base l d = if replace_defaults then l else l @ d in
    { headers = String.Caseless.Set.of_list (base headers default_headers)
    ; query = String.Set.of_list (base query default_query)
    ; hook
    }
  ;;

  let default = create ()

  let header t ~name value =
    if Set.mem t.headers name then redacted else t.hook ~name value
  ;;

  let target t uri =
    let query =
      Uri.query uri
      |> List.map ~f:(fun (name, values) ->
        ( name
        , List.map values ~f:(fun v ->
            if Set.mem t.query name then redacted else t.hook ~name v) ))
    in
    Uri.path_and_query (Uri.with_query uri query)
  ;;
end

let header_name = "x-request-id"

let valid_request_id s =
  let n = String.length s in
  n >= 1
  && n <= 128
  && String.for_all s ~f:(fun c ->
    Char.is_alphanum c || List.mem [ '.'; '_'; ':'; '-' ] c ~equal:Char.equal)
;;

let fields_key : (string * string) Queue.t Univ_map.Key.t =
  Univ_map.Key.create
    ~name:"service_kit.request_log.fields"
    [%sexp_of: (string * string) Queue.t]
;;

let add_field (req : Request.t) name value =
  Option.iter (Request.find req fields_key) ~f:(fun q -> Queue.enqueue q (name, value))
;;

let default_random () = Codec.base64url_encode (Codec.random_bytes 12)

let middleware
  ?(redact = Redact.default)
  ?(log_headers = [])
  ?(random = default_random)
  ~clock
  ~sink
  ()
  : Router.Middleware.t
  =
  fun handler req ->
  let started = Clock.now clock in
  let request_id =
    match Request.header req header_name with
    | Some id when valid_request_id id -> id
    | Some _ | None -> random ()
  in
  let fields = Queue.create () in
  let req = Request.set { req with request_id } fields_key fields in
  Async.Deferred.map (handler req) ~f:(fun resp ->
    let entry =
      { Entry.request_id
      ; meth = Cohttp.Code.string_of_method req.meth
      ; route = req.route
      ; target = Redact.target redact req.uri
      ; status = Response.status_code resp
      ; duration = Time_ns.diff (Clock.now clock) started
      ; remote = req.remote
      ; headers =
          List.filter_map log_headers ~f:(fun name ->
            let name = String.lowercase name in
            Option.map (Request.header req name) ~f:(fun v ->
              name, Redact.header redact ~name v))
      ; fields = Queue.to_list fields
      }
    in
    sink entry;
    Response.with_header resp header_name request_id)
;;

let log_sink log entry = Async.Log.info_s log [%sexp (entry : Entry.t)]
