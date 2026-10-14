open! Core
open! Async

let default_on_exn exn =
  Log.Global.error_s [%message "request handler raised" (exn : exn)]
;;

let respond (r : Response.t) =
  Cohttp_async.Server.respond_string
    ~headers:(Cohttp.Header.of_list r.headers)
    ~status:r.status
    r.body
;;

let read_body ~max_body ~body (req : Cohttp.Request.t) =
  let declared =
    Option.bind
      (Cohttp.Header.get (Cohttp.Request.headers req) "content-length")
      ~f:Int.of_string_opt
  in
  match declared with
  | Some n when n > max_body -> return None
  | Some _ | None ->
    let%map s = Cohttp_async.Body.to_string body in
    Option.some_if (String.length s <= max_body) s
;;

let to_cohttp
  ?(max_body = 1 lsl 20)
  ?(on_exn = default_on_exn)
  ?type_base
  handler
  ~body
  addr
  (req : Cohttp.Request.t)
  =
  match%bind read_body ~max_body ~body req with
  | None ->
    respond (Response.problem ?type_base (Problem.payload_too_large ~max_bytes:max_body))
  | Some body ->
    let uri = Cohttp.Request.uri req in
    let request =
      { Request.meth = Cohttp.Request.meth req
      ; uri
      ; path = Request.split_path (Uri.path uri)
      ; headers = Cohttp.Request.headers req
      ; body
      ; remote = Some (Unix.Inet_addr.to_string (Socket.Address.Inet.addr addr))
      ; route = "unmatched"
      ; request_id = ""
      ; context = Univ_map.empty
      }
    in
    (match%bind
       Monitor.try_with ~extract_exn:true ~rest:`Log (fun () -> handler request)
     with
     | Ok response -> respond response
     | Error exn ->
       on_exn exn;
       respond (Response.problem ?type_base Problem.internal))
;;

let start ?max_body ?on_exn ?type_base ~port handler =
  Cohttp_async.Server.create
    ~on_handler_error:`Raise
    (Tcp.Where_to_listen.of_port port)
    (to_cohttp ?max_body ?on_exn ?type_base handler)
;;
