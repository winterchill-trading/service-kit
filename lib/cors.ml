open! Core
open! Async

type t =
  { methods : Cohttp.Code.meth list
  ; allow_headers : string list
  ; expose_headers : string list
  ; max_age : Time_ns.Span.t
  ; credentials : bool
  ; origins : [ `Any | `Only of string list ]
  }

let create
  ?(methods = [ `GET; `POST; `PUT; `PATCH; `DELETE ])
  ?(allow_headers =
    [ "authorization"; "content-type"; "idempotency-key"; "x-request-id" ])
  ?(expose_headers = [ "x-request-id"; "retry-after"; "idempotent-replayed" ])
  ?(max_age = Time_ns.Span.of_min 10.)
  ?(credentials = false)
  ~origins
  ()
  =
  (match origins with
   | `Any when credentials ->
     raise_s [%message "Cors.create: credentials with any origin"]
   | `Any | `Only _ -> ());
  { methods; allow_headers; expose_headers; max_age; credentials; origins }
;;

let allows t origin =
  match t.origins with
  | `Any -> true
  | `Only l -> List.mem l origin ~equal:String.equal
;;

let allow_origin t origin =
  match t.origins with
  | `Any -> "*"
  | `Only _ -> origin
;;

let common t origin =
  [ "access-control-allow-origin", allow_origin t origin; "vary", "Origin" ]
  @ if t.credentials then [ "access-control-allow-credentials", "true" ] else []
;;

let add_headers (resp : Response.t) headers =
  List.fold headers ~init:resp ~f:(fun r (n, v) -> Response.with_header r n v)
;;

let middleware t handler (req : Request.t) =
  let origin = Request.header req "origin" in
  let is_preflight =
    (match req.meth with
     | `OPTIONS -> true
     | _ -> false)
    && Option.is_some origin
    && Option.is_some (Request.header req "access-control-request-method")
  in
  match origin with
  | Some origin when is_preflight ->
    let headers =
      if allows t origin
      then
        common t origin
        @ [ ( "access-control-allow-methods"
            , String.concat ~sep:", " (List.map t.methods ~f:Cohttp.Code.string_of_method)
            )
          ; "access-control-allow-headers", String.concat ~sep:", " t.allow_headers
          ; ( "access-control-max-age"
            , Int.to_string (Float.iround_down_exn (Time_ns.Span.to_sec t.max_age)) )
          ]
      else [ "vary", "Origin" ]
    in
    return (Response.empty `No_content ~headers)
  | Some origin when allows t origin ->
    let%map resp = handler req in
    add_headers
      resp
      (common t origin
       @
       if List.is_empty t.expose_headers
       then []
       else [ "access-control-expose-headers", String.concat ~sep:", " t.expose_headers ]
      )
  | Some _ ->
    let%map resp = handler req in
    Response.with_header resp "vary" "Origin"
  | None -> handler req
;;
