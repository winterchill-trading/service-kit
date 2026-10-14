open! Core

(* Not [open Async]: its [Clock] would shadow this library's. *)
let return = Async.return

module Keyring = struct
  type t =
    { current_id : string
    ; keys : string String.Map.t
    }

  let valid_id id =
    let n = String.length id in
    n >= 1
    && n <= 32
    && String.for_all id ~f:(fun c ->
      Char.is_alphanum c || Char.equal c '_' || Char.equal c '-')
  ;;

  let create ?(previous = []) ~current () =
    let all = current :: previous in
    List.iter all ~f:(fun (id, secret) ->
      if not (valid_id id)
      then raise_s [%message "Session.Keyring: invalid key id" (id : string)];
      if String.length secret < 32
      then
        raise_s [%message "Session.Keyring: secret shorter than 32 bytes" (id : string)]);
    match String.Map.of_alist all with
    | `Duplicate_key id ->
      raise_s [%message "Session.Keyring: repeated key id" (id : string)]
    | `Ok keys -> { current_id = fst current; keys }
  ;;

  let current_id t = t.current_id
  let find t id = Map.find t.keys id
end

module Claims = struct
  module Stable = struct
    module V1 = struct
      type t =
        { session_id : string
        ; token_id : string
        ; subject : string
        ; started_at : Time_ns.Stable.V1.t
        ; issued_at : Time_ns.Stable.V1.t
        ; expires_at : Time_ns.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  (* Alternate_sexp prints times in UTC whatever the machine's zone. *)
  type t = Stable.V1.t =
    { session_id : string
    ; token_id : string
    ; subject : string
    ; started_at : Time_ns.Alternate_sexp.t
    ; issued_at : Time_ns.Alternate_sexp.t
    ; expires_at : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp, compare, equal]
end

module Error = struct
  type t =
    | Malformed
    | Unknown_key
    | Bad_signature
    | Expired
    | Revoked
  [@@deriving sexp, compare, equal]

  let to_problem = function
    | Expired -> Problem.unauthorized "session_expired"
    | Malformed | Unknown_key | Bad_signature | Revoked ->
      Problem.unauthorized "session_invalid"
  ;;
end

type t =
  { ttl : Time_ns.Span.t
  ; max_lifetime : Time_ns.Span.t
  ; is_revoked : Claims.t -> bool Async.Deferred.t
  ; random : int -> string
  ; keyring : Keyring.t
  ; clock : Clock.t
  }

let create
  ?(ttl = Time_ns.Span.of_hr 24.)
  ?(max_lifetime = Time_ns.Span.of_day 30.)
  ?(is_revoked = fun _ -> return false)
  ?(random = Codec.random_bytes)
  ~keyring
  ~clock
  ()
  =
  { ttl; max_lifetime; is_revoked; random; keyring; clock }
;;

let version = "v1"

let sign t (claims : Claims.t) =
  let kid = Keyring.current_id t.keyring in
  let secret = Keyring.find t.keyring kid |> Option.value_exn in
  let payload =
    Codec.base64url_encode (Binable.to_string (module Claims.Stable.V1) claims)
  in
  let signed = String.concat ~sep:"." [ version; kid; payload ] in
  signed ^ "." ^ Codec.base64url_encode (Codec.hmac_sha256 ~key:secret signed)
;;

let new_id t = Codec.hex (t.random 16)

let issue t ~subject =
  let now = Clock.now t.clock in
  let session_id = new_id t in
  let token_id = new_id t in
  let claims =
    { Claims.session_id
    ; token_id
    ; subject
    ; started_at = now
    ; issued_at = now
    ; expires_at = Time_ns.add now t.ttl
    }
  in
  sign t claims, claims
;;

let decode t token : (Claims.t, Error.t) Result.t =
  match String.split token ~on:'.' with
  | [ v; kid; payload; mac ] when String.equal v version ->
    (match Keyring.find t.keyring kid with
     | None -> Error Unknown_key
     | Some secret ->
       (match Codec.base64url_decode mac with
        | None -> Error Malformed
        | Some mac ->
          let signed = String.concat ~sep:"." [ v; kid; payload ] in
          if not (Codec.equal_constant_time mac (Codec.hmac_sha256 ~key:secret signed))
          then Error Bad_signature
          else (
            match
              Option.bind (Codec.base64url_decode payload) ~f:(fun bytes ->
                Option.try_with (fun () ->
                  Binable.of_string (module Claims.Stable.V1) bytes))
            with
            | None -> Error Malformed
            | Some claims ->
              if Time_ns.( >= ) (Clock.now t.clock) claims.expires_at
              then Error Expired
              else Ok claims)))
  | _ -> Error Malformed
;;

let verify t token =
  match decode t token with
  | Error _ as e -> return e
  | Ok claims ->
    Async.Deferred.map (t.is_revoked claims) ~f:(fun revoked ->
      if revoked then Error Error.Revoked else Ok claims)
;;

let rotate t token =
  Async.Deferred.map (verify t token) ~f:(function
    | Error _ as e -> e
    | Ok old ->
      let now = Clock.now t.clock in
      let limit = Time_ns.add old.started_at t.max_lifetime in
      if Time_ns.( >= ) now limit
      then Error Error.Expired
      else (
        let claims =
          { old with
            token_id = new_id t
          ; issued_at = now
          ; expires_at = Time_ns.min limit (Time_ns.add now t.ttl)
          }
        in
        Ok (sign t claims, claims)))
;;

let of_authorization_header value =
  match String.lsplit2 (String.strip value) ~on:' ' with
  | Some (scheme, token) when String.Caseless.equal scheme "bearer" ->
    let token = String.strip token in
    if String.is_empty token then None else Some token
  | _ -> None
;;

let claims_key : Claims.t Univ_map.Key.t =
  Univ_map.Key.create ~name:"service_kit.session.claims" Claims.sexp_of_t
;;

let claims req = Request.find req claims_key

let middleware ?(optional = false) t : Router.Middleware.t =
  fun handler req ->
  let refuse (p : Problem.t) =
    return
      (Response.problem { p with headers = p.headers @ [ "www-authenticate", "Bearer" ] })
  in
  let refuse_error e = refuse (Error.to_problem e) in
  match Request.header req "authorization" with
  | None when optional -> handler req
  | None -> refuse (Problem.unauthorized "session_required")
  | Some value ->
    (match of_authorization_header value with
     | None -> refuse_error Malformed
     | Some token ->
       Async.Deferred.bind (verify t token) ~f:(function
         | Error e -> refuse_error e
         | Ok c -> handler (Request.set req claims_key c)))
;;

module Cookie = struct
  module Same_site = struct
    type t =
      | Strict
      | Lax
      | No_restriction
    [@@deriving sexp, compare, equal]

    let to_string = function
      | Strict -> "Strict"
      | Lax -> "Lax"
      | No_restriction -> "None"
    ;;
  end

  type t =
    { name : string
    ; secure : bool
    ; http_only : bool
    ; same_site : Same_site.t
    ; path : string
    ; domain : string option
    }
  [@@deriving sexp_of]

  (* RFC 6265 cookie-name: an RFC 7230 token. *)
  let valid_name name =
    (not (String.is_empty name))
    && String.for_all name ~f:(fun c ->
      Char.to_int c > 0x20
      && Char.to_int c < 0x7f
      && not (String.mem "()<>@,;:\\\"/[]?={}" c))
  ;;

  let create
    ?(name = "__Host-session")
    ?(secure = true)
    ?(http_only = true)
    ?(same_site = Same_site.Lax)
    ?(path = "/")
    ?domain
    ()
    =
    let fail msg = raise_s [%message ("Session.Cookie.create: " ^ msg) name] in
    if not (valid_name name) then fail "invalid cookie name";
    if not (String.is_prefix path ~prefix:"/") then fail "path must start with /";
    if String.exists path ~f:(fun c -> Char.equal c ';' || Char.is_whitespace c)
    then fail "path holds ; or white space";
    if String.is_prefix name ~prefix:"__Host-"
       && ((not secure) || Option.is_some domain || not (String.equal path "/"))
    then fail "a __Host- cookie needs Secure, Path=/ and no Domain";
    if String.is_prefix name ~prefix:"__Secure-" && not secure
    then fail "a __Secure- cookie needs Secure";
    if Same_site.equal same_site No_restriction && not secure
    then fail "SameSite=None needs Secure";
    { name; secure; http_only; same_site; path; domain }
  ;;

  let name t = t.name

  let attributes t ~max_age_seconds =
    List.concat
      [ [ "Path=" ^ t.path ]
      ; Option.value_map t.domain ~default:[] ~f:(fun d -> [ "Domain=" ^ d ])
      ; [ sprintf "Max-Age=%d" max_age_seconds ]
      ; (if t.secure then [ "Secure" ] else [])
      ; (if t.http_only then [ "HttpOnly" ] else [])
      ; [ "SameSite=" ^ Same_site.to_string t.same_site ]
      ]
  ;;

  let header t ~value ~max_age_seconds =
    ( "set-cookie"
    , String.concat ~sep:"; " ((t.name ^ "=" ^ value) :: attributes t ~max_age_seconds) )
  ;;

  let set t ~value ~max_age =
    let seconds =
      Time_ns.Span.to_sec max_age |> Float.round_up |> Float.iround_towards_zero_exn
    in
    header t ~value ~max_age_seconds:(Int.max 0 seconds)
  ;;

  let clear t = header t ~value:"" ~max_age_seconds:0

  let parse_header value =
    String.split value ~on:';'
    |> List.filter_map ~f:(fun pair ->
      match String.lsplit2 (String.strip pair) ~on:'=' with
      | None -> None
      | Some (name, value) ->
        let value = String.strip value in
        let value =
          if String.length value >= 2
             && Char.equal value.[0] '"'
             && Char.equal value.[String.length value - 1] '"'
          then String.sub value ~pos:1 ~len:(String.length value - 2)
          else value
        in
        Some (String.strip name, value))
  ;;

  let find t (req : Request.t) =
    Http.Header.get_multi req.headers "cookie"
    |> List.concat_map ~f:parse_header
    |> List.find_map ~f:(fun (name, value) ->
      Option.some_if (String.equal name t.name && not (String.is_empty value)) value)
  ;;
end

module type Store = sig
  type t

  val insert : t -> token_hash:string -> Claims.t -> unit Or_error.t Async.Deferred.t
  val find : t -> token_hash:string -> Claims.t option Or_error.t Async.Deferred.t
  val remove : t -> token_hash:string -> unit Or_error.t Async.Deferred.t
  val remove_session : t -> session_id:string -> int Or_error.t Async.Deferred.t
  val purge : t -> before:Time_ns.t -> int Or_error.t Async.Deferred.t
end

module Memory_store = struct
  type t = (string, Claims.t) Hashtbl.t

  let create () = Hashtbl.create (module String)
  let length = Hashtbl.length

  let insert t ~token_hash claims =
    match Hashtbl.add t ~key:token_hash ~data:claims with
    | `Ok -> return (Ok ())
    | `Duplicate -> return (Or_error.error_string "session token already stored")
  ;;

  let find t ~token_hash = return (Ok (Hashtbl.find t token_hash))

  let remove t ~token_hash =
    Hashtbl.remove t token_hash;
    return (Ok ())
  ;;

  let remove_where t ~f =
    let n = Hashtbl.length t in
    Hashtbl.filter_inplace t ~f:(fun c -> not (f c));
    return (Ok (n - Hashtbl.length t))
  ;;

  let remove_session t ~session_id =
    remove_where t ~f:(fun (c : Claims.t) -> String.equal c.session_id session_id)
  ;;

  let purge t ~before =
    remove_where t ~f:(fun (c : Claims.t) -> Time_ns.( < ) c.expires_at before)
  ;;
end

module Server_side (Store : Store) = struct
  type t =
    { ttl : Time_ns.Span.t
    ; max_lifetime : Time_ns.Span.t
    ; random : int -> string
    ; cookie : Cookie.t
    ; store : Store.t
    ; clock : Clock.t
    }

  let create
    ?(ttl = Time_ns.Span.of_hr 24.)
    ?(max_lifetime = Time_ns.Span.of_day 30.)
    ?(random = Codec.random_bytes)
    ?(cookie = Cookie.create ())
    ~store
    ~clock
    ()
    =
    { ttl; max_lifetime; random; cookie; store; clock }
  ;;

  let cookie t = t.cookie

  (* 32 random bytes; the store only ever sees their SHA-256, so a copy of the session
     table does not hold usable tokens. *)
  let token_bytes = 32
  let token_hash token = Codec.sha256_hex token
  let new_token t = Codec.base64url_encode (t.random token_bytes)

  let well_formed token =
    match Codec.base64url_decode token with
    | Some bytes -> String.length bytes = token_bytes
    | None -> false
  ;;

  let store_new t (claims : Claims.t) =
    let token = new_token t in
    Async.Deferred.map
      (Store.insert t.store ~token_hash:(token_hash token) claims)
      ~f:(Or_error.map ~f:(fun () -> token, claims))
  ;;

  let issue t ~subject =
    let now = Clock.now t.clock in
    store_new
      t
      { Claims.session_id = Codec.hex (t.random 16)
      ; token_id = Codec.hex (t.random 16)
      ; subject
      ; started_at = now
      ; issued_at = now
      ; expires_at = Time_ns.add now t.ttl
      }
  ;;

  let verify t token =
    if not (well_formed token)
    then return (Ok (Error Error.Malformed))
    else
      Async.Deferred.map
        (Store.find t.store ~token_hash:(token_hash token))
        ~f:
          (Or_error.map ~f:(function
            | None -> Error Error.Revoked
            | Some (claims : Claims.t) ->
              if Time_ns.( >= ) (Clock.now t.clock) claims.expires_at
              then Error Error.Expired
              else Ok claims))
  ;;

  let revoke t token =
    if well_formed token
    then Store.remove t.store ~token_hash:(token_hash token)
    else return (Ok ())
  ;;

  let revoke_session t ~session_id = Store.remove_session t.store ~session_id

  let rotate t token =
    let open Async.Deferred.Let_syntax in
    match%bind verify t token with
    | Error err -> return (Error err)
    | Ok (Error e) -> return (Ok (Error e))
    | Ok (Ok old) ->
      let now = Clock.now t.clock in
      let limit = Time_ns.add old.started_at t.max_lifetime in
      if Time_ns.( >= ) now limit
      then return (Ok (Error Error.Expired))
      else (
        let claims =
          { old with
            token_id = Codec.hex (t.random 16)
          ; issued_at = now
          ; expires_at = Time_ns.min limit (Time_ns.add now t.ttl)
          }
        in
        match%bind store_new t claims with
        | Error _ as e -> return e
        | Ok issued ->
          (* The old token stops working at once: a cookie has one value, so nothing
             should present it again. *)
          let%map removed = revoke t token in
          Or_error.map removed ~f:(fun () -> Ok issued))
  ;;

  let set_cookie t (token, (claims : Claims.t)) =
    Cookie.set
      t.cookie
      ~value:token
      ~max_age:(Time_ns.diff claims.expires_at (Clock.now t.clock))
  ;;

  let clear_cookie t = Cookie.clear t.cookie
  let token t req = Cookie.find t.cookie req

  let middleware ?(optional = false) ?(bearer = false) t : Router.Middleware.t =
    fun handler req ->
    let challenge = if bearer then [ "www-authenticate", "Bearer" ] else [] in
    let refuse ?(clear = false) (p : Problem.t) =
      let extra = if clear then [ clear_cookie t ] else [] in
      return (Response.problem { p with headers = p.headers @ challenge @ extra })
    in
    let presented =
      match token t req with
      | Some token -> `Cookie token
      | None ->
        (match Request.header req "authorization" with
         | Some value when bearer ->
           (match of_authorization_header value with
            | Some token -> `Bearer token
            | None -> `Malformed)
         | Some _ | None -> `Absent)
    in
    match presented with
    | `Absent when optional -> handler req
    | `Absent -> refuse (Problem.unauthorized "session_required")
    | `Malformed -> refuse (Error.to_problem Malformed)
    | `Cookie token | `Bearer token ->
      let from_cookie =
        match presented with
        | `Cookie _ -> true
        | _ -> false
      in
      Async.Deferred.bind (verify t token) ~f:(function
        | Error _ ->
          return (Response.problem (Problem.service_unavailable "session_unavailable"))
        | Ok (Error e) -> refuse ~clear:from_cookie (Error.to_problem e)
        | Ok (Ok c) -> handler (Request.set req claims_key c))
  ;;

  let purge t = Store.purge t.store ~before:(Clock.now t.clock)
end
