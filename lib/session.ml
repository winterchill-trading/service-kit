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
