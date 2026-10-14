open! Core

module type Purpose = sig
  type t [@@deriving sexp, compare, equal]

  val to_string : t -> string
  val of_string : string -> t option
end

module type Spent = sig
  type t

  val mark
    :  t
    -> grant_id:string
    -> until:Time_ns.t
    -> [ `First_use | `Already_used ] Or_error.t Async.Deferred.t
end

module Memory_spent = struct
  type t =
    { clock : Clock.t
    ; until : Time_ns.t String.Table.t
    }

  let create ~clock () = { clock; until = String.Table.create () }

  let mark t ~grant_id ~until =
    let now = Clock.now t.clock in
    Hashtbl.filter_inplace t.until ~f:(fun u -> Time_ns.( > ) u now);
    if Hashtbl.mem t.until grant_id
    then Async.return (Ok `Already_used)
    else (
      Hashtbl.set t.until ~key:grant_id ~data:until;
      Async.return (Ok `First_use))
  ;;

  let length t = Hashtbl.length t.until
end

module Error = struct
  type t =
    | Malformed
    | Bad_signature
    | Expired
    | Wrong_session
    | Wrong_purpose
    | Already_used
    | Store_failed of Core.Error.t
  [@@deriving sexp_of]

  let to_problem = function
    | Store_failed _ -> Problem.internal
    | Malformed | Bad_signature | Expired | Wrong_session | Wrong_purpose | Already_used
      -> Problem.forbidden "step_up_required"
  ;;
end

module Stable = struct
  module Claims = struct
    module V1 = struct
      type t =
        { grant_id : string
        ; session_id : string
        ; purpose : string
        ; issued_at : Time_ns.Stable.V1.t
        ; expires_at : Time_ns.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end
end

module Make (Purpose : Purpose) (Spent : Spent) = struct
  module Grant = struct
    type t =
      { grant_id : string
      ; session_id : string
      ; purpose : Purpose.t
      ; issued_at : Time_ns.Alternate_sexp.t
      ; expires_at : Time_ns.Alternate_sexp.t
      }
    [@@deriving sexp_of, compare, equal]
  end

  type t =
    { ttl : Time_ns.Span.t
    ; random : int -> string
    ; key : string
    ; spent : Spent.t
    ; clock : Clock.t
    }

  let create
    ?(ttl = Time_ns.Span.of_min 5.)
    ?(random = Codec.random_bytes)
    ~key
    ~spent
    ~clock
    ()
    =
    if String.length key < 32
    then raise_s [%message "Step_up.create: key shorter than 32 bytes"];
    { ttl; random; key; spent; clock }
  ;;

  let prefix = "su1"

  let issue t ~session_id ~purpose =
    let now = Clock.now t.clock in
    let grant =
      { Grant.grant_id = Codec.hex (t.random 16)
      ; session_id
      ; purpose
      ; issued_at = now
      ; expires_at = Time_ns.add now t.ttl
      }
    in
    let claims =
      { Stable.Claims.V1.grant_id = grant.grant_id
      ; session_id
      ; purpose = Purpose.to_string purpose
      ; issued_at = grant.issued_at
      ; expires_at = grant.expires_at
      }
    in
    let payload =
      Codec.base64url_encode (Binable.to_string (module Stable.Claims.V1) claims)
    in
    let signed = prefix ^ "." ^ payload in
    signed ^ "." ^ Codec.base64url_encode (Codec.hmac_sha256 ~key:t.key signed), grant
  ;;

  let check t token ~session_id ~purpose : (Grant.t, Error.t) Result.t =
    match String.split token ~on:'.' with
    | [ p; payload; mac ] when String.equal p prefix ->
      (match Codec.base64url_decode mac with
       | None -> Error Malformed
       | Some mac ->
         if not
              (Codec.equal_constant_time
                 mac
                 (Codec.hmac_sha256 ~key:t.key (prefix ^ "." ^ payload)))
         then Error Bad_signature
         else (
           match
             Option.bind (Codec.base64url_decode payload) ~f:(fun bytes ->
               Option.try_with (fun () ->
                 Binable.of_string (module Stable.Claims.V1) bytes))
           with
           | None -> Error Malformed
           | Some claims ->
             (match Purpose.of_string claims.purpose with
              | None -> Error Wrong_purpose
              | Some granted ->
                if Time_ns.( >= ) (Clock.now t.clock) claims.expires_at
                then Error Expired
                else if not (String.equal claims.session_id session_id)
                then Error Wrong_session
                else if not (Purpose.equal granted purpose)
                then Error Wrong_purpose
                else
                  Ok
                    { Grant.grant_id = claims.grant_id
                    ; session_id = claims.session_id
                    ; purpose = granted
                    ; issued_at = claims.issued_at
                    ; expires_at = claims.expires_at
                    })))
    | _ -> Error Malformed
  ;;

  let consume t token ~session_id ~purpose =
    match check t token ~session_id ~purpose with
    | Error _ as e -> Async.return e
    | Ok grant ->
      Async.Deferred.map
        (Spent.mark t.spent ~grant_id:grant.grant_id ~until:grant.expires_at)
        ~f:(function
        | Ok `First_use -> Ok grant
        | Ok `Already_used -> Error Error.Already_used
        | Error e -> Error (Error.Store_failed e))
  ;;

  let header_name = "x-step-up-grant"

  let require t ~purpose ~session_id : Router.Middleware.t =
    fun handler req ->
    let required () =
      Async.return
        (Response.problem
           (Problem.forbidden
              "step_up_required"
              ~params:[ "purpose", String (Purpose.to_string purpose) ]))
    in
    match session_id req, Request.header req header_name with
    | None, _ | _, None -> required ()
    | Some session_id, Some token ->
      Async.Deferred.bind (consume t token ~session_id ~purpose) ~f:(function
        | Ok _ -> handler req
        | Error (Store_failed _ as e) ->
          Async.return (Response.problem (Error.to_problem e))
        | Error _ -> required ())
  ;;
end
