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

module Binding = struct
  type t =
    { operation : string
    ; idempotency_key : string
    }
  [@@deriving sexp, compare, equal]
end

module type Bindings = sig
  type t

  val bind
    :  t
    -> grant_id:string
    -> binding:Binding.t
    -> until:Time_ns.t
    -> [ `Bound | `Same_request | `Other_request of Binding.t ] Or_error.t
         Async.Deferred.t
end

module Memory_bindings = struct
  type t =
    { clock : Clock.t
    ; bound : (Binding.t * Time_ns.t) String.Table.t
    }

  let create ~clock () = { clock; bound = String.Table.create () }

  let bind t ~grant_id ~binding ~until =
    let now = Clock.now t.clock in
    Hashtbl.filter_inplace t.bound ~f:(fun (_, u) -> Time_ns.( > ) u now);
    match Hashtbl.find t.bound grant_id with
    | Some (existing, _) ->
      if Binding.equal existing binding
      then Async.return (Ok `Same_request)
      else Async.return (Ok (`Other_request existing))
    | None ->
      Hashtbl.set t.bound ~key:grant_id ~data:(binding, until);
      Async.return (Ok `Bound)
  ;;

  let length t = Hashtbl.length t.bound
end

module Mode = struct
  type t =
    | Consume_once
    | Bind_on_first_use : (module Bindings with type t = 'a) * 'a -> t

  let consume_once = Consume_once
  let bind_on_first_use m store = Bind_on_first_use (m, store)
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
    { mode : Mode.t
    ; ttl : Time_ns.Span.t
    ; random : int -> string
    ; key : string
    ; spent : Spent.t
    ; clock : Clock.t
    }

  let create
    ?(mode = Mode.consume_once)
    ?(ttl = Time_ns.Span.of_min 5.)
    ?(random = Codec.random_bytes)
    ~key
    ~spent
    ~clock
    ()
    =
    if String.length key < 32
    then raise_s [%message "Step_up.create: key shorter than 32 bytes"];
    { mode; ttl; random; key; spent; clock }
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

  let consume_once t (grant : Grant.t) =
    Async.Deferred.map
      (Spent.mark t.spent ~grant_id:grant.grant_id ~until:grant.expires_at)
      ~f:(function
      | Ok `First_use -> Ok grant
      | Ok `Already_used -> Error Error.Already_used
      | Error e -> Error (Error.Store_failed e))
  ;;

  let bind
    (type a)
    (module B : Bindings with type t = a)
    (store : a)
    (grant : Grant.t)
    binding
    =
    Async.Deferred.map
      (B.bind store ~grant_id:grant.grant_id ~binding ~until:grant.expires_at)
      ~f:(function
      | Ok (`Bound | `Same_request) -> Ok grant
      | Ok (`Other_request _) -> Error Error.Already_used
      | Error e -> Error (Error.Store_failed e))
  ;;

  (* A binding no later request can repeat, for uses that do not allow retries. *)
  let unrepeatable t =
    { Binding.operation = ""; idempotency_key = "once:" ^ Codec.hex (t.random 16) }
  ;;

  let use_with t token ~session_id ~purpose ~binding =
    match check t token ~session_id ~purpose with
    | Error _ as e -> Async.return e
    | Ok grant ->
      (match t.mode with
       | Consume_once -> consume_once t grant
       | Bind_on_first_use (m, store) ->
         bind
           m
           store
           grant
           (Option.value_or_thunk binding ~default:(fun () -> unrepeatable t)))
  ;;

  let consume t token ~session_id ~purpose =
    use_with t token ~session_id ~purpose ~binding:None
  ;;

  let use t token ~session_id ~purpose ~binding =
    use_with t token ~session_id ~purpose ~binding:(Some binding)
  ;;

  let default_operation (req : Request.t) =
    Cohttp.Code.string_of_method req.meth ^ " " ^ req.route
  ;;

  let header_name = "x-step-up-grant"

  let require ?(operation = default_operation) t ~purpose ~session_id
    : Router.Middleware.t
    =
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
      let binding =
        Option.map (Request.header req Idempotency.header_name) ~f:(fun idempotency_key ->
          { Binding.operation = operation req; idempotency_key })
      in
      Async.Deferred.bind (use_with t token ~session_id ~purpose ~binding) ~f:(function
        | Ok _ -> handler req
        | Error (Store_failed _ as e) ->
          Async.return (Response.problem (Error.to_problem e))
        | Error _ -> required ())
  ;;
end
