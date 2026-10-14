open! Core

(* Kept under another name: [open Async] shadows this library's [Clock]. *)
module Kit_clock = Clock
open! Async

module Stable = struct
  module Scope = struct
    module V1 = struct
      type t =
        { caller : string
        ; meth : string
        ; route : string
        ; key : string
        }
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  module Stored = struct
    module V1 = struct
      type t =
        { status : int
        ; headers : (string * string) list
        ; body : string
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  module Record = struct
    module V1 = struct
      type state =
        | In_progress
        | Completed of Stored.V1.t
      [@@deriving bin_io, sexp, compare, equal]

      type t =
        { scope : Scope.V1.t
        ; request_digest : string
        ; created_at : Time_ns.Stable.V1.t
        ; state : state
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end
end

module Scope = struct
  type t = Stable.Scope.V1.t =
    { caller : string
    ; meth : string
    ; route : string
    ; key : string
    }
  [@@deriving sexp, compare, equal, hash]
end

module Stored = struct
  type t = Stable.Stored.V1.t =
    { status : int
    ; headers : (string * string) list
    ; body : string
    }
  [@@deriving sexp, compare, equal]
end

module Record = struct
  type state = Stable.Record.V1.state =
    | In_progress
    | Completed of Stored.t
  [@@deriving sexp, compare, equal]

  (* Alternate_sexp prints times in UTC whatever the machine's zone. *)
  type t = Stable.Record.V1.t =
    { scope : Scope.t
    ; request_digest : string
    ; created_at : Time_ns.Alternate_sexp.t
    ; state : state
    }
  [@@deriving sexp, compare, equal]
end

module type Store = sig
  type t

  val claim
    :  t
    -> Record.t
    -> reclaim_before:Time_ns.t
    -> stale_before:Time_ns.t
    -> [ `Claimed | `Exists of Record.t ] Or_error.t Deferred.t

  val complete : t -> Scope.t -> Stored.t -> unit Or_error.t Deferred.t
  val release : t -> Scope.t -> unit Or_error.t Deferred.t
  val purge : t -> before:Time_ns.t -> int Or_error.t Deferred.t
end

module Memory_store = struct
  type t = (Scope.t, Record.t) Hashtbl.t

  let create () = Hashtbl.create (module Scope)
  let length = Hashtbl.length

  let claim t (record : Record.t) ~reclaim_before ~stale_before =
    let take () =
      Hashtbl.set t ~key:record.scope ~data:record;
      return (Ok `Claimed)
    in
    match Hashtbl.find t record.scope with
    | None -> take ()
    | Some existing ->
      let expired = Time_ns.( < ) existing.created_at reclaim_before in
      let stale =
        match existing.state with
        | In_progress -> Time_ns.( < ) existing.created_at stale_before
        | Completed _ -> false
      in
      if expired || stale then take () else return (Ok (`Exists existing))
  ;;

  let complete t scope stored =
    match Hashtbl.find t scope with
    | None ->
      return (Or_error.error_s [%message "no idempotency record" (scope : Scope.t)])
    | Some r ->
      Hashtbl.set t ~key:scope ~data:{ r with Record.state = Completed stored };
      return (Ok ())
  ;;

  let release t scope =
    Hashtbl.remove t scope;
    return (Ok ())
  ;;

  let purge t ~before =
    let n = Hashtbl.length t in
    Hashtbl.filter_inplace t ~f:(fun (r : Record.t) -> Time_ns.( >= ) r.created_at before);
    return (Ok (n - Hashtbl.length t))
  ;;
end

module Failure = struct
  type t =
    | Release
    | Record of Stored.t
end

let header_name = "idempotency-key"

let valid_key k =
  let n = String.length k in
  n >= 1
  && n <= 255
  && String.for_all k ~f:(fun c -> Char.to_int c >= 0x21 && Char.to_int c <= 0x7e)
;;

module Key_policy = struct
  type t =
    | Strict
    | Permissive of { max_bytes : int }
  [@@deriving sexp_of]

  let strict = Strict

  let permissive ?(max_bytes = 255) () =
    if max_bytes < 1
    then
      raise_s
        [%message "Idempotency.Key_policy.permissive: max_bytes < 1" (max_bytes : int)];
    Permissive { max_bytes }
  ;;

  let accepts t key =
    match t with
    | Strict -> valid_key key
    | Permissive { max_bytes } ->
      let n = String.length key in
      n >= 1 && n <= max_bytes && String.Utf8.is_valid key
  ;;
end

let rec canonical (json : Yojson.Safe.t) : Yojson.Safe.t =
  match json with
  | `Assoc fields ->
    `Assoc
      (List.map fields ~f:(fun (k, v) -> k, canonical v)
       |> List.stable_sort ~compare:(fun (a, _) (b, _) -> String.compare a b))
  | `List l -> `List (List.map l ~f:canonical)
  | other -> other
;;

let request_digest (req : Request.t) =
  let body =
    match Yojson.Safe.from_string req.body with
    | json -> "json:" ^ Yojson.Safe.to_string (canonical json)
    | exception Yojson.Json_error _ -> "raw:" ^ req.body
  in
  Codec.sha256_hex
    (String.concat
       ~sep:"\n"
       [ Cohttp.Code.string_of_method req.meth; Uri.path_and_query req.uri; body ])
;;

(* 401 and 403 depend on credentials and step-up grants that a retry may fix; 409 and 429
   describe momentary state. Replaying any of them would pin a transient refusal to the
   key. *)
let default_store_status status =
  status < 500 && not (List.mem [ 401; 403; 409; 429 ] status ~equal:Int.equal)
;;

let default_caller req =
  match Request.header req "authorization" with
  | None -> "anonymous"
  | Some auth -> "auth:" ^ Codec.sha256_hex auth
;;

let unsafe (meth : Cohttp.Code.meth) =
  match meth with
  | `POST | `PUT | `PATCH | `DELETE -> true
  | _ -> false
;;

module Make (Store : Store) = struct
  type t =
    { retention : Time_ns.Span.t
    ; stale_after : Time_ns.Span.t
    ; store_status : int -> bool
    ; caller : Request.t -> string
    ; required : bool
    ; on_store_error : Error.t -> unit
    ; on_handler_exn : Failure.t
    ; key_policy : Key_policy.t
    ; store : Store.t
    ; clock : Kit_clock.t
    }

  let create
    ?(retention = Time_ns.Span.of_hr 24.)
    ?(stale_after = Time_ns.Span.of_min 5.)
    ?(store_status = default_store_status)
    ?(caller = default_caller)
    ?(required = false)
    ?(on_store_error = ignore)
    ?(on_handler_exn = Failure.Release)
    ?(key_policy = Key_policy.strict)
    ~store
    ~clock
    ()
    =
    { retention
    ; stale_after
    ; store_status
    ; caller
    ; required
    ; on_store_error
    ; on_handler_exn
    ; key_policy
    ; store
    ; clock
    }
  ;;

  let problem p = return (Response.problem p)

  let replay (stored : Stored.t) =
    { Response.status = Cohttp.Code.status_of_code stored.status
    ; headers = stored.headers @ [ "idempotent-replayed", "true" ]
    ; body = stored.body
    }
  ;;

  let report t = function
    | Ok () -> ()
    | Error e -> t.on_store_error e
  ;;

  (* A store that raises is treated like one that answers [Error], so a failing store
     cannot skip the release below and leave the key in progress. *)
  let guarded f =
    match%map Monitor.try_with_or_error ~extract_exn:true f with
    | Ok r -> r
    | Error _ as e -> e
  ;;

  let release t scope = guarded (fun () -> Store.release t.store scope)

  (* After a handler raised: either free the key or complete it with the configured
     answer. If completing fails, releasing is the fallback; if that fails too, the record
     is taken over once [stale_after] has passed. *)
  let after_exn t scope =
    match t.on_handler_exn with
    | Release -> release t scope >>| report t
    | Record stored ->
      (match%bind guarded (fun () -> Store.complete t.store scope stored) with
       | Ok () -> return ()
       | Error e ->
         t.on_store_error e;
         release t scope >>| report t)
  ;;

  let run t handler (req : Request.t) key =
    let now = Kit_clock.now t.clock in
    let scope =
      { Scope.caller = t.caller req
      ; meth = Cohttp.Code.string_of_method req.meth
      ; route = req.route
      ; key
      }
    in
    let request_digest = request_digest req in
    let record =
      { Record.scope; request_digest; created_at = now; state = In_progress }
    in
    match%bind
      guarded (fun () ->
        Store.claim
          t.store
          record
          ~reclaim_before:(Time_ns.sub now t.retention)
          ~stale_before:(Time_ns.sub now t.stale_after))
    with
    | Error e ->
      t.on_store_error e;
      problem (Problem.service_unavailable "idempotency_unavailable")
    | Ok (`Exists existing) ->
      if not (String.equal existing.request_digest request_digest)
      then problem (Problem.conflict "idempotency_key_reused")
      else (
        match existing.state with
        | Completed stored -> return (replay stored)
        | In_progress ->
          problem
            (Problem.create
               `Conflict
               "idempotency_key_in_use"
               ~headers:[ "retry-after", "1" ]))
    | Ok `Claimed ->
      (match%bind Monitor.try_with ~extract_exn:true (fun () -> handler req) with
       | Error exn ->
         let%bind () = after_exn t scope in
         raise exn
       | Ok (resp : Response.t) ->
         let status = Response.status_code resp in
         let%map result =
           if t.store_status status
           then
             guarded (fun () ->
               Store.complete
                 t.store
                 scope
                 { Stored.status; headers = resp.headers; body = resp.body })
           else release t scope
         in
         report t result;
         resp)
  ;;

  let middleware t : Router.Middleware.t =
    fun handler req ->
    if not (unsafe req.meth)
    then handler req
    else (
      match Request.header req header_name with
      | None when t.required -> problem (Problem.bad_request "idempotency_key_missing")
      | None -> handler req
      | Some key when not (Key_policy.accepts t.key_policy key) ->
        problem (Problem.bad_request "invalid_idempotency_key")
      | Some key -> run t handler req key)
  ;;

  let purge t =
    Store.purge t.store ~before:(Time_ns.sub (Kit_clock.now t.clock) t.retention)
  ;;
end
