open! Core
open Service_kit

(* [open Async] would shadow [Service_kit.Clock]; take what is needed by name. *)
let return = Async.return

module Purpose = struct
  type t = Send_transfer [@@deriving sexp, compare, equal]

  let to_string Send_transfer = "send_transfer"

  let of_string = function
    | "send_transfer" -> Some Send_transfer
    | _ -> None
  ;;
end

module Grants = Step_up.Make (Purpose) (Step_up.Memory_spent)
module Idem = Idempotency.Make (Idempotency.Memory_store)

module Cursor = Pagination.Cursor (struct
    type t = int [@@deriving bin_io, sexp_of]
  end)

type transfer =
  { seq : int
  ; from : string
  ; to_ : string
  ; cents : int
  }

type t =
  { clock : Clock.t
  ; sessions : Session.t
  ; grants : Grants.t
  ; idempotency : Idem.t
  ; balances : int String.Table.t
  ; mutable transfers : transfer list (* newest first *)
  ; mutable next_seq : int
  ; cursor_key : string
  ; registry : Metrics.t
  ; requests : Metrics.Requests.t
  ; limits : Rate_limit.t
  ; log : Request_log.Entry.t -> unit
  ; random : int -> string
  }

(* A demo passcode; a real service checks a stored hash. *)
let passcode = "0000"
let starting_balance = 100_00

let create ?(random = Codec.random_bytes) ?(log = ignore) ~clock () =
  let secret () = random 32 in
  let registry = Metrics.create () in
  { clock
  ; sessions =
      Session.create
        ~random
        ~keyring:(Session.Keyring.create ~current:("k1", secret ()) ())
        ~clock
        ()
  ; grants =
      Grants.create
        ~random
        ~key:(secret ())
        ~spent:(Step_up.Memory_spent.create ~clock ())
        ~clock
        ()
  ; idempotency =
      Idem.create
        ~caller:(fun req ->
          Option.value_map (Session.claims req) ~default:"anonymous" ~f:(fun c ->
            c.subject))
        ~store:(Idempotency.Memory_store.create ())
        ~clock
        ()
  ; balances = String.Table.create ()
  ; transfers = []
  ; next_seq = 1
  ; cursor_key = secret ()
  ; registry
  ; requests = Metrics.Requests.create registry
  ; limits = Rate_limit.create (Rate_limit.Config.per_minute ~burst:60 600)
  ; log
  ; random
  }
;;

let codes =
  [ "transfer_sent"
  ; "insufficient_funds"
  ; "unknown_recipient"
  ; "invalid_amount"
  ; "invalid_passcode"
  ; "step_up_required"
  ; "invalid_user"
  ; "invalid_purpose"
  ]
;;

let catalogue =
  Message.Catalogue.create
    [ ( "transfer_sent"
      , { params = [ "amount", Decimal; "to", String ]
        ; templates = [ "en", "Sent {amount} to {to}"; "de", "{amount} an {to} gesendet" ]
        } )
    ; ( "insufficient_funds"
      , { params = [ "available", Decimal ]
        ; templates =
            [ "en", "Only {available} available"; "de", "Nur {available} verfügbar" ]
        } )
    ; ( "unknown_recipient"
      , { params = []
        ; templates = [ "en", "Unknown recipient"; "de", "Unbekannter Empfänger" ]
        } )
    ; ( "invalid_amount"
      , { params = []
        ; templates =
            [ "en", "Enter an amount like 12.50"; "de", "Betrag wie 12.50 eingeben" ]
        } )
    ; ( "invalid_passcode"
      , { params = []; templates = [ "en", "Wrong passcode"; "de", "Falscher Code" ] } )
    ; ( "step_up_required"
      , { params = [ "purpose", String ]
        ; templates = [ "en", "Confirm with your passcode"; "de", "Mit Code bestätigen" ]
        } )
    ; ( "invalid_user"
      , { params = []
        ; templates = [ "en", "Choose another name"; "de", "Anderen Namen wählen" ]
        } )
    ; ( "invalid_purpose"
      , { params = []; templates = [ "en", "Unknown action"; "de", "Unbekannte Aktion" ] }
      )
    ]
  |> ok_exn
;;

let balance t user =
  Hashtbl.find t.balances user |> Option.value ~default:starting_balance
;;

let format_cents c = sprintf "%d.%02d" (c / 100) (c % 100)

(* "12.50" or "12" to cents; nothing else. *)
let parse_cents s =
  let digits s = (not (String.is_empty s)) && String.for_all s ~f:Char.is_digit in
  match String.lsplit2 s ~on:'.' with
  | None when digits s && String.length s <= 9 -> Some (Int.of_string s * 100)
  | Some (whole, frac)
    when digits whole && String.length whole <= 9 && digits frac && String.length frac = 2
    -> Some ((Int.of_string whole * 100) + Int.of_string frac)
  | _ -> None
;;

let field json name =
  match json with
  | `Assoc fields ->
    (match List.Assoc.find fields name ~equal:String.equal with
     | Some (`String s) -> Some s
     | _ -> None)
  | _ -> None
;;

let with_body req f =
  match Request.json req with
  | Error p -> return (Response.problem p)
  | Ok json -> f json
;;

let subject req = (Option.value_exn (Session.claims req)).Session.Claims.subject

let sign_in t req =
  with_body req (fun json ->
    match field json "user" with
    | Some user when Param.is_valid_name user ->
      let token, _ = Session.issue t.sessions ~subject:user in
      return (Response.json ~status:`Created (`Assoc [ "token", `String token ]))
    | _ -> return (Response.problem (Problem.bad_request "invalid_user")))
;;

let step_up t req =
  with_body req (fun json ->
    let claims = Option.value_exn (Session.claims req) in
    match
      field json "purpose" |> Option.bind ~f:Purpose.of_string, field json "passcode"
    with
    | Some purpose, Some code when String.equal code passcode ->
      let grant, _ = Grants.issue t.grants ~session_id:claims.session_id ~purpose in
      return (Response.json ~status:`Created (`Assoc [ "grant", `String grant ]))
    | Some _, _ -> return (Response.problem (Problem.forbidden "invalid_passcode"))
    | None, _ -> return (Response.problem (Problem.bad_request "invalid_purpose")))
;;

let send t req =
  with_body req (fun json ->
    let from = subject req in
    match field json "to", Option.bind (field json "amount") ~f:parse_cents with
    | _, None | _, Some 0 ->
      return (Response.problem (Problem.bad_request "invalid_amount"))
    | None, _ -> return (Response.problem (Problem.not_found "unknown_recipient"))
    | Some to_, _ when String.equal to_ from || not (Param.is_valid_name to_) ->
      return (Response.problem (Problem.not_found "unknown_recipient"))
    | Some to_, Some cents ->
      let available = balance t from in
      if cents > available
      then
        return
          (Response.problem
             (Problem.conflict
                "insufficient_funds"
                ~params:[ "available", Decimal (format_cents available) ]))
      else (
        Hashtbl.set t.balances ~key:from ~data:(available - cents);
        Hashtbl.set t.balances ~key:to_ ~data:(balance t to_ + cents);
        let seq = t.next_seq in
        t.next_seq <- seq + 1;
        t.transfers <- { seq; from; to_; cents } :: t.transfers;
        Request_log.add_field req "transfer" (Int.to_string seq);
        let message =
          Message.create
            "transfer_sent"
            ~params:[ "amount", Decimal (format_cents cents); "to", String to_ ]
        in
        return
          (Response.json
             ~status:`Created
             ~headers:[ "location", sprintf "/transfers/%d" seq ]
             (`Assoc [ "id", `Int seq; "message", Message.to_yojson message ]))))
;;

let list t req =
  let user = subject req in
  match Pagination.limit ~default:2 ~max:50 (Request.query req "limit") with
  | Error p -> return (Response.problem p)
  | Ok limit ->
    let after =
      match Request.query req "cursor" with
      | None -> Ok None
      | Some c -> Result.map (Cursor.decode ~key:t.cursor_key c) ~f:Option.some
    in
    (match after with
     | Error p -> return (Response.problem p)
     | Ok after ->
       let mine =
         List.filter t.transfers ~f:(fun x ->
           (String.equal x.from user || String.equal x.to_ user)
           && Option.for_all after ~f:(fun a -> x.seq < a))
       in
       let page =
         Pagination.Page.of_overfetch
           (List.take mine (limit + 1))
           ~limit
           ~cursor:(fun x -> Cursor.encode ~key:t.cursor_key x.seq)
       in
       return
         (Response.json
            (Pagination.Page.to_yojson
               (fun x ->
                 `Assoc
                   [ "id", `Int x.seq
                   ; "from", `String x.from
                   ; "to", `String x.to_
                   ; "amount", `String (format_cents x.cents)
                   ])
               page)))
;;

let router t =
  let signed_in = Session.middleware t.sessions in
  let health =
    Health.create [ Health.Check.create "memory" (fun () -> return (Ok ())) ]
  in
  Router.create
    ~middleware:
      [ Request_log.middleware
          ~random:(fun () -> Codec.base64url_encode (t.random 9))
          ~clock:t.clock
          ~sink:t.log
          ()
      ; Cors.create ~origins:(`Only [ "http://localhost:8000" ]) () |> Cors.middleware
      ; Metrics.Requests.middleware t.requests ~clock:t.clock
      ; Rate_limit.middleware t.limits ~clock:t.clock
      ]
    (Health.routes health
     @ Router.
         [ Metrics.route t.registry
         ; Route.post Path.(root / "sessions") (sign_in t)
         ; Route.post Path.(root / "step-up") ~middleware:[ signed_in ] (step_up t)
         ; Route.get
             Path.(root / "balance")
             ~middleware:[ signed_in ]
             (fun req ->
               return
                 (Response.json
                    (`Assoc
                      [ "balance", `String (format_cents (balance t (subject req))) ])))
         ; Route.post
             Path.(root / "transfers")
             ~middleware:
               [ signed_in
               ; Idem.middleware t.idempotency
               ; Grants.require t.grants ~purpose:Send_transfer ~session_id:(fun req ->
                   Option.map (Session.claims req) ~f:(fun c -> c.session_id))
               ]
             (send t)
         ; Route.get Path.(root / "transfers") ~middleware:[ signed_in ] (list t)
         ])
;;
