open! Core
open! Async
open Service_kit
module Idem = Idempotency.Make (Idempotency.Memory_store)

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"

type env =
  { manual : Clock.Manual.t
  ; store : Idempotency.Memory_store.t
  ; idem : Idem.t
  ; runs : int ref
  ; router : Router.t
  ; gate : unit Ivar.t ref
  }

let setup ?required () =
  let manual = Clock.Manual.create ~start:t0 in
  let store = Idempotency.Memory_store.create () in
  let idem = Idem.create ?required ~store ~clock:(Clock.Manual.clock manual) () in
  let runs = ref 0 in
  let gate = ref (Ivar.create_full ()) in
  let router =
    Router.create
      ~middleware:[ Idem.middleware idem ]
      Router.
        [ Route.post
            Path.(root / "transfers")
            (fun req ->
              incr runs;
              let%bind () = Ivar.read !gate in
              match Request.json req with
              | Ok (`Assoc fields) when List.Assoc.mem fields "fail" ~equal:String.equal
                -> return (Response.problem Problem.internal)
              | Ok (`Assoc fields) when List.Assoc.mem fields "raise" ~equal:String.equal
                -> failwith "handler bug"
              | _ ->
                return
                  (Response.json
                     ~status:`Created
                     (`Assoc [ "transfer", `Int !runs ])
                     ~headers:[ "location", sprintf "/transfers/%d" !runs ]))
        ; Route.get Path.(root / "transfers") (fun _ -> return (Response.text "list"))
        ]
  in
  { manual; store; idem; runs; router; gate }
;;

let call ?(key = Some "k-1") ?(auth = "Bearer alice") ?(meth = `POST) env body =
  let headers =
    [ "authorization", auth ]
    @ Option.value_map key ~default:[] ~f:(fun k -> [ "idempotency-key", k ])
  in
  Router.handle env.router (Request.create meth "/transfers" ~headers ~body)
;;

let show (r : Response.t) =
  printf
    "%d %s%s\n"
    (Response.status_code r)
    r.body
    (match Response.header r "idempotent-replayed" with
     | Some _ -> " (replayed)"
     | None -> "")
;;

let%expect_test "replay, reuse with another body, other callers" =
  let env = setup () in
  let%bind () = call env {|{"amount": "10.00", "to": "bob"}|} >>| show in
  (* Same request, keys in another order: replayed. *)
  let%bind () = call env {|{"to": "bob", "amount": "10.00"}|} >>| show in
  let%bind () = call env {|{"amount": "99.00", "to": "bob"}|} >>| show in
  let%bind () =
    call env ~auth:"Bearer carol" {|{"amount": "10.00", "to": "bob"}|} >>| show
  in
  let%bind () = call env ~key:None {|{"amount": "10.00", "to": "bob"}|} >>| show in
  let%bind () = call env ~meth:`GET "" >>| show in
  print_s [%sexp (!(env.runs) : int)];
  [%expect
    {|
    201 {"transfer":1}
    201 {"transfer":1} (replayed)
    409 {"type":"about:blank","title":"Conflict","status":409,"code":"idempotency_key_reused"}
    201 {"transfer":2}
    201 {"transfer":3}
    200 list
    3
    |}];
  return ()
;;

let%expect_test "in progress, failures and exceptions release the key" =
  let env = setup () in
  env.gate := Ivar.create ();
  let first = call env "{}" in
  let%bind () =
    call env "{}"
    >>| fun r ->
    show r;
    print_s [%sexp (Response.header r "retry-after" : string option)]
  in
  Ivar.fill_exn !(env.gate) ();
  let%bind () = first >>| show in
  let%bind () = call env ~key:(Some "k-2") {|{"fail": true}|} >>| show in
  let%bind () = call env ~key:(Some "k-2") {|{"fail": true}|} >>| show in
  let%bind r =
    Monitor.try_with ~extract_exn:true (fun () ->
      call env ~key:(Some "k-3") {|{"raise": 1}|})
  in
  print_s [%sexp (Result.is_error r : bool)];
  print_s [%sexp (Idempotency.Memory_store.length env.store : int)];
  print_s [%sexp (!(env.runs) : int)];
  [%expect
    {|
    409 {"type":"about:blank","title":"Conflict","status":409,"code":"idempotency_key_in_use"}
    (1)
    201 {"transfer":1}
    500 {"type":"about:blank","title":"Internal Server Error","status":500,"code":"internal_error"}
    500 {"type":"about:blank","title":"Internal Server Error","status":500,"code":"internal_error"}
    true
    1
    4
    |}];
  return ()
;;

let%expect_test "retention and stale records" =
  let env = setup () in
  let%bind () = call env "{}" >>| show in
  Clock.Manual.advance env.manual (Time_ns.Span.of_hr 25.);
  let%bind () = call env "{}" >>| show in
  let%bind n = Idem.purge env.idem in
  print_s [%sexp (n : int Or_error.t)];
  Clock.Manual.advance env.manual (Time_ns.Span.of_hr 25.);
  let%bind n = Idem.purge env.idem in
  print_s [%sexp (n : int Or_error.t)];
  (* A record left in progress by a crashed instance is taken over once stale. *)
  let crashed =
    { Idempotency.Record.scope =
        { caller = "auth:" ^ Codec.sha256_hex "Bearer alice"
        ; meth = "POST"
        ; route = "/transfers"
        ; key = "k-9"
        }
    ; request_digest =
        Idempotency.request_digest (Request.create `POST "/transfers" ~body:"{}")
    ; created_at = Clock.Manual.now env.manual
    ; state = In_progress
    }
  in
  let%bind (_ : _ Or_error.t) =
    Idempotency.Memory_store.claim
      env.store
      crashed
      ~reclaim_before:Time_ns.epoch
      ~stale_before:Time_ns.epoch
  in
  let%bind () = call env ~key:(Some "k-9") "{}" >>| show in
  Clock.Manual.advance env.manual (Time_ns.Span.of_min 6.);
  let%bind () = call env ~key:(Some "k-9") "{}" >>| show in
  [%expect
    {|
    201 {"transfer":1}
    201 {"transfer":2}
    (Ok 0)
    (Ok 1)
    409 {"type":"about:blank","title":"Conflict","status":409,"code":"idempotency_key_in_use"}
    201 {"transfer":3}
    |}];
  return ()
;;

let%expect_test "keys" =
  let env = setup ~required:true () in
  let%bind () = call env ~key:None "{}" >>| show in
  let%bind () = call env ~key:(Some "has space") "{}" >>| show in
  let%bind () = call env ~key:(Some (String.make 256 'a')) "{}" >>| show in
  [%expect
    {|
    400 {"type":"about:blank","title":"Bad Request","status":400,"code":"idempotency_key_missing"}
    400 {"type":"about:blank","title":"Bad Request","status":400,"code":"invalid_idempotency_key"}
    400 {"type":"about:blank","title":"Bad Request","status":400,"code":"invalid_idempotency_key"}
    |}];
  return ()
;;

let%expect_test "digest ignores key order and whitespace only" =
  let d meth path body = Idempotency.request_digest (Request.create meth path ~body) in
  let a = d `POST "/t" {|{"a":1,"b":[1,2]}|} in
  print_s
    [%sexp
      [ (String.equal a (d `POST "/t" {| { "b" : [1,2], "a" : 1 } |}) : bool)
      ; (String.equal a (d `POST "/t" {|{"a":1,"b":[2,1]}|}) : bool)
      ; (String.equal a (d `PUT "/t" {|{"a":1,"b":[1,2]}|}) : bool)
      ; (String.equal a (d `POST "/t?x=1" {|{"a":1,"b":[1,2]}|}) : bool)
      ; (String.equal (d `POST "/t" "not json") (d `POST "/t" "not  json") : bool)
      ]];
  [%expect {| (true false false false false) |}];
  return ()
;;

let%expect_test "stable digest" =
  print_endline [%bin_digest: Idempotency.Stable.Record.V1.t];
  [%expect {| d7b592d4bbf83ae9787b389996e4d632 |}];
  return ()
;;

let%test_unit "digest does not depend on object key order" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind fields =
      List.gen_with_length 6 [%quickcheck.generator: string * int]
      >>| List.dedup_and_sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
    in
    let%map shuffled = List.gen_permutations fields in
    fields, shuffled
  in
  Quickcheck.test
    gen
    ~sexp_of:[%sexp_of: (string * int) list * (string * int) list]
    ~f:(fun (a, b) ->
      let body fields =
        Yojson.Safe.to_string (`Assoc (List.map fields ~f:(fun (k, v) -> k, `Int v)))
      in
      let d fields =
        Idempotency.request_digest (Request.create `POST "/t" ~body:(body fields))
      in
      [%test_result: string] (d b) ~expect:(d a))
;;
