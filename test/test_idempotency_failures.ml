open! Core
open! Async
open Service_kit

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"

(* A memory store whose writes can be made to raise or fail, to show that a handler
   exception never leaves its key in progress. *)
module Flaky_store = struct
  type t =
    { inner : Idempotency.Memory_store.t
    ; mutable complete : [ `Ok | `Error | `Raise ]
    ; mutable release : [ `Ok | `Raise ]
    }

  let create () =
    { inner = Idempotency.Memory_store.create (); complete = `Ok; release = `Ok }
  ;;

  let claim t = Idempotency.Memory_store.claim t.inner

  let complete t scope stored =
    match t.complete with
    | `Ok -> Idempotency.Memory_store.complete t.inner scope stored
    | `Error -> return (Or_error.error_string "complete failed")
    | `Raise -> failwith "complete raised"
  ;;

  let release t scope =
    match t.release with
    | `Ok -> Idempotency.Memory_store.release t.inner scope
    | `Raise -> failwith "release raised"
  ;;

  let purge t = Idempotency.Memory_store.purge t.inner
end

module Idem = Idempotency.Make (Flaky_store)

let setup ?on_handler_exn () =
  let store = Flaky_store.create () in
  let errors = ref [] in
  let idem =
    Idem.create
      ?on_handler_exn
      ~on_store_error:(fun e -> errors := Error.to_string_hum e :: !errors)
      ~store
      ~clock:(Clock.Manual.clock (Clock.Manual.create ~start:t0))
      ()
  in
  let runs = ref 0 in
  let app =
    Idem.middleware idem (fun _ ->
      incr runs;
      if !runs = 1 then failwith "handler bug" else return (Response.text "done"))
  in
  store, errors, runs, app
;;

let call app =
  match%map
    Monitor.try_with ~extract_exn:true (fun () ->
      app (Request.create `POST "/t" ~headers:[ "idempotency-key", "k" ] ~body:"{}"))
  with
  | Error exn -> print_s [%message "raised" (exn : exn)]
  | Ok r ->
    print_s
      [%message
        ""
          ~status:(Response.status_code r : int)
          ~body:r.body
          ~replayed:(Response.header r "idempotent-replayed" : string option)]
;;

let%expect_test "a raising handler releases its key by default" =
  let store, errors, runs, app = setup () in
  let%bind () = call app in
  print_s [%sexp (Idempotency.Memory_store.length store.inner : int)];
  let%bind () = call app in
  print_s [%sexp (!runs : int), (!errors : string list)];
  [%expect
    {|
    (raised (exn (Failure "handler bug")))
    0
    ((status 200) (body done) (replayed ()))
    (2 ())
    |}];
  return ()
;;

let%expect_test "a release that raises is reported, not propagated" =
  let store, errors, _, app = setup () in
  store.release <- `Raise;
  let%bind () = call app in
  print_s [%sexp (!errors : string list)];
  [%expect
    {|
    (raised (exn (Failure "handler bug")))
    ("(Failure \"release raised\")")
    |}];
  return ()
;;

let failure =
  { Idempotency.Stored.status = 500
  ; headers = [ "content-type", "application/problem+json" ]
  ; body = {|{"code":"internal_error"}|}
  }
;;

let%expect_test "Record completes the key with the given answer" =
  let _, _, runs, app = setup ~on_handler_exn:(Record failure) () in
  let%bind () = call app in
  let%bind () = call app in
  print_s [%sexp (!runs : int)];
  [%expect
    {|
    (raised (exn (Failure "handler bug")))
    ((status 500) (body "{\"code\":\"internal_error\"}") (replayed (true)))
    1
    |}];
  return ()
;;

let%expect_test "Record falls back to releasing when completing fails" =
  let store, errors, runs, app = setup ~on_handler_exn:(Record failure) () in
  store.complete <- `Raise;
  let%bind () = call app in
  store.complete <- `Ok;
  let%bind () = call app in
  print_s [%sexp (!runs : int), (!errors : string list)];
  [%expect
    {|
    (raised (exn (Failure "handler bug")))
    ((status 200) (body done) (replayed ()))
    (2 ("(Failure \"complete raised\")"))
    |}];
  return ()
;;

let%expect_test "Record_exn stores an answer computed from the exception" =
  let answer exn =
    { failure with
      body = sprintf {|{"code":"internal_error","detail":%S}|} (Exn.to_string exn)
    }
  in
  let _, errors, runs, app = setup ~on_handler_exn:(Record_exn answer) () in
  let%bind () = call app in
  let%bind () = call app in
  print_s [%sexp (!runs : int), (!errors : string list)];
  [%expect
    {|
    (raised (exn (Failure "handler bug")))
    ((status 500)
     (body
      "{\"code\":\"internal_error\",\"detail\":\"(Failure \\\"handler bug\\\")\"}")
     (replayed (true)))
    (1 ())
    |}];
  return ()
;;

let%expect_test "Record_exn releases the key when the answer function raises" =
  let store, errors, runs, app =
    setup ~on_handler_exn:(Record_exn (fun _ -> failwith "no answer")) ()
  in
  let%bind () = call app in
  print_s [%sexp (Idempotency.Memory_store.length store.inner : int)];
  let%bind () = call app in
  print_s [%sexp (!runs : int), (!errors : string list)];
  [%expect
    {|
    (raised (exn (Failure "handler bug")))
    0
    ((status 200) (body done) (replayed ()))
    (2 ("(on_handler_exn (Failure \"no answer\"))"))
    |}];
  return ()
;;
