open! Core
open Service_kit

let t0 = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"
let at ms = Time_ns.add t0 (Time_ns.Span.of_int_ms ms)

let%expect_test "burst, then one token per refill" =
  let limits = Rate_limit.create (Rate_limit.Config.per_minute ~burst:3 60) in
  let check ms =
    print_s
      [%sexp
        (ms : int)
        , (Rate_limit.check limits ~key:"10.0.0.1" ~now:(at ms) : Rate_limit.Decision.t)]
  in
  List.iter [ 0; 0; 0; 0; 400; 1000; 1000; 5000 ] ~f:check;
  print_s
    [%sexp (Rate_limit.check limits ~key:"10.0.0.2" ~now:(at 0) : Rate_limit.Decision.t)];
  [%expect
    {|
    (0 (Allowed (remaining 2)))
    (0 (Allowed (remaining 1)))
    (0 (Allowed (remaining 0)))
    (0 (Denied (retry_after 1s)))
    (400 (Denied (retry_after 600ms)))
    (1000 (Allowed (remaining 0)))
    (1000 (Denied (retry_after 1s)))
    (5000 (Allowed (remaining 2)))
    (Allowed (remaining 2))
    |}]
;;

let%expect_test "cost" =
  let c = Rate_limit.Config.create ~burst:5 ~refill_every:(Time_ns.Span.of_sec 1.) in
  let show (d, _) = print_s [%sexp (d : Rate_limit.Decision.t)] in
  show (Rate_limit.Bucket.take ~cost:4 c Rate_limit.Bucket.full ~now:t0);
  show (Rate_limit.Bucket.take ~cost:6 c Rate_limit.Bucket.full ~now:t0);
  let _, b = Rate_limit.Bucket.take ~cost:4 c Rate_limit.Bucket.full ~now:t0 in
  show (Rate_limit.Bucket.take ~cost:3 c b ~now:t0);
  print_s [%sexp (Rate_limit.Bucket.available c b ~now:(at 1500) : int)];
  [%expect
    {|
    (Allowed (remaining 1))
    (Denied (retry_after 5s))
    (Denied (retry_after 2s))
    2
    |}]
;;

let%expect_test "full buckets are dropped past max_keys" =
  let limits =
    Rate_limit.create
      ~max_keys:2
      (Rate_limit.Config.create ~burst:1 ~refill_every:(Time_ns.Span.of_sec 1.))
  in
  ignore (Rate_limit.check limits ~key:"a" ~now:(at 0) : Rate_limit.Decision.t);
  ignore (Rate_limit.check limits ~key:"b" ~now:(at 0) : Rate_limit.Decision.t);
  ignore (Rate_limit.check limits ~key:"c" ~now:(at 5000) : Rate_limit.Decision.t);
  print_s [%sexp (Rate_limit.tracked limits : int)];
  [%expect {| 1 |}]
;;

(* Over any run of requests, no more than [burst] plus the tokens refilled since the first
   request are granted, and a denied request succeeds once [retry_after] has passed. *)
let%test_unit "never grants more than burst plus refill" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind burst = Int.gen_incl 1 10
    and every_ms = Int.gen_incl 1 1000
    and gaps = List.gen_with_length 50 (Int.gen_incl 0 300)
    and costs = List.gen_with_length 50 (Int.gen_incl 1 3) in
    return (burst, every_ms, List.zip_exn gaps costs)
  in
  Quickcheck.test
    gen
    ~sexp_of:[%sexp_of: int * int * (int * int) list]
    ~f:(fun (burst, every_ms, requests) ->
      let c =
        Rate_limit.Config.create ~burst ~refill_every:(Time_ns.Span.of_int_ms every_ms)
      in
      let bucket = ref Rate_limit.Bucket.full in
      let now = ref 0 in
      let granted = ref 0 in
      List.iter requests ~f:(fun (gap, cost) ->
        now := !now + gap;
        let decision, b = Rate_limit.Bucket.take ~cost c !bucket ~now:(at !now) in
        (match decision with
         | Allowed _ -> granted := !granted + cost
         | Denied { retry_after } ->
           let later = Time_ns.add (at !now) retry_after in
           (match Rate_limit.Bucket.take ~cost c !bucket ~now:later with
            | Allowed _, _ -> ()
            | Denied _, _ when cost > burst -> ()
            | Denied _, _ -> failwith "denied again after retry_after"));
        bucket := b);
      assert (!granted <= burst + (!now / every_ms)))
;;
