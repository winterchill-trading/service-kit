open! Core
open Service_kit

let%expect_test "exposition" =
  let registry = Metrics.create ~namespace:"app" () in
  let logins =
    Metrics.Counter.create
      registry
      "logins_total"
      ~help:"Sign-ins\nby result"
      ~labels:[ "result" ]
  in
  Metrics.Counter.incr logins [ "ok" ];
  Metrics.Counter.incr logins [ "ok" ];
  Metrics.Counter.incr logins [ "bad \"code\"\\" ] ~by:0.5;
  let queue = Metrics.Gauge.create registry "queue_length" in
  Metrics.Gauge.set queue [] 3.;
  Metrics.Gauge.add queue [] (-1.);
  Metrics.Gauge.computed registry "uptime_seconds" (fun () -> 12.25);
  let latency =
    Metrics.Histogram.create
      registry
      "latency_seconds"
      ~labels:[ "op" ]
      ~buckets:[ 0.1; 1.; 0.1 ]
  in
  List.iter [ 0.05; 0.1; 0.7; 3. ] ~f:(Metrics.Histogram.observe latency [ "read" ]);
  print_string (Metrics.render registry);
  [%expect
    {|
    # TYPE app_latency_seconds histogram
    app_latency_seconds_bucket{op="read",le="0.1"} 2
    app_latency_seconds_bucket{op="read",le="1"} 3
    app_latency_seconds_bucket{op="read",le="+Inf"} 4
    app_latency_seconds_sum{op="read"} 3.85
    app_latency_seconds_count{op="read"} 4
    # HELP app_logins_total Sign-ins\nby result
    # TYPE app_logins_total counter
    app_logins_total{result="bad \"code\"\\"} 0.5
    app_logins_total{result="ok"} 2
    # TYPE app_queue_length gauge
    app_queue_length 2
    # TYPE app_uptime_seconds gauge
    app_uptime_seconds 12.25
    |}]
;;

let%expect_test "requests" =
  let registry = Metrics.create () in
  let requests = Metrics.Requests.create registry in
  Metrics.Requests.observe
    requests
    ~route:"/accounts/:id"
    ~meth:"GET"
    ~status:200
    ~duration:(Time_ns.Span.of_ms 12.);
  print_string (Metrics.render registry);
  [%expect
    {|
    # HELP http_request_duration_seconds HTTP request duration by route pattern and method
    # TYPE http_request_duration_seconds histogram
    http_request_duration_seconds_bucket{route="/accounts/:id",method="GET",le="0.005"} 0
    http_request_duration_seconds_bucket{route="/accounts/:id",method="GET",le="0.01"} 0
    http_request_duration_seconds_bucket{route="/accounts/:id",method="GET",le="0.025"} 1
    http_request_duration_seconds_bucket{route="/accounts/:id",method="GET",le="0.05"} 1
    http_request_duration_seconds_bucket{route="/accounts/:id",method="GET",le="0.1"} 1
    http_request_duration_seconds_bucket{route="/accounts/:id",method="GET",le="0.25"} 1
    http_request_duration_seconds_bucket{route="/accounts/:id",method="GET",le="0.5"} 1
    http_request_duration_seconds_bucket{route="/accounts/:id",method="GET",le="1"} 1
    http_request_duration_seconds_bucket{route="/accounts/:id",method="GET",le="2.5"} 1
    http_request_duration_seconds_bucket{route="/accounts/:id",method="GET",le="5"} 1
    http_request_duration_seconds_bucket{route="/accounts/:id",method="GET",le="10"} 1
    http_request_duration_seconds_bucket{route="/accounts/:id",method="GET",le="+Inf"} 1
    http_request_duration_seconds_sum{route="/accounts/:id",method="GET"} 0.012
    http_request_duration_seconds_count{route="/accounts/:id",method="GET"} 1
    # HELP http_requests_total HTTP requests by route pattern, method and status
    # TYPE http_requests_total counter
    http_requests_total{route="/accounts/:id",method="GET",status="200"} 1
    |}]
;;

let%expect_test "misuse raises" =
  let registry = Metrics.create () in
  let c = Metrics.Counter.create registry "c_total" ~labels:[ "a" ] in
  Expect_test_helpers_core.require_does_raise (fun () -> Metrics.Counter.incr c []);
  Expect_test_helpers_core.require_does_raise (fun () ->
    Metrics.Counter.incr c [ "x" ] ~by:(-1.));
  Expect_test_helpers_core.require_does_raise (fun () ->
    Metrics.Counter.create registry "c_total");
  Expect_test_helpers_core.require_does_raise (fun () ->
    Metrics.Counter.create registry "1bad");
  Expect_test_helpers_core.require_does_raise (fun () ->
    Metrics.Counter.create registry "ok_total" ~labels:[ "__reserved" ]);
  Expect_test_helpers_core.require_does_raise (fun () ->
    Metrics.Histogram.create registry "h" ~labels:[ "le" ]);
  [%expect
    {|
    ("Metrics: wrong number of label values"
      (name c_total)
      (labels (a))
      (values ()))
    ("Metrics.Counter.incr: negative" (name c_total))
    ("Metrics: already registered" c_total)
    ("Metrics: invalid name" 1bad)
    ("Metrics: invalid label" ok_total __reserved)
    ("Metrics.Histogram: le is reserved" h)
    |}]
;;

let%test_unit "histogram buckets are cumulative and end at the count" =
  Quickcheck.test
    (List.quickcheck_generator (Float.gen_incl (-1.) 20.))
    ~sexp_of:[%sexp_of: float list]
    ~f:(fun observations ->
      let registry = Metrics.create () in
      let h = Metrics.Histogram.create registry "h" in
      List.iter observations ~f:(Metrics.Histogram.observe h []);
      let counts =
        String.split_lines (Metrics.render registry)
        |> List.filter ~f:(String.is_prefix ~prefix:"h_bucket")
        |> List.map ~f:(fun l -> Int.of_string (List.last_exn (String.split l ~on:' ')))
      in
      assert (List.is_sorted counts ~compare:Int.compare);
      (* No observations means no series at all. *)
      [%test_result: int]
        (Option.value (List.last counts) ~default:0)
        ~expect:(List.length observations))
;;
