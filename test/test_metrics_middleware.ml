open! Core
open! Async
open Service_kit

let%expect_test "requests are counted by route pattern" =
  let manual = Clock.Manual.create ~start:Time_ns.epoch in
  let registry = Metrics.create () in
  let requests = Metrics.Requests.create registry in
  let router =
    Router.create
      ~middleware:
        [ Metrics.Requests.middleware requests ~clock:(Clock.Manual.clock manual) ]
      [ Metrics.route registry
      ; Router.Route.get
          Router.Path.(root / "accounts" /: string "id")
          (fun _ _ ->
            Clock.Manual.advance manual (Time_ns.Span.of_ms 30.);
            return (Response.text "ok"))
      ]
  in
  let get path = Router.handle router (Request.create `GET path) in
  let%bind (_ : Response.t) = get "/accounts/1" in
  let%bind (_ : Response.t) = get "/accounts/2" in
  let%bind (_ : Response.t) = get "/nope" in
  let%bind resp = get "/metrics" in
  print_s [%sexp (Response.header resp "content-type" : string option)];
  String.split_lines resp.body
  |> List.filter ~f:(fun l ->
    String.is_prefix l ~prefix:"http_requests_total"
    || String.is_substring l ~substring:"le=\"0.05\"")
  |> List.iter ~f:print_endline;
  [%expect
    {|
    ("text/plain; version=0.0.4; charset=utf-8")
    http_request_duration_seconds_bucket{route="/accounts/:id",method="GET",le="0.05"} 2
    http_request_duration_seconds_bucket{route="unmatched",method="GET",le="0.05"} 1
    http_requests_total{route="/accounts/:id",method="GET",status="200"} 2
    http_requests_total{route="unmatched",method="GET",status="404"} 1
    |}];
  return ()
;;
