open! Core
open! Async
open Service_kit

module Purpose = struct
  type t = Send_payment [@@deriving sexp, compare, equal]

  let to_string Send_payment = "send_payment"

  let of_string = function
    | "send_payment" -> Some Send_payment
    | _ -> None
  ;;
end

module Grants = Step_up.Make (Purpose) (Step_up.Memory_spent)

let%expect_test "session then step-up" =
  let clock =
    Clock.Manual.clock
      (Clock.Manual.create
         ~start:(Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"))
  in
  let sessions =
    Session.create
      ~keyring:(Session.Keyring.create ~current:("k1", String.make 32 's') ())
      ~clock
      ()
  in
  let grants =
    Grants.create
      ~key:(String.make 32 'g')
      ~spent:(Step_up.Memory_spent.create ~clock ())
      ~clock
      ()
  in
  let session_id req = Option.map (Session.claims req) ~f:(fun c -> c.session_id) in
  let router =
    Router.create
      Router.
        [ Route.get
            Path.(root / "me")
            ~middleware:[ Session.middleware sessions ]
            (fun req ->
              return (Response.text (Option.value_exn (Session.claims req)).subject))
        ; Route.get
            Path.(root / "hello")
            ~middleware:[ Session.middleware ~optional:true sessions ]
            (fun req ->
              return
                (Response.text
                   (Option.value_map
                      (Session.claims req)
                      ~default:"anonymous"
                      ~f:(fun c -> c.subject))))
        ; Route.post
            Path.(root / "payments")
            ~middleware:
              [ Session.middleware sessions
              ; Grants.require grants ~purpose:Send_payment ~session_id
              ]
            (fun _ -> return (Response.text "paid"))
        ]
  in
  let token, claims = Session.issue sessions ~subject:"user-7" in
  let call ?(headers = []) meth path =
    let%map r = Router.handle router (Request.create meth path ~headers) in
    printf
      "%d %s%s\n"
      (Response.status_code r)
      r.body
      (Option.value_map (Response.header r "www-authenticate") ~default:"" ~f:(fun v ->
         " [www-authenticate: " ^ v ^ "]"))
  in
  let bearer = "authorization", "Bearer " ^ token in
  let%bind () = call `GET "/me" in
  let%bind () = call `GET "/me" ~headers:[ "authorization", "Bearer nope" ] in
  let%bind () = call `GET "/me" ~headers:[ bearer ] in
  let%bind () = call `GET "/hello" in
  let%bind () = call `GET "/hello" ~headers:[ bearer ] in
  let%bind () = call `POST "/payments" ~headers:[ bearer ] in
  let grant, _ =
    Grants.issue grants ~session_id:claims.session_id ~purpose:Send_payment
  in
  let%bind () = call `POST "/payments" ~headers:[ bearer; Grants.header_name, grant ] in
  let%bind () = call `POST "/payments" ~headers:[ bearer; Grants.header_name, grant ] in
  [%expect
    {|
    401 {"type":"about:blank","title":"Unauthorized","status":401,"code":"session_required"} [www-authenticate: Bearer]
    401 {"type":"about:blank","title":"Unauthorized","status":401,"code":"session_invalid"} [www-authenticate: Bearer]
    200 user-7
    200 anonymous
    200 user-7
    403 {"type":"about:blank","title":"Forbidden","status":403,"code":"step_up_required","params":{"purpose":"send_payment"}}
    200 paid
    403 {"type":"about:blank","title":"Forbidden","status":403,"code":"step_up_required","params":{"purpose":"send_payment"}}
    |}];
  return ()
;;
