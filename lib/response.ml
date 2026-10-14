open! Core

type t =
  { status : Cohttp.Code.status_code
  ; headers : (string * string) list
  ; body : string
  }
[@@deriving sexp_of]

let json ?(status = `OK) ?(headers = []) json =
  { status
  ; headers = ("content-type", "application/json") :: headers
  ; body = Yojson.Safe.to_string json
  }
;;

let text
  ?(status = `OK)
  ?(headers = [])
  ?(content_type = "text/plain; charset=utf-8")
  body
  =
  { status; headers = ("content-type", content_type) :: headers; body }
;;

let empty ?(headers = []) status = { status; headers; body = "" }

let problem ?type_base (p : Problem.t) =
  { status = p.status
  ; headers = ("content-type", Problem.content_type) :: p.headers
  ; body = Yojson.Safe.to_string (Problem.to_yojson ?type_base p)
  }
;;

let status_code t = Cohttp.Code.code_of_status t.status

let header t name =
  List.find_map t.headers ~f:(fun (n, v) ->
    Option.some_if (String.Caseless.equal n name) v)
;;

let with_header t name value =
  { t with
    headers =
      List.filter t.headers ~f:(fun (n, _) -> not (String.Caseless.equal n name))
      @ [ name, value ]
  }
;;

let render_problem ~render req (p : Problem.t) =
  let resp = render req p in
  let missing =
    List.filter p.headers ~f:(fun (name, _) -> Option.is_none (header resp name))
  in
  { resp with headers = resp.headers @ missing }
;;
