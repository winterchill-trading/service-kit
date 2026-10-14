open! Core
open! Async

type handler = Request.t -> Response.t Deferred.t

module Middleware = struct
  type t = handler -> handler

  let compose ts h = List.fold_right ts ~init:h ~f:(fun m h -> m h)
end

module Path = struct
  type 'a var =
    { name : string
    ; parse : string -> 'a option
    }

  let custom name ~parse = { name; parse }
  let string name = custom name ~parse:Option.some

  let int name =
    custom name ~parse:(fun s ->
      let digits = Option.value (String.chop_prefix s ~prefix:"-") ~default:s in
      if (not (String.is_empty digits)) && String.for_all digits ~f:Char.is_digit
      then Int.of_string_opt s
      else None)
  ;;

  (* Built from the root outwards, so the last segment is the outermost constructor; ['r]
     is what remains of the handler type after the parameters so far. *)
  type ('fn, 'r) t =
    | Root : ('r, 'r) t
    | Lit : ('fn, 'r) t * string -> ('fn, 'r) t
    | Var : ('fn, 'a -> 'r) t * 'a var -> ('fn, 'r) t

  let root = Root
  let ( / ) p s = Lit (p, s)
  let ( /: ) p v = Var (p, v)

  let pattern p =
    let rec go : type fn r. (fn, r) t -> string list -> string list =
      fun p acc ->
      match p with
      | Root -> acc
      | Lit (p, s) -> go p (s :: acc)
      | Var (p, v) -> go p ((":" ^ v.name) :: acc)
    in
    "/" ^ String.concat ~sep:"/" (go p [])
  ;;

  (* Matches against the segments in reverse, peeling the outermost constructor. *)
  let rec matcher : type fn r. (fn, r) t -> string list -> (fn -> r) option =
    fun p rev ->
    match p, rev with
    | Root, [] -> Some Fn.id
    | Root, _ :: _ -> None
    | Lit _, [] | Var _, [] -> None
    | Lit (p, s), x :: rest -> if String.equal s x then matcher p rest else None
    | Var (p, v), x :: rest ->
      (match v.parse x with
       | None -> None
       | Some a -> Option.map (matcher p rest) ~f:(fun k f -> k f a))
  ;;

  let parse p segments fn = Option.map (matcher p (List.rev segments)) ~f:(fun k -> k fn)
end

module Route = struct
  type t =
    | T :
        { meth : Cohttp.Code.meth
        ; path : ('fn, handler) Path.t
        ; fn : 'fn
        ; middleware : Middleware.t
        }
        -> t

  let create ?(middleware = []) meth path fn =
    T { meth; path; fn; middleware = Middleware.compose middleware }
  ;;

  let get ?middleware = create ?middleware `GET
  let post ?middleware = create ?middleware `POST
  let put ?middleware = create ?middleware `PUT
  let patch ?middleware = create ?middleware `PATCH
  let delete ?middleware = create ?middleware `DELETE
  let describe (T r) = r.meth, Path.pattern r.path
end

type t =
  { routes : Route.t list
  ; middleware : Middleware.t
  ; render_problem : Request.t -> Problem.t -> Response.t
  }

let create ?(middleware = []) ?(render_problem = fun _ p -> Response.problem p) routes =
  let described =
    List.map routes ~f:(fun r ->
      let meth, pattern = Route.describe r in
      Cohttp.Code.string_of_method meth, pattern)
  in
  (match List.find_a_dup described ~compare:[%compare: string * string] with
   | Some (meth, pattern) ->
     raise_s [%message "Router.create: repeated route" meth pattern]
   | None -> ());
  { routes; middleware = Middleware.compose middleware; render_problem }
;;

let routes t =
  List.map t.routes ~f:(fun r ->
    let meth, pattern = Route.describe r in
    Cohttp.Code.string_of_method meth, pattern)
;;

(* [Cohttp.Code.meth] has no [equal]; compare by name. *)
let same_meth a b =
  String.equal (Cohttp.Code.string_of_method a) (Cohttp.Code.string_of_method b)
;;

let refuse t req p = return (Response.render_problem ~render:t.render_problem req p)

let handle t (req : Request.t) =
  let matching =
    List.filter_map t.routes ~f:(fun (Route.T r as route) ->
      Option.map (Path.parse r.path req.path r.fn) ~f:(fun h -> route, h))
  in
  let found = List.find matching ~f:(fun (Route.T r, _) -> same_meth r.meth req.meth) in
  match found with
  | Some (Route.T r, h) ->
    t.middleware (r.middleware h) { req with route = Path.pattern r.path }
  | None ->
    (match matching with
     | (Route.T first, _) :: _ ->
       let allowed = List.map matching ~f:(fun (Route.T r, _) -> r.meth) in
       t.middleware
         (fun req -> refuse t req (Problem.method_not_allowed ~allowed))
         { req with route = Path.pattern first.path }
     | [] ->
       t.middleware
         (fun req -> refuse t req (Problem.not_found "not_found"))
         { req with route = "unmatched" })
;;
