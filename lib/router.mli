(** Typed routes, method dispatch and middleware.

    A path is built from the root with [/] for a literal segment and [/:] for a parameter;
    the handler's type follows from the parameters:

    {[
      let routes =
        Router.
          [ Route.get Path.(root / "accounts") list_accounts
            (* list_accounts : Request.t -> Response.t Deferred.t *)
          ; Route.get Path.(root / "accounts" /: string "id") get_account
            (* get_account : string -> Request.t -> Response.t Deferred.t *)
          ; Route.post
              Path.(root / "accounts" /: string "id" / "cards" /: int "n")
              ~middleware:[ require_step_up ]
              card_action
            (* card_action : string -> int -> Request.t -> Response.t Deferred.t *)
          ]
      in
      let router = Router.create ~middleware:[ request_log; cors ] routes in
      Server.start ~port (Router.handle router)
    ]}

    Routes are tried in order; the first whose path matches wins. A path that matches some
    route but not with this method is answered 405 with [Allow]; no match is 404
    [not_found]. Global middleware runs around every request, matched or not, after the
    router has set {!Request.route} to the route pattern, so logs and metrics never see
    raw paths. Route middleware runs inside it, for that route only. *)

open! Core
open! Async

(** Handlers answer a request asynchronously. *)
type handler = Request.t -> Response.t Deferred.t

(** Handler wrappers. *)
module Middleware : sig
  (** A middleware wraps a handler: it can change the request, answer on its own, or
      change the response. *)
  type t = handler -> handler

  (** [compose [a; b; c] h] is [a (b (c h))]: [a] sees the request first and the response
      last. *)
  val compose : t list -> t
end

(** Typed paths. *)
module Path : sig
  (** A path parameter: how to read one segment. *)
  type 'a var

  (** Any segment. *)
  val string : string -> string var

  (** A segment that is a decimal integer (with optional [-]). *)
  val int : string -> int var

  (** A segment [parse] accepts; [name] appears in the route pattern as [:name]. *)
  val custom : string -> parse:(string -> 'a option) -> 'a var

  (** A path whose parameters, applied to a handler of type ['fn], leave ['r]. *)
  type ('fn, 'r) t

  (** [/]. *)
  val root : ('r, 'r) t

  (** A literal segment. *)
  val ( / ) : ('fn, 'r) t -> string -> ('fn, 'r) t

  (** A parameter segment. *)
  val ( /: ) : ('fn, 'a -> 'r) t -> 'a var -> ('fn, 'r) t

  (** The pattern, e.g. [/accounts/:id/cards]. *)
  val pattern : (_, _) t -> string

  (** Applies [fn] to the parameters of [segments], if they match. *)
  val parse : ('fn, 'r) t -> string list -> 'fn -> 'r option
end

(** One method and path with its handler. *)
module Route : sig
  type t

  (** A route for [meth] at [path] with handler [fn]. *)
  val create
    :  ?middleware:Middleware.t list
    -> Cohttp.Code.meth
    -> ('fn, handler) Path.t
    -> 'fn
    -> t

  (** [create `GET]. *)
  val get : ?middleware:Middleware.t list -> ('fn, handler) Path.t -> 'fn -> t

  (** [create `POST]. *)
  val post : ?middleware:Middleware.t list -> ('fn, handler) Path.t -> 'fn -> t

  (** [create `PUT]. *)
  val put : ?middleware:Middleware.t list -> ('fn, handler) Path.t -> 'fn -> t

  (** [create `PATCH]. *)
  val patch : ?middleware:Middleware.t list -> ('fn, handler) Path.t -> 'fn -> t

  (** [create `DELETE]. *)
  val delete : ?middleware:Middleware.t list -> ('fn, handler) Path.t -> 'fn -> t

  (** The method and pattern. *)
  val describe : t -> Cohttp.Code.meth * string
end

(** A list of routes with global middleware. *)
type t

(** A router. Raises when two routes share a method and a pattern. [render_problem]
    (default: {!Response.problem}) renders the router's own 404 and 405 answers through
    {!Response.render_problem}, so they can use the service's error body; the [Allow]
    header of a 405 is kept. *)
val create
  :  ?middleware:Middleware.t list
  -> ?render_problem:(Request.t -> Problem.t -> Response.t)
  -> Route.t list
  -> t

(** Every route's method and pattern, in order. *)
val routes : t -> (string * string) list

(** Dispatches a request through the global middleware to its route. *)
val handle : t -> handler
