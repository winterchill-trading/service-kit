open! Core

module Config = struct
  type t =
    { burst : int
    ; refill_every : Time_ns.Span.t
    }
  [@@deriving sexp, compare, equal]

  let create ~burst ~refill_every =
    if burst < 1 || not (Time_ns.Span.is_positive refill_every)
    then
      raise_s
        [%message
          "Rate_limit.Config.create" (burst : int) (refill_every : Time_ns.Span.t)];
    { burst; refill_every }
  ;;

  let per_minute ~burst n =
    if n < 1 then raise_s [%message "Rate_limit.Config.per_minute" (n : int)];
    create ~burst ~refill_every:(Time_ns.Span.of_int_ns (60_000_000_000 / n))
  ;;
end

module Decision = struct
  type t =
    | Allowed of { remaining : int }
    | Denied of { retry_after : Time_ns.Span.t }
  [@@deriving sexp, compare, equal]
end

module Bucket = struct
  (* The theoretical arrival time: the instant at which the bucket would be full again if
     nothing else were taken. Its distance from [now], in units of [refill_every], is the
     number of tokens missing. [Time_ns.epoch] reads as full for any realistic [now]. *)
  type t = Time_ns.Alternate_sexp.t [@@deriving sexp, compare, equal]

  let full = Time_ns.epoch
  let ns = Time_ns.Span.to_int_ns
  let capacity_ns (c : Config.t) = c.burst * ns c.refill_every

  let available (c : Config.t) t ~now =
    let missing_ns = Int.max 0 (ns (Time_ns.diff t now)) in
    (capacity_ns c - missing_ns) / ns c.refill_every
  ;;

  let take ?(cost = 1) (c : Config.t) t ~now =
    if cost < 1 then raise_s [%message "Rate_limit.Bucket.take: cost < 1" (cost : int)];
    let start = Time_ns.max t now in
    let next = Time_ns.add start (Time_ns.Span.of_int_ns (cost * ns c.refill_every)) in
    let missing_ns = ns (Time_ns.diff next now) in
    if missing_ns <= capacity_ns c
    then
      ( Decision.Allowed { remaining = (capacity_ns c - missing_ns) / ns c.refill_every }
      , next )
    else if cost > c.burst
    then Denied { retry_after = Time_ns.Span.of_int_ns (capacity_ns c) }, t
    else Denied { retry_after = Time_ns.Span.of_int_ns (missing_ns - capacity_ns c) }, t
  ;;
end

type t =
  { config : Config.t
  ; max_keys : int
  ; buckets : Bucket.t String.Table.t
  }

let create ?(max_keys = 10_000) config =
  { config; max_keys; buckets = String.Table.create () }
;;

let config t = t.config
let tracked t = Hashtbl.length t.buckets

let check ?cost t ~key ~now =
  let bucket = Hashtbl.find t.buckets key |> Option.value ~default:Bucket.full in
  let decision, bucket = Bucket.take ?cost t.config bucket ~now in
  (match decision with
   | Denied _ -> ()
   | Allowed _ ->
     Hashtbl.set t.buckets ~key ~data:bucket;
     if Hashtbl.length t.buckets > t.max_keys
     then Hashtbl.filter_inplace t.buckets ~f:(fun b -> Time_ns.( > ) b now));
  decision
;;
