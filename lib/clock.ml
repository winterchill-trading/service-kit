open! Core

type t = unit -> Time_ns.t

let wall = Time_ns.now
let of_fun f = f
let of_time_source source () = Async.Time_source.now source
let now t = t ()

module Manual = struct
  type t = { mutable now : Time_ns.t } [@@deriving sexp_of]

  let create ~start = { now = start }
  let clock t () = t.now
  let now t = t.now

  let advance t span =
    if Time_ns.Span.is_negative span
    then raise_s [%message "Clock.Manual.advance: negative span" (span : Time_ns.Span.t)];
    t.now <- Time_ns.add t.now span
  ;;

  let set t time =
    if Time_ns.( < ) time t.now
    then
      raise_s
        [%message
          "Clock.Manual.set: time moves backwards"
            (time : Time_ns.Alternate_sexp.t)
            ~now:(t.now : Time_ns.Alternate_sexp.t)];
    t.now <- time
  ;;
end
