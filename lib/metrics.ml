open! Core

let content_type = "text/plain; version=0.0.4; charset=utf-8"

let valid_metric_name s =
  (not (String.is_empty s))
  && (Char.is_alpha s.[0] || Char.equal s.[0] '_' || Char.equal s.[0] ':')
  && String.for_all s ~f:(fun c ->
    Char.is_alphanum c || Char.equal c '_' || Char.equal c ':')
;;

let valid_label_name s =
  (not (String.is_empty s))
  && (Char.is_alpha s.[0] || Char.equal s.[0] '_')
  && (not (String.is_prefix s ~prefix:"__"))
  && String.for_all s ~f:(fun c -> Char.is_alphanum c || Char.equal c '_')
;;

(* Integers print without a fraction; anything else with the shortest digits that read
   back to the same float. *)
let format_float f =
  if Float.is_nan f
  then "NaN"
  else if Float.( = ) f Float.infinity
  then "+Inf"
  else if Float.( = ) f Float.neg_infinity
  then "-Inf"
  else if Float.is_integer f && Float.( < ) (Float.abs f) 1e15
  then sprintf "%.0f" f
  else (
    let short = sprintf "%.15g" f in
    if Float.( = ) (Float.of_string short) f then short else sprintf "%.17g" f)
;;

let escape_label_value s =
  String.concat_map s ~f:(function
    | '\\' -> "\\\\"
    | '"' -> "\\\""
    | '\n' -> "\\n"
    | c -> String.of_char c)
;;

let escape_help s =
  String.concat_map s ~f:(function
    | '\\' -> "\\\\"
    | '\n' -> "\\n"
    | c -> String.of_char c)
;;

let label_set names values =
  match names with
  | [] -> ""
  | _ ->
    "{"
    ^ String.concat
        ~sep:","
        (List.map2_exn names values ~f:(fun n v ->
           sprintf "%s=\"%s\"" n (escape_label_value v)))
    ^ "}"
;;

(* Series are keyed by their label values. *)
module Key = struct
  type t = string list [@@deriving compare, hash, sexp_of]
end

module Values = struct
  type t = (Key.t, float) Hashtbl.t
end

module Hist = struct
  type series =
    { counts : int array (* per bucket, not cumulative; last is +Inf *)
    ; mutable sum : float
    ; mutable count : int
    }

  type t =
    { buckets : float array
    ; series : (Key.t, series) Hashtbl.t
    }
end

type kind =
  | Counter of Values.t
  | Gauge of Values.t
  | Computed of (unit -> float)
  | Histogram of Hist.t

type metric =
  { name : string
  ; help : string option
  ; labels : string list
  ; kind : kind
  }

type t =
  { namespace : string option
  ; metrics : metric String.Table.t
  }

let create ?namespace () =
  Option.iter namespace ~f:(fun ns ->
    if not (valid_metric_name ns) then raise_s [%message "Metrics: invalid namespace" ns]);
  { namespace; metrics = String.Table.create () }
;;

let register t ?help ~labels name kind =
  let name =
    match t.namespace with
    | Some ns -> ns ^ "_" ^ name
    | None -> name
  in
  if not (valid_metric_name name) then raise_s [%message "Metrics: invalid name" name];
  List.iter labels ~f:(fun l ->
    if not (valid_label_name l) then raise_s [%message "Metrics: invalid label" name l]);
  if List.contains_dup labels ~compare:String.compare
  then raise_s [%message "Metrics: repeated label" name];
  if Hashtbl.mem t.metrics name then raise_s [%message "Metrics: already registered" name];
  let metric = { name; help; labels; kind } in
  Hashtbl.set t.metrics ~key:name ~data:metric;
  metric
;;

let check_arity (m : metric) values =
  if List.length values <> List.length m.labels
  then
    raise_s
      [%message
        "Metrics: wrong number of label values"
          ~name:m.name
          ~labels:(m.labels : string list)
          (values : string list)]
;;

module Counter = struct
  type t = metric * Values.t

  let create ?help ?(labels = []) registry name =
    let values = Hashtbl.create (module Key) in
    register registry ?help ~labels name (Counter values), values
  ;;

  let incr ?(by = 1.) ((m, values) : t) label_values =
    check_arity m label_values;
    if Float.is_negative by
    then raise_s [%message "Metrics.Counter.incr: negative" ~name:m.name];
    Hashtbl.update values label_values ~f:(fun v -> Option.value v ~default:0. +. by)
  ;;
end

module Gauge = struct
  type t = metric * Values.t

  let create ?help ?(labels = []) registry name =
    let values = Hashtbl.create (module Key) in
    register registry ?help ~labels name (Gauge values), values
  ;;

  let set ((m, values) : t) label_values v =
    check_arity m label_values;
    Hashtbl.set values ~key:label_values ~data:v
  ;;

  let add ((m, values) : t) label_values delta =
    check_arity m label_values;
    Hashtbl.update values label_values ~f:(fun v -> Option.value v ~default:0. +. delta)
  ;;

  let computed ?help registry name f =
    ignore (register registry ?help ~labels:[] name (Computed f) : metric)
  ;;
end

module Histogram = struct
  type t = metric * Hist.t

  let default_buckets = [ 0.005; 0.01; 0.025; 0.05; 0.1; 0.25; 0.5; 1.; 2.5; 5.; 10. ]

  let create ?help ?(labels = []) ?(buckets = default_buckets) registry name =
    if List.mem labels "le" ~equal:String.equal
    then raise_s [%message "Metrics.Histogram: le is reserved" name];
    let buckets =
      List.filter buckets ~f:Float.is_finite
      |> List.dedup_and_sort ~compare:Float.compare
      |> Array.of_list
    in
    let h = { Hist.buckets; series = Hashtbl.create (module Key) } in
    register registry ?help ~labels name (Histogram h), h
  ;;

  let observe ((m, h) : t) label_values v =
    check_arity m label_values;
    let s =
      Hashtbl.find_or_add h.series label_values ~default:(fun () ->
        { Hist.counts = Array.create ~len:(Array.length h.buckets + 1) 0
        ; sum = 0.
        ; count = 0
        })
    in
    (* The first bucket whose bound is at least [v]; a linear scan, since bucket lists are
       short (and [Array.binary_search] returns a local option under OxCaml). *)
    let i =
      let n = Array.length h.buckets in
      let rec go i = if i >= n || Float.( <= ) v h.buckets.(i) then i else go (i + 1) in
      go 0
    in
    s.counts.(i) <- s.counts.(i) + 1;
    s.sum <- s.sum +. v;
    s.count <- s.count + 1
  ;;
end

module Requests = struct
  type t =
    { total : Counter.t
    ; duration : Histogram.t
    }

  let create registry =
    { total =
        Counter.create
          registry
          "http_requests_total"
          ~help:"HTTP requests by route pattern, method and status"
          ~labels:[ "route"; "method"; "status" ]
    ; duration =
        Histogram.create
          registry
          "http_request_duration_seconds"
          ~help:"HTTP request duration by route pattern and method"
          ~labels:[ "route"; "method" ]
    }
  ;;

  let observe t ~route ~meth ~status ~duration =
    Counter.incr t.total [ route; meth; Int.to_string status ];
    Histogram.observe t.duration [ route; meth ] (Time_ns.Span.to_sec duration)
  ;;

  let middleware t ~clock : Router.Middleware.t =
    fun handler req ->
    let started = Clock.now clock in
    Async.Deferred.map (handler req) ~f:(fun resp ->
      observe
        t
        ~route:req.route
        ~meth:(Cohttp.Code.string_of_method req.meth)
        ~status:(Response.status_code resp)
        ~duration:(Time_ns.diff (Clock.now clock) started);
      resp)
  ;;
end

let sorted_series table =
  Hashtbl.to_alist table
  |> List.sort ~compare:(fun (a, _) (b, _) -> [%compare: string list] a b)
;;

let render t =
  let buf = Buffer.create 1024 in
  let line fmt =
    ksprintf
      (fun s ->
        Buffer.add_string buf s;
        Buffer.add_char buf '\n')
      fmt
  in
  Hashtbl.data t.metrics
  |> List.sort ~compare:(fun a b -> String.compare a.name b.name)
  |> List.iter ~f:(fun m ->
    Option.iter m.help ~f:(fun h -> line "# HELP %s %s" m.name (escape_help h));
    let type_ =
      match m.kind with
      | Counter _ -> "counter"
      | Gauge _ | Computed _ -> "gauge"
      | Histogram _ -> "histogram"
    in
    line "# TYPE %s %s" m.name type_;
    match m.kind with
    | Counter values | Gauge values ->
      List.iter (sorted_series values) ~f:(fun (lv, v) ->
        line "%s%s %s" m.name (label_set m.labels lv) (format_float v))
    | Computed f -> line "%s %s" m.name (format_float (f ()))
    | Histogram h ->
      List.iter (sorted_series h.series) ~f:(fun (lv, s) ->
        let cumulative = ref 0 in
        Array.iteri s.counts ~f:(fun i c ->
          cumulative := !cumulative + c;
          let le =
            if i < Array.length h.buckets then format_float h.buckets.(i) else "+Inf"
          in
          line
            "%s_bucket%s %d"
            m.name
            (label_set (m.labels @ [ "le" ]) (lv @ [ le ]))
            !cumulative);
        line "%s_sum%s %s" m.name (label_set m.labels lv) (format_float s.sum);
        line "%s_count%s %d" m.name (label_set m.labels lv) s.count));
  Buffer.contents buf
;;

let route t =
  Router.Route.get
    Router.Path.(root / "metrics")
    (fun _ -> Async.return (Response.text ~content_type (render t)))
;;
