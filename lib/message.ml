open! Core

module Stable = struct
  module V1 = struct
    type t =
      { code : string
      ; params : (string * Param.Stable.V1.t) list
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { code : string
  ; params : (string * Param.t) list
  }
[@@deriving sexp, compare, equal]

let check_names ~what code names =
  let open Or_error.Let_syntax in
  let%bind () =
    if Param.is_valid_name code
    then Ok ()
    else Or_error.error_s [%message (what ^ ": invalid code") (code : string)]
  in
  let%bind () =
    List.find names ~f:(Fn.non Param.is_valid_name)
    |> Option.value_map ~default:(Ok ()) ~f:(fun name ->
      Or_error.error_s
        [%message (what ^ ": invalid param name") (code : string) (name : string)])
  in
  match List.find_a_dup names ~compare:String.compare with
  | Some name ->
    Or_error.error_s
      [%message (what ^ ": repeated param") (code : string) (name : string)]
  | None -> Ok ()
;;

let create ?(params = []) code =
  check_names ~what:"Message.create" code (List.map params ~f:fst) |> ok_exn;
  { code; params }
;;

let to_yojson t : Yojson.Safe.t =
  `Assoc
    (("code", `String t.code)
     ::
     (if List.is_empty t.params then [] else [ "params", Param.params_to_yojson t.params ])
    )
;;

module Template = struct
  type piece =
    | Text of string
    | Hole of string

  (* [{name}] is a hole, [{{] and [}}] are literal braces, anything else unbalanced is an
     error. *)
  let parse s =
    let n = String.length s in
    let buf = Buffer.create n in
    let pieces = ref [] in
    let flush () =
      if Buffer.length buf > 0
      then (
        pieces := Text (Buffer.contents buf) :: !pieces;
        Buffer.clear buf)
    in
    let rec go i =
      if i >= n
      then (
        flush ();
        Ok (List.rev !pieces))
      else (
        match s.[i] with
        | '{' when i + 1 < n && Char.equal s.[i + 1] '{' ->
          Buffer.add_char buf '{';
          go (i + 2)
        | '}' when i + 1 < n && Char.equal s.[i + 1] '}' ->
          Buffer.add_char buf '}';
          go (i + 2)
        | '{' ->
          (match String.index_from s (i + 1) '}' with
           | None -> Or_error.error_s [%message "unclosed brace" ~template:s]
           | Some j ->
             flush ();
             pieces := Hole (String.sub s ~pos:(i + 1) ~len:(j - i - 1)) :: !pieces;
             go (j + 1))
        | '}' -> Or_error.error_s [%message "unbalanced closing brace" ~template:s]
        | c ->
          Buffer.add_char buf c;
          go (i + 1))
    in
    go 0
  ;;
end

let rec render_param (p : Param.t) =
  match p with
  | String s | Decimal s -> s
  | Int i -> Int.to_string i
  | Bool b -> Bool.to_string b
  | Time t -> Time_ns.to_string_iso8601_basic t ~zone:Time_float.Zone.utc
  | List l -> String.concat ~sep:", " (List.map l ~f:render_param)
;;

module Catalogue = struct
  module Entry = struct
    type t =
      { params : (string * Param.Kind.t) list
      ; templates : (string * string) list
      }
    [@@deriving sexp, compare, equal]
  end

  type message = t
  type nonrec t = Entry.t String.Map.t [@@deriving sexp_of]

  let check_entry code (entry : Entry.t) =
    let open Or_error.Let_syntax in
    let%bind () =
      check_names ~what:"Message.Catalogue" code (List.map entry.params ~f:fst)
    in
    let%bind () =
      match List.find_a_dup (List.map entry.templates ~f:fst) ~compare:String.compare with
      | Some locale ->
        Or_error.error_s
          [%message
            "Message.Catalogue: repeated locale" (code : string) (locale : string)]
      | None -> Ok ()
    in
    List.map entry.templates ~f:(fun (locale, template) ->
      let%bind pieces =
        Template.parse template
        |> Or_error.tag_s
             ~tag:[%message "Message.Catalogue" (code : string) (locale : string)]
      in
      List.filter_map pieces ~f:(function
        | Text _ -> None
        | Hole name ->
          if List.Assoc.mem entry.params name ~equal:String.equal
          then None
          else
            Some
              (Or_error.error_s
                 [%message
                   "Message.Catalogue: template names an undeclared param"
                     (code : string)
                     (locale : string)
                     (name : string)]))
      |> Or_error.all_unit)
    |> Or_error.all_unit
  ;;

  let create entries =
    let open Or_error.Let_syntax in
    let%bind map =
      match String.Map.of_alist entries with
      | `Ok map -> Ok map
      | `Duplicate_key code ->
        Or_error.error_s [%message "Message.Catalogue: repeated code" (code : string)]
    in
    let%map () =
      Map.to_alist map
      |> List.map ~f:(fun (code, e) -> check_entry code e)
      |> Or_error.all_unit
    in
    map
  ;;

  let find t code = Map.find t code
  let codes t = Map.keys t

  let validate_params t ~code ~params =
    match find t code with
    | None -> Or_error.error_s [%message "unknown code" (code : string)]
    | Some (entry : Entry.t) ->
      let declared = List.map entry.params ~f:fst |> String.Set.of_list in
      let given = List.map params ~f:fst |> String.Set.of_list in
      let missing = Set.diff declared given |> Set.to_list in
      let extra = Set.diff given declared |> Set.to_list in
      if not (List.is_empty missing && List.is_empty extra)
      then
        Or_error.error_s
          [%message
            "params do not match the catalogue"
              (code : string)
              (missing : string list)
              (extra : string list)]
      else
        List.filter_map params ~f:(fun (name, value) ->
          let kind = List.Assoc.find_exn entry.params name ~equal:String.equal in
          if Param.has_kind value kind
          then None
          else
            Some
              (Or_error.error_s
                 [%message
                   "param has the wrong kind"
                     (code : string)
                     (name : string)
                     ~expected:(Param.Kind.to_string kind)]))
        |> Or_error.all_unit
  ;;

  let validate t (m : message) = validate_params t ~code:m.code ~params:m.params
  let validate_problem t (p : Problem.t) = validate_params t ~code:p.code ~params:p.params

  let check ?(exhaustive = true) t ~codes ~locales =
    let errors =
      List.concat
        [ List.filter_map codes ~f:(fun code ->
            if Map.mem t code then None else Some [%message "no entry" (code : string)])
        ; Map.to_alist t
          |> List.concat_map ~f:(fun (code, (entry : Entry.t)) ->
            List.filter_map locales ~f:(fun locale ->
              if List.Assoc.mem entry.templates locale ~equal:String.equal
              then None
              else Some [%message "no template" (code : string) (locale : string)]))
        ; (if exhaustive
           then (
             let wanted = String.Set.of_list codes in
             Map.keys t
             |> List.filter_map ~f:(fun code ->
               if Set.mem wanted code
               then None
               else Some [%message "unused entry" (code : string)]))
           else [])
        ]
    in
    if List.is_empty errors
    then Ok ()
    else Or_error.error_s [%message "Message.Catalogue.check" ~_:(errors : Sexp.t list)]
  ;;

  let render ?fallback t ~locale (m : message) =
    let open Or_error.Let_syntax in
    let%bind () = validate t m in
    let (entry : Entry.t) = Map.find_exn t m.code in
    let template =
      match List.Assoc.find entry.templates locale ~equal:String.equal with
      | Some _ as found -> found
      | None ->
        Option.bind fallback ~f:(fun l ->
          List.Assoc.find entry.templates l ~equal:String.equal)
    in
    match template with
    | None ->
      Or_error.error_s [%message "no template" ~code:(m.code : string) (locale : string)]
    | Some template ->
      (* Templates were parsed when the catalogue was built. *)
      let pieces = Template.parse template |> ok_exn in
      Ok
        (List.map pieces ~f:(function
           | Text s -> s
           | Hole name ->
             render_param (List.Assoc.find_exn m.params name ~equal:String.equal))
         |> String.concat)
  ;;

  let to_yojson t : Yojson.Safe.t =
    `Assoc
      (Map.to_alist t
       |> List.map ~f:(fun (code, (entry : Entry.t)) ->
         ( code
         , `Assoc
             [ ( "params"
               , `Assoc
                   (List.map entry.params ~f:(fun (n, k) ->
                      n, `String (Param.Kind.to_string k))) )
             ; ( "templates"
               , `Assoc (List.map entry.templates ~f:(fun (l, s) -> l, `String s)) )
             ] )))
  ;;
end
