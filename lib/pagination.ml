open! Core

let version = '\001'
let mac_length = 16

module Cursor (Position : sig
    type t [@@deriving bin_io, sexp_of]
  end) =
struct
  let mac ~key body = String.prefix (Codec.hmac_sha256 ~key body) mac_length

  let encode ?key position =
    let body = String.of_char version ^ Binable.to_string (module Position) position in
    Codec.base64url_encode
      (match key with
       | None -> body
       | Some key -> body ^ mac ~key body)
  ;;

  let invalid = Error (Problem.bad_request "invalid_cursor")

  let decode ?key cursor =
    match Codec.base64url_decode cursor with
    | None -> invalid
    | Some bytes ->
      let body =
        match key with
        | None -> Some bytes
        | Some key ->
          let n = String.length bytes - mac_length in
          if n < 1
          then None
          else (
            let body = String.prefix bytes n in
            if Codec.equal_constant_time (String.drop_prefix bytes n) (mac ~key body)
            then Some body
            else None)
      in
      (match body with
       | Some body when String.length body > 1 && Char.equal body.[0] version ->
         (match
            Option.try_with (fun () ->
              let buf = Bigstring.of_string body in
              let pos_ref = ref 1 in
              let v = Position.bin_read_t buf ~pos_ref in
              (* Trailing bytes mean the cursor was not made by [encode]. *)
              if !pos_ref <> String.length body then failwith "trailing bytes";
              v)
          with
          | Some v -> Ok v
          | None -> invalid)
       | _ -> invalid)
  ;;
end

let limit ?(default = 50) ?(max = 200) = function
  | None -> Ok default
  | Some s ->
    (match Int.of_string_opt (String.strip s) with
     | Some n when n >= 1 && n <= max -> Ok n
     | _ -> Error (Problem.bad_request "invalid_limit" ~params:[ "max", Int max ]))
;;

module Page = struct
  type 'a t =
    { items : 'a list
    ; next_cursor : string option
    }
  [@@deriving sexp_of, compare, equal]

  let of_overfetch items ~limit ~cursor =
    if List.length items > limit
    then (
      let items = List.take items limit in
      { items; next_cursor = Option.map (List.last items) ~f:cursor })
    else { items; next_cursor = None }
  ;;

  let to_yojson f t : Yojson.Safe.t =
    `Assoc
      [ "items", `List (List.map t.items ~f)
      ; ( "next_cursor"
        , Option.value_map t.next_cursor ~default:`Null ~f:(fun c -> `String c) )
      ]
  ;;
end
