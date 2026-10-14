open! Core

let base64url_encode s =
  Cryptokit.transform_string (Cryptokit.Base64.encode_compact ()) s
  |> String.map ~f:(function
    | '+' -> '-'
    | '/' -> '_'
    | c -> c)
;;

let base64url_decode s =
  let valid =
    String.for_all s ~f:(fun c ->
      Char.is_alphanum c || Char.equal c '-' || Char.equal c '_')
  in
  (* A single leftover character cannot encode a whole byte. *)
  if (not valid) || String.length s % 4 = 1
  then None
  else (
    let standard =
      String.map s ~f:(function
        | '-' -> '+'
        | '_' -> '/'
        | c -> c)
    in
    Option.try_with (fun () ->
      Cryptokit.transform_string (Cryptokit.Base64.decode ()) standard))
;;

let hex s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s

let hmac_sha256 ~key message =
  Cryptokit.hash_string (Cryptokit.MAC.hmac_sha256 key) message
;;

let sha256_hex s = hex (Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) s)
let equal_constant_time a b = Cryptokit.string_equal a b
let random_bytes n = Cryptokit.Random.string Cryptokit.Random.secure_rng n
