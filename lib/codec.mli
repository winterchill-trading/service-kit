(** Byte-level helpers shared by the token formats ([Session], [Step_up], [Pagination])
    and by [Idempotency]'s body digest. *)

open! Core

(** URL-safe base64 (RFC 4648 section 5) without padding. *)
val base64url_encode : string -> string

(** Inverse of {!base64url_encode}. [None] on any character outside the URL-safe alphabet,
    on padding, or on an impossible length. *)
val base64url_decode : string -> string option

(** Lower-case hexadecimal. *)
val hex : string -> string

(** Raw 32-byte HMAC-SHA256 of [message] under [key]. *)
val hmac_sha256 : key:string -> string -> string

(** Lower-case hex SHA-256 of [s]. *)
val sha256_hex : string -> string

(** String equality whose running time depends only on the lengths, for comparing MACs. *)
val equal_constant_time : string -> string -> bool

(** [n] bytes from the system's secure random source. *)
val random_bytes : int -> string
