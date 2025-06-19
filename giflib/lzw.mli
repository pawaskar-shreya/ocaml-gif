(** {1 LZW Compression and Decompression}

    Provides functions for compressing and decompressing image data
    using the LZW algorithm, as used in GIF image encoding. *)

val decode : Bytes.t -> int -> Bytes.t
(** Decompresses LZW-encoded data given the initial code size. *)

val encode : Bytes.t -> int -> Bytes.t
(** Compresses data using LZW algorithm with the given initial code size. *)

val flatten_codes : ?pad:bool -> int -> (Z.t * int) list -> Bytes.t
(** Flattens a list of (code, bit length) pairs into a byte stream *)

val get_bits : Bytes.t -> int -> int -> int
(* Extracts a number of bits from a byte array starting at a bit offset *)