val decode : Bytes.t -> int -> Bytes.t
val encode : Bytes.t -> int -> Bytes.t
val flatten_codes : int -> (Z.t * int) list -> Bytes.t
val get_bits : Bytes.t -> int -> int -> int
