type t

val from_file : string -> t
val from_image : Image.t -> t
val to_file : t -> string -> unit
val get_image : t -> int -> Image.t
val image_count : t -> int

exception Error of string
