type t

val from_file : string -> t
val from_image : Image.image -> t

val to_file : t -> string -> unit
val get_image : t -> int -> Image.image
val image_count : t -> int

exception Error of string
