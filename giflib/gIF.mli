(** {1 GIF file}

    The container object for one or more {! Image}. *)

type t

val from_file : string -> t
(** [from_file filename] will load GIF file from the name provided. Will throw
    exception on error. *)

val from_image : Image.t -> t
val to_file : t -> string -> unit

val get_image : t -> int -> Image.t
(** Get a container image by index *)

val image_count : t -> int
(** Get the number of images in the container GIF *)

val dimensions : t -> int * int
(** Returns the screen dimensions of the GIF file. Note that individual images
    may render to a smaller area than the overall dimensions - you should check
    the offset and dimensions of each image and render it relative to this
    frame. *)

exception Error of string
