(** {1 Color Table}

    Represents a color table used in GIF images. *)

type color = int * int * int
(** Represents a single RGB color, with each component in the range 0–255. *)

type t = color array
(** A color table is an array of RGB colors. *)

val size : 'a array -> int
(** Returns the number of entries in the color table. *)

val create : int -> (int * int * int) array
(** Creates a color table of the given size, initialized to black *)

val get : 'a array -> int -> 'a
(** Retrieves the color at the specified index in the color table. *)

val set : 'a array -> int -> 'a -> unit
(** Sets the color at the specified index in the color table. *)

val of_list : 'a list -> 'a array
(** Converts a list of colors to a color table array. *)
