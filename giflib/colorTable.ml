type color = (int * int * int) ;;

  type t = color array

  let size tbl = Array.length tbl ;;

  let create n = Array.make n (0, 0, 0) ;;

  let get tbl n = Array.get tbl n;;

  let set tbl n colors = tbl.(n) <- colors ;;

  let of_list xs = Array.of_list xs;;
