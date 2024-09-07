type t = {
  x_offset : int;
  y_offset : int;
  width : int;
  height : int;
  palette : ColorTable.t;
  pixels : int array;
}

let v ?offset dim palette pixels =
  let width, height = dim in
  let x_offset, y_offset = match offset with None -> (0, 0) | Some x -> x in
  { x_offset; y_offset; width; height; palette; pixels }

let dimensions i = (i.width, i.height)
let offset i = (i.x_offset, i.y_offset)
let palette i = i.palette
let pixels i = i.pixels
let rgb_pixels i = Array.map (fun p -> i.palette.(p)) i.pixels
