type t = {
  width : int;
  height : int;
  palette : ColorTable.t;
  pixels : int array;
}

let v dim palette pixels =
  let width, height = dim in
  { width; height; palette; pixels }

let dimensions i = (i.width, i.height)
let palette i = i.palette
let pixels i = i.pixels
let rgb_pixels i = Array.map (fun p -> i.palette.(p)) i.pixels
