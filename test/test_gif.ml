open OUnit2
open Gif

let test_example_file _ =
  let g = GIF.from_file "../../../test/testdata/test1.gif" in
  let i = GIF.get_image g 0 in
  assert_equal (i.width, i.height) (16, 16);
  assert_equal (Array.length i.pixels) (i.height * i.width);
  assert_equal (ColorTable.size i.palette) 4

let test_get_image_fail _ =
  let g = GIF.from_file "../../../test/testdata/test1.gif" in
  assert_raises (GIF.Error "get_image: frame not found") (fun () -> GIF.get_image g 1)

let suite =
  "BasicLoading" >::: [
    "Load file" >:: test_example_file;
    "Fail get imagae" >:: test_get_image_fail;
  ]

let () =
  run_test_tt_main suite
