open OUnit2
open Giflib

let test_example_file _ =
  let g = GIF.from_file "../../../test/testdata/test1.gif" in
  assert_equal (GIF.image_count g) 1;
  let i = GIF.get_image g 0 in
  assert_equal (i.width, i.height) (16, 16);
  assert_equal (Array.length i.pixels) (i.height * i.width);
  assert_equal (ColorTable.size i.palette) 4;
  for y = 0 to i.height - 1 do
    for x = 0 to i.width - 1 do
      let expected = 1 - (y / 2 mod 2) in
      let v = i.pixels.((y * i.width) + x) in
      assert_equal v expected
    done
  done

let test_get_image_fail _ =
  let g = GIF.from_file "../../../test/testdata/test1.gif" in
  assert_raises (GIF.Error "get_image: frame not found") (fun () ->
      GIF.get_image g 1)

let test_read_image_twice _ =
  let g = GIF.from_file "../../../test/testdata/test1.gif" in
  let _ = GIF.get_image g 0 in
  let i = GIF.get_image g 0 in
  assert_equal (i.width, i.height) (16, 16);
  assert_equal (Array.length i.pixels) (i.height * i.width);
  assert_equal (ColorTable.size i.palette) 4

let test_read_mono_image _ =
  let g = GIF.from_file "../../../test/testdata/flitter.gif" in
  assert_equal (GIF.image_count g) 1;
  let i = GIF.get_image g 0 in
  assert_equal ~msg:"resolution" (i.width, i.height) (640, 480);
  assert_equal ~msg:"pixel count" (Array.length i.pixels) (i.height * i.width);
  assert_equal ~msg:"palette" ~printer:string_of_int 256
    (ColorTable.size i.palette)

let suite =
  "BasicLoading"
  >::: [
         "Load file" >:: test_example_file;
         "Fail get imagae" >:: test_get_image_fail;
         "Test re-read image" >:: test_read_image_twice;
         "Test read mono image" >:: test_read_mono_image;
       ]

let () = run_test_tt_main suite
