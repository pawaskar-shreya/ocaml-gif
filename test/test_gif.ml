open OUnit2
open Giflib

let test_example_file _ =
  let g = GIF.from_file "../../../test/testdata/test1.gif" in
  assert_equal ~msg:"image count" 1 (GIF.image_count g);
  assert_equal ~msg:"screen dims" (16, 16) (GIF.dimensions g);
  let i = GIF.get_image g 0 in
  let w, h = Image.dimensions i in
  assert_equal ~msg:"dimensions" (16, 16) (w, h);
  assert_equal ~msg:"offset" (0, 0) (Image.offset i);
  assert_equal ~msg:"transparent" (Some 2) (Image.transparent i);
  assert_equal ~msg:"delay" (Some 0) (Image.delay_time i);
  let pixels = Image.pixels i in
  assert_equal (w * h) (Array.length pixels);
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let expected = 1 - (y / 2 mod 2) in
      let v = pixels.((y * w) + x) in
      assert_equal v expected
    done
  done;
  let palette = Image.palette i in
  assert_equal ~msg:"palette size" ~printer:string_of_int
    (ColorTable.size palette) 4;
  assert_equal ~msg:"black pixel" (0, 0, 0) (ColorTable.get palette 1);
  assert_equal ~msg:"orange pixel" (255, 118, 17) (ColorTable.get palette 0)

let test_get_image_fail _ =
  let g = GIF.from_file "../../../test/testdata/test1.gif" in
  assert_raises (GIF.Error "get_image: frame not found") (fun () ->
      GIF.get_image g 1)

let test_read_image_twice _ =
  let g = GIF.from_file "../../../test/testdata/test1.gif" in
  let _ = GIF.get_image g 0 in
  let i = GIF.get_image g 0 in
  let w, h = Image.dimensions i in
  assert_equal ~msg:"dimensions" (16, 16) (w, h);
  let pixels = Image.pixels i in
  assert_equal (w * h) (Array.length pixels);
  assert_equal (ColorTable.size (Image.palette i)) 4

let test_read_mono_image _ =
  let g = GIF.from_file "../../../test/testdata/flitter.gif" in
  assert_equal 1 (GIF.image_count g);
  assert_equal ~msg:"screen dims" (640, 480) (GIF.dimensions g);
  let i = GIF.get_image g 0 in
  let w, h = Image.dimensions i in
  assert_equal ~msg:"resolution" (640, 480) (w, h);
  let pixels = Image.pixels i in
  assert_equal ~msg:"pixel count" (w * h) (Array.length pixels);
  assert_equal ~msg:"palette" ~printer:string_of_int 256
    (ColorTable.size (Image.palette i))

let suite =
  "BasicLoading"
  >::: [
         "Load file" >:: test_example_file;
         "Fail get imagae" >:: test_get_image_fail;
         "Test re-read image" >:: test_read_image_twice;
         "Test read mono image" >:: test_read_mono_image;
       ]

let () = run_test_tt_main suite
