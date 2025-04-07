open OUnit2
open Giflib

let test_flatten_empty _ =
  let res = Lzw.flatten_codes 8 [] in
  assert_equal (Bytes.length res) 0

let test_flatten_byte_aligned_codes _ =
  let res = Lzw.flatten_codes 8 [ (Z.of_int 0xAB, 8); (Z.of_int 0xCD, 8) ] in
  assert_equal (Bytes.length res) 2;
  assert_equal (Bytes.get res 0) (char_of_int 0xAB);
  assert_equal (Bytes.get res 1) (char_of_int 0xCD)

let test_flatten_nibble_aligned_codes _ =
  let res = Lzw.flatten_codes 4 [ (Z.of_int 0xAB, 8); (Z.of_int 0xCD, 8) ] in
  assert_equal (Bytes.length res) 4;
  assert_equal (Bytes.get res 0) (char_of_int 0xB);
  assert_equal (Bytes.get res 1) (char_of_int 0xA);
  assert_equal (Bytes.get res 2) (char_of_int 0xD);
  assert_equal (Bytes.get res 3) (char_of_int 0xC)

let test_flatten_unaligned_codes _ =
  let res =
    Lzw.flatten_codes 8
      [ (Z.of_int 0xA, 4); (Z.of_int 0xCB, 8); (Z.of_int 0xD, 4) ]
  in
  assert_equal (Bytes.length res) 2;
  assert_equal (Bytes.get res 0) (char_of_int 0xBA);
  assert_equal (Bytes.get res 1) (char_of_int 0xDC)

let test_get_bits_basic _ =
  for i = 0 to 4 do
    let res = Lzw.get_bits (Bytes.of_string "hello") (i * 8) 8 in
    assert_equal res (int_of_char (String.get "hello" i))
  done

let test_all_bit_width_1 _ =
  for v = 0 to 255 do
    for i = 0 to 8 do
      let res = Lzw.get_bits (Bytes.init 1 (fun _ -> char_of_int v)) 0 i in
      assert_equal res (v land (0xFF lsr (8 - i)))
    done
  done

let test_all_bit_width_2 _ =
  for v = 0 to 255 do
    for i = 0 to 7 do
      let res =
        Lzw.get_bits (Bytes.init 1 (fun _ -> char_of_int v)) (7 - i) i
      in
      assert_equal res ((v lsr (7 - i)) land (0xFF lsr (8 - i)))
    done
  done

let test_get_larger_bits _ =
  let src = Lzw.flatten_codes 8 [ (Z.of_int 0xAB, 8); (Z.of_int 0xCD, 8) ] in
  for v = 0 to 16 do
    let expected = 0xCDAB land ((1 lsl v) - 1) in
    let actual = Lzw.get_bits src 0 v in
    assert_equal expected actual
  done

let test_get_longer_slices _ =
  let src = Lzw.flatten_codes 8 [ (Z.of_int 0xAB, 8); (Z.of_int 0xCD, 8) ] in
  for v = 0 to 16 do
    let expected = (0xCDAB lsr (16 - v)) land ((1 lsl v) - 1) in
    let actual = Lzw.get_bits src (16 - v) v in
    assert_equal expected actual
  done

let test_encode_decode _ =
  let src = Lzw.flatten_codes 8 [ (Z.of_int 0xAB, 8); (Z.of_int 0xCD, 8); (Z.of_int 0xEF, 8) ] in
  [ 8; 6; 4; 3; 2]
  |> List.iter (fun bpp ->
         let encoded = Lzw.encode src bpp in
         let decoded = Lzw.decode encoded bpp in
         (* This test is a bit odd, as decode expands the data to one
            pixel per byte, rather than as a straight code. *)
         assert_equal ~msg:"byte count" ~printer:string_of_int
           (Bytes.length src * 8 / bpp)
           (Bytes.length decoded);
         for i = 0 to Bytes.length src - 1 do
           let expected = Lzw.get_bits src (i * bpp) bpp
           and actual = int_of_char (Bytes.get decoded i) in
           assert_equal ~msg:"data" ~printer:string_of_int expected actual
         done)

let suite =
  "LZW"
  >::: [
         "Flatten empty code list" >:: test_flatten_empty;
         "Flatten byte aligned code list" >:: test_flatten_byte_aligned_codes;
         "Flatten nibble aligned code list"
         >:: test_flatten_nibble_aligned_codes;
         "Flatten unaligned code list" >:: test_flatten_unaligned_codes;
         "Get bits of string" >:: test_get_bits_basic;
         "Get all bits of char" >:: test_all_bit_width_1;
         "Get all bits of char" >:: test_all_bit_width_2;
         "Get bits bigger than byte" >:: test_get_larger_bits;
         "Get bits bigger than byte with offset" >:: test_get_longer_slices;
         "Test encode/decode cycle" >:: test_encode_decode;
       ]

let () = run_test_tt_main suite
