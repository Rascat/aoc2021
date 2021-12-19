open OUnit2
open Segment

let tests =
  "test suite for segment"
  >::: [
    ("code represents one" >:: fun _ ->
      assert_equal ~printer:string_of_bool true (is_one "ab"))
  ]

let _ = run_test_tt_main tests