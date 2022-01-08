open OUnit2
open Smoke

(* The following heightmap is used for testing:
 *
 *[2]1 9 9 9 4 3 2 1 0
 * 3 9 8[7]8 9 4 9 2 1
 * 9 8 5 6 7 8 9 8 9 2 
 * 8 7 6 7 8 9 6 7 8 9
 * 9 8 9 9 9 6 5 6 7[8]
 *
 * idx [2]: 0
 * idx [7]: 13
 * idx [8]: 49
 *)

let heightmap = {
  row_length = 10;
   values = [
    2;
    1;
    9;
    9;
    9;
    4;
    3;
    2;
    1;
    0;
    3;
    9;
    8;
    7;
    8;
    9;
    4;
    9;
    2;
    1;
    9;
    8;
    5;
    6;
    7;
    8;
    9;
    8;
    9;
    2;
    8;
    7;
    6;
    7;
    8;
    9;
    6;
    7;
    8;
    9;
    9;
    8;
    9;
    9;
    9;
    6;
    5;
    6;
    7;
    8;
  ]}

let tests =
  "test suite for smoke"
  >::: [
         ( "up from first seven" >:: fun _ ->
           assert_equal (Some 9) (up 13 heightmap) );
         ( "left from second three" >:: fun _ ->
           assert_equal None (left 10 heightmap) );
         ( "right from first zero" >:: fun _ ->
           assert_equal None (right 9 heightmap) );
         ( "down from first seven" >:: fun _ ->
           assert_equal (Some 6) (down 13 heightmap) );
         ( "left from first seven" >:: fun _ ->
           assert_equal (Some 8) (left 13 heightmap) );
         ( "right from first seven" >:: fun _ ->
           assert_equal (Some 8) (right 13 heightmap) );
         ( "left from first two" >:: fun _ ->
           assert_equal None (left 0 heightmap) );
         ( "right from last eight" >:: fun _ ->
           assert_equal None (right 49 heightmap) );
         ( "down from last eight" >:: fun _ ->
           assert_equal None (down 49 heightmap) );
         ( "solve example" >:: fun _ ->
           assert_equal ~printer:string_of_int 15 (Part1.solve heightmap) );
         ( "solve example from file" >:: fun _ ->
           assert_equal ~printer:string_of_int 15 (Part1.run "example.txt") );
       ]

let _ = run_test_tt_main tests