open OUnit2
open Whales

let tests =
  "test suite for whales"
  >::: [
         ( "find median of an odd nr of ints" >:: fun _ ->
           assert_equal ~printer:string_of_int 2
             (median [ 16; 1; 2; 0; 4; 2; 7; 1; 2 ]) );
         ( "find median of an even nr of ints" >:: fun _ ->
           assert_equal ~printer:string_of_int 2
             (median [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ]) );
         ( "compute fuel consumption" >:: fun _ ->
           assert_equal ~printer:string_of_int 37
             (fuel_consumption [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ] 2) );
         ( "compute increasing fuel consumption" >:: fun _ ->
           assert_equal ~printer:string_of_int 168
             (increasing_fuel_consumption [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ] 5) );
         ( "compute sum one to five" >:: fun _ ->
           assert_equal ~printer:string_of_int 15 (sum_one_to_n 5) );
       ]

let _ = run_test_tt_main tests