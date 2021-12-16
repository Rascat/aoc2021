open OUnit2
open Hydrothermal


let print_tuples lst : string =
  let rec loop str lst =
    match lst with
    | [] -> str
    | (a, b) :: rest -> loop (Printf.sprintf "%s(%i, %i) " str a b) rest
  in
  loop "" lst

let point_count_to_string (pc : point_count) =
  let rec loop str = function
    | [] -> str
    | ((x, y), c) :: rest ->
        loop (Printf.sprintf "%s(%i, %i) -> %i" str x y c) rest
  in
  loop "" pc

let tests =
  "test suite for hydrothermal"
  >::: [
         ( "parse point (0,0)" >:: fun _ ->
           assert_equal (0, 0) (parse_point "0,0") );
         ( "parse point (1,5)" >:: fun _ ->
           assert_equal (1, 5) (parse_point "1,5") );
         ( "parse point wrong format" >:: fun _ ->
           assert_raises (Failure "Wrong point format") (fun () ->
               parse_point "1;2") );
         ( "parse line (0,0) -> (3,5)" >:: fun _ ->
           assert_equal ((0, 0), (3, 5)) (parse_line "0,0 -> 3,5") );
         ("get x of (4,5)" >:: fun _ -> assert_equal 4 (get_x (4, 5)));
         ("get y of (5,9)" >:: fun _ -> assert_equal 9 (get_y (5, 9)));
         ( "get from of (5,9) -> (4,5)" >:: fun _ ->
           assert_equal (5, 9) (get_from ((5, 9), (4, 5))) );
         ( "get to of (5,9) -> (4,5)" >:: fun _ ->
           assert_equal (4, 5) (get_to ((5, 9), (4, 5))) );
         ( "(0,0) -> (5,0) is horizontal" >:: fun _ ->
           assert_equal true (is_horzitontal ((0, 0), (5, 0))) );
         ( "(0,0) -> (5,0) is not vertical" >:: fun _ ->
           assert_equal false (is_vertical ((0, 0), (5, 0))) );
         ( "(0,5) -> (0,0) is vertical" >:: fun _ ->
           assert_equal true (is_vertical ((0, 5), (0, 0))) );
         ( "(0,5) -> (0,0) is not horizontal" >:: fun _ ->
           assert_equal false (is_horzitontal ((0, 5), (0, 0))) );
         ( "compute covered points for vertical line" >:: fun _ ->
           assert_equal ~printer:print_tuples
             [ (1, 3); (1, 2); (1, 1) ]
             (covered_points ((1, 1), (1, 3))) );
         ( "compute covered points for horizontal line" >:: fun _ ->
           assert_equal ~printer:print_tuples
             [ (3, 1); (2, 1); (1, 1) ]
             (covered_points ((3, 1),(1, 1))) );
         ( "point coverage for unhorizontal/unvertical lines fails" >:: fun _ ->
           assert_raises (Failure "Only horizontal/vertical lines supported")
             (fun () -> covered_points ((0, 0), (3, 3))) );
         ( "counting of point coverage" >:: fun _ ->
           assert_equal ~printer:point_count_to_string
             [ ((0, 1), 2); ((0, 0), 1) ]
             (count_coverage [ (0, 0); (0, 1); (0, 1) ] PointMap.empty ) );
       ]

let _ = run_test_tt_main tests