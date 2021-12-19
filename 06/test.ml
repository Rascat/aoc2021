open OUnit2
open Lanternfish
open Fish

let fc i = Fish.create i

let format_fishes (fs : Fish.t list) : string =
  let rec loop str = function
    | [] -> str
    | h :: t -> loop (str ^ Printf.sprintf "'%i', " h.timer) t
  in
  loop "" fs

let tests =
  "test suite for lanternfish"
  >::: [
         ("parse fish '4'" >:: fun _ -> assert_equal (create 4) (parse_fish "4"));
         ( "decrement fish '5'" >:: fun _ ->
          let f = Fish.create 5 in
          let _ = Fish.decr f in
           assert_equal  (create 4)  f );
         ( "decrement fish '0'" >:: fun _ ->
          let f = Fish.create 0 in
          let _ = Fish.decr f in
           assert_equal (create 6 ) f );
         ( "1 day after ['0'; '3'; '8']" >:: fun _ ->
           assert_equal  ~printer:format_fishes
             [ create 6; create 2; create 7; create 8;]
             (day [ create 0; create 3; create 8 ]) );
         ( "3 days after ['3'; '4'; '3'; '1'; '2']" >:: fun _ ->
           assert_equal ~printer:format_fishes
             [
               create 0;
               create 1;
               create 0;
               create 5;
               create 6;
               create 7;
               create 8;
             ]
             (n_days [ create 3; create 4; create 3; create 1; create 2 ] 3) );
         ( "5934 fish after 80 days" >:: fun _ ->
           assert_equal ~printer:string_of_int 5934
             (n_days [ create 3; create 4; create 3; create 1; create 2 ] 80
             |> List.length) );
       ]

let _ = run_test_tt_main tests