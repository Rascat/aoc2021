
let test_data = [
    199;
    200;
    208;
    210;
    200;
    207;
    240;
    269;
    260;
    263;
    264;
]

let rec window_sum_3 lst = 
    match lst with
    | a :: b :: c :: rest -> (a + b + c) :: window_sum_3 (b :: c :: rest)
    | _ -> []

let rec str_lst_to_int_lst lst =
    match lst with
    | a :: rest -> (int_of_string a) :: (str_lst_to_int_lst rest)
    | [] -> []

let rec fs2 lst acc = 
    match lst with
    | a :: b :: rest -> if a < b then fs2 (b :: rest) acc + 1 else fs2 (b :: rest) acc
    | _ -> acc

let rec print_list_int lst = 
    match lst with
    | [] -> print_endline ""
    | h :: t ->
    begin
    print_int h;
    print_endline "";
    print_list_int t;
    end;;

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let file = "input.txt"

let () =
    let data = read_lines file in
       print_int (fs2 data 0)
    