type board = int list list
type game = { numbers: int list; boards: board list }

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


let rec lst_of_nth_loop (d: int list list) (n: int) (acc: int list) =
  match d with
  | [] -> acc
  | a :: t -> lst_of_nth_loop t n ((List.nth a n) :: acc)

let rec range i j = if i >= j then [] else i :: (range (i+1) j)

let get_cols (d: int list list) =
  let is = range 0 (List.length d) in
  List.map (fun i -> (
    lst_of_nth_loop d i []
  )) is


let read_numbers fn = 
  read_lines fn |> List.hd |> String.split_on_char ',' |> List.map (fun x -> int_of_string x)

let parse_board (b: string list) =
  let rows_str = List.map (fun x -> Str.split (Str.regexp "[ ]+") x) b in 
  let rows = List.map (fun x -> List.map (fun y -> int_of_string y) x) rows_str in
  (get_cols rows) @ rows @ []

let read_boards fn =
  let lines = read_lines fn in
  let get_board_lines lns = match lns with a :: b :: t -> t | _ -> [] in
  let rec read_boards_loop (lines: string list) (acc1: string list list) (acc2: string list) =
   match lines with
    | [] -> acc1
    | h :: t -> if h = "" then read_boards_loop t (acc2 :: acc1) [] else read_boards_loop t acc1 (h :: acc2) in 
  let boards = read_boards_loop (get_board_lines lines) [] [] in 
  List.map (fun x -> parse_board x) boards

let mock_board = 
  [
    "22 13 14";
    " 8  2 23";
    "21  9 14"
  ]

let bingo elements nums =
  List.for_all (fun x -> List.mem x nums) elements


let create_game fn = { numbers = read_numbers fn; boards = read_boards fn}


let () =
  let g = { numbers = read_numbers "input.txt"; boards = read_boards "input.txt"} in
  print_int (List.length g.numbers);
  print_int (List.length g.boards);
