type board = { rows: int list list; cols: int list list}
type game = { numbers: int list; boards: board list }
type win = { board: board; rowCol: int list; numbers: int list}

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


let get_val = function
  | None -> failwith "No value"
  | Some s -> s

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

let parse_board (b: string list): board =
  let rows_str = List.map (fun x -> Str.split (Str.regexp "[ ]+") x) b in 
  let rows = List.map (fun x -> List.map (fun y -> int_of_string y) x) rows_str in
  { rows = rows; cols = (get_cols rows)}

let read_boards (fn: string): board list =
  let lines = read_lines fn in
  let get_board_lines lns = match lns with a :: b :: t -> t | _ -> [] in
  let rec read_boards_loop (lines: string list) (acc1: string list list) (acc2: string list) =
   match lines with
    | [] -> acc1
    | h :: t -> if h = "" then read_boards_loop t (acc2 :: acc1) [] else read_boards_loop t acc1 (h :: acc2) in 
  let boards = read_boards_loop (get_board_lines lines) [] [] in 
  List.map (fun x -> parse_board x) boards

let rec firstn n = function
  | [] -> failwith "firstn"
  | h :: t -> if n = 1 then [h] else h :: (firstn (n-1) t)

let mock_board = 
  [
    "22 13 14";
    " 8  2 23";
    "21  9 14"
  ]

let bingo elements nums =
  List.for_all (fun x -> List.mem x nums) elements

let check_board (b: board) (nums: int list) = 
  match List.find_opt (fun x -> bingo x nums) (b.rows @ b.cols) with
  | None -> None
  | Some s -> Some { board = b; rowCol = s; numbers = nums}


let rec check_boards bs nums =
  match bs with
  | [] -> None
  | h :: t -> begin
    match check_board h nums with
    | None -> check_boards t nums
    | Some s -> Some s
  end


let create_game fn = { numbers = read_numbers fn; boards = read_boards fn}

let sum_list lst = List.fold_left (+) 0 lst

let sum_unmarked (w: win) =
  List.flatten w.board.rows |> List.filter (fun x -> Bool.not (List.mem x w.numbers)) |> sum_list

let rec iter (is: int list) (g: game): win option =
  match is with
  | [] -> None
  | i :: is -> begin
    match check_boards g.boards (firstn i g.numbers) with
    | None -> iter is g
    | Some s -> Some s
  end

let () =
  let g = create_game "input.txt" in
  let is = range 5 (List.length g.numbers) in
  let win = iter is g in
  let solution = sum_unmarked (get_val win) in
  print_endline (string_of_int solution)
  

(* Winning row/col: 48 27 40 9 60 *)
(* Sum of unmarked numbers: 1038 *)
(* Last number: 48 *)
(* Solution is 932 * 48 = 44736 *)