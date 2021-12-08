let test_data = [
    "forward 5";
    "down 5";
    "forward 8";
    "up 3";
    "down 8";
    "forward 2";
]

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let split_on_space = String.split_on_char ' '

let split_input input = List.map split_on_space input

type command = { direction: string; amount: int }
type position = int * int

let get_x position =
    match position with
    | (x, y) -> x

let get_y position =
    match position with
    | (x, y) -> y

let parse_command lst = 
    match lst with
    | a :: b :: empty -> { direction = a; amount = int_of_string b}
    | _ -> raise (Failure "Something went wrong")

let parse_input fn = 
    read_lines fn |> split_input |> List.map parse_command

let rec compute_position (cmd_lst: command list) (pos: position): position = 
    match cmd_lst with
    | a :: rest -> 
        (match a.direction with
        | "forward" -> compute_position rest ((get_x pos) + a.amount, (get_y pos))
        | "up" -> compute_position rest ((get_x pos), (get_y pos) - a.amount)
        | "down" -> compute_position rest ((get_x pos), (get_y pos) + a.amount)
        | _ -> raise (Failure "Something went wrong"))
    | [] -> pos


let rec compute_position_aim (cmd_lst: command list) (aim: int) (pos: position): position = 
    match cmd_lst with
    | a :: rest -> 
        (match a.direction with
        | "forward" -> compute_position_aim rest aim ((get_x pos) + a.amount, (get_y pos) + (a.amount * aim))
        | "up" -> compute_position_aim rest (aim - a.amount) ((get_x pos), (get_y pos))
        | "down" -> compute_position_aim rest (aim + a.amount) ((get_x pos), (get_y pos))
        | _ -> raise (Failure "Something went wrong"))
    | [] -> pos





    