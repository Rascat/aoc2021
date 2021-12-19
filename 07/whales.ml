let read_lines name : string list =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
        close_in ic;
        List.rev acc
  in
  loop []

let is_even x = x mod 2 = 0
let is_odd x = x mod 2 != 0

let median (lst : int list) : int =
  let sorted = List.sort (fun a b -> a - b) lst in
  let length = List.length sorted in
  if is_odd length then List.nth sorted (((length + 1) / 2) - 1)
  else (List.nth sorted ((length / 2) - 1) + List.nth sorted (length / 2)) / 2


let average (lst: int list): int =
  (List.fold_left (fun acc x -> acc + x) 0 lst) / (List.length lst)

let fuel_consumption (lst: int list) (pos: int): int =
  List.fold_left (fun acc x -> acc + (Int.abs (pos - x))) 0 lst

let sum_one_to_n n =
  (n * (n + 1)) / 2

let increasing_fuel_consumption (lst: int list) (pos: int): int =
  List.fold_left (fun acc x -> acc + (sum_one_to_n (Int.abs (pos - x)))) 0 lst

module Part1 = struct
  let run fn =
    let positions = read_lines fn |> List.hd |> String.split_on_char ','
    |> List.map (fun x -> int_of_string x) in
    let optimal_pos = median positions in
    fuel_consumption positions optimal_pos
end

module Part2 = struct
  let run fn =
    let positions = read_lines fn |> List.hd |> String.split_on_char ','
    |> List.map (fun x -> int_of_string x) in
    let optimal_pos = average positions in
    increasing_fuel_consumption positions optimal_pos
end