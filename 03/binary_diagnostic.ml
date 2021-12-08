let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


let get_opt str i = try Some (str.[i]) with Invalid_argument _ -> None


let rec iter (str: string) (i: int) (acc: int array): int array = 
    match get_opt str i with
    | None -> acc
    | Some s -> if s = '1' then (
        acc.(i) <- (acc.(i) + 1);
        iter str (i+1) acc;
    ) else iter str (i+1) acc

let rec loop lst acc =
    match lst with
    | a :: rest -> loop rest (iter a 0 acc)
    | [] -> acc


let calc_gamma list = List.map (fun x -> if x > 500 then 1 else 0) list
let calc_epsilon list = List.map (fun x -> if x <= 500 then 1 else 0) list

let rec list_to_string lst str =
    match lst with
    | a :: rest -> list_to_string rest (str ^ (string_of_int a))
    | [] -> str


let () =
    let data = read_lines "input.txt" in
    let length = String.length (List.hd data) in
    let bin_arr = Array.make length 0 in
    let result = loop data bin_arr in
    let result_lst = Array.to_list result in
    print_endline (list_to_string (calc_gamma result_lst) "");
    print_endline "";
    print_endline (list_to_string (calc_epsilon result_lst) "");