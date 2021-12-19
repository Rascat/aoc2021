(* module OutputCodes = struct
  type t = string * string * string * string

  let create c1 c2 c3 c4 = (c1, c2, c3, c4)
end
 *)
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

let get_output_code_string = function
  | [ _; o ] -> o
  | _ -> failwith "No output code"

let is_one (code : string) : bool = String.length code = 2
let is_four code = String.length code = 4
let is_seven code = String.length code = 3
let is_eight code = String.length code = 7

module Part1 = struct
  let run fn =
    let data =
      read_lines fn |> List.map (fun x -> Str.split (Str.regexp " | ") x)
    in
    let num_uniq_codes =
      List.map (fun x -> get_output_code_string x) data
      |> List.map (fun x -> String.split_on_char ' ' x)
      |> List.flatten
      |> List.filter (fun x -> is_one x || is_four x || is_seven x || is_eight x)
      |> List.length

    in
    num_uniq_codes
end
