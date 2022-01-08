type cell = {
  center : int;
  up : int option;
  down : int option;
  left : int option;
  right : int option;
}

type heightmap = { row_length : int; values : int list }

let get_height = function None -> Int.max_int | Some s -> s

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

(* thanks, stackoverflow.com *)
let explode s = List.init (String.length s) (String.get s)

let get_center c = c.center

let up (idx : int) (h : heightmap) : 'a option =
  let idx_up = idx - h.row_length in
  if idx_up < 0 then None else List.nth_opt h.values idx_up

let down (idx : int) (h : heightmap) : 'a option =
  let idx_down = idx + h.row_length in
  List.nth_opt h.values idx_down

let left (idx : int) (h : heightmap) : 'a option =
  let idx_left = idx - 1 in
  if idx_left < 0 || idx mod h.row_length = 0 then None else List.nth_opt h.values idx_left

let right (idx : int) (h : heightmap) : 'a option =
  let idx_right = idx + 1 in
  if idx_right < 0 || (idx mod h.row_length) = h.row_length - 1 then None else List.nth_opt h.values idx_right

let is_low_point (c : cell) =
  c.center < get_height c.up
  && c.center < get_height c.down
  && c.center < get_height c.left
  && c.center < get_height c.right

module Part1 = struct
  let solve heightmap = 
    let cells =
      List.mapi
        (fun i v ->
          {
            center = v;
            up = up i heightmap;
            down = down i heightmap;
            left = left i heightmap;
            right = right i heightmap;
          })
        heightmap.values
    in
    List.fold_left (fun acc c -> if is_low_point c then acc + c.center + 1 else acc) 0 cells
    
  let run fn =
    let data = read_lines fn in
    let heightmap =
      {
        row_length = List.hd data |> String.length;
        values =
          List.map (fun x -> explode x) data
          |> List.flatten
          |> List.map (fun c -> Printf.sprintf "%c" c)
          |> List.map int_of_string;
      }
    in
    solve heightmap
end

(* Not 434 :( )*)