open Str
open Bool

type point = int * int
type line = point * point
type point_count = (point * int) list

let  insert k v lst = (k, v) :: lst

(* Insert or update element in assoc list *)


module PointKey = struct
  type t = point
  let compare p1 p2 = if fst p1 != fst p2 then fst p1 - fst p2 else snd p1 - snd p2
end

module PointMap = Map.Make(PointKey)

let update_point (k: point) m = if PointMap.mem k m
  then PointMap.add k ((PointMap.find k m) + 1) m
  else PointMap.add k 1 m

let get_x = fst
let get_y = snd
let get_from = fst
let get_to = snd

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

let parse_point (str : string) : point =
  match String.split_on_char ',' str with
  | a :: b :: _ -> (int_of_string a, int_of_string b)
  | _ -> failwith "Wrong point format"

let parse_line (str : string) : line =
  match split (regexp " -> ") str with
  | a :: b :: _ -> (parse_point a, parse_point b)
  | _ -> failwith "Wrong line format"

let is_horzitontal = function (_, y1), (_, y2) -> y1 = y2
let is_vertical = function (x1, _), (x2, _) -> x1 = x2
let inc_x = function x, y -> (x + 1, y)
let inc_y = function x, y -> (x, y + 1)

let rec compute_horizontal (l: line) (p: point) (acc: point list): point list =
  if (get_x p) <= (get_x (get_to l)) then compute_horizontal l (inc_x p) (p :: acc)
  else acc

let rec compute_vertical (l: line) (p: point) (acc: point list): point list =
  if (get_y p) <= (get_y (get_to l)) then compute_vertical l (inc_y p) (p :: acc)
  else acc

let covered_points (l : line) : point list =
   if (not (is_horzitontal l)) && (not (is_vertical l)) then failwith "Only horizontal/vertical lines supported" else (
     if is_horzitontal l then (
        match l with
        | ((x1, y1), (x2, y2)) -> (
          if x1 > x2 then 
            compute_horizontal ((x2, y2), (x1, y1)) (x2, y2) [] 
          else compute_horizontal l (get_from l) []
        ) 
      ) else (
        match l with
        | ((x1, y1),(x2, y2)) -> (
          if y1 > y2 then 
            compute_vertical ((x2, y2), (x1, y1)) (x2, y2) [] 
          else compute_vertical l (get_from l) []
        )
      )
   )

let rec count_coverage (pts: point list) (m: int PointMap.t) =
  match pts with
  | [] -> m
  | h :: t -> (
    count_coverage t (update_point h m)
  )

let () =
    let data = read_lines "input.txt" in
    let _ = print_endline "parsing lines ..." in
    let lines = List.map (fun x -> parse_line x) data in
    let _ = print_endline  (string_of_int (List.length lines)) in
    let _ = print_endline "filter non-orthogonal lines" in
    let ortho_lines = List.filter (fun x -> is_horzitontal x || is_vertical x) lines in
    let _ = print_endline  (string_of_int (List.length ortho_lines)) in
    let _ = print_endline "computing covered points per line" in
    let covered_points_per_line = List.map (fun x -> covered_points x) ortho_lines in
    let _ = print_endline "counting coverage per point" in
    let covered_points = List.flatten covered_points_per_line in
    let _ = print_endline (Printf.sprintf "#Points: %i" (List.length covered_points)) in
    let count_map = count_coverage covered_points PointMap.empty in
    let _ = print_endline "filtering points with less than two crossings" in
    let more_than_one = PointMap.filter (fun _ v -> v > 1) count_map in
    print_endline (string_of_int(PointMap.cardinal more_than_one))