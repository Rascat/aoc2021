module Fish = struct
  type t = { mutable timer : int }

  let create t = { timer = t }

  (* let decr f =
     if f.timer = 0 then [ { timer = 6 }; { timer = 8 } ]
     else [ { timer = f.timer - 1 } ] *)
  let decr f = if f.timer = 0 then f.timer <- 6 else f.timer <- f.timer - 1
end

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

let parse_fish (str : string) : Fish.t = Fish.create (int_of_string str)

let init_fishes (d : string) : Fish.t list =
  let rec loop (d : string list) (acc : Fish.t list) =
    match d with [] -> acc | h :: t -> loop t (parse_fish h :: acc)
  in
  List.rev (loop (String.split_on_char ',' d) [])

let day (fs : Fish.t list) =
  (* List.map (fun x -> Fish.decr x) fs
     |> List.flatten *)
  let count_new = ref 0 in
  let _ =
    List.iter
      (fun (f: Fish.t) ->
        if f.timer = 0 then (
          count_new := !count_new + 1;
          Fish.decr f)
        else Fish.decr f)
      fs
  in
  let rec loop acc n =
    match n with 0 -> acc | n -> loop ((Fish.create 8) :: acc) (n - 1)
  in
  loop (List.rev fs) !count_new |> List.rev

let n_days (fs : Fish.t list) (n : int) : Fish.t list =
  let rec loop fs = function 0 -> fs | n -> loop (day fs) (n - 1) in
  loop fs n