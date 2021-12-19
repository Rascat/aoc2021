#require "core.top"
open Core

(* Solution taken from https://github.com/jubnzv/aoc2021/blob/master/day06/solution.ml *)

let solve limit =
  In_channel.read_all "input.txt"
  |> (fun s -> String.sub s 0 ((String.length s) - 1))
  |> String.split ~on:','
  |> List.map ~f:Int.of_string
  |> (fun inp -> List.init 9 ~f:(fun i -> List.count ~f:(fun j -> i = j) inp))
  |> (fun l ->
    let p = function [i0;i1;i2;i3;i4;i5;i6;i7;i8] -> [i1;i2;i3;i4;i5;i6;i7+i0;i8;i0] in
    let rec s fs n = if n > 0 then s (p fs) (n - 1) else fs in
    s l limit)
  |> List.fold_left ~init:0 ~f:(+)

module Part1 = struct
  let run () = solve 80
end

module Part2 = struct
  let run () = solve 256
end