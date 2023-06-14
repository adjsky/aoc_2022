open Printf
open Lib

let input_path = "./inputs/4.txt"

let parse_range raw =
  match String.split_on_char '-' raw with
  | splitted when List.length splitted != 2 -> failwith "invalid range"
  | splitted ->
      let ints = List.map int_of_string splitted in
      (List.hd ints, List.rev ints |> List.hd)

let parse_line raw =
  match String.split_on_char ',' raw with
  | splitted when List.length splitted != 2 -> failwith "invalid line"
  | splitted ->
      let ranges = List.map parse_range splitted in
      (List.hd ranges, List.rev ranges |> List.hd)

let some_range_within_other ranges =
  let (a1, b1), (a2, b2) = ranges in
  (a2 >= a1 && b2 <= b1) || (a2 <= a1 && b2 >= b1)

let ranges_overlapping ranges =
  let (a1, b1), (a2, b2) = ranges in
  a2 <= b1 && b2 >= a1

let () =
  read_lines input_path
  |> List.fold_left
       (fun counter line ->
         match parse_line line with
         | ranges when some_range_within_other ranges -> counter + 1
         | _ -> counter)
       0
  |> printf "%d\n"

let () =
  read_lines input_path
  |> List.fold_left
       (fun counter line ->
         match parse_line line with
         | ranges when ranges_overlapping ranges -> counter + 1
         | _ -> counter)
       0
  |> printf "%d\n"
