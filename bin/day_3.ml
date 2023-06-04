open Printf
open Lib

let elves_in_group = 3
let input_path = "./inputs/3.txt"
let offset ch base = Char.code ch - Char.code base

let get_char_priority char =
  match char with
  | 'a' .. 'z' -> 1 + offset char 'a'
  | 'A' .. 'Z' -> 27 + offset char 'A'
  | _ -> failwith @@ sprintf "received invalid char: %c" char

let process_group list = List.map remove_duplicates list |> String.concat ""

let rec process_lines_by_groups n i lines =
  let start = i * n in
  if start + n > Array.length lines then []
  else
    let common_char =
      Array.sub lines start n |> Array.to_list |> process_group
      |> find_duplicate_n_times 3
    in
    common_char :: process_lines_by_groups n (i + 1) lines

let process_lines_by_half lines =
  List.map
    (fun line -> split_half line |> process_group |> find_duplicate_n_times 2)
    lines

let sum_duplicates duplicates =
  List.fold_left
    (fun sum error_char -> sum + get_char_priority error_char)
    0 duplicates

let () =
  read_lines input_path |> process_lines_by_half |> sum_duplicates
  |> printf "%d\n"

let () =
  read_lines input_path |> Array.of_list
  |> process_lines_by_groups elves_in_group 0
  |> sum_duplicates |> printf "%d\n"
