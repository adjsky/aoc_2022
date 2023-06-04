open Printf
open Lib

let input_path = "./inputs/1.txt"
let n_elves = 3

let rec first_n n xs =
  match xs with
  | [] -> []
  | x :: xs -> if n = 1 then [ x ] else x :: first_n (n - 1) xs

let process_lines lines =
  let result, _ =
    List.fold_left
      (fun (elf_calories, acc) line ->
        match line with
        | "" -> (acc :: elf_calories, 0)
        | _ -> (elf_calories, acc + int_of_string line))
      ([], 0) lines
  in
  result

let () =
  let calories =
    read_lines input_path |> process_lines
    |> List.sort (fun a b -> b - a)
    |> first_n n_elves |> List.fold_left ( + ) 0
  in
  printf "%d\n" calories
