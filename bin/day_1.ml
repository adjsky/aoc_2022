let input_path = "./inputs/1.txt"
let n_elves = 3

let rec first_n n xs =
  match xs with
  | [] -> []
  | x :: xs -> if n = 1 then [ x ] else x :: first_n (n - 1) xs

let () =
  let lines = Lib.read_lines input_path in
  let elf_calories, _ =
    List.fold_left
      (fun (elf_calories, acc) line ->
        match line with
        | "" -> (acc :: elf_calories, 0)
        | _ -> (elf_calories, acc + int_of_string line))
      ([], 0) lines
  in
  let sorted_elf_calories = List.sort (fun a b -> b - a) elf_calories in
  let calories =
    first_n n_elves sorted_elf_calories |> List.fold_left ( + ) 0
  in
  print_endline @@ string_of_int calories
