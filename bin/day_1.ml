let input_file = "./inputs/1.txt"
let n_elves = 3

let rec first_n n xs =
  match xs with
  | [] -> []
  | x :: xs -> if n = 1 then [ x ] else x :: first_n (n - 1) xs

let () =
  let ic = open_in input_file in
  let lines = In_channel.input_all ic |> String.split_on_char '\n' in
  let elve_calories, _ =
    List.fold_left
      (fun (elve_calories, acc) line ->
        match line with
        | "" -> (acc :: elve_calories, 0)
        | _ -> (elve_calories, acc + int_of_string line))
      ([], 0) lines
  in
  let sorted_elve_calories = List.sort (fun a b -> b - a) elve_calories in
  let calories =
    first_n n_elves sorted_elve_calories |> List.fold_left ( + ) 0
  in
  print_endline @@ string_of_int calories
