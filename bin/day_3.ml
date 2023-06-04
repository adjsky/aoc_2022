open Printf

let elves_in_group = 3
let input_path = "./inputs/3.txt"
let lowercase_start = Char.code 'a'
let lowercase_end = Char.code 'z'
let uppercase_start = Char.code 'A'
let uppercase_end = Char.code 'Z'

let is_uppercase char =
  let code = Char.code char in
  uppercase_start <= code && code <= uppercase_end

let is_lowercase char =
  let code = Char.code char in
  lowercase_start <= code && code <= lowercase_end

let get_char_priority char =
  if is_lowercase char then Char.code char - lowercase_start + 1
  else if is_uppercase char then Char.code char - uppercase_start + 1 + 26
  else failwith @@ sprintf "received invalid char: %c" char

let char_occurrences str target_char =
  let count = ref 0 in
  String.iter (fun ch -> if ch = target_char then incr count) str;
  !count

let rec find_duplicate_n_times n str =
  match str with
  | "" -> failwith "no duplicates found"
  | _ ->
      let head = str.[0] in
      let occurences = char_occurrences str head in
      if occurences == n then head
      else
        let length = String.length str in
        let sub = String.sub str 1 (length - 1) in
        find_duplicate_n_times n sub

let process_group list =
  let wo_duplicates =
    List.map
      (fun s ->
        let s_length = String.length s in
        let buffer = Buffer.create s_length in
        let occurences = Hashtbl.create s_length in
        let () =
          String.iter
            (fun c ->
              match Hashtbl.find_opt occurences c with
              | None ->
                  Hashtbl.add occurences c true;
                  Buffer.add_char buffer c
              | Some _ -> ())
            s
        in
        Buffer.contents buffer)
      list
  in
  String.concat "" wo_duplicates

let rec process_lines_by_groups lines n i =
  let start = i * n in
  if start + n > Array.length lines then []
  else
    let group = Array.sub lines start n in
    let processed = process_group (Array.to_list group) in
    let common_char = find_duplicate_n_times 3 processed in
    common_char :: process_lines_by_groups lines n (i + 1)

let process_lines_by_half lines =
  List.map
    (fun line ->
      let half_n = String.length line / 2 in
      let first_half = String.sub line 0 half_n in
      let second_half = String.sub line half_n half_n in
      let processed = process_group [ first_half; second_half ] in
      find_duplicate_n_times 2 processed)
    lines

let sum_duplicates duplicates =
  List.fold_left
    (fun sum error_char -> sum + get_char_priority error_char)
    0 duplicates

let () =
  let lines = Lib.read_lines input_path in
  let duplicates_first = process_lines_by_half lines in
  let duplicates_second =
    process_lines_by_groups (Array.of_list lines) elves_in_group 0
  in
  let sum_first = sum_duplicates duplicates_first in
  let sum_second = sum_duplicates duplicates_second in
  printf "first: %d, second: %d\n" sum_first sum_second
