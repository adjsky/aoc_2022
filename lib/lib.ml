let read_lines file_path =
  let ic = open_in file_path in
  In_channel.input_all ic |> String.split_on_char '\n'

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

let remove_duplicates s =
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
  Buffer.contents buffer

let split_half s =
  let half_n = String.length s / 2 in
  [ String.sub s 0 half_n; String.sub s half_n half_n ]
