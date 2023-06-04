open Printf
open Lib

let input_path = "./inputs/2.txt"

type hand_shape = Rock | Scissors | Papper
type round_outcome = Win | Lose | Draw

let get_winner_shape = function
  | Scissors -> Rock
  | Papper -> Scissors
  | Rock -> Papper

let get_lose_shape = function
  | Rock -> Scissors
  | Scissors -> Papper
  | Papper -> Rock

let get_hand_shape shape expected_round_outcome =
  match expected_round_outcome with
  | Draw -> shape
  | Win -> get_winner_shape shape
  | Lose -> get_lose_shape shape

let hand_shape_score = function Rock -> 1 | Papper -> 2 | Scissors -> 3
let round_outcome_score = function Win -> 6 | Draw -> 3 | Lose -> 0

let parse_opponent_column = function
  | "A" -> Rock
  | "B" -> Papper
  | "C" -> Scissors
  | _ -> failwith "invalid opponent column"

let parse_my_column = function
  | "X" -> Lose
  | "Y" -> Draw
  | "Z" -> Win
  | _ -> failwith "invalid my column"

let play_round my_shape opponent_shape =
  match (my_shape, opponent_shape) with
  | _ when my_shape = opponent_shape -> Draw
  | _ -> if get_winner_shape opponent_shape = my_shape then Win else Lose

let parse_row row =
  let splitted = String.split_on_char ' ' row in
  match List.length splitted with
  | length when length != 2 -> failwith @@ sprintf "failed parsing row: %s" row
  | _ ->
      ( parse_opponent_column @@ List.nth splitted 0,
        parse_my_column @@ List.nth splitted 1 )

let parse_rows rows =
  rows
  |> List.map (fun row ->
         let opponent_shape, expected_round_outcome = parse_row row in
         let my_shape = get_hand_shape opponent_shape expected_round_outcome in
         let round_outcome = play_round my_shape opponent_shape in
         round_outcome_score round_outcome + hand_shape_score my_shape)
  |> List.fold_left ( + ) 0

let () =
  let score = read_lines input_path |> parse_rows in
  printf "%d\n" score
