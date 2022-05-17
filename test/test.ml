(**Testing method: What our group implemented throughout this project to
   ensure correctness is different from traditional, case-by-case Ounit
   Testing. Considering the multitude of choices that a player can
   choose to make during the playing process, we as developers decided
   to put ourselves in the shoes of players and play-tested our program
   incrementally as we code. Our test cases here may not be the most
   comprehensive, since Monopoly is a game with infinite possibilities,
   we have to manually test the boundary possibilities.*)

open OUnit2
open Game
open Monopoly
open Player
open Yojson

let map =
  Monopoly.generate_map (Yojson.Basic.from_file "data/map_info.json")

let rec num_spaces num =
  match num with
  | 0 -> ""
  | _ -> " " ^ num_spaces (num - 1)

let print_special_place place =
  let name = Monopoly.name_of_place place in
  if name = "COMMUNITY CHEST" then
    [
      "|             |";
      "|COMMUNITY    |";
      "|CHEST        |";
      "|             |";
    ]
  else
    let space = 13 - String.length name in
    if space > 0 then
      [
        "|             |";
        "|" ^ name ^ num_spaces space ^ "|";
        "|             |";
        "|             |";
      ]
    else
      [
        "|             |";
        "|" ^ name ^ "|";
        "|             |";
        "|             |";
      ]

let list_map_help str =
  let space = 13 - String.length str in
  if space > 0 then "|" ^ str ^ num_spaces space ^ "|"
  else "|" ^ str ^ "|"

let print_nonspecial_place place =
  if Monopoly.is_real_estate place then
    let name_price_list =
      String.split_on_char ' '
        (Monopoly.name_of_place place
        ^ " "
        ^ Monopoly.color_of_place place
        ^ " $"
        ^ string_of_int (Monopoly.price_of_place place))
    in
    List.map list_map_help name_price_list
  else
    let name_price_list =
      String.split_on_char ' '
        (Monopoly.name_of_place place
        ^ " None" ^ " $"
        ^ string_of_int (Monopoly.price_of_place place))
    in
    List.map list_map_help name_price_list

let print_place place =
  if Monopoly.is_special place then print_special_place place
  else print_nonspecial_place place

let rec print_list list =
  match list with
  | [] -> print_string ""
  | h :: t ->
      print_endline h;
      print_list t

let list_fold_help_first str place_lines = str ^ List.nth place_lines 0
let list_fold_help_second str place_lines = str ^ List.nth place_lines 1
let list_fold_help_third str place_lines = str ^ List.nth place_lines 2
let list_fold_help_fourth str place_lines = str ^ List.nth place_lines 3

let list_filter_help_20_30 place =
  let id = Monopoly.id_of_place place in
  if id >= 20 && id <= 30 then Some place else None

let print_place_20_30 map =
  print_endline
    "_____________________________________________________________________________________________________________________________________________________________________";
  let place_20_30 = List.filter_map list_filter_help_20_30 map in
  let list_place_20_30 = List.map print_place place_20_30 in
  let first_line =
    List.fold_left list_fold_help_first "" list_place_20_30
  in
  let second_line =
    List.fold_left list_fold_help_second "" list_place_20_30
  in
  let third_line =
    List.fold_left list_fold_help_third "" list_place_20_30
  in
  let fourth_line =
    List.fold_left list_fold_help_fourth "" list_place_20_30
  in
  print_list [ first_line; second_line; third_line; fourth_line ];
  print_endline
    "_____________________________________________________________________________________________________________________________________________________________________"

let list_filter_help_0_10 place =
  let id = Monopoly.id_of_place place in
  if id >= 0 && id <= 10 then Some place else None

let print_place_0_10 map =
  print_endline
    "_____________________________________________________________________________________________________________________________________________________________________";
  let place_0_10 = List.filter_map list_filter_help_0_10 map in
  let list_place_0_10 = List.rev (List.map print_place place_0_10) in
  let first_line =
    List.fold_left list_fold_help_first "" list_place_0_10
  in
  let second_line =
    List.fold_left list_fold_help_second "" list_place_0_10
  in
  let third_line =
    List.fold_left list_fold_help_third "" list_place_0_10
  in
  let fourth_line =
    List.fold_left list_fold_help_fourth "" list_place_0_10
  in
  print_list [ first_line; second_line; third_line; fourth_line ];
  print_endline
    "_____________________________________________________________________________________________________________________________________________________________________"

let list_filter_help_11_19 place =
  let id = Monopoly.id_of_place place in
  if id >= 11 && id <= 19 then Some place else None

let list_filter_help_31_39 place =
  let id = Monopoly.id_of_place place in
  if id >= 31 && id <= 39 then Some place else None

let rec pair_places places_11_19 places_31_39 =
  match (places_11_19, places_31_39) with
  | [], [] -> []
  | h1 :: t1, h2 :: t2 -> [ [ h1; h2 ] ] @ pair_places t1 t2
  | _ -> failwith "unequal"

let print_pair init pair =
  let lines_11_19 = List.nth pair 0 in
  let lines_31_39 = List.nth pair 1 in
  let first_line =
    List.nth lines_11_19 0 ^ num_spaces 135 ^ List.nth lines_31_39 0
  in
  let second_line =
    List.nth lines_11_19 1 ^ num_spaces 135 ^ List.nth lines_31_39 1
  in
  let third_line =
    List.nth lines_11_19 2 ^ num_spaces 135 ^ List.nth lines_31_39 2
  in
  let fourth_line =
    List.nth lines_11_19 3 ^ num_spaces 135 ^ List.nth lines_31_39 3
  in
  init
  @ [
      "_______________" ^ num_spaces 135 ^ "_______________";
      first_line;
      second_line;
      third_line;
      fourth_line;
      "_______________" ^ num_spaces 135 ^ "_______________";
    ]

let print_pair_3 pair =
  let lines_11_19 = List.nth pair 0 in
  let lines_31_39 = List.nth pair 1 in
  let first_line =
    List.nth lines_11_19 0 ^ num_spaces 135 ^ List.nth lines_31_39 0
  in
  let second_line =
    List.nth lines_11_19 1 ^ num_spaces 135 ^ List.nth lines_31_39 1
  in
  let third_line =
    List.nth lines_11_19 2 ^ num_spaces 135 ^ List.nth lines_31_39 2
  in
  let fourth_line =
    List.nth lines_11_19 3 ^ num_spaces 27
    ^ "$$\\      $$\\  $$$$$$\\  $$\\   $$\\  $$$$$$\\  $$$$$$$\\   \
       $$$$$$\\  $$\\   $$\\     $$\\ " ^ num_spaces 27
    ^ List.nth lines_31_39 3
  in
  [
    "_______________" ^ num_spaces 135 ^ "_______________";
    first_line;
    second_line;
    third_line;
    fourth_line;
    "_______________" ^ num_spaces 27
    ^ "$$$\\    $$$ |$$  __$$\\ $$$\\  $$ |$$  __$$\\ $$  __$$\\ $$  \
       __$$\\ $$ |  \\$$\\   $$  |" ^ num_spaces 27 ^ "_______________";
  ]

let print_pair_4 pair =
  let lines_11_19 = List.nth pair 0 in
  let lines_31_39 = List.nth pair 1 in
  let first_line =
    List.nth lines_11_19 0 ^ num_spaces 27
    ^ "$$\\$$\\$$ $$ |$$ |  $$ |$$ $$\\$$ |$$ |  $$ |$$$$$$$  |$$ |  \
       $$ |$$ |    \\$$$$  /  " ^ num_spaces 27 ^ List.nth lines_31_39 0
  in
  let second_line =
    List.nth lines_11_19 1 ^ num_spaces 27
    ^ "$$ \\$$$  $$ |$$ |  $$ |$$ \\$$$$ |$$ |  $$ |$$  ____/ $$ |  $$ \
       |$$ |     \\$$  /   " ^ num_spaces 27 ^ List.nth lines_31_39 1
  in
  let third_line =
    List.nth lines_11_19 2 ^ num_spaces 27
    ^ "$$ |\\$  /$$ |$$ |  $$ |$$ |\\$$$ |$$ |  $$ |$$ |      $$ |  $$ \
       |$$ |      $$ |    " ^ num_spaces 27 ^ List.nth lines_31_39 2
  in
  let fourth_line =
    List.nth lines_11_19 3 ^ num_spaces 27
    ^ "$$ | \\_/ $$ | $$$$$$  |$$ | \\$$ | $$$$$$  |$$ |       $$$$$$  \
       |$$$$$$$$\\ $$ |    " ^ num_spaces 27 ^ List.nth lines_31_39 3
  in
  [
    "_______________" ^ num_spaces 27
    ^ "$$$$\\  $$$$ |$$ /  $$ |$$$$\\ $$ |$$ /  $$ |$$ |  $$ |$$ /  $$ \
       |$$ |   \\$$\\ $$  / " ^ num_spaces 27 ^ "_______________";
    first_line;
    second_line;
    third_line;
    fourth_line;
    "_______________" ^ num_spaces 27
    ^ "\\__|     \\__| \\______/ \\__|  \\__| \\______/ \\__|       \
       \\______/ \\________|\\__|    " ^ num_spaces 27
    ^ "_______________";
  ]

let print_place_11_19_31_39 map =
  let place_11_19 = List.filter_map list_filter_help_11_19 map in
  let place_31_39 = List.filter_map list_filter_help_31_39 map in
  let list_place_11_19 = List.rev (List.map print_place place_11_19) in
  let list_place_31_39 = List.map print_place place_31_39 in
  let list_pairs = pair_places list_place_11_19 list_place_31_39 in
  let up_pairs =
    [
      List.nth list_pairs 0;
      List.nth list_pairs 1;
      List.nth list_pairs 2;
    ]
  in
  let down_pairs =
    [
      List.nth list_pairs 5;
      List.nth list_pairs 6;
      List.nth list_pairs 7;
      List.nth list_pairs 8;
    ]
  in
  let up_lines = List.fold_left print_pair [] up_pairs in
  let line_3 = print_pair_3 (List.nth list_pairs 3) in
  let line_4 = print_pair_4 (List.nth list_pairs 4) in
  let down_lines = List.fold_left print_pair [] down_pairs in
  let lines = up_lines @ line_3 @ line_4 @ down_lines in
  print_list lines

let print_map map =
  print_place_20_30 map;
  print_place_11_19_31_39 map;
  print_place_0_10 map

let main () = print_map map
let () = main ()
