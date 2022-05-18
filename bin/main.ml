open Game

let chance_cards =
  Monopoly.chance_cards_from_json
    (Yojson.Basic.from_file "data/map_info.json")

let community_chest_cards =
  Monopoly.community_chest_cards_from_json
    (Yojson.Basic.from_file "data/map_info.json")

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

(*----------------------print_map---------------------------------------------*)

let rec make_players players num map =
  match num with
  | 0 ->
      print_endline "Welcome to Monopoly!";
      players
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Enter your player name:";
      let player_name = read_line () in
      let new_player = Player.make_new_player player_name map in
      print_endline
        "Your new player is initiated with the following information: ";
      print_endline (Player.player_to_string new_player);
      make_players (new_player :: players) (num - 1) map

let rec replace_player lst player =
  match lst with
  | [] -> []
  | h :: t ->
      if Player.name_of_player h = Player.name_of_player player then
        player :: replace_player t player
      else h :: replace_player t player

let rec remove_player lst player =
  match lst with
  | [] -> []
  | h :: t ->
      if Player.name_of_player h = Player.name_of_player player then
        remove_player t player
      else h :: remove_player t player

let rec auction map players place price number original_players =
  List.map Player.player_to_string players |> List.iter print_endline;
  print_int number;
  match players with
  | [] -> (map, original_players)
  | [ _ ] ->
      let final_player =
        Player.auction_place (List.nth players number) place price
      in
      print_endline
        ("\n"
        ^ Player.name_of_player final_player
        ^ ", you have won the auction! The property is now sold to you \
           for the price of " ^ string_of_int price);
      map :=
        Monopoly.update_map
          (Monopoly.buy_place
             (Player.name_of_player final_player)
             place)
          !map;
      print_endline
        ("Your new player information is: \n"
        ^ Player.player_to_string final_player);
      (map, replace_player original_players final_player)
  | _ -> (
      print_endline
        ("\nProperty information in auction:\n"
        ^ Monopoly.place_to_string place);
      print_endline
        ("\n"
        ^ Player.name_of_player (List.nth players number)
        ^ ", please enter your action in this auction: (Add 1) (Add \
           10) (Add 100) (Quit)");
      let action = read_line () in
      match action with
      | "Add 1" ->
          let bet = price + 1 in
          if bet <= Player.money_of_player (List.nth players number)
          then (
            print_endline
              ("\nYou added 1. The price is now: " ^ string_of_int bet);
            if number >= List.length players - 1 then
              auction map players place bet 0 original_players
            else
              auction map players place bet (number + 1)
                original_players)
          else (
            print_endline "\nYou cannot add 1. You should enter Quit";
            auction map players place price number original_players)
      | "Add 10" ->
          let bet = price + 10 in
          if bet <= Player.money_of_player (List.nth players number)
          then (
            print_endline
              ("\nYou added 10. The price is now: " ^ string_of_int bet);
            if number >= List.length players - 1 then
              auction map players place bet 0 original_players
            else
              auction map players place bet (number + 1)
                original_players)
          else (
            print_endline "\nYou cannot add 10. Please choose again.";
            auction map players place price number original_players)
      | "Add 100" ->
          let bet = price + 100 in
          if bet <= Player.money_of_player (List.nth players number)
          then (
            print_endline
              ("\nYou added 100. The price is now: " ^ string_of_int bet);
            if number >= List.length players - 1 then
              auction map players place bet 0 original_players
            else
              auction map players place bet (number + 1)
                original_players)
          else (
            print_endline "\nYou cannot add 100. Please choose again.";
            auction map players place price number original_players)
      | "Quit" ->
          print_endline "\nYou have quited the auction.";
          let players_left =
            remove_player players (List.nth players number)
          in
          if not (number = 0) then
            auction map players_left place price (number - 1)
              original_players
          else
            auction map players_left place price number original_players
      | _ ->
          print_endline "\nYour input is not valid. Please try again.";
          auction map players place price number original_players)

let rec action_on_nonspecial player map players =
  let location = Player.location_of_player player in
  let player_name = Player.name_of_player player in
  print_endline
    ("\n" ^ player_name
   ^ ", please choose from the following: (Buy) (Build House) (Sell \
      House) (Build Hotel) (Sell Hotel) (Auction) (Trade) (See Map) \
      (End)");
  let action = read_line () in
  match action with
  | "Buy" -> (
      try
        let final_player = Player.buy_place player location in
        map :=
          Monopoly.update_map
            (Monopoly.buy_place
               (Player.name_of_player final_player)
               location)
            !map;
        print_endline "\nYou bought the place!";
        print_endline "\nYour new player information:";
        print_endline (Player.player_to_string final_player);
        players := replace_player !players final_player;
        action_on_nonspecial final_player map players
      with _ ->
        print_endline
          "You cannot buy this place for it is already owned.";
        action_on_nonspecial player map players)
  | "Auction" ->
      if Monopoly.owner_of_place location = "" then (
        print_endline "\nYou chose to auction this land.";
        let map_players_pair =
          auction map !players location 0 0 !players
        in
        let new_map =
          match map_players_pair with
          | h, t -> h
        in
        let updated_players =
          ref
            (match map_players_pair with
            | h, t -> t)
        in
        let final_player =
          Player.find_player
            (Player.name_of_player player)
            !updated_players
        in
        action_on_nonspecial final_player new_map updated_players)
      else (
        print_endline
          "You cannot auction this land for this land already has an \
           owner.";
        action_on_nonspecial player map players)
  | "Build House" ->
      if Player.can_add_house player location !map then (
        let final_player = Player.add_house player location in
        let new_location = Player.location_of_player final_player in
        map := Monopoly.update_map new_location !map;
        print_endline
          ("\nYou built a house on "
          ^ Monopoly.name_of_place new_location
          ^ "!");
        print_endline "\nYour new player information:";
        print_endline (Player.player_to_string final_player);
        players := replace_player !players final_player;
        action_on_nonspecial final_player map players)
      else (
        print_endline "\nYou cannot build a house.";
        action_on_nonspecial player map players)
  | "Sell House" ->
      if Player.can_sell_house player location then (
        let final_player = Player.sell_house player location in
        let new_location = Player.location_of_player final_player in
        map := Monopoly.update_map new_location !map;
        print_endline
          ("\nYou sold a house on "
          ^ Monopoly.name_of_place new_location
          ^ "!");
        print_endline "\nYour new player information:";
        print_endline (Player.player_to_string final_player);
        players := replace_player !players final_player;
        action_on_nonspecial final_player map players)
      else (
        print_endline "\nYou cannot sell a house.";
        action_on_nonspecial player map players)
  | "Build Hotel" ->
      if Player.can_add_hotel player location !map then (
        let final_player = Player.add_hotel player location in
        let new_location = Player.location_of_player final_player in
        map := Monopoly.update_map new_location !map;
        print_endline
          ("\nYou built a hotel on "
          ^ Monopoly.name_of_place new_location
          ^ "!");
        print_endline "\nYour new player information:";
        print_endline (Player.player_to_string final_player);
        players := replace_player !players final_player;
        action_on_nonspecial final_player map players)
      else (
        print_endline "\nYou cannot build a hotel.";
        action_on_nonspecial player map players)
  | "Sell Hotel" ->
      if Player.can_sell_hotel player location then (
        let final_player = Player.sell_hotel player location in
        let new_location = Player.location_of_player final_player in
        map := Monopoly.update_map new_location !map;
        print_endline
          ("\nYou sold a hotel on "
          ^ Monopoly.name_of_place new_location
          ^ "!");
        print_endline "\nYour new player information:";
        print_endline (Player.player_to_string final_player);
        players := replace_player !players final_player;
        action_on_nonspecial final_player map players)
      else (
        print_endline "\nYou cannot sell a hotel.";
        action_on_nonspecial player map players)
  | "Trade" ->
      print_endline "\nYou chose to trade with another player.";
      print_endline
        "\nEnter the player's name with whom you want to trade with:";
      (* let target_player = read_line () in *)
      print_endline "\nYour new player information:";
      print_endline (Player.player_to_string player);
      action_on_nonspecial player map players
  | "See Map" ->
      print_map !map;
      action_on_nonspecial player map players
  | "End" -> players
  | _ ->
      print_endline
        "\nYou did not enter a valid input. Please try again.";
      action_on_nonspecial player map players

let pay_rent player map players dice =
  let location = Player.location_of_player player in
  let owner_name = Monopoly.owner_of_place location in
  if owner_name <> Player.name_of_player player && owner_name <> "" then (
    let player_pair =
      Player.pay_rent player
        (Monopoly.rent_of_place location !map dice)
        (Player.find_player owner_name !players)
    in
    let owner =
      match player_pair with
      | h, t -> t
    in
    let payer =
      match player_pair with
      | h, t -> h
    in
    players := replace_player (replace_player !players owner) payer;
    print_endline
      ("\nYou paid the rent of "
      ^ string_of_int (Monopoly.rent_of_place location !map dice));
    print_endline
      ("\nYour new player information is "
      ^ Player.player_to_string payer);
    action_on_nonspecial payer map players)
  else action_on_nonspecial player map players

let pay_double_rent player map players dice =
  let location = Player.location_of_player player in
  let owner_name = Monopoly.owner_of_place location in
  if owner_name <> Player.name_of_player player && owner_name <> "" then (
    let player_pair =
      Player.pay_rent player
        (Monopoly.rent_of_place location !map dice * 2)
        (Player.find_player owner_name !players)
    in
    let owner =
      match player_pair with
      | h, t -> t
    in
    let payer =
      match player_pair with
      | h, t -> h
    in
    players := replace_player (replace_player !players owner) payer;
    print_endline
      ("\nYou paid the rent of "
      ^ string_of_int (Monopoly.rent_of_place location !map dice));
    print_endline
      ("\nYour new player information is "
      ^ Player.player_to_string payer);
    action_on_nonspecial payer map players)
  else action_on_nonspecial player map players

let rec each_player_get_50 players player =
  match !players with
  | [] -> ()
  | h :: t ->
      if h = player then each_player_get_50 (ref t) player
      else
        let new_player = Player.get_money player 50 in
        players := replace_player !players new_player

let rec each_player_pay_10 players player =
  match !players with
  | [] -> ()
  | h :: t ->
      if h = player then each_player_pay_10 (ref t) player
      else
        let new_player = Player.get_money player (-10) in
        players := replace_player !players new_player

let rec action_on_special player map players =
  let location = Player.location_of_player player in
  let name_of_place = Monopoly.name_of_place location in
  let current_id = Monopoly.id_of_place location in
  print_endline ("You are at: " ^ name_of_place);
  if
    name_of_place = "GO" || name_of_place = "JAIL"
    || name_of_place = "FREE PARKING"
  then
    print_endline
      ("You arrived at " ^ name_of_place ^ ", nothing happens.")
  else if name_of_place = "CHANCE" then (
    print_endline "You drew a chance card: ";
    Random.self_init ();
    let random = Random.int 15 in
    let card = Monopoly.draw_chance_card random chance_cards in
    print_endline card;
    match card with
    | "Advance to Boardwalk." ->
        let step = 39 - current_id in
        let moved_player = Player.move player step !map in
        players := replace_player !players moved_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string moved_player)
    | "Advance to Go (Collect $200)." ->
        let step = ref (0 - current_id) in
        if !step < 0 then step := !step + 40 else step := !step;
        let moved_player = Player.move player !step !map in
        players := replace_player !players moved_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string moved_player)
    | "Advance to Illinois Avenue. If you pass Go, collect $200." ->
        let step = ref (24 - current_id) in
        if !step < 0 then step := !step + 40 else step := !step;
        let moved_player = Player.move player !step !map in
        players := replace_player !players moved_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string moved_player)
    | "Advance to St. Charles Place. If you pass Go, collect $200." ->
        let step = ref (11 - current_id) in
        if !step < 0 then step := !step + 40 else step := !step;
        let moved_player = Player.move player !step !map in
        players := replace_player !players moved_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string moved_player)
    | "Advance to the nearest Railroad. If unowned, you may buy it \
       from the bank. If owned, pay the owner twice the rental to \
       which they are otherwise entitled." ->
        let step = ref 0 in
        let step1 = ref (5 - current_id) in
        if !step1 < 0 then step1 := !step1 + 40 else step1 := !step1;
        let step2 = ref (15 - current_id) in
        if !step2 < 0 then step2 := !step2 + 40 else step2 := !step2;
        let step3 = ref (25 - current_id) in
        if !step3 < 0 then step3 := !step3 + 40 else step3 := !step3;
        let step4 = ref (35 - current_id) in
        if !step4 < 0 then step4 := !step4 + 40 else step4 := !step4;
        if step1 < step2 && step1 < step3 && step1 < step4 then
          step := !step1
        else if step2 < step1 && step2 < step3 && step2 < step4 then
          step := !step2
        else if step3 < step1 && step3 < step2 && step3 < step4 then
          step := !step3
        else step := !step4;
        let moved_player = Player.move player !step !map in
        players := replace_player !players moved_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string moved_player);
        let location = Player.location_of_player moved_player in
        if Monopoly.owner_of_place location = "No Owner" then (
          print_endline "Would you like to buy this place? (Yes) (No)";
          let response = read_line () in
          match response with
          | "Yes" -> (
              try
                let final_player = Player.buy_place player location in
                map :=
                  Monopoly.update_map
                    (Monopoly.buy_place
                       (Player.name_of_player final_player)
                       location)
                    !map;
                print_endline "\nYou bought the place!";
                print_endline "\nYour new player information:";
                print_endline (Player.player_to_string final_player);
                players := replace_player !players final_player
              with _ -> print_endline "You cannot buy this place.")
          | _ -> print_string "")
        else players := !(pay_double_rent moved_player map players 0)
    | "Advance to the nearest Utility. If unowned, you may buy it from \
       the Bank. If owned, throw dice and pay the owner a total ten \
       times amount thrown." ->
        let step = ref 0 in
        let step1 = ref (28 - current_id) in
        if !step1 < 0 then step1 := !step1 + 40 else step1 := !step1;
        let step2 = ref (12 - current_id) in
        if !step2 < 0 then step2 := !step2 + 40 else step2 := !step2;
        if step1 < step2 then step := !step1 else step := !step2;
        let moved_player = Player.move player !step !map in
        players := replace_player !players moved_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string moved_player);
        let location = Player.location_of_player moved_player in
        if Monopoly.owner_of_place location = "No Owner" then (
          print_endline "Would you like to buy this place? (Yes) (No)";
          let response = read_line () in
          match response with
          | "Yes" -> (
              try
                let final_player = Player.buy_place player location in
                map :=
                  Monopoly.update_map
                    (Monopoly.buy_place
                       (Player.name_of_player final_player)
                       location)
                    !map;
                print_endline "\nYou bought the place!";
                print_endline "\nYour new player information:";
                print_endline (Player.player_to_string final_player);
                players := replace_player !players final_player
              with _ -> print_endline "You cannot buy this place.")
          | _ -> print_string "")
        else
          let correct_roll = ref false in
          while not !correct_roll do
            print_string "\nEnter 'Roll' to roll the dice:\n";
            let player_input = read_line () in
            if player_input = "Roll" then correct_roll := true
            else print_endline "\nPlease enter exacly 'Roll' to roll."
          done;
          Random.self_init ();
          let dice1 = Random.int 6 + 1 in
          let dice2 = Random.int 6 + 1 in
          print_endline
            ("\nYour first dice: " ^ string_of_int dice1
           ^ "; your second dice: " ^ string_of_int dice2);
          players :=
            !(pay_double_rent moved_player map players (dice1 + dice2))
    | "Bank pays you dividend of $50." ->
        let new_player = Player.get_money player 50 in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "Get Out of Jail Free." ->
        let new_player = Player.add_jail_card player in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "Go Back 3 Spaces." ->
        let new_player = Player.back_3_space player !map in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "Go to Jail. Go directly to Jail, do not pass Go, do not collect \
       $200." ->
        let new_player = Player.go_to_jail player !map in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
        (* To be implemented with more rules about jail *)
    | "Make general repairs on all your property. For each house, pay \
       $25. For each hotel pay $100." ->
        let own = Player.get_own player in
        let house_num = Player.get_house_num own in
        let hotel_num = Player.get_hotel_num own in
        let paid = (-25 * house_num) + (-50 * hotel_num) in
        let new_player = Player.get_money player paid in
        print_endline ("You have paid $" ^ string_of_int paid);
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "Speeding fine $15." ->
        let new_player = Player.get_money player (-15) in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "Take a trip to Reading Railroad. If you pass Go, collect $200."
      ->
        let step = ref (5 - current_id) in
        if !step < 0 then step := !step + 40 else step := !step;
        let moved_player = Player.move player !step !map in
        players := replace_player !players moved_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string moved_player)
    | "You have been elected Chairman of the Board. Pay each player \
       $50." ->
        let player_num = List.length !players in
        let new_player =
          Player.get_money player ((player_num - 1) * -50)
        in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player);
        each_player_get_50 players new_player
    | _ -> failwith "no such chance card")
  else if name_of_place = "COMMUNITY CHEST" then (
    print_endline "You drew a community chest card: ";
    Random.self_init ();
    let random = Random.int 16 in
    let card =
      Monopoly.draw_community_chest_card random community_chest_cards
    in
    print_endline card;
    match card with
    | "Advance to Go (Collect $200)." ->
        let step = ref (0 - current_id) in
        if !step < 0 then step := !step + 40 else step := !step;
        let moved_player = Player.move player !step !map in
        players := replace_player !players moved_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string moved_player)
    | "Bank error in your favor. Collect $200." ->
        let new_player = Player.get_money player 200 in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "Doctor's fee. Pay $50." ->
        let new_player = Player.get_money player (-50) in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "From sale of stock you get $50." ->
        let new_player = Player.get_money player 50 in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "Get Out of Jail Free." ->
        let new_player = Player.add_jail_card player in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "Go to Jail. Go directly to Jail, do not pass Go, do not collect \
       $200." ->
        let new_player = Player.go_to_jail player !map in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
        (* To be implemented with more rules about jail *)
    | "Holiday fund matures. Receive $100." ->
        let new_player = Player.get_money player 100 in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "Income tax refund. Collect $20." ->
        let new_player = Player.get_money player 20 in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "It is your birthday. Collect $10 from every player." ->
        let player_num = List.length !players in
        let new_player =
          Player.get_money player ((player_num - 1) * 10)
        in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player);
        each_player_pay_10 players new_player
    | "Life insurance matures. Collect $100." ->
        let new_player = Player.get_money player 100 in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "Pay hospital fees of $100." ->
        let new_player = Player.get_money player (-100) in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "Pay school fees of $50." ->
        let new_player = Player.get_money player (-50) in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "Receive $25 consultancy fee." ->
        let new_player = Player.get_money player 25 in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "You are assessed for street repair. $40 per house. $115 per \
       hotel." ->
        let own = Player.get_own player in
        let house_num = Player.get_house_num own in
        let hotel_num = Player.get_hotel_num own in
        let paid = (-40 * house_num) + (-115 * hotel_num) in
        let new_player = Player.get_money player paid in
        print_endline ("You have paid $" ^ string_of_int paid);
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "You have won second price in a beauty contest. Collect $10." ->
        let new_player = Player.get_money player 10 in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | "You inherit $100." ->
        let new_player = Player.get_money player 100 in
        players := replace_player !players new_player;
        print_endline "\nYour new player information is:";
        print_endline (Player.player_to_string new_player)
    | _ -> failwith "no such community chest card")
  else if name_of_place = "INCOME TAX" then (
    let new_player = Player.get_money player (-200) in
    players := replace_player !players new_player;
    print_endline "You paid $200 for Income Tax. \n";
    print_endline "\nYour new player information is:";
    print_endline (Player.player_to_string new_player))
  else if name_of_place = "LUXURY TAX" then (
    let new_player = Player.get_money player (-100) in
    players := replace_player !players new_player;
    print_endline "You paid $100 for Luxury Tax. \n";
    print_endline "\nYour new player information is:";
    print_endline (Player.player_to_string new_player))
  else if name_of_place = "GO TO JAIL" then (
    let new_player = Player.go_to_jail player !map in
    players := replace_player !players new_player;
    print_endline "\nYour new player information is:";
    print_endline (Player.player_to_string new_player)
    (* To be implemented with more rules about jail *))
  else failwith "no such special";
  let player_name = Player.name_of_player player in
  print_endline
    ("\n" ^ player_name
   ^ ", please choose from the following: (Trade) (See Map) (End)");
  let action = read_line () in
  match action with
  | "Trade" ->
      print_endline "You chose to trade with another player.";
      action_on_special player map players
  | "See Map" ->
      print_map !map;
      action_on_special player map players
  | "End" -> ()
  | _ ->
      print_endline "You did not enter a valid input. Please try again.";
      action_on_special player map players

let round player players map =
  print_endline
    ("\nNow it's " ^ Player.name_of_player player ^ "'s round. ");

  if Player.is_in_jail player = true then (
    let dice_rolls = [] in
    let correct_input = ref false in
    while not !correct_input do
      print_endline
        "\n\
         You are in jail. You can spend 50 dollars to get out, keep on \
         rolling dice until you get a double, or use a jail card.";
      print_endline
        "\nChoose from the following: (Pay) (Roll) (Use Card)";
      let input = read_line () in
      match input with
      | "Pay" ->
          if Player.money_of_player player >= 50 then begin
            players :=
              replace_player !players (Player.pay_jail_fee player);
            print_endline
              "You paid 50$ to get out. \n\
              \ Here is your new player information: ";
            print_endline (Player.player_to_string player)
          end
          else print_endline "You do not have enough to pay jail fees."
      | "Roll" -> ()
      | "Use Card" ->
          if Player.num_jail_cards player > 0 then
            players :=
              replace_player !players (Player.use_jail_card player)
          else
            print_endline "You do not have enough jail cards to do so."
      | _ -> print_endline "Please recheck your input."
    done;
    ref (replace_player !players (Player.update_jail_round player)))
  else
    let correct_roll = ref false in
    while not !correct_roll do
      print_string "\nEnter 'Roll' to roll the dice:\n";
      let player_input = read_line () in
      if player_input = "Roll" then correct_roll := true
      else print_endline "\nPlease enter exacly 'Roll' to roll."
    done;
    Random.self_init ();
    let dice1 = Random.int 6 + 1 in
    let dice2 = Random.int 6 + 1 in
    print_endline
      ("\nYour first dice: " ^ string_of_int dice1
     ^ "; your second dice: " ^ string_of_int dice2);
    let moved_player = Player.move player (dice1 + dice2) !map in
    players := replace_player !players moved_player;
    print_endline "\nYour new player information is:";
    print_endline (Player.player_to_string moved_player);
    let location = Player.location_of_player moved_player in
    if Monopoly.is_special location then (
      action_on_special moved_player map players;
      players)
    else pay_rent moved_player map players (dice1 + dice2)

let rec cycle_aux players ind map =
  round (List.nth !players ind) players map

let rec progress players map acc =
  print_endline "\nEnter 'End Game' to end game: ";
  let end_game = read_line () in
  if end_game = "End Game" then
    print_endline "\nSee you again next time!"
  else if acc = List.length !players - 1 then
    progress (cycle_aux players 0 map) map 0
  else progress (cycle_aux players (acc + 1) map) map (acc + 1)

let main () =
  let map =
    ref
      (Monopoly.generate_map
         (Yojson.Basic.from_file "data/map_info.json"))
  in
  print_endline "How many players do you need?";
  let player_num = int_of_string (read_line ()) in
  let players = ref (make_players [] player_num !map) in
  progress players map (-1)

let () = main ()