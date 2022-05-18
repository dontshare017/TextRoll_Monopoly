open Monopoly

type player = {
  in_jail : bool;
  jail_round : int;
  name : string;
  location : Monopoly.place;
  money : int;
  own : Monopoly.place list;
  jail_card : int;
  mortgage : Monopoly.place list;
  bankrupted : bool;
  double_num : int;
}

type players = player list

let players = []

let make_new_player name map =
  {
    in_jail = false;
    jail_round = 0;
    name;
    location = Monopoly.start_place map;
    money = 1500;
    own = [];
    jail_card = 0;
    mortgage = [];
    bankrupted = false;
    double_num = 0;
  }

let name_of_player player = player.name
let location_of_player player = player.location

let player_to_string player =
  "[Name]: " ^ player.name ^ "\n" ^ "[Location]: "
  ^ Monopoly.place_to_string player.location
  ^ "\n" ^ "[Money]: "
  ^ string_of_int player.money
  (* The list of owned houses are missing here. Needs to be implemented
     later.*)
  ^ "\n"
  ^ "[Jail Cards]: "
  ^ string_of_int player.jail_card
  ^ "\n" ^ "[Bankruptcy Status]: "
  (* This list of mortgaged houses are missing here. Needs to be
     implemented later*)
  ^ string_of_bool player.bankrupted
  ^ "\n"

let rec find_player name = function
  | [] -> failwith "No Such Player"
  | h :: t -> if h.name = name then h else find_player name t

let move player dice map =
  let old_id = Monopoly.id_of_place player.location in
  let new_id = (old_id + dice) mod 40 in
  if new_id > old_id then
    { player with location = Monopoly.place_of_id map new_id }
  else
    {
      player with
      location = Monopoly.place_of_id map new_id;
      money = player.money + 200;
    }

let buy_place player place =
  let owner = Monopoly.owner_of_place place in
  let price = Monopoly.price_of_place place in
  if owner = "No Owner" || owner <> "" then
    failwith "You cannot buy this place."
  else if player.money >= price then
    {
      player with
      money = player.money - price;
      own = Monopoly.buy_place player.name place :: player.own;
      location = Monopoly.buy_place player.name place;
    }
  else failwith "No Money"
(* Change this later. It should tell the player he/she does not have
   enough money.*)

let make_bankrupt p = { p with bankrupted = true; own = [] }

let rec return_property p map =
  match map with
  | [] -> []
  | h :: t ->
      if List.mem h p.own then
        Monopoly.reset_owner h :: return_property p t
      else h :: return_property p t

let auction_place player place price =
  let owner = Monopoly.owner_of_place place in
  if owner = "No Owner" || owner <> "" then
    failwith "You cannot auction this place."
  else if price > player.money then make_bankrupt player
  else
    {
      player with
      money = player.money - price;
      own = Monopoly.buy_place player.name place :: player.own;
      location = Monopoly.buy_place player.name place;
    }

let mortgage_place player place =
  if List.mem place player.own && not (List.mem place player.mortgage)
  then
    {
      player with
      mortgage = place :: player.mortgage;
      money = player.money + Monopoly.mortgage_of_place place;
    }
  else player

let is_bankrupt p = p.bankrupted

let pay_rent player rent player2 =
  if player.money >= rent then
    ( { player with money = player.money - rent },
      { player2 with money = player2.money + rent } )
  else (make_bankrupt player, player2)
(* Change this later. The player should try to sell assets to get money
   to pay player2. If the player does not have enough assets to sell,
   the player is bankrupted and player2 should have all the assets of
   player1.*)

let can_add_house player place map =
  let house_num = Monopoly.house_num_of_place place in
  let hotel_num = Monopoly.hotel_num_of_place place in
  house_num < 4 && hotel_num = 0
  && Monopoly.owner_of_place place = player.name
  && Monopoly.check_color_set player.name
       (Monopoly.find_color_set place map)

let add_house player place =
  {
    player with
    money = player.money - Monopoly.house_cost_of_place place;
    own = Monopoly.update_map (Monopoly.add_house place) player.own;
    location = Monopoly.add_house place;
  }

let can_sell_house player place =
  let house_num = Monopoly.house_num_of_place place in
  house_num > 0 && Monopoly.owner_of_place place = player.name

let sell_house player place =
  {
    player with
    money = player.money + Monopoly.house_sell_of_place place;
    own = Monopoly.update_map (Monopoly.sell_house place) player.own;
    location = Monopoly.sell_house place;
  }

let can_add_hotel player place map =
  let house_num = Monopoly.house_num_of_place place in
  let hotel_num = Monopoly.hotel_num_of_place place in
  house_num = 4 && hotel_num = 0
  && List.mem place player.own
  && Monopoly.check_color_set player.name
       (Monopoly.find_color_set place map)

let add_hotel player place =
  {
    player with
    money = player.money - Monopoly.hotel_cost_of_place place;
    own = Monopoly.update_map (Monopoly.add_hotel place) player.own;
    location = Monopoly.add_hotel place;
  }

let can_sell_hotel player place =
  let hotel_num = Monopoly.hotel_num_of_place place in
  hotel_num = 1 && List.mem place player.own

let sell_hotel player place =
  {
    player with
    money = player.money + Monopoly.hotel_sell_of_place place;
    own = Monopoly.update_map (Monopoly.sell_hotel place) player.own;
    location = Monopoly.sell_hotel place;
  }

(* [roll_dice player dice_lst] is the dice result for the [player].
   [dice_lst]'s length is how many dice rolls the [player] has done. If
   [player] gets a double dice, meaning two dice of the same value, dice
   will be automatically rolled again. If [player] rolls three double in
   a row, a 0 will be returned.*)

let rec roll_dice dice_lst =
  let dice1 = Random.int 6 + 1 in
  let dice2 = Random.int 6 + 1 in
  if List.length dice_lst < 2 then
    if dice1 <> dice2 then dice1 + dice2 else roll_dice (1 :: dice_lst)
  else if dice1 = dice2 then 0
  else dice1 + dice2

let money_of_player player = player.money

let get_money player amount =
  { player with money = player.money + amount }

let add_jail_card player =
  { player with jail_card = player.jail_card + 1 }

let back_3_space player map =
  let old_id = Monopoly.id_of_place player.location in
  let new_id = (old_id - 3) mod 40 in
  { player with location = Monopoly.place_of_id map new_id }

let go_to_jail player map =
  { player with location = Monopoly.place_of_id map 10; in_jail = true }

let get_own player = player.own

let rec get_house_num own =
  match own with
  | [] -> 0
  | h :: t ->
      if Monopoly.is_real_estate h then
        if Monopoly.hotel_num_of_place h = 1 then 0 + get_house_num t
        else Monopoly.house_num_of_place h + get_house_num t
      else get_house_num t

let rec get_hotel_num own =
  match own with
  | [] -> 0
  | h :: t ->
      if Monopoly.is_real_estate h then
        Monopoly.hotel_num_of_place h + get_hotel_num t
      else get_hotel_num t

let update_jail_round p =
  if p.jail_round = 2 then { p with jail_round = 0; in_jail = false }
  else { p with jail_round = p.jail_round + 1 }

let is_in_jail p = p.in_jail = true
let num_jail_cards p = p.jail_card

let use_jail_card p =
  {
    p with
    jail_card = p.jail_card - 1;
    jail_round = 0;
    in_jail = false;
  }

let pay_jail_fee p =
  { p with money = p.money - 50; jail_round = 0; in_jail = false }

let out_of_jail p = { p with jail_round = 0; in_jail = false }
