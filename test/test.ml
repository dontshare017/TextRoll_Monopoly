(**Testing method: What our group implemented throughout this project to
   ensure correctness is different from traditional, case-by-case Ounit
   Testing. Considering the multitude of choices that a player can
   choose to make during the playing process, we as developers decided
   to put ourselves in the shoes of players and play-test our program
   incrementally as we code. Our test cases here may not be the most
   comprehensive, since Monopoly is a game with infinite possibilities.
   In our manual testing process, however, we tested whether our main
   game logic stores data and behaves correctly, whether major features
   available to the players are able to be interacted with correctly,
   and if our display of in-game information is user-friendly. In our
   test suite, we will focus more on tetsing individual functions in our
   backend logics, since it's the executable main file in bin folder
   that connects the bulk of our backend logics together. We believe
   ensuring the correctness of our individual functions makes writing
   in-game logic easier.*)

open OUnit2
open Game
open Monopoly
open Player
open Yojson

let map =
  Monopoly.generate_map (Yojson.Basic.from_file "data/map_info.json")

let chance_cards =
  Monopoly.chance_cards_from_json
    (Yojson.Basic.from_file "data/map_info.json")

let community_chest_cards =
  Monopoly.community_chest_cards_from_json
    (Yojson.Basic.from_file "data/map_info.json")

let player = Player.make_new_player "test_player" map
let player' = Player.make_new_player "test_player'" map
let players = [ player; player' ]

let start_place_test name map expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Monopoly.name_of_place (Monopoly.start_place map))

let place_of_id_test name map id expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.place_of_id map id)

let mortgage_of_place_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.mortgage_of_place place)

let color_of_place_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.color_of_place place)

let id_of_place_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.id_of_place place)

let is_special_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.is_special place)

let is_real_estate_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.is_real_estate place)

let price_of_place_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.price_of_place place)

let owner_of_place_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.owner_of_place place)

let num_electric_own_test name owner map expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.num_electric_own owner map)

let num_railroad_own_test name owner map expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.num_railroad_own owner map)

let find_color_set_test name place map expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (List.map Monopoly.name_of_place
       (Monopoly.find_color_set place map))

let check_color_set_test name player color_set expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Monopoly.check_color_set player color_set)

let rent_of_place_test name place map dice expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.rent_of_place place map dice)

let buy_place_test name player place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Monopoly.owner_of_place (Monopoly.buy_place player place))

let house_num_of_place_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.house_num_of_place place)

let hotel_num_of_place_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.hotel_num_of_place place)

let house_cost_of_place_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.house_cost_of_place place)

let add_house_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Monopoly.house_num_of_place (Monopoly.add_house place))

let house_sell_of_place_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.house_sell_of_place place)

let sell_house_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Monopoly.house_num_of_place (Monopoly.sell_house place))

let hotel_cost_of_place_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Monopoly.hotel_cost_of_place place)

let add_hotel_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Monopoly.hotel_num_of_place (Monopoly.add_hotel place))

let sell_hotel_test name place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Monopoly.hotel_num_of_place (Monopoly.sell_hotel place))

let draw_chance_card_test name index chance_cards expected_output : test
    =
  name >:: fun _ ->
  assert_equal expected_output
    (Monopoly.draw_chance_card index chance_cards)

let draw_community_chest_card_test
    name
    index
    community_chest_cards
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Monopoly.draw_community_chest_card index community_chest_cards)

let monopoly_tests =
  [
    start_place_test "start_place_test" map "GO";
    place_of_id_test "place_of_id_test KENTUCKY AVENUE 21" map 21
      (List.nth map 21);
    mortgage_of_place_test "mortgage_of_place_test 110 KENTUCKY AVENUE"
      (List.nth map 21) 110;
    color_of_place_test "color_of_place_test red KENTUCKY AVENUE"
      (List.nth map 21) "red";
    id_of_place_test "id_of_place_test 21 KENTUCKY AVENUE"
      (List.nth map 21) 21;
    is_special_test "is_special_test false KENTUCKY AVENUE"
      (List.nth map 21) false;
    is_real_estate_test "is_real_estate_test true KENTUCKY AVENUE"
      (List.nth map 21) true;
    price_of_place_test "price_of_place_test 220 KENTUCKY AVENUE"
      (List.nth map 21) 220;
    owner_of_place_test "owner_of_place_test No Owner KENTUCKY AVENUE"
      (List.nth map 21) "";
    num_electric_own_test "num_electric_own_test 0" "test_player" map 0;
    num_railroad_own_test "num_railroad_own_test 0" "test_player" map 0;
    find_color_set_test "find_color_set_test red" (List.nth map 21) map
      [ "KENTUCKY AVENUE"; "INDIANA AVENUE"; "ILLINOIS AVENUE" ];
    check_color_set_test "check_color_set_test false KENTUCKY AVENUE"
      "test_player"
      [ List.nth map 21; List.nth map 23; List.nth map 24 ]
      false;
    rent_of_place_test "rent_of_place_test 18 KENTUCKY AVENUE"
      (Monopoly.buy_place "test_player" (List.nth map 21))
      map 0 18;
    buy_place_test "buy_place_test test_player KENTUCKY AVENUE"
      "test_player" (List.nth map 21) "test_player";
    house_num_of_place_test "house_num_of_place_test 0 KENTUCKY AVENUE"
      (List.nth map 21) 0;
    hotel_num_of_place_test "hotel_num_of_place_test 0 KENTUCKY AVENUE"
      (List.nth map 21) 0;
    house_cost_of_place_test
      "house_cost_of_place_test 150 KENTUCKY AVENUE" (List.nth map 21)
      150;
    add_house_test "add_house_test KENTUCKY AVENUE" (List.nth map 21) 1;
    house_sell_of_place_test
      "house_sell_of_place_test 75 KENTUCKY AVENUE" (List.nth map 21) 75;
    sell_house_test "sell_house_test KENTUCKY AVENUE"
      (Monopoly.add_house (List.nth map 21))
      0;
    hotel_cost_of_place_test
      "hotel_cost_of_place_test 150 KENTUCKY AVENUE" (List.nth map 21)
      150;
    add_hotel_test "add_hotel_test KENTUCKY AVENUE" (List.nth map 21) 1;
    sell_hotel_test "sell_hotel_test KENTUCKY AVENUE"
      (Monopoly.add_hotel (List.nth map 21))
      0;
    draw_chance_card_test "draw_chance_card_test index 0" 0 chance_cards
      "Advance to Boardwalk.";
    draw_community_chest_card_test
      "draw_community_chest_card_test index 0" 0 community_chest_cards
      "Advance to Go (Collect $200).";
  ]

let name_of_player_test name player expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.name_of_player player)

let location_of_player_test name player expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.location_of_player player)

let find_player_test name player players expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.find_player player players)

let move_test name player dice map expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.location_of_player (Player.move player dice map))

let get_own_test name player expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Player.get_own player)

let buy_place_own_test name player place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.get_own (Player.buy_place player place))

let buy_place_money_test name player place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.money_of_player (Player.buy_place player place))

let auction_place_own_test name player place price expected_output :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.get_own (Player.auction_place player place price))

let auction_place_money_test name player place price expected_output :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.money_of_player (Player.auction_place player place price))

let mortgage_place_money_test name player place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.money_of_player (Player.mortgage_place player place))

let pay_rent_payer_test name player rent player' expected_output : test
    =
  name >:: fun _ ->
  assert_equal expected_output
    (let player_pair = Player.pay_rent player rent player' in
     let payer =
       match player_pair with
       | h, t -> h
     in
     Player.money_of_player payer)

let pay_rent_owner_test name player rent player' expected_output : test
    =
  name >:: fun _ ->
  assert_equal expected_output
    (let player_pair = Player.pay_rent player rent player' in
     let owner =
       match player_pair with
       | h, t -> t
     in
     Player.money_of_player owner)

let can_add_house_test name player place map expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.can_add_house player place map)

let add_house_test name player place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.money_of_player (Player.add_house player place))

let can_sell_house_test name player place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.can_sell_house player place)

let can_add_hotel_test name player place map expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.can_add_hotel player place map)

let can_sell_hotel_test name player place expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.can_sell_hotel player place)

let money_of_player_test name player expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.money_of_player player)

let get_money_test name player num expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.money_of_player (Player.get_money player num))

let num_jail_cards_test name player expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.num_jail_cards player)

let is_in_jail_test name player expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.is_in_jail player)

let get_house_num_test name own expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.get_house_num own)

let get_hotel_num_test name own expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.get_hotel_num own)

let player_tests =
  [
    name_of_player_test "name_of_player_test test_player" player
      "test_player";
    location_of_player_test "location_of_player_test" player
      (List.nth map 0);
    find_player_test "find_player_test player" "test_player" players
      player;
    move_test "move_test 5" player 5 map (List.nth map 5);
    get_own_test "get_own_test" player [];
    buy_place_own_test "buy_place_test" player (List.nth map 6)
      [ Monopoly.buy_place "test_player" (List.nth map 6) ];
    buy_place_money_test "buy_place_test" player (List.nth map 6) 1400;
    auction_place_own_test "auction_place_test" player (List.nth map 6)
      100
      [ Monopoly.buy_place "test_player" (List.nth map 6) ];
    auction_place_money_test "auction_place_test" player
      (List.nth map 6) 100 1400;
    mortgage_place_money_test "mortgage_place_test"
      (buy_place player (List.nth map 6))
      (Monopoly.buy_place "test_player" (List.nth map 6))
      1450;
    pay_rent_payer_test "pay_rent_payer_test" player 50
      (buy_place player (List.nth map 6))
      1450;
    pay_rent_owner_test "pay_rent_owner_test" player 50
      (buy_place player (List.nth map 6))
      1450;
    can_add_house_test "can_add_house_test true"
      (buy_place
         (buy_place
            (buy_place player (List.nth map 6))
            (List.nth map 8))
         (List.nth map 9))
      (Monopoly.buy_place "test_player" (List.nth map 6))
      (Monopoly.update_map
         (Monopoly.buy_place "test_player" (List.nth map 9))
         (Monopoly.update_map
            (Monopoly.buy_place "test_player" (List.nth map 8))
            ((Monopoly.update_map
                (Monopoly.buy_place "test_player" (List.nth map 6)))
               map)))
      true;
    can_add_house_test "can_add_house_test false" player
      (List.nth map 6) map false;
    add_house_test "add_house_test" player (List.nth map 6) 1450;
    can_sell_house_test "can_add_house_test false" player
      (List.nth map 6) false;
    can_add_hotel_test "can_add_hotel_test false" player
      (List.nth map 6) map false;
    can_sell_hotel_test "can_sell_hotel_test false" player
      (List.nth map 6) false;
    money_of_player_test "money_of_player_test" player 1500;
    get_money_test "get_money_test" player 100 1600;
    num_jail_cards_test "num_jail_cards_test" player 0;
    is_in_jail_test "is_in_jail_test" player false;
    get_house_num_test "get_house_num_test" (Player.get_own player) 0;
    get_hotel_num_test "get_hotel_num_test" (Player.get_own player) 0;
  ]

let suite =
  "test suite for Monopoly"
  >::: List.flatten [ monopoly_tests; player_tests ]

let _ = run_test_tt_main suite
