(** Representation of the map of the game monopoly.*)
type place
(** The abstact type of value that represents the map of the game.*)

type chance_card
(** The type of card players will draw when step on place chance.*)

type community_chest_card
(** The type of card players will draw when step on place community
    chest.*)

val start_place : place list -> place
(** [start_place map] is the [place] of the starting place in [map].*)

val place_to_string : place -> string
(** [place_to_string place] is the string representation of [place].*)

val generate_map : Yojson.Basic.t -> place list
(** [generate_map] returns the list of places that is a representation
    of [map].*)

val chance_cards_from_json : Yojson.Basic.t -> chance_card list
(** [chance_cards_from_json json] is the list of chance cards in the
    game monopoly from [json].*)

val community_chest_cards_from_json :
  Yojson.Basic.t -> community_chest_card list
(** [community_chest_cards_from_json json] is the list of community
    chest cards in the game monopoly from [json].*)

val place_of_id : place list -> int -> place
(** [place_of_id map id] is the place of which has [id] in [map].*)

val mortgage_of_place : place -> int
(** [mortgage_of_place place] is the amount of money the player will get
    mortgaging [place] to bank. Requires: [place] is a real estate.*)

val name_of_place : place -> string
(** [name_of_place place] is the name of [place] in map.*)

val color_of_place : place -> string
(** [color_of_place] is the color of place. Requires: [place] is a real
    estate. *)

val id_of_place : place -> int
(** [id_of_place place] is the id of [place] in map.*)

val is_special : place -> bool
(** [is_special place] is true if [place] is special; otherwise false.*)

val is_real_estate : place -> bool
(** [is_real_estate place] is true if [place] is a real estate;
    othereise false.*)

val price_of_place : place -> int
(** [price_of_place map id] is the price of that place on [map] that has
    [id]. If [place] is a Special place, raise failure "No Price".*)

val owner_of_place : place -> string
(** [owner_of_place map id] is the name of the owner of the place on
    [map] that has [id]. If the place is not owned by anyone, it is an
    empty string. If [place] cannot be owned by anyone, it is "No Owner"*)

val num_electric_own : string -> place list -> int
(** [num_electric_own owner map] is the number of electric companys the
    player [owner] owns in [map].*)

val num_railroad_own : string -> place list -> int
(** [num_railroad_own owner map] is the numebr of railroads the player
    [owner] owns in [map].*)

val find_color_set : place -> place list -> place list
(** [find_color_set real_estate map] is the list of places on the map
    with the same color as [real estate]. If there are no matching
    colors, an empty list will be returned.*)

val check_color_set : string -> place list -> bool
(** [check_color_set player color_set] is true if [player] is the owner
    of all the real estates in [color_set]; otherwise, false.*)

val rent_of_place : place -> place list -> int -> int
(** [rent_of_place place map dice] is the rent of [place] in [map].
    [dice] is the number of steps the player steps to [place]*)

val buy_place : string -> place -> place
(** [buy_place name place] is the place that has been bought by player
    [name].*)

val house_num_of_place : place -> int
(** [house_num_of_place place] is the numebr of houses build on [place].
    Requires: [place] is a real estate.*)

val hotel_num_of_place : place -> int
(** [hotel_num_of_place place] is the numebr of hotel build on [place].
    Requires: [place] is a real estate.*)

val house_cost_of_place : place -> int
(** [house_cost_of_place place] is the cost of building a house on
    [place]. Requires: [place] is a real estate.*)

val add_house : place -> place
(** [add_house place] is the [place] after adding 1 house. Requires:
    [place] is a real estate.*)

val house_sell_of_place : place -> int
(** [house_sell_of_place place] is the money of selling a house on
    [place]. Requires: [place] is a real estate.*)

val sell_house : place -> place
(** [sell_house place] is the [place] after selling 1 house. Requires:
    [place] is a real estate.*)

val hotel_cost_of_place : place -> int
(** [hotel_cost_of_place place] is the cost of building a hotel on
    [place]. Requires: [place] is a real estate.*)

val hotel_sell_of_place : place -> int
(** [hotel_sell_of_place place] is the money of selling a hotel on
    [place]. Requires: [place] is a real estate.*)

val add_hotel : place -> place
(** [add_hotel place] is the [place] after adding 1 hotel. Requires:
    [place] is a real estate.*)

val sell_hotel : place -> place
(** [sell_hotel place] is the [place] after selling 1 hotel. Requires:
    [place] is a real estate.*)

val update_map : place -> place list -> place list
(** [update_map place map] is the map where [place] is substituted in
    [map] with the same id.*)

val draw_chance_card : int -> chance_card list -> string
(** [draw_chance_card index chance_cards] is the name of the chance card
    of [index] in [chance_cards].*)

val draw_community_chest_card :
  int -> community_chest_card list -> string
(** [draw_community_chest_card index community_chest_cards] is the name
    of the community chest card of [index] in [community_chest_cards].*)
