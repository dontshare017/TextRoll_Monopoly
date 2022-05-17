open Monopoly

type player
(** The abstract type of values representing the players of the game
    monopoly*)

val players : player list

val player_to_string : player -> string
(**A string representation of [player] in its current state.*)

val name_of_player : player -> string
(**[name_of_player] is [player]'s name as a string.*)

val location_of_player : player -> Monopoly.place
(** [location_of_player player] is the location of [player].*)

val make_new_player : string -> Monopoly.place list -> player
(** [make_new_player name] is the initialized new player with [name].*)

val find_player : string -> player list -> player
(** [find_player name players] is the [player] with [name] in [players].
    If [player] with [name] is not in [players], raise failure "No Such
    Player".*)

val move : player -> int -> Monopoly.place list -> player
(** [move player dice map] is the [player] who moves [dice] steps
    forward on [map]. If the [player] passes Special place Go, 200 will
    be added to their money.*)

val buy_place : player -> Monopoly.place -> player
(** [buy_place player place] is the [player] who buys [place] which is
    buyable, meaning that it should be a real estate, a rail road or an
    electric company. If the place is not buyable or has been owned by
    other players, raise failure "You cannot buy this place". If
    [player] does not have enough money to but the [place], raise
    failure "No Money".*)

val auction_place : player -> Monopoly.place -> int -> player
(** [auction_place player place price] is the [player] who buys [place]
    by auction.*)

val mortgage_place : player -> Monopoly.place -> player
(** [mortgage_place player place] is the player who mortgaged [place] to
    bank. The returned player has [place] added to its mortgage and have
    money added.*)

val pay_rent : player -> int -> player -> player * player
(** [pay_rent player1 rent player2] is (player1, player2) where
    [player1] pays [rent] to [player2]. If [player1] does not have
    enough money to pay, raise failure "No Money".*)

val pay_tax : player -> int -> player
(** [pay_tax player tax] is [player] who pays [tax] amount of money.*)

val can_add_house :
  player -> Monopoly.place -> Monopoly.place list -> bool
(** [can_add_house player place map] is true if [player] is the owner of
    [place], [player] has completed the color set of [place], [place]
    has less than 4 houses and no hotel; othereise, false. Requires:
    [place] is a real estate.*)

val add_house : player -> Monopoly.place -> player
(** [add_house player place] is the [player] who pays the cost of
    building a house on [place] and has [place] updated. Requires:
    [place] is a real estate.*)

val can_sell_house : player -> Monopoly.place -> bool
(** [can_sell_house player place] is true if [player] is the owner of
    [place] and [place] has at least 1 house; otherwise, false.
    Requires: [place] is a real estate.*)

val sell_house : player -> Monopoly.place -> player
(** [sell_house player place] is the [player] who gets money by selling
    a house on [place]. Requires: [place] is a real estate.*)

val can_add_hotel :
  player -> Monopoly.place -> Monopoly.place list -> bool
(** [can_add_hotel player place map] is true if [player] is the owner of
    [place], [player] has completed the color set of [place] and [place]
    has exactly 4 houses and no hotel; othereise, false. Requires:
    [place] is a real estate.*)

val add_hotel : player -> Monopoly.place -> player
(** [add_hotel player place] is the [player] who pays the cost of
    building a hotel on [place] and has [place] updated. Requires:
    [place] is a real estate.*)

val can_sell_hotel : player -> Monopoly.place -> bool
(** [can_sell_hotel player place] is true if [player] is the owner of
    [place] and [place] has exactly 1 hotel; otherwise, false. Requires:
    [place] is a real estate.*)

val sell_hotel : player -> Monopoly.place -> player
(** [sell_hotel player place] is the [player] who gets money by selling
    a hotel on [place]. Requires: [place] is a real estate.*)

val check_double : int -> int -> player -> player
(** [check_double dice1 dice2 player] is the player who have rolled
    [dice1] and [dice2]. If [dice1] and [dice2] are equal, the number of
    double of the player will add one; otherwise, the number turns to
    zero.*)

val money_of_player : player -> int
(** [money_of_player player] is the money of [player].*)

val get_money : player -> int -> player
(** [get_money player amount] is the [player] who received [amount] of
    money.*)

val add_jail_card : player -> player
(** [add_jail_card player] is the [player] who received a get out of
    jail card.*)

val back_3_space : player -> place list -> player
(** [back_3_space player map] is the [player] who went back 3 spaces on
    the [map]*)
