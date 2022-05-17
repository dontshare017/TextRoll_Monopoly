open Game
open Yojson.Basic

module type MonopolySig = sig
  type place

  val start_place : place list -> place
  val place_to_string : place -> string
  val generate_map : Yojson.Basic.t -> place list
  val place_of_id : place list -> int -> place
  val mortgage_of_place : place -> int
  val name_of_place : place -> string
  val id_of_place : place -> int
  val price_of_place : place -> int
  val owner_of_place : place -> string
  val house_cost_of_place : place -> int
  val num_electric_own : string -> place list -> int
  val num_railroad_own : string -> place list -> int
  val find_color_set : place -> place list -> place list
  val check_color_set : string -> place list -> bool
  val rent_of_place : place -> place list -> int -> int
  val tax_of_place : place -> int
  val house_num_of_place : place -> int
  val hotel_num_of_place : place -> int
  val house_cost_of_place : place -> int
  val add_house : place -> place
  val house_sell_of_place : place -> int
  val sell_house : place -> place
  val hotel_cost_of_place : place -> int
  val hotel_sell_of_place : place -> int
  val add_hotel : place -> place
  val sell_hotel : place -> place
end

module MonopolyCheck : MonopolySig = Monopoly

module type PlayerSig = sig
  type player

  val players : player list
  val player_to_string : player -> string
  val name_of_player : player -> string
  val location_of_player : player -> Monopoly.place
  val make_new_player : string -> Monopoly.place list -> player
  val find_player : string -> player list -> player
  val move : player -> int -> Monopoly.place list -> player
  val buy_place : player -> Monopoly.place -> player
  val mortgage_place : player -> Monopoly.place -> player
  val pay_rent : player -> int -> player -> player * player
  val pay_tax : player -> int -> player

  val can_add_house :
    player -> Monopoly.place -> Monopoly.place list -> bool

  val add_house : player -> Monopoly.place -> player
  val can_sell_house : player -> Monopoly.place -> bool
  val sell_house : player -> Monopoly.place -> player

  val can_add_hotel :
    player -> Monopoly.place -> Monopoly.place list -> bool

  val can_sell_hotel : player -> Monopoly.place -> bool
  val sell_hotel : player -> Monopoly.place -> player
  val check_double : int -> int -> player -> player
end

module PlayerCheck : PlayerSig = Player

module type AuthorsSig = sig
  val hours_worked : int
end

module AuthorsCheck : AuthorsSig = Authors

let _ = if Authors.hours_worked < 0 then exit 1