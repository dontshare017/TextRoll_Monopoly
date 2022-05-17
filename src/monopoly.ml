open Yojson

type title_deed = {
  name : string;
  rent : int;
  rent_with_color_set : int;
  with_1_house : int;
  with_2_house : int;
  with_3_house : int;
  with_4_house : int;
  with_hotel : int;
  mortgage : int;
  house_cost : int;
  hotel_cost : int;
  house_sell : int;
  hotel_sell : int;
}

type real_estate = {
  name : string;
  id : int;
  color : string;
  price : int;
  house : int;
  hotel : int;
  title : title_deed;
  owned_by : string;
}

type railroad = {
  name : string;
  id : int;
  price : int;
  rent : int;
  mortgage : int;
  owned_by : string;
}

type electric_company = {
  name : string;
  id : int;
  price : int;
  mortgage : int;
  owned_by : string;
}

type special =
  | Chance of string * int
  | Community_Chest of string * int
  | Income_Tax of string * int
  | Luxury_Tax of string * int
  | In_Jail of string * int
  | Go_To_Jail of string * int
  | Go of string * int
  | Free_Parking of string * int

type place =
  | Real_Estate of real_estate
  | Railroad of railroad
  | Electric_Company of electric_company
  | Special of special

type chance_card = {
  name : string;
  chance_id : int;
}

type chance_cards = chance_card list

type community_chest_card = {
  name : string;
  id : int;
}

type community_chest_cards = community_chest_card list

let chance_card_from_json json =
  {
    name =
      json
      |> Yojson.Basic.Util.member "name"
      |> Yojson.Basic.Util.to_string;
    chance_id =
      json
      |> Yojson.Basic.Util.member "chance_id"
      |> Yojson.Basic.Util.to_int;
  }

let chance_cards_from_json json =
  json
  |> Yojson.Basic.Util.member "Chance Cards"
  |> Yojson.Basic.Util.to_list
  |> List.map chance_card_from_json

let community_chest_card_from_json json =
  {
    name =
      json
      |> Yojson.Basic.Util.member "name"
      |> Yojson.Basic.Util.to_string;
    id =
      json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_int;
  }

let community_chest_cards_from_json json =
  json
  |> Yojson.Basic.Util.member "Community Chest Cards"
  |> Yojson.Basic.Util.to_list
  |> List.map community_chest_card_from_json

let real_estate_from_json json =
  {
    name =
      json
      |> Yojson.Basic.Util.member "name"
      |> Yojson.Basic.Util.to_string;
    id =
      json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_int;
    color =
      json
      |> Yojson.Basic.Util.member "color"
      |> Yojson.Basic.Util.to_string;
    price =
      json
      |> Yojson.Basic.Util.member "price"
      |> Yojson.Basic.Util.to_int;
    house = 0;
    hotel = 0;
    title =
      {
        name =
          json
          |> Yojson.Basic.Util.member "name"
          |> Yojson.Basic.Util.to_string;
        rent =
          json
          |> Yojson.Basic.Util.member "rent"
          |> Yojson.Basic.Util.to_int;
        rent_with_color_set =
          json
          |> Yojson.Basic.Util.member "rent_with_color_set"
          |> Yojson.Basic.Util.to_int;
        with_1_house =
          json
          |> Yojson.Basic.Util.member "with_1_house"
          |> Yojson.Basic.Util.to_int;
        with_2_house =
          json
          |> Yojson.Basic.Util.member "with_2_house"
          |> Yojson.Basic.Util.to_int;
        with_3_house =
          json
          |> Yojson.Basic.Util.member "with_3_house"
          |> Yojson.Basic.Util.to_int;
        with_4_house =
          json
          |> Yojson.Basic.Util.member "with_4_house"
          |> Yojson.Basic.Util.to_int;
        with_hotel =
          json
          |> Yojson.Basic.Util.member "with_hotel"
          |> Yojson.Basic.Util.to_int;
        mortgage =
          json
          |> Yojson.Basic.Util.member "mortgage"
          |> Yojson.Basic.Util.to_int;
        house_cost =
          json
          |> Yojson.Basic.Util.member "house_cost"
          |> Yojson.Basic.Util.to_int;
        hotel_cost =
          json
          |> Yojson.Basic.Util.member "hotel_cost"
          |> Yojson.Basic.Util.to_int;
        house_sell =
          json
          |> Yojson.Basic.Util.member "house_sell"
          |> Yojson.Basic.Util.to_int;
        hotel_sell =
          json
          |> Yojson.Basic.Util.member "hotel_sell"
          |> Yojson.Basic.Util.to_int;
      };
    owned_by = "";
  }

let railroad_from_json json =
  {
    name =
      json
      |> Yojson.Basic.Util.member "name"
      |> Yojson.Basic.Util.to_string;
    id =
      json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_int;
    price =
      json
      |> Yojson.Basic.Util.member "price"
      |> Yojson.Basic.Util.to_int;
    rent =
      json
      |> Yojson.Basic.Util.member "rent"
      |> Yojson.Basic.Util.to_int;
    mortgage =
      json
      |> Yojson.Basic.Util.member "mortgage"
      |> Yojson.Basic.Util.to_int;
    owned_by = "";
  }

let electric_company_from_json json =
  {
    name =
      json
      |> Yojson.Basic.Util.member "name"
      |> Yojson.Basic.Util.to_string;
    id =
      json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_int;
    price =
      json
      |> Yojson.Basic.Util.member "price"
      |> Yojson.Basic.Util.to_int;
    mortgage =
      json
      |> Yojson.Basic.Util.member "mortgage"
      |> Yojson.Basic.Util.to_int;
    owned_by = "";
  }

let special_from_json json =
  let name =
    json
    |> Yojson.Basic.Util.member "name"
    |> Yojson.Basic.Util.to_string
  in
  let id =
    json |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_int
  in
  if name = "GO" then Go (name, id)
  else if name = "COMMUNITY CHEST" then Community_Chest (name, id)
  else if name = "INCOME TAX" then Income_Tax (name, id)
  else if name = "CHANCE" then Chance (name, id)
  else if name = "JAIL" then In_Jail (name, id)
  else if name = "FREE PARKING" then Free_Parking (name, id)
  else if name = "GO TO JAIL" then Go_To_Jail (name, id)
  else Luxury_Tax (name, id)

let place_from_json json desired_type =
  if desired_type = "real estate" then
    let real_estate_list =
      json
      |> Yojson.Basic.Util.member "real_estate"
      |> Yojson.Basic.Util.to_list
      |> List.map real_estate_from_json
    in
    let rec convert lst =
      match lst with
      | [] -> []
      | h :: t -> Real_Estate h :: convert t
    in
    convert real_estate_list
  else if desired_type = "special" then
    let special_list =
      json
      |> Yojson.Basic.Util.member "special"
      |> Yojson.Basic.Util.to_list
      |> List.map special_from_json
    in
    let rec convert lst =
      match lst with
      | [] -> []
      | h :: t -> Special h :: convert t
    in
    convert special_list
  else if desired_type = "railroad" then
    let railroad_list =
      json
      |> Yojson.Basic.Util.member "railroad"
      |> Yojson.Basic.Util.to_list
      |> List.map railroad_from_json
    in
    let rec convert lst =
      match lst with
      | [] -> []
      | h :: t -> Railroad h :: convert t
    in
    convert railroad_list
  else
    let electric_company_list =
      json
      |> Yojson.Basic.Util.member "electric_company"
      |> Yojson.Basic.Util.to_list
      |> List.map electric_company_from_json
    in
    let rec convert lst =
      match lst with
      | [] -> []
      | h :: t -> Electric_Company h :: convert t
    in
    convert electric_company_list

type map = place list

let place_to_string place =
  match place with
  | Special (Go (name, id)) ->
      "name: " ^ name ^ ", index on map: " ^ string_of_int id
  | Special (Chance (name, id)) ->
      "name: " ^ name ^ ", index on map: " ^ string_of_int id
  | Special (Community_Chest (name, id)) ->
      "name: " ^ name ^ ", index on map: " ^ string_of_int id
  | Special (Income_Tax (name, id)) ->
      "name: " ^ name ^ ", index on map: " ^ string_of_int id
  | Special (Luxury_Tax (name, id)) ->
      "name: " ^ name ^ ", index on map: " ^ string_of_int id
  | Special (In_Jail (name, id)) ->
      "name: " ^ name ^ ", index on map: " ^ string_of_int id
  | Special (Free_Parking (name, id)) ->
      "name: " ^ name ^ ", index on map: " ^ string_of_int id
  | Special (Go_To_Jail (name, id)) ->
      "name: " ^ name ^ ", index on map: " ^ string_of_int id
  | Real_Estate r ->
      "name: " ^ r.name ^ ", index on map: " ^ string_of_int r.id
      ^ ", color: " ^ r.color ^ ", price: " ^ string_of_int r.price
      ^ ", house: " ^ string_of_int r.house ^ ", hotel: "
      ^ string_of_int r.hotel ^ ", owned by: " ^ r.owned_by
      (* [title_deed] is not displayed here. Will make a function later
         to display such information when player clicks on it.*)
  | Railroad rr ->
      "name: " ^ rr.name ^ ", index on map: " ^ string_of_int rr.id
      ^ ", price: " ^ string_of_int rr.price ^ ", rent: "
      ^ string_of_int rr.rent ^ ", mortgage: "
      ^ string_of_int rr.mortgage
      ^ ", owned by : " ^ rr.owned_by
  | Electric_Company ec ->
      "name: " ^ ec.name ^ ", index on map: " ^ string_of_int ec.id
      ^ ", price: " ^ string_of_int ec.price ^ ", mortgage: "
      ^ string_of_int ec.mortgage
      ^ ", owned by : " ^ ec.owned_by

let color_of_place place =
  match place with
  | Real_Estate r -> r.color
  | _ -> failwith "No color"

let rec place_of_id map id =
  match map with
  | [] -> raise (Failure "No Place")
  | h :: t -> (
      match h with
      | Real_Estate r -> if r.id = id then h else place_of_id t id
      | Railroad rr -> if rr.id = id then h else place_of_id t id
      | Electric_Company ec ->
          if ec.id = id then h else place_of_id t id
      | Special s -> (
          match s with
          | Chance (_, int_id) ->
              if int_id = id then h else place_of_id t id
          | Community_Chest (_, int_id) ->
              if int_id = id then h else place_of_id t id
          | Income_Tax (_, int_id) ->
              if int_id = id then h else place_of_id t id
          | Luxury_Tax (_, int_id) ->
              if int_id = id then h else place_of_id t id
          | In_Jail (_, int_id) ->
              if int_id = id then h else place_of_id t id
          | Go_To_Jail (_, int_id) ->
              if int_id = id then h else place_of_id t id
          | Go (_, int_id) ->
              if int_id = id then h else place_of_id t id
          | Free_Parking (_, int_id) ->
              if int_id = id then h else place_of_id t id))

(**[real_estate_of_place] is the real estate with all its information
   that [place] contains. Requires: [place] us a real estate.*)
let real_estate_of_place place =
  match place with
  | Real_Estate rr -> rr
  | _ -> failwith "Not Real Estate"

let mortgage_of_place place =
  let real_estate = real_estate_of_place place in
  real_estate.title.mortgage

let name_of_place place =
  match place with
  | Real_Estate r -> r.name
  | Railroad rr -> rr.name
  | Electric_Company ec -> ec.name
  | Special s -> (
      match s with
      | Chance (name, _) -> name
      | Community_Chest (name, _) -> name
      | Income_Tax (name, _) -> name
      | Luxury_Tax (name, _) -> name
      | In_Jail (name, _) -> name
      | Go_To_Jail (name, _) -> name
      | Go (name, _) -> name
      | Free_Parking (name, _) -> name)

let id_of_place place =
  match place with
  | Real_Estate r -> r.id
  | Railroad rr -> rr.id
  | Electric_Company ec -> ec.id
  | Special s -> (
      match s with
      | Chance (_, int_id) -> int_id
      | Community_Chest (_, int_id) -> int_id
      | Income_Tax (_, int_id) -> int_id
      | Luxury_Tax (_, int_id) -> int_id
      | In_Jail (_, int_id) -> int_id
      | Go_To_Jail (_, int_id) -> int_id
      | Go (_, int_id) -> int_id
      | Free_Parking (_, int_id) -> int_id)

let is_special place =
  match place with
  | Special s -> true
  | _ -> false

let is_real_estate place =
  match place with
  | Real_Estate r -> true
  | _ -> false

let compare place1 place2 =
  if id_of_place place1 <= id_of_place place2 then -1 else 1

let generate_map json : map =
  let map =
    place_from_json json "real estate"
    @ place_from_json json "special"
    @ place_from_json json "railroad"
    @ place_from_json json "electric company"
  in
  List.sort compare map

let start_place (map : map) =
  match map with
  | [] -> failwith "Empty Map"
  | h :: _ -> h

let price_of_place place =
  match place with
  | Real_Estate r -> r.price
  | Railroad rr -> rr.price
  | Electric_Company ec -> ec.price
  | _ -> failwith "No Price"

let owner_of_place place =
  match place with
  | Real_Estate r -> r.owned_by
  | Railroad r -> r.owned_by
  | Electric_Company e -> e.owned_by
  | _ -> "No Owner"
(*Change this later. Bug if the player name is "No Owner"*)

let rec num_electric_own owner = function
  | [] -> 0
  | h :: t -> (
      match h with
      | Electric_Company e ->
          if e.owned_by = owner then 1 + num_electric_own owner t
          else num_electric_own owner t
      | _ -> num_electric_own owner t)

let rec num_railroad_own owner = function
  | [] -> 0
  | h :: t -> (
      match h with
      | Railroad r ->
          if r.owned_by = owner then 1 + num_railroad_own owner t
          else num_railroad_own owner t
      | _ -> num_railroad_own owner t)

let rec find_color_set place map =
  let real_estate = real_estate_of_place place in
  match map with
  | [] -> []
  | h :: t -> (
      match h with
      | Real_Estate r ->
          if r.color = real_estate.color then
            h :: find_color_set place t
          else find_color_set place t
      | _ -> find_color_set place t)

let rec check_color_set name color_set =
  match color_set with
  | [] -> true
  | h :: t ->
      if owner_of_place h = name then check_color_set name t else false

let rent_of_place place map dice =
  match place with
  | Real_Estate r ->
      if
        check_color_set r.owned_by (find_color_set place map)
        && r.house = 0 && r.hotel = 0
      then r.title.rent_with_color_set
      else if
        check_color_set r.owned_by (find_color_set place map)
        && r.house = 1 && r.hotel = 0
      then r.title.with_1_house
      else if
        check_color_set r.owned_by (find_color_set place map)
        && r.house = 2 && r.hotel = 0
      then r.title.with_2_house
      else if
        check_color_set r.owned_by (find_color_set place map)
        && r.house = 3 && r.hotel = 0
      then r.title.with_3_house
      else if
        check_color_set r.owned_by (find_color_set place map)
        && r.house = 4 && r.hotel = 0
      then r.title.with_4_house
      else if
        check_color_set r.owned_by (find_color_set place map)
        && r.hotel = 1
      then r.title.with_hotel
      else r.title.rent
  | Railroad r ->
      let num = num_railroad_own r.owned_by map in
      num * r.rent
  | Electric_Company e ->
      if num_electric_own e.owned_by map = 1 then dice * 4
      else dice * 10
  | _ -> failwith "No Rent"

let buy_place name place =
  match place with
  | Special s -> failwith "Can't purchase this"
  | Real_Estate r -> Real_Estate { r with owned_by = name }
  | Railroad r -> Railroad { r with owned_by = name }
  | Electric_Company e -> Electric_Company { e with owned_by = name }

let rec update_map place map =
  match map with
  | [] -> []
  | h :: t ->
      if id_of_place place = id_of_place h then place :: t
      else h :: update_map place t

let house_num_of_place place =
  let r = real_estate_of_place place in
  r.house

let hotel_num_of_place place =
  let r = real_estate_of_place place in
  r.hotel

let house_cost_of_place place =
  let r = real_estate_of_place place in
  r.title.house_cost

let house_sell_of_place place =
  let r = real_estate_of_place place in
  r.title.house_sell

let hotel_cost_of_place place =
  let r = real_estate_of_place place in
  r.title.hotel_cost

let hotel_sell_of_place place =
  let r = real_estate_of_place place in
  r.title.hotel_sell

let add_house place =
  let real_estate = real_estate_of_place place in
  Real_Estate { real_estate with house = real_estate.house + 1 }

let sell_house place =
  let real_estate = real_estate_of_place place in
  Real_Estate { real_estate with house = real_estate.house - 1 }

let add_hotel place =
  let real_estate = real_estate_of_place place in
  Real_Estate { real_estate with hotel = real_estate.hotel + 1 }

let sell_hotel place =
  let real_estate = real_estate_of_place place in
  Real_Estate { real_estate with hotel = real_estate.hotel - 1 }

let draw_chance_card index chance_cards =
  let chance_card = List.nth chance_cards index in
  chance_card.name