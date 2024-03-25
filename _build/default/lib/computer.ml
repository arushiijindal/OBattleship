open Player
(** PVE - The computer uses a form of the algorithm called the Hunt / Target Algorithm 
    developed by Nick Berry. 
    Credits: http://www.datagenetics.com/blog/december32011/index.html *)

open Battleship
open Board
open Gameplay

let target_mode = ref false
let last_hit = ref ("", 0)

let validate_coor matrix coor ship =
  let col, row = coor in
  let col_char = Char.uppercase_ascii (String.get col 0) in
  let input_check =
    col_char >= 'A' && col_char <= 'J' && row >= 0 && row <= 9
  in
  let input = col ^ string_of_int row in
  if ship.orientation = "V" && row + ship.size <= 10 then
    input_check && all_empty matrix (list_coords input ship ship.size)
  else if ship.orientation = "H" && assign_num col_char + ship.size <= 10 then
    input_check && all_empty matrix (list_coords input ship ship.size)
  else false

let rec get_rand_coord_orient matrix ship =
  Random.self_init ();
  let orient = if Random.bool () then "H" else "V" in
  let col = String.make 1 (Char.chr (int_of_char 'A' + Random.int 10)) in
  let row = Random.int 10 in
  let pre_coor = (col, row) in
  match orient with
  | "H" ->
      let new_ship = { ship with orientation = "H" } in
      if validate_coor matrix pre_coor new_ship then (pre_coor, "H")
      else get_rand_coord_orient matrix ship
  | "V" ->
      let new_ship = { ship with orientation = "V" } in
      if validate_coor matrix pre_coor new_ship then (pre_coor, "V")
      else get_rand_coord_orient matrix ship
  | _ -> get_rand_coord_orient matrix ship

let place_ship_random matrix ship =
  let ship_size =
    match ship with
    | Carrier -> 5
    | Battleship -> 4
    | Cruiser -> 3
    | Submarine -> 3
    | Destroyer -> 2
  in
  let pre_ship =
    {
      ship_type = ship;
      size = ship_size;
      hits = 0;
      is_sunk = false;
      orientation = "NA";
    }
  in
  let coor, orient = get_rand_coord_orient matrix pre_ship in
  let col, row = coor in
  let ship =
    {
      ship_type = ship;
      size = ship_size;
      hits = 0;
      is_sunk = false;
      orientation = orient;
    }
  in
  let occupied_state = Occupied ship in
  match orient with
  | "H" ->
      for i = 0 to ship_size - 1 do
        let new_col =
          Char.chr (Char.code col.[0] + i) |> Char.uppercase_ascii
        in
        Board.mark_coordinate matrix (String.make 1 new_col, row) occupied_state
      done;
      Board.print_matrix matrix false
  | "V" ->
      for i = 0 to ship_size - 1 do
        let new_row = row + i in
        Board.mark_coordinate matrix (col, new_row) occupied_state
      done;
      Board.print_matrix matrix false
  | _ -> Board.print_matrix matrix false

let generate_random () =
  let col = Char.escaped (Char.chr (Random.int 10 + int_of_char 'A')) in
  let row = Random.int 10 in
  Some (col, row)

let get_coord matrix coord =
  let col_str, row = coord in
  let col = int_of_char (String.get col_str 0) - int_of_char 'A' in
  matrix.(row).(col)

let generate_target last_hit matrix =
  let col_str, row = last_hit in
  let col = col_str.[0] in
  let adj_coords =
    [
      (col_str, row - 1);
      (col_str, row + 1);
      (String.make 1 (char_of_int (int_of_char col - 1)), row);
      (String.make 1 (char_of_int (int_of_char col + 1)), row);
    ]
  in
  let valid_coords =
    List.filter
      (fun (col, row) ->
        let col_valid = col >= "A" && col <= "J" in
        let row_valid = row >= 0 && row <= 9 in
        col_valid && row_valid)
      adj_coords
  in
  let final_coords =
    List.filter
      (fun (col, row) ->
        let coord = get_coord matrix (col, row) in
        match coord.state with Hit | Miss -> false | _ -> true)
      valid_coords
  in
  match final_coords with [] -> None | _ -> Some (List.hd final_coords)

let ship_is_sunk ship player =
  match ship.ship_type with
  | Carrier ->
      if player.carrier_hits = 5 then (
        player.num_sunk <- player.num_sunk + 1;
        player.carrier_hits <- -1;
        print_string "The Computer sunk a carrier!\n";
        true)
      else false
  | Battleship ->
      if player.battleship_hits = 4 then (
        player.num_sunk <- player.num_sunk + 1;
        player.battleship_hits <- -1;
        print_string "The Computer sunk a battleship!\n";
        true)
      else false
  | Cruiser ->
      if player.cruiser_hits = 3 then (
        player.num_sunk <- player.num_sunk + 1;
        player.cruiser_hits <- -1;
        print_string "The Computer sunk a cruiser!\n";
        true)
      else false
  | Submarine ->
      if player.submarine_hits = 3 then (
        player.num_sunk <- player.num_sunk + 1;
        player.submarine_hits <- -1;
        print_string "The Computer sunk a submarine!\n";
        true)
      else false
  | Destroyer ->
      if player.destroyer_hits = 2 then (
        player.num_sunk <- player.num_sunk + 1;
        player.destroyer_hits <- -1;
        print_string "The Computer sunk a destroyer!\n";
        true)
      else false

let rec computer_turn matrix comp =
  let input =
    if !target_mode then generate_target !last_hit matrix
    else generate_random ()
  in
  match input with
  | Some c -> (
      let coordinate = get_coord matrix c in
      match coordinate.state with
      | Empty ->
          print_string "Computer missed!\n";
          matrix.(coordinate.coord |> snd).(assign_num
                                              (String.get
                                                 (coordinate.coord |> fst) 0)) <-
            { coordinate with state = Miss };
          Board.print_matrix matrix true
      | Occupied ship ->
          print_string "Computer hit!\n";
          matrix.(coordinate.coord |> snd).(assign_num
                                              (String.get
                                                 (coordinate.coord |> fst) 0)) <-
            { coordinate with state = Hit };
          let _ =
            match ship.ship_type with
            | Carrier -> comp.carrier_hits <- comp.carrier_hits + 1
            | Battleship -> comp.battleship_hits <- comp.battleship_hits + 1
            | Cruiser -> comp.cruiser_hits <- comp.cruiser_hits + 1
            | Submarine -> comp.submarine_hits <- comp.submarine_hits + 1
            | Destroyer -> comp.destroyer_hits <- comp.destroyer_hits + 1
          in
          last_hit := coordinate.coord;
          if ship_is_sunk ship comp then (
            print_string "Computer sunk the ship!\n";
            target_mode := false)
          else target_mode := true;
          Board.print_matrix matrix true
      | Hit | Miss -> computer_turn matrix comp)
  | None ->
      target_mode := false;
      computer_turn matrix comp

let game_loop_pve p1_matrix computer_matrix player1 computer =
  let rec play_turn_pve matrix player =
    match player.number with
    | 1 ->
        player_turn_pve matrix player;
        update player;
        if player.num_sunk = 5 then (
          print_string "Player wins!\n";
          exit 0)
        else play_turn_pve p1_matrix computer
    | 2 ->
        computer_turn p1_matrix computer;
        update computer;
        if computer.num_sunk = 5 then (
          print_string "Computer wins!\n";
          exit 0)
        else play_turn_pve computer_matrix player1
    | _ -> play_turn_pve matrix player
  in
  play_turn_pve computer_matrix player1
