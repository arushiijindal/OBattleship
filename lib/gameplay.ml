open Board
open Battleship
open Player

let p1_num_sunk = ref 0
let p2_num_sunk = ref 0
let p1_fog = ref ("A", 0)
let p2_fog = ref ("A", 0)
let p1_sweep = ref ("A", 0)
let p2_sweep = ref ("A", 0)
let p1_act_sweep = ref false
let p1_act_fog = ref false
let p2_act_sweep = ref false
let p2_act_fog = ref false
let p1_count = ref 0
let p2_count = ref 0
let p1_use_fog = ref false
let p2_use_fog = ref false
let fog_board = Board.create_empty 10 10

let validate_input input =
  if String.length input <> 2 then false
  else if
    let pre_col = String.get input 0 in
    let pre_row = String.get input 1 in
    let pre_check =
      pre_col >= 'A' && pre_col <= 'J' && pre_row >= '0' && pre_row <= '9'
    in
    not pre_check
  then false
  else if String.length input <> 2 then false
  else
    let col_char = Char.uppercase_ascii (String.get input 0) in
    let row_char = String.get input 1 in
    col_char >= 'A' && col_char <= 'J' && row_char >= '0' && row_char <= '9'

let is_empty matrix input =
  let p = get_coordinate matrix input in
  if p.state = Empty then true else false

let next_letter str =
  match str with
  | "A" -> "B"
  | "B" -> "C"
  | "C" -> "D"
  | "D" -> "E"
  | "E" -> "F"
  | "F" -> "G"
  | "G" -> "H"
  | "H" -> "I"
  | "I" -> "J"
  | "J" -> "J"
  | _ -> "oops"

let rec list_coords input ship n =
  let col_char = String.get input 0 |> String.make 1 in
  let row_char = String.get input 1 |> String.make 1 |> int_of_string in
  match n with
  | 0 -> []
  | _ ->
      if ship.orientation = "V" then
        (col_char ^ string_of_int row_char)
        :: list_coords (col_char ^ string_of_int (row_char + 1)) ship (n - 1)
      else
        (col_char ^ string_of_int row_char)
        :: list_coords
             (next_letter col_char ^ string_of_int row_char)
             ship (n - 1)

let rec all_true lst =
  match lst with
  | [] -> true
  | h :: t -> if h = false then false else all_true t

let all_empty matrix lst =
  let bb = List.map (is_empty matrix) lst in
  all_true bb

let validate_input2 matrix input ship =
  (*checks input string is valid length*)
  if String.length input <> 2 then false
  else
    (*checks input string is within A0 - J9*)
    let pre_col = String.get input 0 in
    let pre_row = String.get input 1 in
    let pre_check =
      pre_col >= 'A' && pre_col <= 'J' && pre_row >= '0' && pre_row <= '9'
    in
    let col_char = Char.uppercase_ascii (String.get input 0) in
    let row_char = String.get input 1 in
    let input_check =
      col_char >= 'A' && col_char <= 'J' && row_char >= '0' && row_char <= '9'
    in
    if not pre_check then false
    else if
      ship.orientation = "V"
      && int_of_string (String.make 1 row_char) + ship.size <= 10
    then input_check && all_empty matrix (list_coords input ship ship.size)
    else if ship.orientation = "H" && assign_num col_char + ship.size <= 10 then
      input_check && all_empty matrix (list_coords input ship ship.size)
    else false

(*Checks if orientation input is valid *)
let check_orientation input =
  input = "H" || input = "V" || input = "h" || input = "v"

let rec hor_ship matrix ship =
  print_string
    "Pick your leftmost coordinate in the form [col][row].\n\
     Please make sure that the letter is uppercase. (eg. A0): ";
  flush stdout;
  let input = read_line () in
  let new_ship = { ship with orientation = "H" } in
  if validate_input2 matrix input new_ship then (input, "H")
  else (
    print_endline "Invalid input. Please enter a valid coordinate.\n";
    hor_ship matrix ship)

let rec vert_ship matrix ship =
  print_string
    "Pick your topmost coordinate in the form [col][row].\n\
     Please make sure that the letter is uppercase. (eg. A0): ";
  flush stdout;
  let input = read_line () in
  let new_ship = { ship with orientation = "V" } in
  if validate_input2 matrix input new_ship then (input, "V")
  else (
    print_endline "Invalid input. Please enter a valid coordinate.\n";
    vert_ship matrix ship)

let rec get_orientation matrix ship =
  print_string
    "Choose the orientation for your ship (type H for horizontal and V for \
     vertical): ";
  flush stdout;
  let input = read_line () in
  if check_orientation input then
    match input with
    | "H" | "h" -> hor_ship matrix ship
    | "V" | "v" -> vert_ship matrix ship
    | _ -> vert_ship matrix ship
  else (
    print_endline "Invalid input. Please enter a valid orientation.\n";
    get_orientation matrix ship)

let print_helper player ship_type =
  Printf.printf "\nPlayer %d, the ship you are placing is: %s\n" player.number
    (match ship_type with
    | Carrier -> "Carrier (5 spaces)"
    | Battleship -> "Battleship (4 spaces)"
    | Cruiser -> "Cruiser (3 spaces)"
    | Submarine -> "Submarine (3 spaces)"
    | Destroyer -> "Destroyer (2 spaces)")

let place_ship matrix ship_type player =
  let ship_size =
    match ship_type with
    | Carrier -> 5
    | Battleship -> 4
    | Cruiser -> 3
    | Submarine -> 3
    | Destroyer -> 2
  in
  print_helper player ship_type;
  let pre_ship =
    {
      ship_type;
      size = ship_size;
      hits = 0;
      is_sunk = false;
      orientation = "NA";
    }
  in
  let coor, orient = get_orientation matrix pre_ship in
  let col, row =
    ( Char.escaped (Char.uppercase_ascii (String.get coor 0)),
      int_of_string (String.sub coor 1 1) )
  in
  let ship =
    {
      ship_type;
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

let rec p1_assign_fog matrix1 =
  let rand_val_let = Random.int 10 + 65 in
  let random_col =
    String.uppercase_ascii (String.make 1 (Char.chr rand_val_let))
  in
  let rand_num = Random.int 10 in
  let gen_random_coord = random_col ^ string_of_int rand_num in
  let coordinate = get_coordinate matrix1 gen_random_coord in
  match coordinate.state with
  | Empty -> p1_fog := (random_col, rand_num)
  (* print_string "Fog for player1 is placed at: ";
     print_endline (fst !p1_fog ^ string_of_int (snd !p1_fog)) *)
  | _ -> p1_assign_fog matrix1

let rec p1_assign_sweep matrix1 =
  let rand_val_let = Random.int 10 + 65 in
  let random_col =
    String.uppercase_ascii (String.make 1 (Char.chr rand_val_let))
  in
  let rand_num = Random.int 10 in
  let gen_random_coord = random_col ^ string_of_int rand_num in
  let coordinate = get_coordinate matrix1 gen_random_coord in
  match coordinate.state with
  | Occupied _ -> p1_sweep := (random_col, rand_num)
  (* print_string "Sweep for player1 is placed at: ";
     print_endline (fst !p1_sweep ^ string_of_int (snd !p1_sweep)) *)
  | _ -> p1_assign_sweep matrix1

let rec p2_assign_fog matrix2 =
  let rand_val_let = Random.int 10 + 65 in
  let random_col =
    String.uppercase_ascii (String.make 1 (Char.chr rand_val_let))
  in
  let rand_num = Random.int 10 in
  let gen_random_coord = random_col ^ string_of_int rand_num in
  let coordinate = get_coordinate matrix2 gen_random_coord in
  match coordinate.state with
  | Empty -> p2_fog := (random_col, rand_num)
  (* print_string "Fog for player2 is placed at: ";
     print_endline (fst !p2_fog ^ string_of_int (snd !p2_fog)) *)
  | _ -> p2_assign_fog matrix2

let rec p2_assign_sweep matrix2 =
  let rand_val_let = Random.int 10 + 65 in
  let random_col =
    String.uppercase_ascii (String.make 1 (Char.chr rand_val_let))
  in
  let rand_num = Random.int 10 in
  let gen_random_coord = random_col ^ string_of_int rand_num in
  let coordinate = get_coordinate matrix2 gen_random_coord in
  match coordinate.state with
  | Occupied _ -> p2_sweep := (random_col, rand_num)
  (* print_string "Sweep for player2 is placed at: ";
     print_endline (fst !p2_sweep ^ string_of_int (snd !p2_sweep)) *)
  | _ -> p2_assign_sweep matrix2

let assign_power_ups matrix1 matrix2 =
  p1_assign_fog matrix1;
  p1_assign_sweep matrix1;
  p2_assign_fog matrix2;
  p2_assign_sweep matrix2

let check_hit_fog input player =
  if player.number = 1 then
    if
      String.make 1 input.[0] = fst !p1_fog
      && String.make 1 input.[1] = string_of_int (snd !p1_fog)
    then (
      p1_act_fog := true;
      print_endline
        "\n\
         You've unlocked the fog power-up! You can activate this power up in \
         any of your next turns. The fog power-up can be used to temporarily \
         make the coordinates that the opponent has hit invisible during their \
         turn.")
    else ()
  else if
    String.make 1 input.[0] = fst !p2_fog
    && String.make 1 input.[1] = string_of_int (snd !p2_fog)
  then (
    p2_act_fog := true;
    print_endline
      "\n\
       You've unlocked the fog power-up! You can activate this power up in any \
       of your next turns. The fog power-up can be used to temporarily make \
       the coordinates that the opponent has hit invisible during their turn.")
  else ()

let check_hit_sweep input player =
  if player.number = 1 then
    if
      String.make 1 input.[0] = fst !p1_sweep
      && String.make 1 input.[1] = string_of_int (snd !p1_sweep)
    then (
      p1_act_sweep := true;
      print_endline
        "\n\
         You've unlocked the sweep power-up! You can activate this power up in \
         any of your next turns. The sweep power-up can be used to attack an \
         entire row or column of the board in one turn.")
    else ()
  else if
    String.make 1 input.[0] = fst !p2_sweep
    && String.make 1 input.[1] = string_of_int (snd !p2_sweep)
  then (
    p2_act_sweep := true;
    print_endline
      "\n\
       You've unlocked the sweep power-up! You can activate this power up in \
       any of your next turns. The sweep power-up can be used to attack an \
       entire row or column of the board in one turn.")
  else ()

let coordinate_check coordinate player matrix input =
  match coordinate.state with
  | Empty ->
      matrix.(coordinate.coord |> snd).(assign_num
                                          (String.get (coordinate.coord |> fst)
                                             0)) <-
        { coordinate with state = Miss };
      check_hit_fog input player
  | Occupied ship -> (
      if player.number = 1 then p1_count := 1 + !p1_count
      else p2_count := 1 + !p2_count;
      matrix.(coordinate.coord |> snd).(assign_num
                                          (String.get (coordinate.coord |> fst)
                                             0)) <-
        { coordinate with state = Hit };
      match ship.ship_type with
      | Carrier -> player.carrier_hits <- player.carrier_hits + 1
      | Battleship -> player.battleship_hits <- player.battleship_hits + 1
      | Cruiser -> player.cruiser_hits <- player.cruiser_hits + 1
      | Submarine -> player.submarine_hits <- player.submarine_hits + 1
      | Destroyer ->
          player.destroyer_hits <- player.destroyer_hits + 1;
          check_hit_sweep input player)
  | Hit | Miss -> ()

let sweep player matrix =
  print_string
    "Do you want to sweep a row or a column (type R for row and C for column)? ";
  flush stdout;
  let input = read_line () in
  if input = "R" then (
    print_string "Which row would you like to sweep (Enter a number)? ";
    flush stdout;
    let row_input = read_line () in
    let cols = [ "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J" ] in
    let helper_row v =
      if validate_input (v ^ row_input) then
        let coordinate = get_coordinate matrix (v ^ row_input) in
        coordinate_check coordinate player matrix input
    in
    List.iter helper_row cols)
  else if input = "C" then (
    print_string "Which column would you like to sweep (Enter a letter)? ";
    flush stdout;
    let col_input = read_line () in
    let rows = [ "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" ] in
    let helper_col v =
      if validate_input (col_input ^ v) then
        let coordinate = get_coordinate matrix (col_input ^ v) in
        coordinate_check coordinate player matrix input
    in
    List.iter helper_col rows)
  else print_endline "This is an invalid input."

let fog_check matrix player =
  if player.number = 1 && !p2_use_fog = true then (
    print_string "Your board has been hidden by fog!\n";
    Board.print_matrix fog_board true;
    p2_use_fog := false)
  else if player.number = 2 && !p1_use_fog = true then (
    print_string "Your board has been hidden by fog!\n";
    Board.print_matrix fog_board true;
    p1_use_fog := false)
  else Board.print_matrix matrix true

let update_ships_and_print ship player matrix =
  match ship.ship_type with
  | Carrier ->
      player.carrier_hits <- player.carrier_hits + 1;
      print_endline "\n";
      Board.print_matrix matrix true
  | Battleship ->
      player.battleship_hits <- player.battleship_hits + 1;
      print_endline "\n";
      Board.print_matrix matrix true
  | Cruiser ->
      player.cruiser_hits <- player.cruiser_hits + 1;
      print_endline "\n";
      Board.print_matrix matrix true
  | Submarine ->
      player.submarine_hits <- player.submarine_hits + 1;
      print_endline "\n";
      Board.print_matrix matrix true
  | Destroyer ->
      player.destroyer_hits <- player.destroyer_hits + 1;
      print_endline "\n";
      Board.print_matrix matrix true

let empty_helper coordinate matrix player input =
  matrix.(coordinate.coord |> snd).(assign_num
                                      (String.get (coordinate.coord |> fst) 0)) <-
    { coordinate with state = Miss };

  print_string "\n";
  Board.print_matrix matrix true;
  print_string "\n\n\t";
  print_string "\027[1mMiss!\027[0m\n";
  check_hit_fog input player;
  print_string "\n";
  if player.number = 1 && !p1_act_fog = true then (
    print_string
      "Would you like to use your fog power-up (enter Y for yes and N for no)? ";
    flush stdout;
    let input = read_line () in
    if input = "Y" then (
      p1_use_fog := true;
      p1_act_fog := false)
    else ())
  else ();
  if player.number = 2 && !p2_act_fog = true then (
    print_string
      "Would you like to use your fog power-up (enter Y for yes and N for no)? ";
    flush stdout;
    let input = read_line () in
    if input = "Y" then (
      p2_use_fog := true;
      p2_act_fog := false)
    else ())
  else ()

let occupied_ship coordinate matrix player ship input =
  if player.number = 1 then p1_count := 1 + !p1_count
  else p2_count := 1 + !p2_count;
  matrix.(coordinate.coord |> snd).(assign_num
                                      (String.get (coordinate.coord |> fst) 0)) <-
    { coordinate with state = Hit };
  update_ships_and_print ship player matrix;
  print_string "\n\n\t";
  print_string "\027[1mHit!\027[0m";
  print_string "\n";
  check_hit_sweep input player;
  print_string "\n";
  if player.number = 1 && !p1_act_fog = true then (
    print_string
      "Would you like to use your fog power-up (enter Y for yes and N for no)? ";
    flush stdout;
    let input = read_line () in
    if input = "Y" then p1_use_fog := true else ())
  else ();
  if player.number = 2 && !p2_act_fog = true then (
    print_string
      "Would you like to use your fog power-up (enter Y for yes and N for no)? ";
    flush stdout;
    let input = read_line () in
    if input = "Y" then p2_use_fog := true else ())
  else ()

let rec player_turn_pve matrix player =
  print_string
    (Printf.sprintf "\027[1mPlayer %d, it's your turn!\027[0m\n\n" player.number);
  print_string "Enter a coordinate to attack (e.g. AO): ";
  flush stdout;
  let input = read_line () in
  if validate_input input then (
    let coordinate = get_coordinate matrix input in
    match coordinate.state with
    | Empty ->
        matrix.(coordinate.coord |> snd).(assign_num
                                            (String.get
                                               (coordinate.coord |> fst) 0)) <-
          { coordinate with state = Miss };

        print_string "\n";
        Board.print_matrix matrix true;
        print_string "\n\n\t";
        print_string "\027[1mMiss!\027[0m\n";
        print_string "\n"
    | Occupied ship ->
        if player.number = 1 then p1_count := 1 + !p1_count
        else p2_count := 1 + !p2_count;
        matrix.(coordinate.coord |> snd).(assign_num
                                            (String.get
                                               (coordinate.coord |> fst) 0)) <-
          { coordinate with state = Hit };
        update_ships_and_print ship player matrix;
        print_string "\n\n\t";
        print_string "\027[1mHit!\027[0m";
        print_string "\n";
        print_string "\n"
    | Hit | Miss ->
        print_string "You've already guessed this coordinate. Try again.\n";
        player_turn_pve matrix player)
  else (
    print_endline "Invalid input. Please enter a valid coordinate.\n";
    player_turn_pve matrix player)

let rec player_turn_pvp matrix player =
  print_string
    (Printf.sprintf "\027[1mPlayer %d, it's your turn!\027[0m\n\n" player.number);
  fog_check matrix player;
  if player.number = 1 && !p1_act_sweep = true then (
    print_string
      "Would you like to use the sweep power_up for this turn (type Y for Yes \
       and N for No)? ";
    flush stdout;
    let choice = read_line () in
    if choice = "N" then ()
    else if choice = "Y" then (
      p1_act_sweep := false;
      sweep player matrix;
      Board.print_matrix matrix true)
    else (
      print_string "Invalid input.";
      player_turn_pvp matrix player))
  else ();
  if player.number = 2 && !p2_act_sweep = true then (
    print_string
      "Would you like to use the Sweep power_up for this turn (type Y for yes \
       and N for No)? ";
    flush stdout;
    let choice = read_line () in
    if choice = "N" then ()
    else if choice = "Y" then (
      p2_act_sweep := false;
      sweep player matrix;
      Board.print_matrix matrix true)
    else (
      print_string "Invalid input.";
      player_turn_pvp matrix player))
  else ();
  print_string "Enter a coordinate to attack (e.g. AO): ";
  flush stdout;
  let input = read_line () in
  if validate_input input then (
    let coordinate = get_coordinate matrix input in
    match coordinate.state with
    | Empty -> empty_helper coordinate matrix player input
    | Occupied ship -> occupied_ship coordinate matrix player ship input
    | Hit | Miss ->
        print_string "You've already guessed this coordinate. Try again.\n";
        player_turn_pvp matrix player)
  else (
    print_endline "Invalid input. Please enter a valid coordinate.\n";
    player_turn_pvp matrix player)

let game_loop p1_matrix p2_matrix player1 player2 =
  let rec play_turn matrix player =
    player_turn_pvp matrix player;
    update player;
    if player.num_sunk = 5 then (
      print_string (Printf.sprintf "Player %d wins!\n" player.number);
      exit 0)
    else
      play_turn
        (if player.number = 1 then p1_matrix else p2_matrix)
        (if player.number = 1 then player2 else player1)
  in
  play_turn p2_matrix player1
