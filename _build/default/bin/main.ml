open Game
open Battleship
open Gameplay
open Player
open Computer

let main () =
  let rows = 10 in
  let cols = 10 in
  let player1_matrix = Board.create_empty rows cols in
  let player2_matrix = Board.create_empty rows cols in
  let empty_matrix = Board.create_empty rows cols in 
  let player1 = default_player () in
  let player2 = { (default_player ()) with number = 2 } in
  let ship_types = [ Carrier; Battleship; Cruiser; Submarine; Destroyer ] in
  print_string "\027[2J";
  flush stdout;
  print_string " \n\n\027[1m\n\027[4mWelcome to Battleship!\027[0m\n\n";

  print_string "\nGame Setup:\n";

  print_string
    "\n\
    \ 1. You will play on a 10x10 grid labeled with letters (A-J) for columns \
     and numbers (0-9) for rows.\n";
  print_string "\n 2. Place your ships on the grid. The fleet consists of:\n";
  print_string "\n  - Carrier (5 cells)\n";
  print_string "\n  - Battleship (4 cells)\n";
  print_string "\n  - Cruiser (3 cells)\n";
  print_string "\n  - Submarine (3 cells)\n";
  print_string "\n  - Destroyer (2 cells)\n";
  print_string
    "\n\
    \ 3. Ships can be placed horizontally (H) or vertically (V) and must not \
     overlap.\n";

  print_string "\nGameplay:\n";

  print_string
    "\n\
    \ 1. You and your opponent take turns to target a coordinate on the grid.\n";
  print_string
    "\n 2. Enter your target in the format \"A3\" or \"F7\" when prompted.\n";
  print_string
    "\n\
    \ 3. A hit is marked with \"H\", a miss with \"M\", and ships with their \
     respective initials.\n";
  print_string
    "\n\
    \ 4. Sunk ships are announced, and the game ends when all opponent's ships \
     are sunk.\n";
  print_string
    "\n\
    \ 5. There are two power-ups: Fog and Sweep placed on hidden locations on \
     the board. \n\
    \    Fog temporarily hides opponent's view of your board. Sweep hits an \
     entire row or column in one turn. \n\
    \    Power-ups are activated when you hit its location on the board.  \n";
  print_newline ();
  print_string "Select a game mode:\n";
  print_string "1. PvP (Player vs. Player)\n";
  print_string "2. PvE (Player vs. Computer)\n";
  print_string "Enter the number of your choice: ";

  flush stdout;
  let rec read_game_mode () =
    let input = read_line () in
    match input with
    | "1" ->
        Board.print_matrix player1_matrix true;
        print_string "\n\027[1mPlayer 1, set up your ships:\027[0m\n\n";
        List.iter
          (fun ship_type -> place_ship player1_matrix ship_type player1)
          ship_types;
        print_string "\027[2J";
        flush stdout;
        Board.print_matrix player2_matrix true;
        print_string "\n\n";
        print_string "\027[1mPlayer 2, set up your ships:\027[0m\n\n";
        List.iter
          (fun ship_type -> place_ship player2_matrix ship_type player2)
          ship_types;

        print_string "\027[2J";
        flush stdout;
        assign_power_ups player1_matrix player2_matrix;
        game_loop player1_matrix player2_matrix player1 player2
    | "2" ->
        Board.print_matrix player2_matrix true;
        print_string "\n\027[1mPlayer, set up your ships:\027[0m\n\n";
        List.iter
          (fun ship_type -> place_ship player1_matrix ship_type player1)
          ship_types;
        print_string "\027[2J";
        flush stdout;
        print_string "\027[1mComputer is setting up ships...\027[0m\n\n";
        List.iter
          (fun ship_type -> place_ship_random player2_matrix ship_type)
          ship_types;
        print_string "\027[2J";
        flush stdout;
        Board.print_matrix empty_matrix true;
        game_loop_pve player1_matrix player2_matrix player1 player2
    | _ ->
        print_endline "Invalid choice. Please enter 1 or 2.";
        read_game_mode ()
  in
  read_game_mode ()

let () = main ()
