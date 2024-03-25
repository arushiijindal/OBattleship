open OUnit2
open Game
open Battleship
open Board
open Gameplay
open Player

(*
    Test Plan :
    Testing is divided into four groups:
    1. Board tests
    2. Gameplay tests
    3. Player tests
    4. Computer tests

    We used glass-box testing on each testable function in Board.ml, Gameplay.ml,
    Player.ml, and Computer.ml.

    We omitted testing functions that utilized I/O channels through OUnit testing
    because running make test on those functions would require us to provide
    user input for each test. However, we were still able to test these functions
    manually by playing OBattleship. We played multiple games and deliberately inputted edge cases and
    invalid inputs to see if our code would handle these cases correctly.

    Our OUnit tests achieve coverage because our glass-box testing approach ensured
    that we entered every branch of code for each function. Through manual testing,
    we simulated real-time games and inputted different cases to test functionality.
    We are confident about the coverage of our testing because both these approaches
    led to the creation of a fool-proof OBattleship. 
  
   Some unit functions (i.e. functions that print output) were omitted from direct OUnit testing:
   - Board.ml
       - print_matrix (prints to terminal, tested manually)  
   - Gameplay.ml: 
      - all_true, all_empty (tested implicitly with validate_input2)
      - hor_ship, vert_ship, get_orientation (user input required --> tested manually)
      - place_ship (tested implicitly with Board.mark_coordinate)
      - coordinate_check (helper function for sweep and fog, tested through manual testing)
      - game_loop (tested manually)
  - Computer.ml
      - place_ship_random (random generation tested manually)
      - generate_random (helper function for computer_turn, tested manually)
      - generate_target (behavior depends on game, tested manually)
      - computer_turn (tested manually)
      - game_loop_pve (tested manually)

  Every other function was tested using OUnit. 

*)

let b = Board.create_empty 10 10

let ship1 =
  {
    ship_type = Submarine;
    size = 4;
    hits = 0;
    is_sunk = false;
    orientation = "H";
  }

let ship2 =
  {
    ship_type = Carrier;
    size = 5;
    hits = 0;
    is_sunk = false;
    orientation = "V";
  }

let board_tests =
  [
    ("check board size" >:: fun _ -> assert_equal 10 (Array.length b));
    ( "empty board contents" >:: fun _ ->
      assert_equal { coord = ("C", 1); state = Empty } b.(1).(2) );
    ("empty coord state" >:: fun _ -> assert_equal Empty b.(3).(0).state);
    ( "marked coord hit state" >:: fun _ ->
      Board.mark_coordinate b ("A", 3) Hit;
      assert_equal Hit b.(3).(0).state );
    ( "marked coord missed state" >:: fun _ ->
      Board.mark_coordinate b ("A", 3) Miss;
      assert_equal Miss b.(3).(0).state );
    ( "mark coord occupied state" >:: fun _ ->
      Board.mark_coordinate b ("A", 3) (Occupied ship1);
      assert_equal (Occupied ship1) b.(3).(0).state );
    ( "get contents of coord" >:: fun _ ->
      assert_equal { coord = ("A", 6); state = Empty } (get_coordinate b "A6")
    );
    ( "mark coord occupied state" >:: fun _ ->
      Board.mark_coordinate b ("C", 9) (Occupied ship2);
      assert_equal (Occupied ship2) b.(9).(2).state );
    ( "get contents of coord" >:: fun _ ->
      Board.mark_coordinate b ("C", 9) (Occupied ship2);
      assert_equal
        { coord = ("C", 9); state = Occupied ship2 }
        (get_coordinate b "C9") );
  ]

let c = Board.create_empty 10 10
(* let d = Board.create_empty 10 10
   let e = Board.create_empty 10 10 *)

(* ship representation of horizontal carrier *)
let carrier_h =
  {
    ship_type = Carrier;
    size = 5;
    hits = 0;
    is_sunk = false;
    orientation = "H";
  }

(* ship representation of vertical carrier *)
let carrier_v =
  {
    ship_type = Carrier;
    size = 5;
    hits = 0;
    is_sunk = false;
    orientation = "V";
  }

(* ship representation of horizontal battleship *)
let bship_h =
  {
    ship_type = Battleship;
    size = 4;
    hits = 0;
    is_sunk = false;
    orientation = "H";
  }

(* ship representation of vertical battleship *)
let bship_v =
  {
    ship_type = Battleship;
    size = 4;
    hits = 0;
    is_sunk = false;
    orientation = "V";
  }

(* ship representation of horizontal cruiser *)
let cruiser_h =
  {
    ship_type = Cruiser;
    size = 3;
    hits = 0;
    is_sunk = false;
    orientation = "H";
  }

(* ship representation of vertical cruiser *)
let cruiser_v =
  {
    ship_type = Cruiser;
    size = 3;
    hits = 0;
    is_sunk = false;
    orientation = "V";
  }

(* ship representation of horizontal submarine *)
let sub_h =
  {
    ship_type = Submarine;
    size = 3;
    hits = 0;
    is_sunk = false;
    orientation = "H";
  }

(* ship representation of vertical submarine *)
let sub_v =
  {
    ship_type = Submarine;
    size = 3;
    hits = 0;
    is_sunk = false;
    orientation = "V";
  }

(* ship representation of horizontal destroyer *)
let dest_h =
  {
    ship_type = Destroyer;
    size = 2;
    hits = 0;
    is_sunk = false;
    orientation = "H";
  }

(* ship representation of vertical destroyer *)
let dest_v =
  {
    ship_type = Destroyer;
    size = 2;
    hits = 0;
    is_sunk = false;
    orientation = "V";
  }

let p1 =
  {
    num_sunk = 0;
    carrier_hits = 0;
    battleship_hits = 0;
    cruiser_hits = 0;
    submarine_hits = 0;
    destroyer_hits = 0;
    number = 1;
  }

let p2 =
  {
    num_sunk = 0;
    carrier_hits = 0;
    battleship_hits = 0;
    cruiser_hits = 0;
    submarine_hits = 0;
    destroyer_hits = 0;
    number = 2;
  }

let gameplay_tests =
  [
    ("is_empty on empty coord" >:: fun _ -> assert_equal true (is_empty c "A8"));
    ( "is_empty on non-empty coord" >:: fun _ ->
      assert_equal false (is_empty b "A3") );
    ( "validate_input on invalid string length" >:: fun _ ->
      assert_equal false (validate_input2 c "sdfjs" carrier_h) );
    ( "validate_input on valid input" >:: fun _ ->
      assert_equal true (validate_input2 c "B4" carrier_h) );
    ( "validate_input on invalid coord" >:: fun _ ->
      assert_equal false (validate_input2 c "K9" carrier_h) );
    ( "validate_input on valid input" >:: fun _ ->
      assert_equal true (validate_input2 c "A9" carrier_h) );
    ( "validate_input on too big vertical carrier" >:: fun _ ->
      assert_equal false (validate_input2 c "B6" carrier_v) );
    ( "validate_input on too big vertical carrier" >:: fun _ ->
      assert_equal false (validate_input2 c "C7" carrier_v) );
    ( "validate_input on too big vertical carrier" >:: fun _ ->
      assert_equal false (validate_input2 c "D8" carrier_v) );
    ( "validate_input on too big vertical carrier" >:: fun _ ->
      assert_equal false (validate_input2 c "A9" carrier_v) );
    ( "validate_input on too big horizontal carrier" >:: fun _ ->
      assert_equal false (validate_input2 c "G9" carrier_h) );
    ( "validate_input on too big horizontal carrier" >:: fun _ ->
      assert_equal false (validate_input2 c "H9" carrier_h) );
    ( "validate_input on too big horizontal carrier" >:: fun _ ->
      assert_equal false (validate_input2 c "I9" carrier_h) );
    ( "validate_input on too big horizontal carrier" >:: fun _ ->
      assert_equal false (validate_input2 c "J9" carrier_h) );
    ( "validate_input on too big vertical batteship" >:: fun _ ->
      assert_equal false (validate_input2 c "J7" bship_v) );
    ( "validate_input on too big vertical batteship" >:: fun _ ->
      assert_equal false (validate_input2 c "J8" bship_v) );
    ( "validate_input on too big vertical batteship" >:: fun _ ->
      assert_equal false (validate_input2 c "J9" bship_v) );
    ( "validate_input on too big horizontal batteship" >:: fun _ ->
      assert_equal false (validate_input2 c "H7" bship_h) );
    ( "validate_input on too big horizontal batteship" >:: fun _ ->
      assert_equal false (validate_input2 c "I8" bship_h) );
    ( "validate_input on too big horizontal batteship" >:: fun _ ->
      assert_equal false (validate_input2 c "J9" bship_h) );
    ( "validate_input on too big vertical cruiser" >:: fun _ ->
      assert_equal false (validate_input2 c "I8" cruiser_v) );
    ( "validate_input on too big vertical cruiser" >:: fun _ ->
      assert_equal false (validate_input2 c "J9" cruiser_v) );
    ( "validate_input on too big horizontal cruiser" >:: fun _ ->
      assert_equal false (validate_input2 c "I8" cruiser_h) );
    ( "validate_input on too big horizontal cruiser" >:: fun _ ->
      assert_equal false (validate_input2 c "J9" cruiser_h) );
    ( "validate_input on too big vertical submarine" >:: fun _ ->
      assert_equal false (validate_input2 c "I8" sub_v) );
    ( "validate_input on too big vertical submarine" >:: fun _ ->
      assert_equal false (validate_input2 c "J9" sub_v) );
    ( "validate_input on too big horizontal submarine" >:: fun _ ->
      assert_equal false (validate_input2 c "I8" sub_h) );
    ( "validate_input on too big horizontal submarine" >:: fun _ ->
      assert_equal false (validate_input2 c "J9" sub_h) );
    ( "validate_input on too big horizontal destroyer" >:: fun _ ->
      assert_equal false (validate_input2 c "J9" dest_h) );
    ( "validate_input on too big vertical destroyer" >:: fun _ ->
      assert_equal false (validate_input2 c "J9" dest_v) );
    ( "list_coords vertically oriented destroyer " >:: fun _ ->
      assert_equal [ "A8"; "A9" ] (list_coords "A8" dest_v dest_v.size) );
    ( "list_coords horizontally oriented destroyer " >:: fun _ ->
      assert_equal [ "I0"; "J0" ] (list_coords "I0" dest_h dest_h.size) );
    ( "list_coords vertically oriented carrier " >:: fun _ ->
      assert_equal
        [ "D3"; "D4"; "D5"; "D6"; "D7" ]
        (list_coords "D3" carrier_v carrier_v.size) );
    ( "list_coords horizontally oriented carrier " >:: fun _ ->
      assert_equal
        [ "D3"; "E3"; "F3"; "G3"; "H3" ]
        (list_coords "D3" carrier_h carrier_h.size) );
    ( "list_coords vertically oriented battleship " >:: fun _ ->
      assert_equal [ "D3"; "D4"; "D5"; "D6" ]
        (list_coords "D3" bship_v bship_v.size) );
    ( "list_coords horizontally oriented battleship " >:: fun _ ->
      assert_equal [ "D3"; "E3"; "F3"; "G3" ]
        (list_coords "D3" bship_h bship_h.size) );
    ( "list_coords vertically oriented cruiser " >:: fun _ ->
      assert_equal [ "D3"; "D4"; "D5" ]
        (list_coords "D3" cruiser_v cruiser_v.size) );
    ( "list_coords horizontally oriented cruiser " >:: fun _ ->
      assert_equal [ "D3"; "E3"; "F3" ]
        (list_coords "D3" cruiser_h cruiser_h.size) );
    ( "list_coords vertically oriented submarine " >:: fun _ ->
      assert_equal [ "D3"; "D4"; "D5" ] (list_coords "D3" sub_v sub_v.size) );
    ( "list_coords horizontally oriented submarine " >:: fun _ ->
      assert_equal [ "D3"; "E3"; "F3" ] (list_coords "D3" sub_h sub_h.size) );
    ( "validate_input overlapping vertical carrier" >:: fun _ ->
      Board.mark_coordinate c ("A", 1) (Occupied bship_h);
      Board.mark_coordinate c ("B", 1) (Occupied bship_h);
      Board.mark_coordinate c ("C", 1) (Occupied bship_h);
      Board.mark_coordinate c ("D", 1) (Occupied bship_h);

      assert_equal false (validate_input2 c "D0" carrier_v) );
    ( "validate_input overlapping horizontal carrier" >:: fun _ ->
      Board.mark_coordinate c ("A", 1) (Occupied bship_h);
      Board.mark_coordinate c ("B", 1) (Occupied bship_h);
      Board.mark_coordinate c ("C", 1) (Occupied bship_h);
      Board.mark_coordinate c ("D", 1) (Occupied bship_h);

      assert_equal false (validate_input2 c "C1" carrier_h) );
    ( "validate_input non-overlapping horizontal submarine" >:: fun _ ->
      Board.mark_coordinate c ("A", 1) (Occupied bship_h);
      Board.mark_coordinate c ("B", 1) (Occupied bship_h);
      Board.mark_coordinate c ("C", 1) (Occupied bship_h);
      Board.mark_coordinate c ("D", 1) (Occupied bship_h);

      assert_equal true (validate_input2 c "D2" sub_h) );
    ( "validate_input non-overlapping vertical submarine" >:: fun _ ->
      Board.mark_coordinate c ("A", 1) (Occupied bship_h);
      Board.mark_coordinate c ("B", 1) (Occupied bship_h);
      Board.mark_coordinate c ("C", 1) (Occupied bship_h);
      Board.mark_coordinate c ("D", 1) (Occupied bship_h);

      assert_equal true (validate_input2 c "E0" sub_v) );
    ( "validate_input non-overlapping vertical submarine" >:: fun _ ->
      Board.mark_coordinate c ("A", 1) (Occupied bship_h);
      Board.mark_coordinate c ("B", 1) (Occupied bship_h);
      Board.mark_coordinate c ("C", 1) (Occupied bship_h);
      Board.mark_coordinate c ("D", 1) (Occupied bship_h);
      assert_equal true (validate_input2 c "E0" sub_v) );
    ( "fog activation after hit player 1" >:: fun _ ->
      p1_act_fog := false;
      assign_power_ups b c;
      check_hit_fog (fst !p1_fog ^ string_of_int (snd !p1_fog)) p1;
      assert_equal true !p1_act_fog );
    ( "fog activation after hit player 2" >:: fun _ ->
      p2_act_fog := false;
      assign_power_ups b c;
      check_hit_fog (fst !p2_fog ^ string_of_int (snd !p2_fog)) p2;
      assert_equal true !p2_act_fog );
    ( "fog not activated after miss player 2" >:: fun _ ->
      p2_act_fog := false;
      let new_b = Board.create_empty 10 10 in
      let new_c = Board.create_empty 10 10 in
      Board.mark_coordinate new_b ("B", 2) (Occupied bship_h);
      Board.mark_coordinate new_c ("C", 3) (Occupied bship_h);
      assign_power_ups new_b new_c;
      check_hit_fog "A1" p2;
      assert_equal false !p2_act_fog );
    ( "sweep activation after hit player 1" >:: fun _ ->
      p1_act_sweep := false;
      assign_power_ups b c;
      check_hit_sweep (fst !p1_sweep ^ string_of_int (snd !p1_sweep)) p1;
      assert_equal true !p1_act_sweep );
    ( "sweep activation after hit player 2" >:: fun _ ->
      p2_act_sweep := false;
      assign_power_ups b c;
      check_hit_sweep (fst !p2_sweep ^ string_of_int (snd !p2_sweep)) p2;
      assert_equal true !p2_act_sweep );
    ( "sweep not activated after miss player 2" >:: fun _ ->
      p2_act_sweep := false;
      let new_b = Board.create_empty 10 10 in
      let new_c = Board.create_empty 10 10 in
      Board.mark_coordinate new_b ("B", 2) (Occupied bship_h);
      Board.mark_coordinate new_c ("C", 3) (Occupied bship_h);

      assign_power_ups new_b new_c;
      check_hit_sweep "A1" p2;
      assert_equal false !p2_act_sweep );
    ( "sweep not activated after miss player 1" >:: fun _ ->
      p1_act_sweep := false;
      let new_b = Board.create_empty 10 10 in
      let new_c = Board.create_empty 10 10 in
      Board.mark_coordinate new_b ("B", 2) (Occupied bship_h);
      Board.mark_coordinate new_c ("C", 3) (Occupied bship_h);

      assign_power_ups new_b new_c;
      check_hit_sweep "A1" p1;
      assert_equal false !p1_act_sweep );
    ( "fog not activated after miss player 1" >:: fun _ ->
      p1_act_fog := false;
      let new_b = Board.create_empty 10 10 in
      let new_c = Board.create_empty 10 10 in
      Board.mark_coordinate new_b ("B", 2) (Occupied bship_h);
      Board.mark_coordinate new_c ("C", 3) (Occupied bship_h);
      assign_power_ups new_b new_c;
      if ("A", 1) = !p1_fog then check_hit_fog "A2" p1
      else check_hit_fog "A1" p1;
      assert_equal false !p1_act_fog );
  ]

let p1 =
  {
    num_sunk = 0;
    carrier_hits = 5;
    battleship_hits = 3;
    cruiser_hits = 1;
    submarine_hits = 1;
    destroyer_hits = 1;
    number = 1;
  }

let p2 =
  {
    num_sunk = 1;
    carrier_hits = 3;
    battleship_hits = 4;
    cruiser_hits = 1;
    submarine_hits = 2;
    destroyer_hits = 1;
    number = 1;
  }

let p2' =
  {
    num_sunk = 1;
    carrier_hits = 3;
    battleship_hits = -1;
    cruiser_hits = 1;
    submarine_hits = 2;
    destroyer_hits = 2;
    number = 1;
  }

let p3 =
  {
    num_sunk = 1;
    carrier_hits = 3;
    battleship_hits = 1;
    cruiser_hits = 1;
    submarine_hits = 2;
    destroyer_hits = 1;
    number = 1;
  }

let p4 =
  {
    num_sunk = 0;
    carrier_hits = 3;
    battleship_hits = 1;
    cruiser_hits = 3;
    submarine_hits = 2;
    destroyer_hits = 1;
    number = 1;
  }

let p5 =
  {
    num_sunk = 3;
    carrier_hits = 3;
    battleship_hits = 1;
    cruiser_hits = 0;
    submarine_hits = 3;
    destroyer_hits = 1;
    number = 1;
  }

let player_tests =
  [
    ( "sink carrier num hits" >:: fun _ ->
      Player.update p1;
      assert_equal (-1) p1.carrier_hits );
    ( "sink carrier num sunk change" >:: fun _ ->
      Player.update p1;
      assert_equal 1 p1.num_sunk );
    ( "no change battleship" >:: fun _ ->
      Player.update p1;
      assert_equal 3 p1.battleship_hits );
    ( "sink battleship num hits" >:: fun _ ->
      Player.update p2;
      assert_equal (-1) p2.battleship_hits );
    ( "sink battleship num sunk change" >:: fun _ ->
      Player.update p2;
      assert_equal 2 p2.num_sunk );
    ( "sink cruiser num hits" >:: fun _ ->
      Player.update p4;
      assert_equal (-1) p4.cruiser_hits );
    ( "sink cruiser num sunk change" >:: fun _ ->
      Player.update p4;
      assert_equal 1 p4.num_sunk );
    ( "sink submarine num hits" >:: fun _ ->
      Player.update p5;
      assert_equal (-1) p5.submarine_hits );
    ( "sink submarine num sunk change" >:: fun _ ->
      Player.update p5;
      assert_equal 4 p5.num_sunk );
    ( "sink destroyer" >:: fun _ ->
      Player.update p2';
      assert_equal 2 p2'.num_sunk );
    ( "sink destroyer num hits change" >:: fun _ ->
      Player.update p2';
      assert_equal (-1) p2'.destroyer_hits );
    ( "ship no sink change" >:: fun _ ->
      Player.update p3;
      assert_equal 1 p3.num_sunk );
    ( "default player test" >:: fun _ ->
      assert_equal
        {
          num_sunk = 0;
          carrier_hits = 0;
          battleship_hits = 0;
          cruiser_hits = 0;
          submarine_hits = 0;
          destroyer_hits = 0;
          number = 1;
        }
        (Player.default_player ()) );
  ]

let cboard = Board.create_empty 10 10
let orient = snd (Computer.get_rand_coord_orient cboard ship2)

let ship3 =
  {
    ship_type = Submarine;
    size = 4;
    hits = 0;
    is_sunk = false;
    orientation = orient;
  }

let get_state (x : point) =
  match x.state with Occupied y -> y | _ -> failwith "impossible"

let pp =
  {
    num_sunk = 0;
    carrier_hits = 5;
    battleship_hits = 3;
    cruiser_hits = 1;
    submarine_hits = 1;
    destroyer_hits = 1;
    number = 1;
  }

let pp1 =
  {
    num_sunk = 1;
    carrier_hits = 3;
    battleship_hits = 4;
    cruiser_hits = 1;
    submarine_hits = 2;
    destroyer_hits = 1;
    number = 1;
  }

let pp2 =
  {
    num_sunk = 0;
    carrier_hits = 3;
    battleship_hits = 1;
    cruiser_hits = 3;
    submarine_hits = 2;
    destroyer_hits = 1;
    number = 1;
  }

let pp3 =
  {
    num_sunk = 3;
    carrier_hits = 3;
    battleship_hits = 1;
    cruiser_hits = 0;
    submarine_hits = 3;
    destroyer_hits = 1;
    number = 1;
  }

let pp4 =
  {
    num_sunk = 1;
    carrier_hits = 3;
    battleship_hits = -1;
    cruiser_hits = 1;
    submarine_hits = 2;
    destroyer_hits = 2;
    number = 1;
  }

let computer_tests =
  [
    ( "validate_coor valid test" >:: fun _ ->
      assert_equal true (Computer.validate_coor cboard ("A", 1) ship1) );
    ( "validate_coor invalid input" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("KJ", 10) ship1) );
    ( "validate_coor invalid input" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("K", 2) ship1) );
    ( "validate_coor invalid input" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("A", 10) ship1) );
    ( "validate_coor on too big horizontal cruiser" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("J", 9) cruiser_h) );
    ( "validate_input on too big vertical submarine" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("I", 8) sub_v) );
    ( "validate_input on too big vertical submarine" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("J", 9) sub_v) );
    ( "validate_input on too big horizontal submarine" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("I", 8) sub_h) );
    ( "validate_input on too big horizontal submarine" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("J", 9) sub_h) );
    ( "validate_input on too big horizontal destroyer" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("J", 9) dest_h) );
    ( "validate_input on too big vertical destroyer" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("J", 9) dest_v) );
    ( "validate_input on too big vertical carrier" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("B", 6) carrier_v) );
    ( "validate_input on too big vertical carrier" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("C", 7) carrier_v) );
    ( "validate_input on too big vertical carrier" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("D", 8) carrier_v) );
    ( "validate_input on too big vertical carrier" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("A", 9) carrier_v) );
    ( "validate_input on too big horizontal carrier" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("G", 9) carrier_h) );
    ( "validate_input on too big horizontal carrier" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("H", 9) carrier_h) );
    ( "validate_input on too big horizontal carrier" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("I", 9) carrier_h) );
    ( "validate_input on too big horizontal carrier" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("J", 9) carrier_h) );
    ( "validate_input on too big vertical batteship" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("J", 7) bship_v) );
    ( "validate_input on too big vertical batteship" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("J", 8) bship_v) );
    ( "validate_input on too big vertical batteship" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("J", 9) bship_v) );
    ( "validate_input on too big horizontal batteship" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("H", 7) bship_h) );
    ( "validate_input on too big horizontal batteship" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("I", 8) bship_h) );
    ( "validate_input on too big horizontal batteship" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("J", 9) bship_h) );
    ( "validate_input on too big vertical cruiser" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("I", 8) cruiser_v) );
    ( "validate_input on too big vertical cruiser" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("J", 9) cruiser_v) );
    ( "validate_input on too big horizontal cruiser" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("I", 8) cruiser_h) );
    ( "validate_input on too big horizontal cruiser" >:: fun _ ->
      assert_equal false (Computer.validate_coor cboard ("J", 9) cruiser_h) );
    ( "validate_input overlapping vertical carrier" >:: fun _ ->
      Board.mark_coordinate cboard ("A", 1) (Occupied bship_h);
      Board.mark_coordinate cboard ("B", 1) (Occupied bship_h);
      Board.mark_coordinate cboard ("C", 1) (Occupied bship_h);
      Board.mark_coordinate cboard ("D", 1) (Occupied bship_h);

      assert_equal false (Computer.validate_coor cboard ("D", 0) carrier_v) );
    ( "validate_input overlapping horizontal carrier" >:: fun _ ->
      Board.mark_coordinate cboard ("A", 1) (Occupied bship_h);
      Board.mark_coordinate cboard ("B", 1) (Occupied bship_h);
      Board.mark_coordinate cboard ("C", 1) (Occupied bship_h);
      Board.mark_coordinate cboard ("D", 1) (Occupied bship_h);

      assert_equal false (Computer.validate_coor cboard ("C", 1) carrier_h) );
    ( "validate_input non-overlapping horizontal submarine" >:: fun _ ->
      Board.mark_coordinate cboard ("A", 1) (Occupied bship_h);
      Board.mark_coordinate cboard ("B", 1) (Occupied bship_h);
      Board.mark_coordinate cboard ("C", 1) (Occupied bship_h);
      Board.mark_coordinate cboard ("D", 1) (Occupied bship_h);

      assert_equal true (Computer.validate_coor cboard ("D", 2) sub_h) );
    ( "validate_input on non-overlapping vertical submarine" >:: fun _ ->
      Board.mark_coordinate cboard ("A", 1) (Occupied bship_h);
      Board.mark_coordinate cboard ("B", 1) (Occupied bship_h);
      Board.mark_coordinate cboard ("C", 1) (Occupied bship_h);
      Board.mark_coordinate cboard ("D", 1) (Occupied bship_h);
      assert_equal true (Computer.validate_coor cboard ("E", 0) sub_v) );
    ( "get_rand_coord_orient to check coordinate" >:: fun _ ->
      let coord = fst (Computer.get_rand_coord_orient cboard ship2) in
      let str_coord = fst coord ^ string_of_int (snd coord) in
      let lst = list_coords str_coord ship3 ship3.size in
      let to_tuple str =
        (String.make 1 str.[0], int_of_string (String.make 1 str.[1]))
      in
      let lst_tuples lst = List.map to_tuple lst in
      List.iter
        (fun x -> Board.mark_coordinate cboard x (Occupied ship3))
        (lst_tuples lst);
      assert_equal (Occupied ship3) (get_coordinate cboard str_coord).state );
    ( "get_rand_coord_orient to check ship orientation" >:: fun _ ->
      let coord = fst (Computer.get_rand_coord_orient cboard ship2) in
      let str_coord = fst coord ^ string_of_int (snd coord) in
      let lst = list_coords str_coord ship3 ship3.size in
      let to_tuple str =
        (String.make 1 str.[0], int_of_string (String.make 1 str.[1]))
      in
      let lst_tuples lst = List.map to_tuple lst in
      List.iter
        (fun x -> Board.mark_coordinate cboard x (Occupied ship3))
        (lst_tuples lst);

      let target_ship = get_state (get_coordinate cboard str_coord) in

      let orient = target_ship.orientation in

      assert_equal ship3.orientation orient );
    ( "get_coord on a point with Empty state" >:: fun _ ->
      let board = Board.create_empty 10 10 in
      assert_equal board.(0).(0) (Computer.get_coord board ("A", 0)) );
    ( "get_coord on a point with Hit state" >:: fun _ ->
      let board = Board.create_empty 10 10 in
      Board.mark_coordinate board ("C", 9) Hit;
      assert_equal board.(9).(2) (Computer.get_coord board ("C", 9)) );
    ( "get_coord on a point with Miss state" >:: fun _ ->
      let board = Board.create_empty 10 10 in
      Board.mark_coordinate board ("J", 5) Miss;
      assert_equal board.(5).(9).state (Computer.get_coord board ("J", 5)).state
    );
    ( "ship_is_sunk on a sunk carrier" >:: fun _ ->
      assert_equal true (Computer.ship_is_sunk carrier_v pp) );
    ( "ship_is_sunk on a not sunk carrier" >:: fun _ ->
      assert_equal false (Computer.ship_is_sunk carrier_v p2) );
    ( "ship_is_sunk on a sunk battleship" >:: fun _ ->
      assert_equal true (Computer.ship_is_sunk bship_h pp1) );
    ( "ship_is_sunk on a not sunk battleship" >:: fun _ ->
      assert_equal false (Computer.ship_is_sunk bship_h pp) );
    ( "ship_is_sunk on a sunk cruiser" >:: fun _ ->
      assert_equal false (Computer.ship_is_sunk cruiser_h pp) );
    ( "ship_is_sunk on a not sunk destroyer" >:: fun _ ->
      assert_equal false (Computer.ship_is_sunk dest_h pp) );
    ( "ship_is_sunk on a not sunk submarine" >:: fun _ ->
      assert_equal false (Computer.ship_is_sunk sub_h pp) );
    ( "ship_is_sunk on a sunk cruiser" >:: fun _ ->
      assert_equal true (Computer.ship_is_sunk cruiser_v pp2) );
    ( "ship_is_sunk on a sunk submarine" >:: fun _ ->
      assert_equal true (Computer.ship_is_sunk sub_h pp3) );
    ( "ship_is_sunk on a sunk destroyer" >:: fun _ ->
      assert_equal true (Computer.ship_is_sunk dest_h pp4) );
  ]

let suite =
  "battleship test suite"
  >::: List.flatten
         [ board_tests; gameplay_tests; player_tests; computer_tests ]

let _ = run_test_tt_main suite
