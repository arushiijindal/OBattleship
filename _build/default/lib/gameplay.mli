open Battleship

val p1_num_sunk : int ref
(** [p1_num_sunk] represents the number of ships sunk on player 1's board *)

val p2_num_sunk : int ref
(** [p1_num_sunk] represents the number of ships sunk on player 2's board *)

val p1_fog : (string * int) ref
(** [p1_fog] represents the coordinate on the board where fog is on 
    player 1's board *)

val p2_fog : (string * int) ref
(** [p2_fog] represents the coordinate on the board where fog is on 
    player 2's board *)

val p1_sweep : (string * int) ref
(** [p1_sweep] represents the coordinate on the board where sweep is on 
    player 1's board *)

val p2_sweep : (string * int) ref
(** [p2_sweep] represents the coordinate on the board where sweep is on 
    player 2's board *)

val p1_act_sweep : bool ref
(** [p1_act_sweep] represents whether sweep has been activated on player 1's board *)

val p1_act_fog : bool ref
(** [p1_act_fog] represents whether fog has been activated on player 1's board *)

val p2_act_sweep : bool ref
(** [p2_act_sweep] represents whether sweep has been activated on player 2's board *)

val p2_act_fog : bool ref
(** [p2_act_fog] represents whether fog has been activated on player 2's board *)

val p1_count : int ref
(** [p1_count] represents number of hits achieved by player 1 *)

val p2_count : int ref
(** [p2_count] represents number of hits achieved by player 2 *)

val p1_use_fog : bool ref
(** [p1_use_fog] represents if player 1 already used the fog power-up. 
    If [true], it deactivates player 1's ability to use fog *)

val p2_use_fog : bool ref
(** [p2_use_fog] represents if player 2 already used the fog power-up. 
    If [true], it deactivates player 2's ability to use fog *)

val fog_board : point array array
(** [fog_board] represents the empty board which is shown to the opponent 
    after the player activates the fog *)

val validate_input : string -> bool
(** [validate_input s] returns [true] if s represents a valid coordinate on
    the board. Returns [false] otherwise *)

val is_empty : point array array -> string -> bool
(** [is_empty] returns [true] if the coordinate represented by s has state 
    Empty on matrix m. Returns [false] otherwise. *)

val next_letter : string -> string
(** [next_letter s] returns the next alphabet to s. 
    For example A -> B, B -> C, and so on... until I -> J. 
    Additionally, J -> J *)

val list_coords : string -> ship -> int -> string list
(** [list_coords s sh n] returns list of coordinates that [ship sh] of length 
    [n] will span if it is placed on the board starting at coordinate s. *)

val all_true : bool list -> bool
(** [all_true lst] returns true if every element in a boolean list is [true] 
    and returns [false] otherwise. *)

val all_empty : point array array -> string list -> bool
(** [all_empty m lst] returns [true] if all the coordinates in the list are 
    Empty. Otherwise returns [false] *)

val validate_input2 : point array array -> string -> ship -> bool
(** [validate_input2 m s sh] returns [true] if the coordinate represented by s 
    is on board [m], if the orientation of ship [sh] is valid, and if [sh]
    will fit on [m] at that coordinate in that orientation. 
    Returns [false] otherwise *)

val check_orientation : string -> bool
(** [check_orientation s] returns [true] if orienation represented by s is valid.
    Returns [false] otherwise *)

val hor_ship : point array array -> ship -> string * string
(** [hor_ship m sh] asks user to input their leftmost coordinate for the 
    horizontal orientation of ship [sh] and then returns [(s1,s2)] where s1 
    is the leftmost coordinate of ship [sh] and s2 is the orientation of [sh]
    which is "H". *)

val vert_ship : point array array -> ship -> string * string
(** [hor_ship m sh] asks user to input their topmost coordinate for the 
    vertical orientation of ship [sh] and then returns [(s1,s2)] where s1 
    is the leftmost coordinate of ship [sh] and s2 is the orientation of [sh]
    which is "V". *)

val get_orientation : point array array -> ship -> string * string
(** [get_orientation m sh] asks user to input the orientation of ship [sh], 
    checks orientation, and then returns returns [(s1,s2)] where s1 
    is the leftmost coordinate of ship [sh] and s2 is the orientation of [sh]. *)

val print_helper : Player.player -> ship_type -> unit
(** [print_helper p st] prints information about the ship the player is placing 
    where [p] is the player placing the ship and [st] is the type of ship being 
    placed. *)

val place_ship : point array array -> ship_type -> Player.player -> unit
(** [place_ships m st p] prints the updated matrix when [player p] places 
    [ship st] on [board m]. *)

val p1_assign_fog : point array array -> unit
(** [p1_assign_fog m] assigns fog for player 1 on a random empty coordinate in 
    the matrix [m]. *)

val p1_assign_sweep : point array array -> unit
(** [p1_assign_sweep m] assigns sweep for player 1 on a random occupied coordinate 
    in the matrix [m]. *)

val p2_assign_fog : point array array -> unit
(** [p1_assign_fog m] assigns fog for player 2 on a random empty coordinate in 
    the matrix [m]. *)

val p2_assign_sweep : point array array -> unit
(** [p1_assign_sweep m] assigns sweep for player 2 on a random occupied coordinate 
    in the matrix [m]. *)

val assign_power_ups : point array array -> point array array -> unit
(** [assign_power_ups m1 m2] assigns the power up [fog] to a random coordinate 
    on [board m1] that has Empty state for player1 and a random 
    coordinate on [board m2] that has Empty state for player2. 
    Similarly, [assign_power_ups m1 m2] assigns the power up [sweep] to a 
    random coordinate on [board m1] that has Occupied state for player1 and 
    a random coordinate on [board m2] that has Occupied state for player2.
    Then it prints the resulting coordinates for each power up to the console. *)

val check_hit_fog : string -> Player.player -> unit
(** [check_hit_fog s p] checks if [player p1] hits the correct cell for [fog] and
    prints a message informing them that [fog] has been activated. Similarly,
    [check_hit_fog s p] checks if [player p2] hits the correct cell for [fog] and
    prints a message informing them that [fog] has been activated. *)

val check_hit_sweep : string -> Player.player -> unit
(** [check_hit_sweep s p] checks if [player p1] hits the correct cell for [sweep] and
    prints a message informing them that [sweep] has been activated. Similarly,
    [check_hit_sweep s p] checks if [player p2] hits the correct cell for [sweep] and
    prints a message informing them that [sweep] has been activated. *)

val coordinate_check :
  point -> Player.player -> point array array -> string -> unit
(** [coordinate_check c p m i] checks the state of coordinate [c] and checks
     whether the coordinate corresponds to either the [fog] power-up or the 
     [sweep] power-up. *)

val sweep : Player.player -> point array array -> unit
(** [sweep p m] asks player which row or column they wish to wipe out and 
    updates the states of those coordinates to [Hit] if they were [Occupied] and 
    [Miss] if they were [Empty]. *)

val fog_check : point array array -> Player.player -> unit
(** [fog_check m p] prints the [fog_board] if the opponent of [player p] chooses
    to activate fog. If not, the usual [matrix m] is printed. *)

val update_ships_and_print : ship -> Player.player -> point array array -> unit
(** [update_ships_and_print s p m] updates player hits based on the ship type 
    prints the game matrix. [s] is the ship being updated, [p] is the player 
    whose hits are being updated, and [m] is the game matrix. *)

val empty_helper : point -> point array array -> Player.player -> string -> unit
(** [empty_helper c m p i] displays the functionality for when the coordinate 
    [point p] is empty. The function prints "Miss!" to the terminal and checks 
    whether [player p] is allowed to use the [fog] functionality. If so,
    [player p] is asked if they would like to use the power-up. *)

val occupied_ship :
  point -> point array array -> Player.player -> ship -> string -> unit
(** [occupied_ship c m p s i] contains the functionality for when the coordinate
    [point p] is occupied. The function prints "Hit!" to the terminal and 
    checks whether [player p] is allowed to use the [sweep] functionality. If
    so, [player p] is asked if they would like to use the power-up.*)

val player_turn_pve : point array array -> Player.player -> unit
(** [player_turn_pve m p] asks [player p] to input their attack coordinate and
    the updated [board m] with all this information is printed to the console. *)

val player_turn_pvp : point array array -> Player.player -> unit
(** If opponent activates [fog] then [player_turn_pvp m p] informs [player p] of 
    [fog] being used by their opponent due to which their visibility of [board m] 
    is zero for one turn. Similarly, if the opponent activates [sweep] then 
    [player_turn_pvp m p] informs [player p] of [sweep] being used by their opponent
    due to which an entire row or column of their board can be sweeped out. 
    Additionally, [player p] is asked to input their attack coordinate and
    the updated board with all this information is printed to the console. *)

val game_loop :
  point array array ->
  point array array ->
  Player.player ->
  Player.player ->
  unit
(** [game_loop m1 m2 p1 p2] generates the game loop that alternate turns 
    between [player p1] and [player p2] and declares the winner when one player 
    sinks all five ships of the opponent. *)
