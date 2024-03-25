open Battleship

val target_mode : bool ref
(** [target_mode] toggles whether the computer is in Hunt or Target mode. 
    [true] -> target mode, [false] -> hunt mode *)

val last_hit : (string * int) ref
(** [last_hit] represents the coordinate that was last hit *)

val validate_coor : point array array -> string * int -> ship -> bool
(** [validate_coor m c s] returns [true] if the coordinate is on the board,
    if the ship orientation is valid, and if the ship will fit on the board 
    at that coordinate in that orientation. Returns [false] otherwise *)

val get_rand_coord_orient : point array array -> ship -> (string * int) * string
(** [get_rand_coord_orient m s] returns a valid tuple [((s1,n),s2)] where (s1, n) 
    represents a random coordinate and s2 represents a random orientation. Used
    for random placement of ships *)

val place_ship_random : point array array -> ship_type -> unit
(** [place_ship_random m s] places the ship in on the matrix at the coordinate 
    and in the orientation chosen by the computer for the its turn *)

val generate_random : unit -> (string * int) option
(** [generate_random ()] returns [Some (s,n)] which represents a randomly 
    generated row-column pair on the board. Otherwise returns [None]*)

val get_coord : point array array -> string * int -> point
(** [get_coord m (s,n)] returns [point] which represents the point on the 
    board that is associated with the coordinate (s,n) *)

val generate_target : string * int -> point array array -> (string * int) option
(** [generate_target (s,n) m] returns [Some (s1, n1)] which represents the next 
    target on the board based on what counted as a hit previously. Otherwise it
    returns [None] *)

val ship_is_sunk : ship -> Player.player -> bool
(** [ship_is_sunk s p] returns [true] if the ship has been sunk and 
    returns [false] otherwise *)

val computer_turn : point array array -> Player.player -> unit
(** [computer_turn m p] simulates the computer's turn and checks if the 
    coordinate attacked is a hit or a miss and prints the relevant messages 
    accordingly *)

val game_loop_pve :
  point array array ->
  point array array ->
  Player.player ->
  Player.player ->
  unit
(** [game_loop_pve m1 m2 p c] generates the game loop that alternate turns 
    between the player and computer and declares the winner when one sinks
    all five ships of the opponent *)
