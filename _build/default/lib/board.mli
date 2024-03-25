open Battleship

val p : point
(** [p] represents the state (Empty, Occupied, Hit or Miss) and coordinate of
    each point on the board *)

val create_empty : int -> int -> point array array
(** [create_empty r c] returns [m] which represents an empty board with 
    r rows and c columns *)

val print_matrix : point array array -> bool -> unit
(** [print_matrix m] prints the board represented by m on the console *)

val mark_coordinate :
  point array array -> coordinate -> coordinate_state -> unit
(** [mark_coordinate m c s] updates the state of the point represented by 
    coordinate c on matrix m with the new state s  *)

val get_coordinate : point array array -> string -> point
(** [get_coordinate m s] returns [p] which represents the point that 
    corresponds to the coordinate represented by s on matrix m  *)

val assign_num : char -> int
(** [assign_num x] returns [n] which represents the number corresponding to x 
    starting with A --> 0, B --> 1, and so on up to J --> 9 *)

val assign_letter : int -> string
(** [assign_letter n] returns [s] which represents the letter corresponding to 
    n starting with 0 --> A, 1 --> B, and so on up to 9 --> J *)
