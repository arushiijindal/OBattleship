type ship_type =
  | Carrier
  | Battleship
  | Cruiser
  | Submarine
  | Destroyer
      (** [ship_type] represents the five types of ships in our game of battleship. *)

type ship = {
  ship_type : ship_type;
  size : int;
  hits : int;
  is_sunk : bool;
  orientation : string;
}
(** [ship] represents the five properties of a ship 
    [ship.ship_type] returns the type of the ship
    [ship.size] returns the number of holes in the ship
    [ship.hits] returns the number of hits in the ship
    [ship.is_sunk] returns [true] if the ship is sunk. Returns [false] otherwise
    [ship.orientation] returns "H" if the orientation of the ship is horizontal, 
    returns "V" if the orientation of the ship is vertical
 *)

type coordinate = string * int
(** [coordinate] represents the point in terms of (s,n) where s is the column 
    represented by alphabets A-J and n is the row represented by numbers 0-9 *)

type coordinate_state =
  | Empty
  | Occupied of ship
  | Hit
  | Miss
      (** [coordinate_state] represents the state of the coordinate that is being
    checked. 
    [Empty] means no ship has been placed nor has an attack been made there 
    [Occupied sh] means that coordinate is occupied by [ship sh]
    [Hit] means that the coordinate was attacked by opponent and one hole on a 
    ship was hit
    [Miss] means that the coordinate was attacked by opponent but no ships were
    hit
  *)

type point = { coord : coordinate; state : coordinate_state }
(** [point] returns the representation of a cell on the board which has a 
    coordinate to locate it and a state that keeps track of the game progress *)
