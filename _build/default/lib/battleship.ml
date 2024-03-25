type ship_type = Carrier | Battleship | Cruiser | Submarine | Destroyer

type ship = {
  ship_type : ship_type;
  size : int;
  hits : int;
  is_sunk : bool;
  orientation : string;
}

type coordinate = string * int
type coordinate_state = Empty | Occupied of ship | Hit | Miss
type point = { coord : coordinate; state : coordinate_state }
