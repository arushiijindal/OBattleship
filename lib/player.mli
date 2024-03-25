type player = {
  mutable num_sunk : int;
  mutable carrier_hits : int;
  mutable battleship_hits : int;
  mutable cruiser_hits : int;
  mutable submarine_hits : int;
  mutable destroyer_hits : int;
  number : int;
}
(** [player] represents the characteristics of a player in the game. 
    [player.num_sunk] represents the number of hits on the player's ships
    [player.carrier_hits] represents the number of hits on the player's carrier
    [player.battleship_hits] represents the number of hits on the player's battleship
    [player.cruiser_hits] represents the number of hits on the player's cruiser
    [player.submarine_hits] represents the number of hits on the player's submarine
    [player.destroyer_hits] represents the number of hits on the player's destroyer
    [player.number] represents whether [player] represents player1 or player2
*)

val default_player : unit -> player
(** [default_player()] represents the initial state of a player at the 
    beginning of a game. All values of [player] except player.number are set to 
    0 and player.number is set to 1. *)

val update : player -> unit
(** [update p] checks if the ships of [player p] have completely sunk and
    if they have, then it increments player.num_sunk by the number of ships
    that have compeletely sunk. *)
