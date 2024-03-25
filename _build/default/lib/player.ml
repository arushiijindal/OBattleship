type player = {
  mutable num_sunk : int;
  mutable carrier_hits : int;
  mutable battleship_hits : int;
  mutable cruiser_hits : int;
  mutable submarine_hits : int;
  mutable destroyer_hits : int;
  number : int;
}

let default_player () : player =
  {
    num_sunk = 0;
    carrier_hits = 0;
    battleship_hits = 0;
    cruiser_hits = 0;
    submarine_hits = 0;
    destroyer_hits = 0;
    number = 1;
  }

let update player =
  if player.carrier_hits = 5 then (
    player.num_sunk <- player.num_sunk + 1;
    player.carrier_hits <- -1;
    print_string "You sunk a carrier!\n")
  else if player.battleship_hits = 4 then (
    player.num_sunk <- player.num_sunk + 1;
    player.battleship_hits <- -1;
    print_string "You sunk a battleship!\n")
  else if player.cruiser_hits = 3 then (
    player.num_sunk <- player.num_sunk + 1;
    player.cruiser_hits <- -1;
    print_string "You sunk a cruiser!\n")
  else if player.submarine_hits = 3 then (
    player.num_sunk <- player.num_sunk + 1;
    player.submarine_hits <- -1;
    print_string "You sunk a submarine!\n")
  else if player.destroyer_hits = 2 then (
    player.num_sunk <- player.num_sunk + 1;
    player.destroyer_hits <- -1;
    print_string "You sunk a destroyer!\n")
