open Battleship

let p = { coord = ("", 0); state = Empty }

let create_empty rows cols =
  let grid = Array.make_matrix rows cols p in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let letter = Char.escaped (Char.chr (Char.code 'A' + j)) in
      grid.(i).(j) <- { p with coord = (letter, i) }
    done
  done;
  grid

let assign_letter i =
  if i = 0 then "A"
  else if i = 1 then "B"
  else if i = 2 then "C"
  else if i = 3 then "D"
  else if i = 4 then "E"
  else if i = 5 then "F"
  else if i = 6 then "G"
  else if i = 7 then "H"
  else if i = 8 then "I"
  else "J"

let print_matrix matrix hid =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  print_string "    ";
  for i = 0 to cols - 1 do
    let col = assign_letter i in
    print_string (col ^ "   ")
  done;
  print_newline ();
  print_string "  =╬===╬===╬===╬===╬===╬===╬===╬===╬===╬===╬=";
  print_newline ();
  for i = 0 to rows - 1 do
    Printf.printf "%2d | " i;
    for j = 0 to cols - 1 do
      match matrix.(i).(j).state with
      | Empty -> print_string "  | "
      | Occupied _ ->
          if not hid then print_string "O | " else print_string "  | "
      | Hit -> print_string "H | "
      | Miss -> print_string "M | "
    done;
    print_newline ();
    print_string "  =╬===╬===╬===╬===╬===╬===╬===╬===╬===╬===╬=";
    print_newline ()
  done

let mark_coordinate matrix coord state =
  match coord with
  | x, y when x >= "A" && x <= "J" && y >= 0 && y < Array.length matrix.(0) ->
      let col_index = int_of_char (String.get x 0) - int_of_char 'A' in
      let new_point = { (matrix.(y).(col_index)) with state } in
      matrix.(y).(col_index) <- new_point
  | _ -> ()

let get_coordinate matrix str =
  let col_str = String.make 1 (String.get str 0) in
  let col = int_of_char (String.get col_str 0) - int_of_char 'A' in
  let row = int_of_string (String.make 1 (String.get str 1)) in
  matrix.(row).(col)

let assign_num s =
  if s = 'A' then 0
  else if s = 'B' then 1
  else if s = 'C' then 2
  else if s = 'D' then 3
  else if s = 'E' then 4
  else if s = 'F' then 5
  else if s = 'G' then 6
  else if s = 'H' then 7
  else if s = 'I' then 8
  else 9
