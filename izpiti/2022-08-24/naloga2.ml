type 'a tape = Tape of { left : 'a list; head : 'a; right : 'a list }

type 'a command = Left | Do of ('a -> 'a) | Right

let example = Tape { left = [ 3; 2; 1 ]; head = 4; right = [ 5; 6 ] }

(* 2. a) *)

let map_list list f =
  let rec map_list_aux list f acc = match list with
  | [] -> acc
  | x :: xs -> map_list_aux xs f (f x :: acc)
  in map_list_aux list f

(* let map (tape : 'a tape) (f: 'a -> 'a) : 'a tape = {(map_list tape.left f); (f tape.head); (map_list tape.right f)} *)
  
(* 2. b) *)

let izvedi _ = failwith "TODO"

(* 2. c) *)

let izvedi_ukaze _ = failwith "TODO"

(* 2. d) *)

let naberi_in_pretvori _ = failwith "TODO"

(* 2. e) *)

let pripravi_ukaze _ = failwith "TODO"
