(* 1. a) *)
let zamenjaj ((a, b), (c, d)) = (a, c), (b, d)
    

(* 1. b) *)
let modus (x, y, z) = 
  if x + y + z = 3 * x then Some x else
    if x = y then Some x else
      if x = z then Some x else
        if y = z then Some y else None


(* 1. c) *)
let uncons list = match list with
| x :: xs -> Some (x, xs)
| [] -> None

(* 1. d) *)
let rec vstavljaj x list = match list with
| [] -> []
| y :: [] -> y :: []
| y :: ys -> y :: x :: (vstavljaj x ys)

(* 1. e) *)

let obrni list = 
  let rec obrni_aux list acc = match list with
  | [] -> acc
  | x :: xs -> obrni_aux xs (x :: acc)
in obrni_aux list []

let popolnoma_obrni list_of_lists = 
  let rec popolnoma_obrni_aux list acc = match list with
  | [] -> acc
  | x :: xs -> popolnoma_obrni_aux xs (obrni x :: acc)
in popolnoma_obrni_aux list_of_lists []
