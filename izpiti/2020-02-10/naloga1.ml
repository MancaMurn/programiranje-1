
let skalarni_produkt (x, y, z) (a, b, c) = 
  x *. a +. y *. b +. z *. c

(* let fix_second f x = f y x  *)


let combine_and_filter f xs ys =
  let rec aux f xs ys acc = 
    match (xs, ys) with
    | (_, []) -> List.rev acc
    | ([], _) -> List.rev acc
    | (z :: zs, w :: ws) -> let element = f z w in 
        if element = None then aux f zs ws acc else
          aux f zs ws ((Option.get element) :: acc)
        in aux f xs ys []



let safe_minus x y = if x > y then Some (x-y) else None

let rec conditional_print f = function
| [] -> ()
| x :: xs when f x -> print_endline x; conditional_print f xs 
| x :: xs -> conditional_print f xs

let conditional_print f list =
  let rec aux f acc = function
  | [] -> acc
  | x :: xs when f x -> if acc = "" then aux f (acc ^ x) xs else aux f (acc ^ ", " ^ x) xs
  | x :: xs -> aux f acc xs
in 
print_string (aux f "" list) 


let long_enough s = String.length s > 3