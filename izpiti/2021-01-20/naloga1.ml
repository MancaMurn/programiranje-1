let razlika_produkt_vosta x y = (x * y) - (x + y)

let zlimaj_para (x, y) (z, w) = (x, y, z, w)

let trojica_graficno (x, y, z) =
  let niz = function
    | None -> "-"
    | Some x -> string_of_int x
  in
  "(" ^ niz x ^ ", " ^ niz y ^ ", " ^ niz z ^ ")"


let nedeljivo_do x n = 
  let rec nedeljivo x m = 
    if m > n then true else
      match x mod m with
    | 0 -> false
    | _ -> nedeljivo x (m + 1)
  in nedeljivo x 2


let rezcepi_pri_None list = 
  let rec razcepi acc1 acc2 = function
  | [] -> List.rev (acc2 :: acc1)
  | (Some x) :: xs ->  razcepi acc1 (x :: acc2) xs
  | None :: xs -> razcepi (acc2 :: acc1) [] xs
in razcepi [] [] list


let l1 =  [Some 1; None; Some 2; Some 3; None; None; Some 4; None];;


