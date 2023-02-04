type 'a kuhinjski_element =
| Ponev of 'a
| Lonec of 'a * 'a
| Omara of 'a list


let kuhinja = [Ponev"tuna"; Lonec("brokoli", "mango"); Omara["sir"; "toast"; "sok"; "ragu"]]

let prestej list = 
  let rec prestej rezultat = function
  | [] -> rezultat
  | (Ponev x) :: xs  -> prestej (rezultat + 1) xs
  | (Lonec (x, y)) :: xs -> prestej (rezultat + 2) xs
  | (Omara lst) :: xs -> prestej (rezultat + List.length lst) xs
in prestej 0 list


let pretvori_seznam f list =
  let rec pretvori_sez f acc = function
  | [] -> List.rev acc
  | x :: xs -> pretvori_sez f (f x :: acc) xs in 
  pretvori_sez f [] list

let pretvori f = function
| Ponev x -> Ponev (f x) 
| Lonec (x, y) -> Lonec (f x, f y)
| Omara list -> Omara (pretvori_seznam f list)


let pospravi list = 
  let rec pospravi_aux acc = function
  | (Ponev x) :: xs  -> pospravi_aux (x :: acc) xs
  | (Lonec (x, y)) :: xs -> pospravi_aux (x :: y :: acc) xs
  | (Omara lst) :: xs -> pospravi_aux (lst @ acc) xs
  | [] -> Omara acc
in pospravi_aux [] list


let sesetj_list list =
  let rec sestej vsota = function
  | [] -> vsota
  | x :: xs -> sestej (x + vsota) xs
in sestej 0 list
let oceni list cenilka =
  let rec oceni_aux rezultat cenilka = function
  | (Ponev x) :: xs  -> oceni_aux (cenilka x + rezultat) cenilka xs
  | (Lonec (x, y)) :: xs -> oceni_aux ((3 * (cenilka x + cenilka y)) + rezultat) cenilka xs
  | (Omara lst) :: xs -> oceni_aux ((5 * sesetj_list (List.map cenilka lst) ) + rezultat) cenilka xs
  | [] -> rezultat
in oceni_aux 0 cenilka list