let pitagorejska_trojica (a,b,c) = a * a + b * b = c * c

let priblizek_korena x = 
  let rec priblizek_aux n x =
    if n * n <= x && (n + 1) * (n + 1) > x then n else 
      priblizek_aux (n + 1) x
    in priblizek_aux 0 x 


let razdeli list = 
  let rec razdeli_aux soda liha = function
  | [] -> List.rev soda @ List.rev liha
  | x :: xs -> if x mod 2 = 0 then razdeli_aux (x :: soda) liha xs else
              razdeli_aux soda (x :: liha) xs
  in razdeli_aux [] [] list


let rec potenca m n =
  let rec potenca m n r =
  match n with
  | 0 -> r
  | _ -> potenca m (n-1) (r * m)
  in potenca m n 1


let izpisi_soda_liha list = 
  let urejen_list = razdeli list in
    let d = List.length urejen_list in 
      let rec izpisi d rezultat = function
      | [] -> rezultat
      | x :: xs -> izpisi (d - 1) (rezultat + x * (potenca 10 (d - 1))) xs
in izpisi d 0 urejen_list


let alternirajoci_konstruktorji list =
  match list with
  | [] -> true
  | x :: xs -> let prvi = x in 
    let rec alter_konst prejsnji = function
    | [] -> true
    | x :: xs -> match (x, prejsnji) with
      | (None, None) -> false
      | (_, None) -> alter_konst x xs
      | (None, _) -> alter_konst x xs
      | (_, _) -> false 
in alter_konst prvi xs


let l1 = [-100;  1; 0;  100; 50];;
let l2 = [14; 11; 10];;
let l3 = [Some 1; None; Some 10; None];;



let najmanjsi_element list =
  match list with
  | [] -> None
  | x :: xs -> let prvi = x in 
    let rec min trenutni = function
    | [] -> Some trenutni
    | x :: xs -> if x < trenutni then min x xs else min trenutni xs
in min prvi xs

let indeks_najmanjsi list = 
  let d = List.length list in 
    let rec indeks k list = 
      if k = d then None else
        match list with
            | [] -> None
            | x :: xs -> if najmanjsi_element list = Some x then Some k else
            indeks (k + 1) xs
            in indeks 0 list  



let najmanjsi_rezultat x flist = 
  let rec rezultati x acc = function
  | [] -> indeks_najmanjsi (List.rev acc)
  | y :: ys -> rezultati x ((y x) :: acc) ys
in rezultati x [] flist
