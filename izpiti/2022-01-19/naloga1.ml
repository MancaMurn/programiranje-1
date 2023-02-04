let sta_pravokotna (x, y, z) (a, b, c) = (x * a + y * b + z * c = 0)

let postkopozicija f g x = g(f x)


let reverse list =
  let rec reverse_aux acc = function
  | [] -> acc
  | x :: xs -> reverse_aux (x :: acc) xs
in reverse_aux [] list


let dopolni x list =
  let rec dopolni_aux x acc = function
  | y :: ys -> if y == None then dopolni_aux x ( x :: acc) ys else dopolni_aux x (Option.get y :: acc) ys
  | [] -> reverse acc
in dopolni_aux x [] list



let rec potenca m n =
  let rec potenca m n r =
  match n with
  | 0 -> r
  | _ -> potenca m (n-1) (r * m)
  in potenca m n 1


let pretvori n list =
  let d = List.length list in 
    let rec pretvori_aux n d r = function
    | x :: xs -> pretvori_aux n (d-1) (((potenca n (d-1)) * x) + r) xs
    | [] -> r
in pretvori_aux n d 0 list