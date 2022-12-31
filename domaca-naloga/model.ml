(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

let primer = 
  [|
    [|Some 2; None; None; None; Some 8; None; Some 3; None; None|];
    [|None; Some 6; None; None; Some 7; None; None; Some 8; Some 4|];
    [|None; Some 3; None; Some 5; None; None; Some 2; None; Some 9|];
    [|None; None; None; Some 1; None; Some 5; Some 4; None; Some 8|];
    [|None; None; None; None; None; None; None; None; None|];
    [|Some 4; None; Some 2; Some 7; None; Some 6; None; None; None|];
    [|Some 3; None; Some 1; None; None; Some 7; None; Some 4; None|];
    [|Some 7; Some 2; None; None; Some 4; None; None; Some 6; None|];
    [|None; None; Some 4; None; Some 1; None; None; None; Some 3|];
  |]

let get_row (grid : 'a grid) (row_ind : int) = 
  grid.(row_ind)

let rows grid = 
  List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let get_box (grid : 'a grid) (box_ind : int) = 
  Array.init 9 (fun ind -> grid.(ind/3 + ((box_ind / 3) * 3)).((ind mod 3) + ((box_ind mod 3) * 3)))

let boxes grid = List.init 9 (get_box grid)


let find_box (i, j) = 
  if 0 <= i && i <= 2 && 0 <= j && j <= 2 then 0 else
    if 0 <= i && i <= 2 && 3 <= j && j <= 5 then 1 else
      if 0 <= i && i <= 2 && 6 <= j && j <= 8 then 2 else
        if 3 <= i && i <= 5 && 0 <= j && j <= 2 then 3 else
          if 3 <= i && i <= 5 && 3 <= j && j <= 5 then 4 else
            if 3 <= i && i <= 5 && 6 <= j && j <= 7 then 5 else
              if 6 <= i && i <= 8 && 0 <= j && j <= 2 then 6 else
                if 6 <= i && i <= 8 && 3 <= j && j <= 5 then 7 else 8

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  Array.init 9 (fun ind -> (Array.map f grid.(ind)))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid) (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

(*Za prikaz sudokuja že imamo na voljo funkcijo print_grid, ki sprejme funkcijo a' -> string in grid*)

let primer_problema = {initial_grid = primer}

let cell_to_string = function
| Some x -> string_of_int x
| None -> " "

let print_problem problem : unit =
  print_grid cell_to_string problem.initial_grid

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = 
  print_problem solution

(*Za preverjanje pravilne rešitve potrebujemo preveriti:
   -> rešitev ustreza problemu (torej da ima rešitev na mestih, ki v problemu niso prazna enake znake)
   -> v vrstici so različne števke
   -> v stolpcu so različne števke
   -> v boxu so različne števke*)


let primer_resitve =  
  [|
    [|Some 2; Some 4; Some 5 ; Some 9; Some 8; Some 1; Some 3; Some 7; Some 6|];
    [|Some 1; Some 6; Some 9; Some 2; Some 7; Some 3; Some 5; Some 8; Some 4|];
    [|Some 8; Some 3; Some 7; Some 5; Some 6; Some 4; Some 2; Some 1; Some 9|];
    [|Some 9; Some 7; Some 6; Some 1; Some 2; Some 5; Some 4; Some 3; Some 8|];
    [|Some 5; Some 1; Some 3; Some 4; Some 2; Some 8; Some 6; Some 2; Some 7|];
    [|Some 4; Some 8; Some 2; Some 7; Some 3; Some 6; Some 9; Some 5; Some 1|];
    [|Some 3; Some 9; Some 1; Some 6; Some 5; Some 7; Some 8; Some 4; Some 2|];
    [|Some 7; Some 2; Some 8; Some 3; Some 4; Some 9; Some 1; Some 6; Some 5|];
    [|Some 6; Some 5; Some 4; Some 8; Some 1; Some 2; Some 7; Some 9; Some 3|];
  |]

let v_1 = [|Some 2; Some 4; Some 5 ; Some 9; Some 8; Some 1; Some 3; Some 7; Some 6|]
let v_2 = [|Some 2; Some 4; None ; None; Some 8; Some 1; None; Some 7; Some 6|]
let v_3 = [|Some 2; Some 5; Some 4 ; Some 9; Some 8; Some 1; Some 3; Some 7; Some 6|]


let option_int_to_int cell =
  match cell with
  | Some x -> x
  | None -> 0     
    
let option_array_to_int_array arr =
  Array.map option_int_to_int arr

let find_int_in_array n array =
  let arr = option_array_to_int_array array in 
    let rec find_in_arr n arr ind =
      match ind with
      | 0 -> arr.(ind) = n
      | _ -> if arr.(ind) = n then true else find_in_arr n arr (ind - 1) 
    in
    find_in_arr n arr 8
  
(*funkcija, ki pri tabeli dolžine 9 (npr. vrsta v mreži) preveri, če vsebuje vse razpične števke od 1 do 9*)
let valid_array arr1 =
  let arr = option_array_to_int_array arr1 in 
  let rec valid_arr n arr = 
    match n with
    | 0 -> true
    | _ -> let f x = (n = x) in
      (if (Array.exists f arr) = true then valid_arr (n - 1) arr else false) 
    in
    valid_arr 9 arr 

  
let rec valid_list_of_arrays = function
| [] -> true
| arr :: rest -> if valid_array arr = true then valid_list_of_arrays rest else false

let valid_rows grid = 
  let r = rows grid in
    valid_list_of_arrays r

let valid_columns grid = 
  let c = columns grid in 
    valid_list_of_arrays c

let valid_boxes grid =
  let b = boxes grid in 
    valid_list_of_arrays b


(*funkcija, ki preveri, če se vrstici ujemata torej, če se celice, ki niso prazne v nobeni od vrstic ujemajo*)
let matching_arrays arr_p arr_s =
  let rec match_arr arr_p arr_s n =
    if n < 0 then true else
    match arr_p.(n) with
    | None -> match_arr arr_p arr_s (n - 1)
    | Some x -> if arr_s.(n) = Some x then match_arr arr_p arr_s (n - 1) else false 
  in
  match_arr arr_p arr_s 8


(* funkcija, ki preveri, če se grid za problem in grid za rešitev ujemata na nepraznih mestih.*)
let solution_to_problem problem solution = 
  let rec sol_to_prob problem solution n =
    if n < 0 then true else
      if matching_arrays problem.(n) solution.(n) = true 
        then sol_to_prob problem solution (n - 1) 
        else false 
  in sol_to_prob problem solution 8



let is_valid_solution problem solution = 
  if solution_to_problem problem solution = false then false else
    if valid_columns solution = false then false else
      if valid_columns solution = false then false else
        valid_boxes solution