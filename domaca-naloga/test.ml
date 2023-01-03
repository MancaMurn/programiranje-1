type 'a grid = 'a Array.t Array.t


type problem = { initial_grid : int option grid }

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



let is_empty_cell grid (i, j) =
  grid.(i).(j) = None



  let option_int_to_int cell =
  match cell with
  | Some x -> x
  | None -> 0     


let option_array_to_int_array arr =
  Array.map option_int_to_int arr

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  Array.init 9 (fun ind -> (Array.map f grid.(ind)))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid


let find_int_in_array n array =
  let arr = option_array_to_int_array array in 
    let rec find_in_arr n arr ind =
      match ind with
      | 0 -> arr.(ind) = n
      | _ -> if arr.(ind) = n then true else find_in_arr n arr (ind - 1) 
    in
    find_in_arr n arr 8


let find_box (i, j) = 
  if 0 <= i && i <= 2 && 0 <= j && j <= 2 then 0 else
    if 0 <= i && i <= 2 && 3 <= j && j <= 5 then 1 else
      if 0 <= i && i <= 2 && 6 <= j && j <= 8 then 2 else
        if 3 <= i && i <= 5 && 0 <= j && j <= 2 then 3 else
          if 3 <= i && i <= 5 && 3 <= j && j <= 5 then 4 else
            if 3 <= i && i <= 5 && 6 <= j && j <= 7 then 5 else
              if 6 <= i && i <= 8 && 0 <= j && j <= 2 then 6 else
                if 6 <= i && i <= 8 && 3 <= j && j <= 5 then 7 else 8





type available = { loc : int * int; possible : int list }

type state = { problem : problem; current_grid : int option grid; options : available list }







let find_options grid (i, j) =
  let r = get_row grid i in
    let c = get_column grid j in    
      let b = get_box grid (find_box (i, j)) in
        let rec find_options_aux (i, j) row column box n acc =
          match n with
          | 0 -> {loc = (i, j); possible = acc }
          | _ -> if ((find_int_in_array n row) = true || (find_int_in_array n column) = true || (find_int_in_array n box) = true )
                  then find_options_aux (i, j) row column box (n-1) acc else
                    find_options_aux (i, j) row column box (n-1) (n :: acc)
        in
        find_options_aux (i, j) r c b 8 [] 









let make_avalible_list grid =
  let rec make_avalible_list_aux grid (i, j) acc =
    match (i, j) with 
    | (0, 0) -> if is_empty_cell grid (i, j) = true then (find_options grid (i, j)) :: acc else acc
    | (_, 0) -> if is_empty_cell grid (i, j) = true then make_avalible_list_aux grid (i-1, 8) ((find_options grid (i, j)) :: acc) else
        make_avalible_list_aux grid (i-1, 8) acc
    | (_, _) -> if is_empty_cell grid (i, j) = true then make_avalible_list_aux grid (i, j-1) ((find_options grid (i, j)) :: acc) else
        make_avalible_list_aux grid (i, j-1) acc
    in 
    make_avalible_list_aux grid (8, 8) []



let initialize_state (problem : problem) : state =
  { problem = problem; current_grid = copy_grid problem.initial_grid; options = make_avalible_list problem.initial_grid }



let rec find_first_n_options n available_list =
  match available_list with
  | [] -> None
  | available :: xs -> if (List.length available.possible ) = n then Some available else
    find_first_n_options n xs 

let find_all_n_options n available_list =
  let rec find_all n acc = function 
  | [] -> acc
  | available :: rest -> if List.length available.possible = n then find_all n (available :: acc) rest else
      find_all n acc rest 
  in
  find_all n [] available_list 

let fill_cell grid (i, j) n =
  grid.(i).(j) <- Some n 

let return_filled_grid grid (i, j) n =
  fill_cell grid (i, j) n;
  copy_grid grid


let return_filled_state state (i, j) n =
  fill_cell state.current_grid (i, j) n; 
  {problem = state.problem; current_grid = state.current_grid; options = make_avalible_list state.current_grid}






let simple_cells grid available_list = 
  let cells = find_all_n_options 1 available_list in 
    let rec fill grid cells = 
      match cells with
      | [] -> grid
      | {loc; possible} :: rest -> match possible with
          | [] -> fill grid rest
          (*xs = [], saj smo našli simple_cells, ki imajo samo eno možnost zapolitve.*)
          | x :: xs -> fill  (return_filled_grid grid loc x)  rest 
    in 
    fill grid cells
      

let fill_simple_cells (state : state) : state = 
  let new_grid = simple_cells state.current_grid state.options in 
    let new_options = make_avalible_list new_grid in 
      {problem = state.problem; current_grid = new_grid; options = new_options}





let branch_state (state : state) : (state * state) option =
  let available = find_first_n_options 2 state.options in 
   if available = None then None else 
    let element = Option.get available in 
  match element.possible with
  (* xs = [], saj smo zgoraj našli celico, ki ji pripada seznam dolžine dva.*)
  | x :: y :: xs -> 
    let st_1 = return_filled_state state element.loc x 
    in
      let st_2 = return_filled_state state element.loc y
      in Some (st_1, st_2) 
  | [] -> None
  | x :: xs -> None




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



  
let primer_problema = {initial_grid = primer}


(* fill_simple_cells (initialize_state primer_problema);; *)