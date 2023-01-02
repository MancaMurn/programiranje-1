type available = { loc : int * int; possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid; options : available list }


let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid


type response = Solved of Model.solution | Unsolved of state | Fail of state


let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state


(* pomožna funkcija, ki bo za dano mrežo in polje v njej poiskala možnosti*)
let find_options grid (i, j) =
  let r = Model.get_row grid i in
    let c = Model.get_column grid j in    
      let b = Model.get_box grid (Model.find_box (i, j)) in
        let rec find_options_aux (i, j) row column box n acc =
          match n with
          | 0 -> {loc = (i, j); possible = acc }
          | _ -> if ((Model.find_int_in_array n row) = true || (Model.find_int_in_array n column) = true || (Model.find_int_in_array n box) = true )
                  then find_options_aux (i, j) row column box (n-1) acc else
                    find_options_aux (i, j) row column box (n-1) (n :: acc)
        in
        find_options_aux (i, j) r c b 8 [] 


let make_avalible_list grid =
  let rec make_avalible_list_aux grid (i, j) acc =
    match (i, j) with 
    | (0, 0) -> if Model.is_empty_cell grid (i, j) = true then (find_options grid (i, j)) :: acc else acc
    | (_, 0) -> if Model.is_empty_cell grid (i, j) = true then make_avalible_list_aux grid (i-1, 8) ((find_options grid (i, j)) :: acc) else
        make_avalible_list_aux grid (i-1, 8) acc
    | (_, _) -> if Model.is_empty_cell grid (i, j) = true then make_avalible_list_aux grid (i, j-1) ((find_options grid (i, j)) :: acc) else
        make_avalible_list_aux grid (i, j-1) acc
    in 
    make_avalible_list_aux grid (8, 8) []


let initialize_state (problem : Model.problem) : state =
  { problem = problem; current_grid = Model.copy_grid problem.initial_grid; options = make_avalible_list problem.initial_grid }


let rec find_first_n_options n available_list =
  match available_list with
  | [] -> failwith "No cell has that many options."
  | available :: xs -> if (List.length available.possible ) = n then available else
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
  Model.copy_grid grid


let remove_from_avaliable_list available_list (i, j) =
  let rec remove_aux available_list (i, j) acc =
  match available_list with
  | [] -> acc
  | available :: rest -> if available.loc = (i, j) then acc else remove_aux rest (i, j) (available :: acc) 
  in
  remove_aux available_list (i, j) []


(*funkcija, ki bo zapolnila vse celice, kjer imamo že na začetku samo eno možnost.*)
let fill_simple_cells (state : state) : state = 
  let sipmle_cells = find_all_n_options 1 state.options in 
    let rec fill state sipmle_cells = 
      match sipmle_cells with
      | [] -> {problem = state.problem; current_grid = state.current_grid; options = state.options }
      | {loc; possible} :: rest -> match possible with
          | [] -> fill state rest
          | x :: xs -> fill {problem = state.problem; 
                            current_grid = (return_filled_grid state.current_grid loc x); 
                            options = remove_from_avaliable_list state.options loc } 
                            rest 
    in 
    fill state sipmle_cells  

(* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
let branch_state (state : state) : (state * state) option =
  let available = find_first_n_options 2 state.options in 
  match available.possible with
  (* xs = [], saj smo zgoraj našli celico, ki ji pripada seznam dolžine dva.*)
  | x :: y :: xs -> 
    let st_1 = {problem = state.problem; 
              current_grid = return_filled_grid state.current_grid available.loc x; 
              options = remove_from_avaliable_list state.options available.loc} 
    in
      let st_2 = {problem = state.problem; 
                current_grid = return_filled_grid state.current_grid available.loc y; 
                options = remove_from_avaliable_list state.options available.loc} 
      in Some (st_1, st_2) 
  | [] -> None
  | x :: xs -> None
  
  


(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  match validate_state state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'


and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> fill_simple_cells |> solve_state