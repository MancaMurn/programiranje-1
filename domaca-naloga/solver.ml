type available = { loc : int * int; possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid; options : available list }

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; problem }

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
  let row = Model.get_row grid i in
    let column = Model.get_column grid j in    
      let box = Model.get_box grid (Model.find_box (i, j)) in
        let rec find_options_aux row column box n acc =
          match n with
          | 0 ->  available = {loc = (row, column); possible = acc }
          | _ -> if ((Model.find_int_in_array n row) = true || (Model.find_int_in_array n column) = true || (Model.find_int_in_array n box) = true )
                  then find_options_aux row column box (n-1) acc else
                    find_options_aux row column box (n-1) (n :: acc)
        in
        find_options_aux row column box 9 [] 

let primer = Model.primer

let rec find_n_options n available_list =
  match available_list with
  | [] -> None
  | available :: xs -> if (List.length available.possible ) = n then available else
    find_n_options xs


let fill_cell grid (i, j) n =
  grid.(i).(j) <- Some n 

let remove_from_avaliable_list available_list (i, j) =
  let rec remove_aux available_list (i, j) acc =
  match available_list with
  | [] -> acc
  | {cell, list} :: rest -> if cell = (i, j) then acc else remove_aux rest (i, j) ({cell, list} :: acc)

  
(* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
let branch_state (state : state) : (state * state) option =
  let available = find_n_options 2 state.options in 
  match available with
  | None -> None
  (* xs = [], saj smo zgoraj našli celico, ki ji pripada ta seznam dolžine dva.*)
  | {(i, j), x :: y :: xs} -> 
    let st_1 = {state.problem, fill_cell state.current_grid (i, j) x , remove_from_avaliable_list state.possible (i, j)} 
    in
    let st_2 = {state.problem, fill_cell state.current_grid (i, j) y , remove_from_avaliable_list state.possible (i, j)} 
    in
    (st_1, st_2) 

  
  


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
  problem |> initialize_state |> solve_state