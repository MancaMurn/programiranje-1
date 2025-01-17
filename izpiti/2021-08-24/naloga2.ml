(*============================================================================*]
  Pri tej nalogi bomo za slovar uporabili kar enostavno implementacijo z 
  asociativnim seznamom, ki smo jo spoznali na predavanjih.
  S spodaj definiranimi funkcijami si lahko pomagate pri vseh podnalogah.
[*============================================================================*)

type ('a, 'b) slovar = ('a * 'b) list

let prazen_slovar : ('a, 'b) slovar = []

let velikost (m : ('a, 'b) slovar) = List.length m

let vsebuje (x : 'a) (m : ('a, 'b) slovar) = List.mem_assoc x m

(* Vrne vrednost, ki pripada ključu ali None *)
let najdi x (m : ('a, 'b) slovar) = List.assoc_opt x m

(* Doda vrednost v slovar in povozi prejšnjo, če obstaja *)
let dodaj (k, v) (m : ('a, 'b) slovar) = (k, v) :: List.remove_assoc k m

(*============================================================================*]
  Matematične izraze predstavimo z dvojiškimi drevesi, v katerih vozlišča predstavljajo 
  aritmetične operacije, listi pa števila ali spremenljivke, predstavljene z nizi.
  Izraz v drevo pretvorimo tako, da pri operaciji levi podizraz vzamemo za levo 
  poddrevo, desni podizraz za desno, v vozlišče pa zapišemo operator.
[*============================================================================*)

type operator = Plus | Minus | Krat | Deljeno

type 'a izraz =
  | Spremenljivka of string
  | Konstanta of 'a
  | Operacija of ('a izraz * operator * 'a izraz)

(* (x - 3)- (y * (z / x))  *)
let primer =
  Operacija
    ( Operacija (Spremenljivka "x", Minus, Konstanta 3),
      Minus,
      Operacija
        ( Spremenljivka "y",
          Krat,
          Operacija (Spremenljivka "z", Deljeno, Spremenljivka "x") ) )

(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `prestej : izraz -> int`, ki vrne število vseh "različnih" 
  spremenljivk v izrazu.
[*----------------------------------------------------------------------------*)


let rec poberi acc = function
| Spremenljivka x -> x :: acc
| Konstanta m -> acc
| Operacija (i1, operator, i2) -> poberi acc i1 @ poberi acc i2 

let rec vsebuje_sez a = function
| [] -> false
| x :: xs -> if x = a then true else vsebuje_sez a xs


let rec pocisti = function
| [] -> []
|x :: xs -> if vsebuje_sez x xs then pocisti xs else [x] @ pocisti xs


let rec prestej izraz = 
  List.length (pocisti (poberi [] izraz))

(* b *)
(*----------------------------------------------------------------------------*]
Napišite funkcijo `izlusci : 'a izraz -> (string * int) slovar`, ki sprejme izraz 
in vrne slovar, ki pove, kolikokrat se posamezna spremenljivka pojavi v izrazu. 
Vrstni red v slovarju ni pomemben.
[*----------------------------------------------------------------------------*)

let izlusci izraz = 
  let seznam = poberi [] izraz in 
  let slovar = prazen_slovar in 
    let rec izlusci_aux slovar = function
    | [] -> slovar
    | x :: xs -> if vsebuje x slovar then izlusci_aux (dodaj (x, Option.get (najdi x slovar) + 1) slovar) xs else 
      izlusci_aux(dodaj (x, 1) slovar) xs
    in
    izlusci_aux slovar seznam 


(* c *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `izracunaj : (string * int) slovar -> int izraz -> option int`, 
  ki sprejme izraz in slovar vrednosti spremenljivk ter poskuša izračunati vrednost 
  izraza. Če to ni mogoče (deljenje z 0 ali manjkajoča definicija spremenljivke), 
  naj bo rezultat `None`. 
    # izracunaj [("x",3); ("y", 4); ("z",5)] primer;;
    - : int option = Some (-4)
[*----------------------------------------------------------------------------*)

let rec izracunaj slovar = function
| Spremenljivka x -> najdi x slovar 
| Konstanta a -> Some a
| Operacija (i1, operator, i2) -> match operator with
      | Plus -> Some (Option.get (izracunaj slovar i1) + Option.get (izracunaj slovar i2))
      | Minus -> Some (Option.get (izracunaj slovar i1) - Option.get (izracunaj slovar i2))
      | Krat -> Some (Option.get (izracunaj slovar i1) * Option.get (izracunaj slovar i2))
      | Deljeno -> let delitelj = izracunaj slovar i2 in 
        if delitelj = Some 0 then None else 
          Some (Option.get (izracunaj slovar i1) / Option.get (delitelj))



(* c *)
(*----------------------------------------------------------------------------*]
  Ocenite časovno zahtevnost funkcije `izracunaj` v odvisnosti od velikosti 
  izraza `n` (torej števila vseh vozlišč in listov v drevesu) ter števila različnih 
  spremenljivk `m`.
  Kako se časovna zahtevnost spremeni, če bi za slovar uporabili uravnoteženo iskalno drevo?
[*----------------------------------------------------------------------------*)


