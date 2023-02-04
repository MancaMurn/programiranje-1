type 'a merkle = List | Vozlisce of 'a vozlisce
and 'a vozlisce = {
  levo : 'a merkle;
  podatek : 'a;
  desno : 'a merkle;
  zgostitev : int;
}

type 'a zgostitev = int -> 'a -> int -> int


let primer_h l p d = ((l * 3) + (p * 5) + (d * 7)) mod 11

let drevo : int merkle = Vozlisce {
  levo = Vozlisce {
    levo = Vozlisce { levo = List; podatek = 10; desno = List; zgostitev = 6 };
    podatek = 14;
    desno = Vozlisce { levo = List; podatek = 474; desno = List; zgostitev = 5 };
    zgostitev = 2;
  };
  podatek = 57;
  desno = Vozlisce {
    levo = List;
    podatek = 12;
    desno = Vozlisce { levo = List; podatek = 513; desno = List; zgostitev = 2 };
    zgostitev = 8;
  };
  zgostitev = 6;
}

let drevo_n : int merkle = Vozlisce {
  levo = Vozlisce {
    levo = Vozlisce { levo = List; podatek = 10; desno = List; zgostitev = 6 };
    podatek = 14;
    desno = Vozlisce { levo = List; podatek = 474; desno = List; zgostitev = 5 };
    zgostitev = 2;
  };
  podatek = 57;
  desno = Vozlisce {
    levo = List;
    podatek = 12;
    desno = Vozlisce { levo = List; podatek = 513; desno = List; zgostitev = 3 };
    zgostitev = 8;
  };
  zgostitev = 5;
}

let drevo_m : int merkle = Vozlisce {
  levo = Vozlisce {
    levo = Vozlisce { levo = List; podatek = 10; desno = List; zgostitev = 6 };
    podatek = 14;
    desno = Vozlisce { levo = List; podatek = 474; desno = List; zgostitev = 5 };
    zgostitev = 2;
  };
  podatek = 57;
  desno = Vozlisce {
    levo = List;
    podatek = 12;
    desno = Vozlisce { levo = List; podatek = 513; desno = List; zgostitev = 2 };
    zgostitev = 8;
  };
  zgostitev = 3;
}

let zgosti = function 
|List -> 0 
|Vozlisce vozlisce -> vozlisce.zgostitev


let rec izracunaj_zgostitev f = function
| List -> 0
| Vozlisce vozlisce -> 
    if ( zgosti vozlisce.levo = 0 && zgosti vozlisce.desno = 0) then f 0 vozlisce.podatek 0 else
      f (izracunaj_zgostitev f vozlisce.levo ) vozlisce.podatek (izracunaj_zgostitev f vozlisce.desno) 


let oceni f drevo = if zgosti drevo = izracunaj_zgostitev f drevo then 0 else 1


let rec prestej_napacne f drevo = match drevo with
| List -> 0
| Vozlisce vozlisce -> prestej_napacne f vozlisce.levo + prestej_napacne f vozlisce.desno + oceni f drevo 

let preveri f drevo = prestej_napacne f drevo = 0

let popravi_vozlisce f vozlisce : int vozlisce = 
  {levo = vozlisce.levo; 
  podatek = vozlisce.podatek; 
  desno = vozlisce.desno; 
  zgostitev = f (zgosti vozlisce.levo) vozlisce.podatek (zgosti vozlisce.desno)}


let rec popravi f drevo =
  if preveri f drevo = true then drevo else 
    match drevo with   
    | List -> drevo
    | Vozlisce vozlisce -> Vozlisce {levo = popravi f vozlisce.levo; 
                            podatek = vozlisce.podatek;
                            desno = popravi f vozlisce.desno;
                            zgostitev = (f (zgosti vozlisce.levo) vozlisce.podatek (zgosti vozlisce.desno))}
