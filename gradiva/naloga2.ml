(*definicija traku*)
type 'a tape = Tape of { left : 'a list; head : 'a; right : 'a list }

type 'a command = Left | Do of ('a -> 'a) | Right

let example = Tape { left = [ 3; 2; 1 ]; head = 4; right = [ 5; 6 ] }

(* 2. a) *)

let obrni sez =
  let rec pomozna acc =
    function 
    | [] -> acc
    | gl::rep -> pomozna (gl::acc) rep
  in pomozna [] sez 

let map sez f =
  let rec pomozna acc =
    function 
    | [] -> obrni acc
    | gl::rep -> pomozna ((f gl) :: acc) rep 
  in pomozna [] sez

let map (trak : 'a tape) f = 
  (*izluščimo kose traku*)
  let Tape {left = levi; head = glava; right = desni} = trak in 
  (*na vsakem kosu posebej izvedemo map*)
  let nov_levi = map levi f in 
  let nov_desni = map desni f in 
  let nova_glava = f glava in
  (*sestavimo nov trak*)
  Tape {left = nov_levi; head = nova_glava; right = nov_desni}


(* 2. b) *)

let izvedi trak command = 
  let Tape {left = levi; head = glava; right = desni} = trak in
  match command with 
  | Do f ->
    Some (Tape {left = levi; head = f glava; right = desni})
  | Left -> 
    (*tu se je funkcija match prekrivala z zunanjo, zato sem uporabila funkcijo if *)
    let obrnjen = obrni levi in (*za lačje dostopanje do zadnjega elementa*)
    if obrnjen = [] then  
      None
    else 
      let gl::rep = obrnjen in (*obrnemo nazaj do pravega zaporedja*)
      Some (Tape {left = obrni rep ; head = gl; right = glava :: desni})
  | Right -> 
    match desni with 
    | [] -> None 
    | gl::rep -> 
      let nov_levi = levi @ [glava] in 
      Some (Tape {left = nov_levi; head = gl; right = rep})

(* 2. c) *)

let izvedi_ukaze trak seznam =
  let rec pomozna acc = 
    function 
    | [] -> acc
    | gl::rep -> 
      let rezultat = izvedi trak gl in 
      match rezultat with 
      | None -> acc
      | Some x -> pomozna x rep 
  in pomozna trak seznam 

(* 2. d) *)

(*pomožna funkcija za shranjevanje stanj glave*)
let spremembe_glave trak seznam = 
  let Tape {left = levi; head = glava; right = desni} = trak in 
  let rec pomozna acc = 
    function 
    | [] -> obrni acc
    | gl::rep -> 
      match gl with 
      | Left | Right -> pomozna acc rep 
      | Do f -> 
        let nov_acc = (glava, f glava) :: acc in
        pomozna nov_acc rep 
  in
  pomozna [] seznam 


let naberi_in_pretvori trak seznam = 
  spremembe_glave trak seznam, izvedi_ukaze trak seznam 


(* 2. e) *)

(*želimo zaporedje ukazov, ki da isti rezultat, kot če bi izvedli map trak f*)
let pripravi_ukaze trak f =
  let Tape {left = levo; head = glava; right = desno} = trak in 
  (*glavo premaknemo na začetek*)
  let rec pomozna acc = 
    function 
    | [] -> acc
    | gl::rep -> pomozna (Left::acc) rep
  in 
  let premiki_levo = pomozna [] levo in (*to bo zacetni akumulator v naslednjem delu*)
  let pomozen_trak = izvedi_ukaze trak premiki_levo in 
  let Tape {left = levo2; head = glava2; right = desno2} = pomozen_trak in
  (*glavo premikamo do konca traku in izvajamo f*)
  let rec pomozna2 acc = 
    function
    | [] -> acc
    | gl::rep -> pomozna2 (Right::(Do f)::acc) rep 
  in 
  let levo_izvedeno = pomozna2 premiki_levo desno2 in 
  (*glavo premaknemo na začetni indeks - ukaz Right po celi dolzini zacetne desne strani*)
  let rec pomozna3 acc = 
    function 
    | [] -> acc
    | gl :: rep -> pomozna3 (Left::acc) rep
  in 
  let koncano = pomozna3 levo_izvedeno desno in
  (*obrnemo vrstni red v seznamu*)
  obrni koncano










