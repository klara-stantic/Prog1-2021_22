(* a *)
(*----------------------------------------------------------------------------*]
  Napišite predikat `je_urejena : int * int * int -> bool`, ki pove, ali je 
  podana trojica celih števil urejena strogo naraščajoče.
[*----------------------------------------------------------------------------*)

let je_urejena ((x, y, z) : int * int * int) : bool =
  if x < y && y < z then 
    true
  else
    false

(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `poskusi_deljenje : float option -> float option -> float option`, 
  ki sprejme morebitni deljenec in morebitni delitelj ter vrne rezultat deljenja, 
  če se to da, ali pa `None`, če ne (kadar kakšnega argumenta ni ali pa bi prišlo 
  do deljenja z nič). 
  
    # poskusi_deljenje (Some 1.0) (Some 2.0);;
    - : float option = Some 0.5
    # poskusi_deljenje (Some 1.0) (Some 0.0);;
    - : float option = None
    # poskusi_deljenje None (Some 2.0);;
    - : float option = None
[*----------------------------------------------------------------------------*)

let poskusi_deljenje deljenec deljitelj =
  match deljenec with 
  | None -> None 
  | Some (a) -> 
    match deljitelj with 
    | None -> None
    | Some 0.0 -> None
    | Some (b) -> Some (a /. b)


(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo `zavrti : 'a list -> int -> 'a list`, ki seznam zavrti 
  za dano število mest v levo (v vsaki rotaciji se prvi element prestavi na 
  konec seznama).
  
    # zavrti [1; 2; 3; 4; 5] 2;;
    - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let zavrti seznam n =
  let rec pomozna sez k=
    match k with 
    | 0 -> sez
    | _ -> 
      match sez with 
      | [] -> []
      | gl::rep -> pomozna (rep @ [gl]) (k-1)
  in pomozna seznam n

(* d *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `razdeli : ('a -> int) -> 'a list -> ('a list *  'a list * 'a list)|`, 
  ki sprejme cenilno funkcijo in seznam elementov. Vrne naj trojico, kjer so na prvem 
  mestu vsi elementi za katere je cenilna funkcija negativna, na drugem vsi, kjer 
  je enaka 0, na tretjem pa vsi preostali elementi.
  Elementi naj v seznamih nastopajo v enakem vrstnem redu kot v prvotnem seznamu. 
  Za vse točke naj bo funkcija repno rekurzivna.
  
    # razdeli ((-) 3) [1; 2; 3; 4; 5; 6];;
    - : int list * int list * int list = ([4; 5; 6], [3], [1; 2])
[*----------------------------------------------------------------------------*)


let rec reverse sez =
  match sez with 
  | [] -> []
  | gl::rep -> (reverse rep) @ [gl]

let razdeli f sez =
  let rec pomozna a1 a2 a3 =
    function 
    | [] -> (reverse a1, reverse  a2, reverse a3)
    | gl::rep -> 
      if f gl = 0 then 
        pomozna a1 (gl :: a2) a3 rep
      else if f gl < 0 then 
        pomozna (gl :: a1) a2 a3 rep 
      else
        pomozna a1 a2 (gl :: a3) rep 
      in pomozna [] [] [] sez 






