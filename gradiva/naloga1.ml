(* 1. a) *)
let zamenjaj prvi drugi = 
  let (a, b) = prvi and 
  (c, d) = drugi in 
  (a, c), (b, d)

(* 1. b) *)
let modus ((a, b, c) : int * int * int) = 
  (*Dovolj je že, da sta poljubni dve komponenti enaki*)
  if a = b || a = c then 
    Some a 
  else if b = c then 
    Some b
  else 
    None 

(* 1. c) *)
let uncons sez =
  (*Preverimo, da ima seznam vsaj en element in ima torej glavo in rep*)
  match sez with 
  | [] -> None
  | gl::rep -> Some (gl, rep)

(* 1. d) *)
let rec vstavljaj el sez = 
  match sez with 
  (*ulovimo izjemo pri praznem seznamu*)
  | [] -> []
  (*želimo, da se element ne pojavi na koncu*)
  | gl::[] -> sez
  (*v vseh ostalih korakih element dodamo seznamu*)
  | gl::gl2::rep -> gl::el:: vstavljaj el (gl2::rep)

(* 1. e) *)
(*funkcije za predstavo problema*)

let obrni sez =
  let rec pomozna acc =
    function 
    | [] -> acc
    | gl::rep -> pomozna (gl::acc) rep
  in pomozna [] sez 

let popolnoma_obrni double_sez =
  let rec pomozna acc = 
    function
    | [] -> acc
    | gl::rep -> 
      let obrnjena_glava = obrni gl in
      pomozna (obrnjena_glava :: acc) rep  
  in pomozna [] double_sez

