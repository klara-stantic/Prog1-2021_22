let stevilo_nzz n k l =
  (*imamo n pikslov, odločimo se, če so polni ali prazni*)
  (*prvi piksel ima 2 možnosti, ki glede na k in l vplivata na naslednje*)
  (*piksel na mestu m bo lahko prazen ali pa del pobarvanih*)
  (*k teče od 1 do k, l teče od 0 do l*, začetna vrednost je 1*)
  let rec pomozna acc d= 
    (*teče po vrednosti k*)
    match d with  
    | 0 -> acc
    | _->  
      let rec pomozna2 acc c = 
        (*teče po vrednosti l, v primeru 0 imamo eno moznost - vsi piksli pobarvani*)
        match c with 
        | 0 -> acc
        | _ -> 
          (*sedaj smo omejeni na fiksen k in l, moznosti so kombinatorične moznosti za ustrezno postavitev*)
          let moznosti = x in 
          let acc2 = acc + moznosti in 
          pomozna2 acc2 (c-1)
      in
      let nov_acc = pomozna2 1 l in
      pomozna nov_acc (d-1)
  in
  pomozna 1 k 