open Graph
open Tools
open Gfile

exception On_a_FINI



let parcours_profondeur gr id_dep id_fin =

  (*selectionner le point de départ*)
  
  let arcs_a_regarder = out_arcs gr id_dep in
  
  (*boucle sur tous les arcs disponibles en sortie du noeud*)
  let rec inner arcs accu = match arcs with

    (*plus d'arcs à regarder -> chemin vide, on a flop*)
  | [] -> []

  | x::xs -> if x.lbl == 0 then inner xs accu else ( (*explore cet arc s'il n'est pas déjà saturé*) 

    (*arc direct vers le puis*)
    if x.tgt == id_fin then [x] 
    else 
      (*sinon, on prend ce point comme étape et regarde s'il peut mener à un chemin*)
      if not (List.mem x.tgt accu ) then
        let res = inner (out_arcs gr x.tgt) (x.src::accu) in

        (*si le chemin avec cet arc comme étape ne donne rien, on explore les autres arcs sortant de notre noeud*)
        if res == [] then inner xs accu else x::res
      
      (*on a bouclé sur un noeud *)
      else []
    )

    in let chemin = inner arcs_a_regarder [] in

    if chemin == [] then 
      raise On_a_FINI

    else
      chemin
    







exception Chemin_Vide
let max_flow_chemin c = match c with

| [] -> raise Chemin_Vide
| h::r -> List.fold_left (fun min flow -> if flow.lbl < min then flow.lbl else min) h.lbl r

  

let rec change_flow gr c m = 
  match c with 
  | [] -> gr
  | x::xs ->( 
    let gr1 = add_arc gr x.src x.tgt (-m) in 
    let gr2 = add_arc gr1 x.tgt x.src m in
    change_flow gr2 xs m
  )

let rec ford_fulkerson gr pt_dep pt_arr outfile = 
  try
    let c = parcours_profondeur gr pt_dep pt_arr in
    let m = max_flow_chemin c in
    let gr2 = change_flow gr c m in
    ford_fulkerson gr2 pt_dep pt_arr outfile
  with
  | On_a_FINI -> let () = export outfile (gmap gr string_of_int) in Printf.printf "==========\ncé fini\n==========\n"