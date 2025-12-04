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
        let res = inner (out_arcs gr x.tgt) (x.tgt::accu) in

        (*si le chemin avec cet arc comme étape ne donne rien, on explore les autres arcs sortant de notre noeud*)
        if res == [] then inner xs accu else x::res
      
      (*on a bouclé sur un noeud *)
      else inner xs accu
    )

    in let chemin = inner arcs_a_regarder [id_dep] in

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


let diff_graph gr_og gr_ford = 
  let gr_out = clone_nodes gr_og in

  e_fold gr_og (fun gr a -> (
    match find_arc gr_ford a.tgt a.src with
    | None -> new_arc gr {src=a.src; tgt=a.tgt; lbl="0/"^(string_of_int a.lbl)}
    | Some n_plus_x ->( (*si un arc existe dans l'autre sens aussi dans le graph OG*)

      match find_arc gr_og a.tgt a.src with
      | None -> (
        (*sinon*)
        new_arc gr {src=a.src; tgt=a.tgt; lbl=(string_of_int n_plus_x.lbl)^"/"^(string_of_int a.lbl)} 
      )
      | Some arc_oppose -> (
        (*sens 2 *)
        if n_plus_x.lbl > arc_oppose.lbl then (
          new_arc gr {src=arc_oppose.src; tgt=arc_oppose.tgt; lbl="0/"^(string_of_int arc_oppose.lbl)} )
          else (
              (*sens 1*)
              new_arc gr {src=arc_oppose.src; tgt=arc_oppose.tgt; lbl=(string_of_int (arc_oppose.lbl-n_plus_x.lbl) )^"/"^(string_of_int arc_oppose.lbl)} )
            
            )
      )

      
      )
    )  
    
     gr_out



let ford_fulkerson gr pt_dep pt_arr = 

  let rec inner gr gr_og pt_dep pt_arr cont =

    try
      let c = parcours_profondeur gr pt_dep pt_arr in
      let m = max_flow_chemin c in
      let gr2 = change_flow gr c m in
      let () = export ("intermediaire_"^(string_of_int cont)^".txt") (gmap gr2 string_of_int) in
    inner gr2 gr_og pt_dep pt_arr (cont+1) 
    with
    | On_a_FINI -> gr


  in inner gr gr pt_dep pt_arr 0


(*

Pour tester si l’équipe z peut encore finir première, on construit un graphe de flux ainsi :

On imagine que z gagne tous ses matchs restants — elle aura donc un nombre maximal possible de victoires = w_z + r_z. 
Medium

On crée un noeud source s et un noeud puits t. 
Medium

Pour chaque match restant non impliquant z (c.-à-d entre deux autres équipes x et y), on crée un noeud “match”, relié à s avec une arête de capacité égale au nombre de fois qu’ils doivent encore s’affronter (r_{xy}). 
Medium

Depuis chaque “match-noeud”, on connecte deux arêtes vers les noeuds “équipe” (x et y), avec capacité infinie — ce qui modélise le fait que l’une ou l’autre des équipes gagnera le match. 
Medium

Enfin, on relie chaque “noeud équipe” x à t avec une arête de capacité (w_z + r_z − w_x), c’est-à-dire le nombre maximal de victoires restantes que x peut obtenir sans dépasser l’objectif de z.

*)


