open Graph
open Printf
    
type path = string

(* Format of text files:
   % This is a comment

   % A node with its coordinates (which are not used), and its id.
   n 88.8 209.7 0
   n 408.9 183.0 1

   % Edges: e source dest label id  (the edge id is not used).
   e 3 1 11 0 
   e 0 2 8 1

*)

(* Compute arbitrary position for a node. Center is 300,300 *)
let iof = int_of_float
let foi = float_of_int

let index_i id = iof (sqrt (foi id *. 1.1))

let compute_x id = 20 + 180 * index_i id

let compute_y id =
  let i0 = index_i id in
  let delta = id - (i0 * i0 * 10 / 11) in
  let sgn = if delta mod 2 = 0 then -1 else 1 in

  300 + sgn * (delta / 2) * 100
  

let write_file path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "%% This is a graph.\n\n" ;

  (* Write all nodes (with fake coordinates) *)
  n_iter_sorted graph (fun id -> fprintf ff "n %d %d %d\n" (compute_x id) (compute_y id) id) ;
  fprintf ff "\n" ;

  (* Write all arcs *)
  let _ = e_fold graph (fun count arc -> fprintf ff "e %d %d %d %s\n" arc.src arc.tgt count arc.lbl ; count + 1) 0 in
  
  fprintf ff "\n%% End of graph\n" ;
  
  close_out ff ;
  ()

(* Reads a line with a node. *)
let read_node graph line =
  try Scanf.sscanf line "n %f %f %d" (fun _ _ id -> new_node graph id)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Ensure that the given node exists in the graph. If not, create it. 
 * (Necessary because the website we use to create online graphs does not generate correct files when some nodes have been deleted.) *)
let ensure graph id = if node_exists graph id then graph else new_node graph id

(* Reads a line with an arc. *)
let read_arc graph line =
  try Scanf.sscanf line "e %d %d %_d %s@%%"
        (fun src tgt lbl -> let lbl = String.trim lbl in new_arc (ensure (ensure graph src) tgt) { src ; tgt ; lbl } )
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a comment or fail. *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          | 'n' -> read_node graph line
          | 'e' -> read_arc graph line

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment graph line
      in      
      loop graph2

    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop empty_graph in
  
  close_in infile ;
  final_graph
  


(*digraph finite_state_machine {
	fontname="Helvetica,Arial,sans-serif"
	node [fontname="Helvetica,Arial,sans-serif"]
	edge [fontname="Helvetica,Arial,sans-serif"]
	rankdir=LR;
	node [shape = doublecircle]; 0 3 4 8;
	node [shape = circle];
	0 -> 2 [label = "SS(B)"];
	0 -> 1 [label = "SS(S)"];
	1 -> 3 [label = "S($end)"];
	2 -> 6 [label = "SS(b)"];
	2 -> 5 [label = "SS(a)"];
	2 -> 4 [label = "S(A)"];
	5 -> 7 [label = "S(b)"];
	5 -> 5 [label = "S(a)"];
	6 -> 6 [label = "S(b)"];
	6 -> 5 [label = "S(a)"];
	7 -> 8 [label = "S(b)"];
	7 -> 5 [label = "S(a)"];
	8 -> 6 [label = "S(b)"];
	8 -> 5 [label = "S(a)"];
}*)
let export path gr = 
  
  let ff = open_out path in 
  fprintf ff "digraph finite_state_machine {
	fontname=\"Helvetica,Arial,sans-serif\"
	node [fontname=\"Helvetica,Arial,sans-serif\"]
	edge [fontname=\"Helvetica,Arial,sans-serif\"]
	rankdir=LR;
	node [shape = circle];";
  
  let _ = e_fold gr (fun count arc -> fprintf ff "%d -> %d [label = \"%s\"];\n" arc.src arc.tgt arc.lbl; count+1 ) 0 in

  fprintf ff "}\n";
  close_out ff;
  ()


(* let export_chemin l path = 
  
  let ff = open_out path in 
  fprintf ff "digraph finite_state_machine {
	fontname=\"Helvetica,Arial,sans-serif\"
	node [fontname=\"Helvetica,Arial,sans-serif\"]
	edge [fontname=\"Helvetica,Arial,sans-serif\"]number_of_instances_left
	rankdir=LR;
	node [shape = circle];";
  
  let _ = List.fold_left (fun count arc -> fprintf ff "%d -> %d [label = \"%s\"];\n" arc.src arc.tgt arc.lbl; count+1 ) 0 l in

  fprintf ff "}\n";
  close_out ff;
  () *)



let export_chemin_2 l  = 
  
  let () = List.iter (fun arc -> printf "%d -> %d label = \"%s\"\n" arc.src arc.tgt (string_of_int arc.lbl) ) l in

  ()



let read_team_node graph line team hashTabIdToNode hashTabNodeToId=
  (* Ici on créer les noeud des équipe qui ne sont pas de l'équipe choisis*)
  try Scanf.sscanf line "t %s %d %d %d" (fun parsed_team _wins _losses _games_left -> (

    if parsed_team <> team then 
      begin
        let key = Hashtbl.length hashTabIdToNode in
        Hashtbl.add hashTabIdToNode key parsed_team ;        
        Hashtbl.add hashTabNodeToId parsed_team key;
        (new_node graph key,None) 
      end
    else
      (graph, Some(_wins + _games_left)) (*c'est l'équipe choisie*)
    
  
  )
  )
  with e ->
    Printf.printf "Cannot read team in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

let read_team_arc graph line team hashTabNodeToId max_win_possible=
    (*Ici on doit créer les arcs des équipe jusqu'au puit, ils ont pour valeur le nombre maximum de victoire 
    possible pour l'équipe choisi*)
  try Scanf.sscanf line "t %s %d %d %d" (fun parsed_team _wins _losses _games_left -> (

    if parsed_team <> team then 
      begin
        let id_team = Hashtbl.find hashTabNodeToId parsed_team in
        let id_puit = Hashtbl.find hashTabNodeToId "P" in
        let capacite = max_win_possible - _wins in
        new_arc graph {src = id_team; tgt = id_puit; lbl = capacite}
      end
    
    else
      graph
  
  )
  )
  with e ->
    Printf.printf "Cannot read team in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"




let read_game_node graph line team hashTabIdToNode hashTabNodeToId=
  (* Ici on lit la ligne game pour créer un noeud de match entre deux équipe.
  On ne créer que des noeud avec des match sans l'équipe utilisé*)
try Scanf.sscanf line "g %s %s %d" (fun t1 t2 _instances -> (

    if t1 <> team && t2 <> team then
      begin
      let key = Hashtbl.length hashTabIdToNode in
      Hashtbl.add hashTabIdToNode key (t1^"-"^t2) ;        
      Hashtbl.add hashTabNodeToId (t1^"-"^t2) key;
      new_node graph key
      end
    else
      graph
  
  )
  
  )
  
  with e ->
    Printf.printf "Cannot game node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"


let read_game_arc graph line team hashTabNodeToId=
    (*Cette fonction créer les arc entre le start et le match. Puis entre
    le noeud match et les équipe correspondante.*)
  try Scanf.sscanf line "g %s %s %d" (fun t1 t2 _instances -> (

    if t1 <> team && t2 <> team then
      begin
        let id_game = Hashtbl.find hashTabNodeToId (t1^"-"^t2) in
        let id_start = Hashtbl.find hashTabNodeToId "S" in
        let id_t1 = Hashtbl.find hashTabNodeToId t1 in
        let id_t2 = Hashtbl.find hashTabNodeToId t2 in
        let capacite = _instances in
        let graph1 = new_arc graph {src = id_start; tgt = id_game; lbl = capacite} in
        let graph2 = new_arc graph1 {src = id_game; tgt = id_t1; lbl = capacite} in
        new_arc graph2 {src = id_game; tgt = id_t2; lbl = capacite}
      end
    else
      graph
  
  )
  
  )
  
  with e ->
    Printf.printf "Cannot game node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"



let _graph_from_file path team =
  (* /!\ On doit utilisé un fichier avec aucune redondence d'information, on ne doit pas avoir
  deux ligne avec des information équivalente. C'est a dire KKR-MI 6 match restant et 
  MI-KKR 6 match restant
  
  Cette fonction transforme un txt representant un tableau de score de cricket en graph permettant
  de performé un algo. Il créer le graphe permettan de claculer la possibilité de victoire de 
  la Team choisi.*)
  let hashTabIdToNode = Hashtbl.create 100 in
  let hashTabNodeToId = Hashtbl.create 100 in

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop_node graph current_vic =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (graph2,found_vic) =
        (* Ignore empty lines *)
        if line = "" then (graph,None)

        (* The first character of a line determines its content : t or g. *)
        else match line.[0] with
          
          (*team_name wins losses games_left *)
          | 't' -> read_team_node graph line team hashTabIdToNode hashTabNodeToId
          (*game team1_name team2_name number_of_instances_left*)
          | 'g' -> (read_game_node graph line team hashTabIdToNode hashTabNodeToId,None)

          (* It should be a comment, otherwise we complain. *)
          | _ -> (graph,None)
      in    
      (* mise a jour du nombre de victoire possible *)
      let new_nb_vic = match found_vic with Some v-> v | None -> current_vic in 
      loop_node graph2 new_nb_vic

    with End_of_file -> (graph,current_vic) (* Done *)
  in

  let rec loop_arc graph max_win_possible = 
    (* Ici on a deja les noeud, on doit maintenant rajouter les arcs*)
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content : t or g. *)
        else match line.[0] with
          
          (*team_name wins losses games_left *)
          | 't' -> read_team_arc graph line team hashTabNodeToId max_win_possible
          (*game team1_name team2_name number_of_instances_left*)
          | 'g' -> read_game_arc graph line team hashTabNodeToId

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment graph line
      in      
      loop_arc graph2 max_win_possible

    with End_of_file -> graph (* Done *)
  in
  (*On ajoute les deux premiers point*)
  let graph_ini = empty_graph in
  Hashtbl.add hashTabIdToNode 0 ("S") ;        
  Hashtbl.add hashTabNodeToId ("S") 0;
  let graph_ini1 = new_node graph_ini 0 in

  Hashtbl.add hashTabIdToNode 1 ("P") ;        
  Hashtbl.add hashTabNodeToId ("P") 1;
  let graph_ini2 = new_node graph_ini1 1 in

  let (graph_after_node,nb_vic_possible) = loop_node graph_ini2 (-1) in

  (* REBOBINER LE FICHIER *)
  seek_in infile 0;

  let final_graph = loop_arc graph_after_node nb_vic_possible in
  
  close_in infile ;
  (final_graph, hashTabIdToNode, hashTabNodeToId)
  

  let read_team_name line =
    try Scanf.sscanf line "t %s %d %d %d" (fun parsed_team _ _ _ -> (
      parsed_team  
  )
  )
  with e ->
    Printf.printf "Cannot read team in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"




let _teams_from_file path  =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop teams =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      (* Ignore empty lines *)
      if line = "" then loop teams

      (* The first character of a line determines its content : t or g. *)
      else match line.[0] with
        
        (*team_name wins losses games_left *)
        | 't' -> loop ((read_team_name line)::teams)
        (*game team1_name team2_name number_of_instances_left*)
        (* It should be a comment, otherwise we complain. *)
        | _ -> loop teams
  
    with End_of_file -> teams (* Done *)

  in loop []