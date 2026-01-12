open Gfile
open Tools
open Max_flow

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  (*and outfile = Sys.argv.(4)*)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  (*let graph = from_file infile in*)

  (*Test the func clone nodes*)
  (* let out_graph = add_arc (gmap graph int_of_string) 4 5 123456789 in
  let chemin = parcours_profondeur out_graph 0 5 in

  (* Rewrite the graph that has been read. *)
  let () = export_chemin_2 chemin in
  let () = Printf.printf "max_flow = %d\n" (max_flow_chemin chemin)in
  let out_graph = change_flow out_graph chemin (max_flow_chemin chemin) in

  let chemin = parcours_profondeur out_graph 0 5 in
  (* Rewrite the graph that has been read. *)
  let () = export_chemin_2 chemin in
  let () = Printf.printf "max_flow = %d\n" (max_flow_chemin chemin)in
  let out_graph = change_flow out_graph chemin (max_flow_chemin chemin) in
  let () = export outfile (gmap out_graph string_of_int) in *)

  (*let gr_og = (gmap graph int_of_string) in 
  let gr_ford = ford_fulkerson gr_og  _source _sink in (
    let () = export "max_flow_out.txt" (diff_graph gr_og gr_ford ) in Printf.printf "==========\ncé fini\n==========\n"


  )*)

  (*CRICKET PROJECT*)
  let teams = _teams_from_file infile in

  if teams == [] then
    Printf.printf "Aucune équipe n'a pu être extraite du fichier."
  else

  let rec  build_all_graphs teams =
    match teams with
      | [] -> []
      | team::ts -> (
        let (graph,_,_) =  _graph_from_file infile team in 
          graph :: (build_all_graphs ts)
        )

  in let graphs = build_all_graphs teams in
  List.iter (fun t-> Printf.printf "TEAM : %s\n" t ) teams ;

  let results_graph = List.map (fun gr -> ford_fulkerson gr 0 1) graphs in

  List.iteri (fun i g -> let () = export ("outfile-graph-equipe-"^(string_of_int i)^".txt")  (gmap g string_of_int) in (Printf.printf "[+] Built graph for team %d\n" i)) graphs;

  let oc = open_out "outfile.txt" in

  List.iteri (fun i gr -> if (is_source_saturated (List.nth graphs i) gr oc) then Printf.fprintf oc "L'équipe %s peut gagner \n" (List.nth teams i) else Printf.fprintf oc "L'équipe %s ne peut pas gagner\n" (List.nth teams i) ) results_graph;

  close_out oc

