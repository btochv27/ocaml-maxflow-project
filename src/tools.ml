(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = n_fold gr (fun ngr id -> new_node ngr id) empty_graph;;
(*test passed with success*)
let gmap gr f = e_fold gr (fun ngr arc -> new_arc ngr {arc with lbl = (f arc.lbl)}) (clone_nodes gr);;
(* Replace _gr and _f by gr and f when you start writing the real function. *)
(*test pass with secces*)

(*Add n to the value of the arc between id1 and id2. If it does not exist this arc is created *)
let add_arc g id1 id2 n = match (find_arc g id1 id2) with
    |Some arc -> new_arc g {src=arc.src; tgt=arc.tgt;lbl = (arc.lbl)+n}
    |None -> new_arc g {src=id1;tgt=id2;lbl=n}
(*test path with success*)
