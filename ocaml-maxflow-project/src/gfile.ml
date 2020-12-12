open Graph
open Printf

type path = string

(* Format of text files:
   % This is a comment

   % A node with its coordinates (which are not used).
   n 88.8 209.7
   n 408.9 183.0

   % The first node has id 0, the next is 1, and so on.

   % Edges: e source dest label
   e 3 1 11
   e 0 2 8

*)

let write_file path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "%% This is a graph.\n\n" ;

  (* Write all nodes (with fake coordinates) *)
  n_iter_sorted graph (fun id -> fprintf ff "n %.1f 1.0\n" (float_of_int id)) ;
  fprintf ff "\n" ;

  (* Write all arcs *)
  e_iter graph (fun id1 id2 lbl -> fprintf ff "e %d %d %s\n" id1 id2 lbl) ;

  fprintf ff "\n%% End of graph\n" ;

  close_out ff ;
  ()

(* Reads a line with a node. *)
let read_node id graph line =
  try Scanf.sscanf line "n %f %f" (fun _ _ -> new_node graph id)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Ensure that the given node exists in the graph. If not, create it. 
 * (Necessary because the website we use to create online graphs does not generate correct files when some nodes have been deleted.) *)
let ensure graph id = if node_exists graph id then graph else new_node graph id

(* Reads a line with an arc. *)
let read_arc graph line =
  try Scanf.sscanf line "e %d %d %s"
        (fun id1 id2 label -> new_arc (ensure (ensure graph id1) id2) id1 id2 label)
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

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (n2, graph2) =
        (* Ignore empty lines *)
        if line = "" then (n, graph)

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          | 'n' -> (n+1, read_node n graph line)
          | 'e' -> (n, read_arc graph line)

          (* It should be a comment, otherwise we complain. *)
          | _ -> (n, read_comment graph line)
      in      
      loop n2 graph2

    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop 0 empty_graph in

  close_in infile ;
  final_graph

(*Fonctions crées pour faciliter l'utilisation de graphe pour bipartite matching *)
(* on crée ces 2 fonctions pour gérer les erreurs générés par les fonction new_node et new_arc *)
let update_node graph n1=try new_node graph n1 with Graph_error e -> graph

let update_arc graph n1 n2=try new_arc graph n1 n2 1 with Graph_error e -> graph

(* pour lire ligne du niveau type de fichier en input*)
let read_line graph line=
  try Scanf.sscanf line "%d %d" (fun n1 n2 -> update_arc (update_arc (update_node (update_arc (update_node graph n1) (-1) n1) n2) n2 0) n1 n2)
  with e ->Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"


(*pour lire la input file du bipartite machine *)
let input_file path=
  let infile = open_in path in
  (* Read all lines until end of file.  *)
  let rec loop graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (graph2) =
        (* Ignore empty lines *)
        if line = "" then (graph)
        else read_line graph line 
      in      
      loop graph2
    with End_of_file -> graph (* Done *)
  in
  let final_graph = loop (update_node (update_node empty_graph 0) (-1)) (* rajout des nodes entrée et sortie*) in
  close_in infile ;
  final_graph

(* fonction export demandé dans le sujet*)
let export path gr= 
  let ff = open_out path in 
  fprintf ff "digraph finite_state_machine {\n\trankdir=LR;\n\tsize=\"8,5\"\n\tnode [shape=circle]; ";
  n_iter gr (fun id -> fprintf ff "%d " id);
  fprintf ff ";\n";
  e_iter gr (fun id1 id2 lbl -> fprintf ff "%d -> %d [label = \"%s\"];\n" id1 id2 lbl);
  fprintf ff "}";



