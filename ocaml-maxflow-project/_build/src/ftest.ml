open Gfile
open Tools
open Algorithms
open Graph

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv > 6 || Array.length Sys.argv <4 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) outfile(2) mode(3) source-id(4) sink-id(5) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2)
  and mode= Sys.argv.(3) in


  (* On gère les modes d'utilisation *)
  let processing=match mode with
    |"1"-> Printf.printf "Vous avez choisi le mode résolution de flow maximum sur graph. Très bon choix!\n%!";
      let _source = int_of_string Sys.argv.(4)
      and _sink = int_of_string Sys.argv.(5) 
      and graph = from_file infile in
      ford (gmap graph int_of_string) _source _sink
    |"2"-> Printf.printf "Vous avez choisi le mode bipartite matching. Très bon choix!\n%!"; 
      let graph = input_file infile in
      let ngraph=ford graph (-1) 0 in
      interpret ngraph;
      ngraph 
    |"3" -> Printf.printf "Vous avez choisi le monde max flow min cost. Très bon choix \n%!";
      let graph=add_arc (add_arc (add_arc (new_node (new_node (new_node empty_graph 0) 1) 2) 0 1 1) 0 2 4) 1 2 1 in
      let ngraph=gmap graph (fun a-> (a,a)) 
      and
      _source = int_of_string Sys.argv.(4)
      and _sink = int_of_string Sys.argv.(5) in
      testp ngraph _source _sink;
      graph
    |_  -> Printf.printf "This mode does not exist yet!\n%!"; exit 0
  in
  let () = export outfile (gmap processing string_of_int)
  (*  export outfile graph*)
  (* Rewrite the graph that has been read. On utilise export pour convertir en dot ou writefile pour réecrire le graph*)
  in


  ()

