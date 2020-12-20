open Graph

type path = id list

val empty_path: path

val find_path: int graph -> id -> id -> path




val pdijkstra: (int*int) graph -> id -> id -> path 

val testp: (int*int) graph ->int->int-> unit

val test: int graph ->int->int-> unit

val pathtolabels: int graph -> path -> int list

val minflow: int graph -> path -> int

val clone_nodes: 'a graph -> 'b graph

val gmap: 'a graph -> ('a -> 'b) -> 'b graph

val add_arc: int graph -> id -> id -> int -> int graph

val null: int graph -> int graph

val changepath: int graph -> path -> int -> int graph 
(*
val changepathd: (int*int) graph -> path -> (int*int) -> (int*int) graph 
*)
val reversepath: path -> path

val interpret: int graph -> unit