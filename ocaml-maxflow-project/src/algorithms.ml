open Tools
open Graph

let ford gr srce snk=
  let rec algo gr fl=match find_path gr srce snk with
    |[] -> fl
    |p ->  let gr'=changepath gr p (-minflow gr p) in
      let res=changepath (gr') (reversepath p) (minflow gr p)
      and flo=changepath fl p (minflow gr p)
      in algo res flo 
  and flowgraph=null gr (*copie de gr mais avec tout les arcs mis à 0 pour pouvoir représenter les flow *)
  in
  algo gr flowgraph

(* pour min cost max flow *) 
(* on utilise successive shortest path *)
(*
let mincost (gr:(int*int) graph) srce snk=
  let rec algo gr fl=match pdijkstra gr srce snk with
    |[] -> fl
    |p ->  let gr'=changepathd gr p ((-minflowd gr p),0) in
      let res=changepathd (gr') (reversepath p) ((minflowd gr p),0)
      and flo=changepath fl p (minflowd gr p)
      in algo res flo 
  and flowgraph=null gr (*copie de gr mais avec tout les arcs mis à 0 pour pouvoir représenter les flow *)
  in
  algo gr flowgraph
  *)