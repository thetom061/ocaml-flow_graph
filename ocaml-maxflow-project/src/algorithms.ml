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

(*
let cost res flo srce snk=
  let rec algo gr=

  and cost=clone_nodes gr in
*)