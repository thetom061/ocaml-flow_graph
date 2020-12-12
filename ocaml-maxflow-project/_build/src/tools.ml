(* Yes, we have to repeat open Graph. *)
open Graph


(* A path is a list of nodes. *)
type path = id list



(* find_path gr forbidden id1 id2 
 *   returns [] if no path can be found.
 *   returns p if a path p from id1 to id2 has been found. 
 *
 *  forbidden is a list of forbidden nodes (they have already been visited)
   On implémente un parcours en profondeur avec pile pour la simplicité de codage
*)

let empty_path=[]

(*utiliser dans ford fulkerson *)
let find_path gr id1 id2=  
  let rec loop fd=function 
    |[] -> []
    |(x::rest) as acu -> begin match List.find_opt (fun (id,a) -> a>0 && not(List.mem id fd)) (out_arcs gr x) with
        |Some (id,a)-> if id=id2 then id::acu else loop (x::fd) (id::acu)
        |None -> loop (x::fd) rest 
      end 
  in
  List.rev (loop [] [id1])

(*print les nodes du path, utilisé pour vérifier si les paths en sortie sont bien correctes *)
let test gr id1 id2= List.iter (fun a -> Printf.printf "Node: %d \n%!" a) (find_path gr id1 id2)

(*transforme une liste de node (path) en une liste de label*)
let pathtolabels gr p=
  let rec loop p acu=match p with
    |[] -> List.rev acu
    |id1::[] -> List.rev acu
    |id1::id2::rest -> match find_arc gr id1 id2 with 
      |Some a -> loop (id2::rest) (a::acu)   
      |None -> loop (id2::rest) (acu) (* cas pas possible en pratique car path censé etre valide *)

  in 
  loop p []

(* retourn le label min des arcs correspondants au path donnée *)
let minflow gr p=
  let rec loop l acu=begin match l with 
    |x::rest -> if x>acu then loop rest acu else loop rest x
    |[] -> acu
  end
  and labels=pathtolabels gr p
  in
  loop labels (List.hd labels)


(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr:'a graph) = n_fold gr (fun a id -> new_node a id) empty_graph

(*pour convertir en un type de graph different*)
let gmap gr f = let noarcs=clone_nodes gr in
  e_fold gr (fun b id1 id2 a -> new_arc b id1 id2 (f a)) noarcs


let add_arc gr id1 id2 n=match find_arc gr id1 id2 with 
  |None ->new_arc gr id1 id2 n
  |Some lbl -> new_arc gr id1 id2 (n+lbl) 

(*copier graphe avec tout les flow nul*)
let null gr=gmap gr (fun a-> 0)

(*pour appliquer un certain flow à un path *)
let changepath gr p n=
  let rec loop gr p n= match p with
    |[] -> gr
    |id1::[] -> gr
    |id1::id2::rest -> loop (add_arc gr id1 id2 n) (id2::rest) n
  in
  loop gr p n

let reversepath p= List.rev p

(*pour interpréter les résultats du mode 2 *)
let interpret gr= e_iter gr (fun id1 id2 a -> match (id1,id2) with
    |(-1,_) -> ()
    |(_,0) -> ()
    |(id1,id2) -> if a=1 then Printf.printf "%d est associé à %d\n%!" id1 id2 else ())