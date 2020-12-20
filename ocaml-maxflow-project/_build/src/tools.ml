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

(* Fonctions demandés par le sujet*)
(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr:'a graph) = n_fold gr (fun a id -> new_node a id) empty_graph

(*pour convertir en un type de graph different*)
let gmap gr f = let noarcs=clone_nodes gr in
  e_fold gr (fun b id1 id2 a -> new_arc b id1 id2 (f a)) noarcs


let add_arc (gr:'a graph) id1 id2 (n:'a)=match find_arc gr id1 id2 with 
  |None ->new_arc gr id1 id2 n
  |Some lbl -> new_arc gr id1 id2 (n+lbl) 


(*toutes les fonctions pour implémenter ford fulkerson *)
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




(*Toutes les fonctions pour implémenter le djikstra *)
(* update cout courant, noeud pere, et si le noeud est marqué ou non*)
let update l node cost father marque= List.fold_left (fun a b -> match b with
    |(n,(_,_,_)) when n=node-> Printf.printf "on passe\n%!"; (n,(cost,father,marque))::a
    |n-> n::a) [] l

(*indique si le node est marqué*)
let ism l node=List.fold_left (fun a b ->match b with
    |(n,(_,_,true)) when n= node -> Printf.printf "%d marqued\n%!" n; true || a
    |(n,(_,_,false)) when n= node -> Printf.printf "%d uunmarqued\n%!" n; a
    |_ ->  a) false l

(*getteurs *)
(*cost*)
let getc (node,(cost,_,marque))=cost
(*cost à partir de list assoc *)
let getca (cost,_,marque)=cost

(*node *)
let getn (node,(cost,_,marque))=node
(*père à partir de list assoc *)
let getfa (cost,father,marque)=father

(*retourne le node avec le cost minimum *)
let min l= 
  let rec loop l acun acuc=match l with
    |[]->acun
    |(node,(cost,_,true))::rest -> Printf.printf "%d oui\n%!" node; loop rest acun acuc
    |(node,(cost,_,false))::rest -> if cost<=acuc then loop rest node cost else loop rest acun acuc
  and first=List.hd l
  in
  loop l (getn first) (max_int)

(*pour savoir si tout les nodes sont marqués *)
let allm l=List.fold_left (fun a b -> match b with
    |(n,(_,_,true)) -> true && a
    |(n,(_,_,false)) ->  a && false) true l

let tomin a b=if a>b then b else a

(*pour retirer le cost du label de l'arc *)
let extc (a,b)=b

(*pour max flow min cost on utilise les plus petits chemins en couts successifs*)
(* prends un (int*int) graph qui represente (capacité,cost) *)
let dijkstra gr id1 id2=
  (*pour enregistrer le meilleur cout courant, sommet précédent, et marquage on fait une assiociation list*)
  let assoc=(n_fold gr (fun b id -> ((id),(max_int-1,id,false))::b) [])
  in
  (*loop de la fonction *)
  let rec loop l=match allm l with 
    |true -> l (*on s'arrête dès que tout les sommets sont marqués *)
    |false -> let m=min l in (*sinon on extrait le minimum on le marque et on update les couts *)
      let marqued=update l m (getca (List.assoc m l)) ((getfa (List.assoc m l))) true (*pour marquer le min *)
      in loop (List.fold_left (fun a b-> match b with (*pour update les couts*)
          |(id,label) -> let now=(List.assoc id a) 
            and prev=(List.assoc m a) in 
            if (ism a id)=false then 
              (if (tomin (getca now) ((getca prev)+(extc label)))=(getca now) then a else (update a id ((getca prev)+(extc label)) m false))
            else a) marqued (out_arcs gr m))
  (*on initialise pour le noeud de départ*)
  and data=update assoc id1 0 id1 false in
  loop data

(* retourne le path à partir du dijkstra *)
let pdijkstra gr id1 id2= let data=dijkstra gr id1 id2 in
  let rec loop node acu=match getfa (List.assoc node data) with
    |n when n=id1 -> id1::acu (* le path est tottalement reconstitué lorsqu'on arrive au noeud d'origine*)
    |n when n=id2 ->  [] (*si on boucle c'est qu'il n'y a pas de solution *)
    |n -> loop n (n::acu) (*on continue tant qu'on atteint pas une condition de fin *)

  in
  loop id2 [id2]

(*pour vérifier si le dijkstra marche bien en testant sur un graph*)
let testp gr id1 id2= List.iter (fun a -> Printf.printf "Node: %d \n%!" a) (pdijkstra gr id1 id2)


(*transforme une liste de node (path) en une liste de flow meme fonction qu'avant mais adapté pour (int*int) graph*)
let pathtolabelsd gr p=
  let rec loop p acu=match p with
    |[] -> List.rev acu
    |id1::[] -> List.rev acu
    |id1::id2::rest -> match find_arc gr id1 id2 with 
      |Some (flow,cost) -> loop (id2::rest) (flow::acu)   
      |None -> loop (id2::rest) (acu) (* cas pas possible en pratique car path censé etre valide *)

  in 
  loop p []


(*en commentaire car empêche la compilation*)
(*

(* retourn le label min des arcs correspondants au path donnée *)
let minflowd gr p=
  let rec loop l acu=begin match l with 
    |x::rest -> if x>acu then loop rest acu else loop rest x
    |[] -> acu
  end
  and labels=pathtolabelsd gr p
  in
  loop labels (List.hd labels)

let add_arcd (gr:(int*int) graph) id1 id2 (n:(int*int))=match find_arc gr id1 id2 with 
  |None ->new_arc gr id1 id2 n
  |Some lbl -> new_arc gr id1 id2 (n+lbl) 


(*pour appliquer un certain flow à un path de type pour graph avec un cost*)
let changepathd (gr:(int*int)graph) p n=
  let rec loop (gr:(int*int)graph) p n= match p with
    |[] -> gr
    |id1::[] -> gr
    |id1::id2::rest -> loop (add_arcd gr id1 id2 n) (id2::rest) n
  in
  loop gr p n
*)