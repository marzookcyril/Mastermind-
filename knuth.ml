(** Algorithme de Knuth avec la minmax *)
open Code;;

module Knuth : 
	sig 
			
		(** Choisit un code a proposer 
		*@param essais la liste des codes deja proposes
		*@param possibles la liste des codes possibles
		*@return le prochain code a essayer
		*)
	val choix  : Code.t list -> Code.t list -> Code.t
	
		(** Filtre les codes possibles
		*@param (code, rep) le code essaye et la reponse correspondante
		*@param possibles la liste de courante de codes possibles
		*@return la nouvelle liste de codes possibles
		*)
	val filtre : (Code.t * (int*int) option) -> Code.t list -> Code.t list

	end = struct 
	
	type 'a arbre_binaire =
      Vide
      |Noeud of 'a arbre_binaire * 'a * 'a arbre_binaire;;
	
	let de_some x = 
		match x with 
		|Some(a,b) -> (a,b)
		|_ -> (12,12);;
	
	let choix essais possibles = 
		let l = List.filter ( fun t -> if List.mem t essais then false else true) possibles in 
			List.nth l (Random.int (List.length l));;
	
	let filtre essais possibles = 
		match essais with
		|(l , Some(x,y)) -> List.fold_left ( fun acc t -> if (Code.reponse t l) = Some(x,y) then t :: acc else acc ) [] possibles 
		|_ -> [];;

	let poid code rep possible =
		let l =  filtre (code, Some(rep)) possible in
			List.length l;;
	
	(*let reponse_knuth*)	
				
	(** Calcule le pire cas de chaque code dans la liste des possibles *)
	let pire_cas code possible reponse =
		List.fold_left (fun acc t -> let x = (poid code t possible) in if x > acc then x else acc ) 0 reponse;;		

	(** Le principe est de prendre le codes éliminant le plus de codes dans la liste possible dans le pire des cas.
		Son poids est établie par le nombre d'éléments qu'il supprime dans la liste des possibles. Et on prend le plus petits des pires poids*)
		
	let trouver_code essai possible = 
		let l = (filtre essai possible) in 
			List.fold_left(fun acc t -> let (u,v) = acc in let x = (pire_cas t l Code.toutes_reponses) in if x < u then (x,t) else acc )(21474836471,[""]) l;;
	
	let knuth essai possible =
		let (u,v) = trouver_code essai possible in 
			v;;

	let rec jouer code_secret essais possible acc =
		match essais with 
		|(l , Some(x,y)) -> if l = code_secret then  acc 
							else
								let r = knuth essais possible in 
								jouer code_secret (r, Code.reponse code_secret r) (filtre essais possible) acc+1
		|_ -> failwith "niquetamere";;

	
	let essai1 = ["blanc";"blanc";"bleu";"bleu"];;

	jouer ["noir"; "rouge";"bleu";"noir"] (essai1 , Code.reponse ["noir"; "rouge";"bleu";"noir"] essai1) Code.tous 1;;


	
end;;
