(** Algorithme d'ia naif ne prenant pas en compte les couleurs *)
open Code;;
Random.self_init();;

module Niveau2 : 
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
	
	
		
	let without  a = 
		match a with 
		|Some(x,y) -> (x,y)
		|_-> failwith "reponse pas corecte";;
		
	
	(** Apres avoir effectué le filtre, on choisit le code qui a le plus de couleur
		en commun avec l'essais 
	  *@param		essai
	  *@param		liste des possibles
	  *@return		code a tester*)
	let choix essais possibles = 
		let code = List.nth essais 0 in 	
			let p = List.fold_left ( fun acc t -> let (u,v) = without (Code.reponse code t) in if v >= fst acc then (v,t) else acc) (0,[]) possibles in 
				let (x,y)= p in
					y;;
							
	(** Cette fonction permet d'enlever l'option sur le tuple et recuperer le 
		premier élément qui correspond aux pions bien placés sans prendre en 
		compte le resultat des couleurs *)
	let without_Some a = 
		match a with 
		|Some(x,y) -> x
		|_-> 0;;
	
	
	(** Ici il faut prendre en compte seulement les pions bien placés 
		et pas les couleurs
	  *@param		essai
	  *@param		liste des possibles
	  *@return		nouvelle liste des possibles*)
	let filtre essais possibles = 
		match essais with
		|(l , Some(x,y)) -> List.fold_left ( fun acc t -> if without_Some (Code.reponse t l) = x then t :: acc else acc ) [] possibles 
		|_ -> [];;
	
end;;

