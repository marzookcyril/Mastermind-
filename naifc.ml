(** Algorithme d'ia naif ne prenant pas en compte les couleurs *)
open Code;;

module Naifc : 
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
	
	(** on enleve de la liste de possible tout les codes deja essayés (dans essais) 
		puis on en choisis un aléatoirement *) 
	let choix essais possibles = 
		let l = List.filter ( fun t -> if List.mem t essais then false else true) possibles in 
			List.nth l (Random.int (List.length l));;

		
	(** Cette fonction permet d'enlever l'option sur le tuple et recuperé le 
		premier élément qui correspond aux pions bien placés sans prendre en 
		compte le resultat des couleurs *)
	let without_Some a = 
		match a with 
		|Some(x,y) -> y
		|_-> 0;;
		
		
	(** Cette fonction permet d'enlever l'option sur le tuple et recuperé le 
		resultat *)
	let without_Some_Tuple a = 
		match a with 
		|Some(x,y) -> (x,y)
		|_-> (0,0);;
	
	
	(** Ici il faut prendre en compte seulement les pions bien placés 
		et pas les couleurs *)
	let filtre essais possibles = 
		match essais with
		|(l , Some(x,y)) -> List.fold_left ( fun acc t -> if without_Some (Code.reponse t l) = x then t :: acc else acc ) [] possibles 
		|_ -> [];;
	
end;;

