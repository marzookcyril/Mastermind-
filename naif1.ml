(** Algorithme d'ia aléatoire *)
open Code;;

module Naif1 : 
	sig 
			
		(** Choisit un code a proposer 
		*@param essais la liste des codes deja proposes
		*@param possibles la liste des codes possibles
		*@return le prochain code a essayer
		*)
	val choix  : Code.t list -> Code.t list -> Code.t

	end = struct 
	
	(** on enleve de la liste de possible tout les codes deja essayés (dans essais) 
		puis on en choisis un aléatoirement *) 
	let choix essais possibles = 
		let l = List.filter ( fun t -> if List.mem t essais then false else true) possibles in 
			List.nth l (Random.int (List.length l));;
	
end;;
