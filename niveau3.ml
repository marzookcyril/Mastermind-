(** Algorithme d'ia naif *)
open Code;;

module Niveau3 : 
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
	
	(** On enleve de la liste de possible tout les codes deja essayés (dans essais) 
		puis on en choisis un aléatoirement 
	  *@param		essai
	  *@param		liste des possibles
	  *@return		code a tester*)
	let choix essais possibles = 
		let l = List.filter ( fun t -> if List.mem t essais then false else true) possibles in 
			List.nth l (Random.int (List.length l));;

			
	
		
	(** On prend dans la liste des possibles tout les éléments ayant 
		la même reponse que l'essai par rapport au code secret que la reponse de l'essai
		avec les autres codes possibles
	  *@param		essai
	  *@param		liste des possibles
	  *@return		nouvelle liste des possibles*)	
	let filtre essais possibles = 
		match essais with
		|(l , Some(x,y)) -> List.fold_left ( fun acc t -> if (Code.reponse t l) = Some(x,y) then t :: acc else acc ) [] possibles 
		|_ -> [];;
	
end;;
