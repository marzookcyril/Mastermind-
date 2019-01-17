(** Algorithme de Knuth avec le minmax *)
open Code;;

module Knuth : 
	sig 
			
	(**Filtre les codes possibles
		*@param (code, rep) le code essaye et la reponse correspondante
		*@param possibles la liste de courante de codes possibles
		*@return la nouvelle liste de codes possibles
		*)
	val filtre : (Code.t * (int*int) option) -> Code.t list -> Code.t list
	
	(**Choisis le prochain code a essayer 
		*@param (code, rep) le code essaye et la reponse correspondante
		*@param possibles la liste de courante de codes possibles
		*@return la nouvelle liste du code a essayer 
		*)
	val knuths : (Code.t * (int*int) option) -> Code.t list -> Code.t 
	
	val jouer : Code.t  -> (Code.t * (int*int) option) -> Code.t list -> int -> (Code.t * (int*int) option) list
	
	end = struct 
	
			
	(**On prend dans la liste des possibles tout les éléments ayant 
		la même reponse que l'essai par rapport au code secret que la reponse de l'essai
		avec les autres codes possibles
	  *@param		essai
	  *@param		liste des possibles
	  *@return		nouvelle liste des possibles
	  *)
	let filtre essais possibles = 
		match essais with
		|(l , Some(x,y)) -> List.fold_left ( fun acc t -> if (Code.reponse t l) = Some(x,y) then t :: acc else acc ) [] possibles
		|_ -> [];;
		
		
		
	(**Renvoie le nombre d'élement restant dans la liste des possibles 
	   en ayant pris code a jouer
	  *@param       code a tester eventuellement 
	  *@param		reponse de l'esai
	  *@param		liste des possibles
	  *@return		poids associé a l'essai
	  *)
	let poid code rep possible =
		let l =  filtre (code, Some(rep)) possible in
			List.length l;;
					
					
					
	(**Calcule le pire cas de chaque code dans la liste des possibles pour toutes les reponses possibles
	  *@param       code a tester 
	  *@param		liste des possibles
	  *@param		liste de toutes les reponses 
	  *@return		pire poids du code 
	  *)
	let pire_cas code possible reponse =
		List.fold_left (fun acc t -> let x = (poid code t possible) in if x >= acc then x else acc ) 0 reponse;;		



	(**Le principe est de prendre le codes éliminant le plus de codes dans la liste possible dans le pire des cas.
	   Son poids est établie par le nombre d'éléments qu'il supprime dans la liste des possibles. Et on prend le plus petits des pires poids
	  *@param		essai
	  *@param		liste des possibles
	  *@return		code a essayer avec la reponses par rapport a l'essai précedent
	  *)	
	let trouver_code essai possible = 
		let l = (filtre essai Code.tous) in 
			List.fold_left(fun acc t -> let (u,v) = acc in let x = (pire_cas t l Code.toutes_reponses) in if x <= u then (x,t) else acc )(max_int,[""]) (filtre essai possible);;
	
	
	
	(**Désormais on choisit le code a essayer 
	  *@param		essai précedent
	  *@param		liste des possibles
	  *@return		code a tester
	  *)
	let knuths essai possible =
		let (u,v) = trouver_code essai possible in 
			v;;


	(**Fonction qui fait tourner l'algo de knuth 
	  *@param		le code secret
	  *@param		l'essai
	  *@param		liste des possibles
	  *@param		accumulateur pour compter le nombre d'essai
	  *@return		liste des code avec la reponse
	  *)
	let rec jouer code_secret essais possible acc =
        match essais with 
        |(l , Some(x,y)) when acc <= 10 -> if l = code_secret then [essais]
                                            else
                                                let r = knuths essais possible in 
                                                    essais :: (jouer code_secret (r, Code.reponse code_secret r) (filtre essais possible) (acc+1))
        |_ -> [];;


	
end;;
