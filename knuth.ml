(** Algorithme de Knuth avec le minmax *)
open Code;;

module Knuth : 
	sig 
			
			(** Filtre les codes possibles
		*@param (code, rep) le code essaye et la reponse correspondante
		*@param possibles la liste de courante de codes possibles
		*@return la nouvelle liste de codes possibles
		*)
	val filtre : (Code.t * (int*int) option) -> Code.t list -> Code.t list
	
		(** choisis le prochain code a	 essayer 
		*@param (code, rep) le code essaye et la reponse correspondante
		*@param possibles la liste de courante de codes possibles
		*@return la nouvelle liste du code a essayer 
		*)
	val knuths : (Code.t * (int*int) option) -> Code.t list -> Code.t 
	
	val jouer : Code.t  -> (Code.t * (int*int) option) -> Code.t list -> int -> (Code.t * (int*int) option) list
	
	end = struct 
	
			
	(** On prend dans la liste des possibles tout les éléments ayant 
		la même reponse que l'essai par rapport au code secret que la reponse de l'essai
		avec les autres codes possibles *)
	let filtre essais possibles = 
		match essais with
		|(l , Some(x,y)) -> List.fold_left ( fun acc t -> if (Code.reponse t l) = Some(x,y) then t :: acc else acc ) [] possibles
		|_ -> [];;
		
	(** Renvoie le nombre d'élement restant dans la liste des possibles 
	  * en ayant pris code a jouer*)
	let poid code rep possible =
		let l =  filtre (code, Some(rep)) possible in
			List.length l;;
					
	(** Calcule le pire cas de chaque code dans la liste des possibles *)
	let pire_cas code possible reponse =
		List.fold_left (fun acc t -> let x = (poid code t possible) in if x >= acc then x else acc ) 0 reponse;;		

	(** Le principe est de prendre le codes éliminant le plus de codes dans la liste possible dans le pire des cas.
	  * Son poids est établie par le nombre d'éléments qu'il supprime dans la liste des possibles. Et on prend le plus petits des pires poids*)	
	let trouver_code essai possible = 
		let l = (filtre essai possible) in 
			List.fold_left(fun acc t -> let (u,v) = acc in let x = (pire_cas t l Code.toutes_reponses) in if x < u then (x,t) else acc )(21474836471,[""]) possible;;
	
	(** Désormais on choisit le code a essayer *)
	let knuths essai possible =
		let (u,v) = trouver_code essai possible in 
			v;;

	let rec jouer code_secret essais possible acc =
        match essais with 
        |(l , Some(x,y)) when acc <= 10 -> if l = code_secret then [essais]
                                            else
                                                let r = knuths essais possible in 
                                                    essais :: (jouer code_secret (r, Code.reponse code_secret r) (filtre essais possible) (acc+1))
        |_ -> [];;


	
end;;
