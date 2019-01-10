
module Code :
	sig
		(** Le type d'un pion *)
		type pion = string
		
		(** Le type d'un code *)
		type t = pion list
		
		(** Nombre de pions par code *)
		val nombre_pions : int
		
		(** Liste des couleurs possibles *)
		val couleurs_possibles : pion list
		
		(** Compare deux codes
		  * @param code1 premier code a comparer
		  * @param code2 second  code a comparer
		  *	@return 0 si les deux codes sont identiques,
				un entier positif si [code1] est strictement plus grand que [code2]
				un entier negatif si [code1] est strictement plus petit que [code2]
		*)
		val compare : t -> t -> int
		
		(** Conversion code vers chaine de caracteres (pour affichage)
		  * @param code code a convertir
		  * @return la representation en chaine de caracteres de [code]
		  *)
		val string_of_code : t -> string
		
		(** Conversion chaine de caracteres vers code (pour saisie)
		  * @param string chaine de caractere saisie
		  * @return le code correspondant a la saisie si la conversion est possible
				[None] si la conversion n'est pas possible
		  *)
		val code_of_string : string -> t option
		
		(** La liste de tous les codes permis *)
		val tous : t list
		

		(** La liste de toutes les reponses possibles *)
		val toutes_reponses : (int*int) list
		
		(** Calcule la reponse d'un code par rapport au code cache
		  * @param      code le code propose
		  * @param vrai_code le code cache
		  * @return un couple (nombre de pions bien places, nombre de pions mal places)
				[None] si la reponse ne peut etre calculee
		  *)
		val reponse : t -> t -> (int*int) option
		
		
	end =
	struct 
		type pion = string

		type t = pion list

		let nombre_pions = 4;;
		
		let couleurs_possibles = ["blanc";"bleu";"jaune";"noir";"rouge";"vert"];;
		
		(** A refaire avec les fonctions list.left*)
		let rec compare c1 c2 = 
			match (c1,c2) with 
			|((t1 :: q1),(t2 :: q2)) -> if t1 > t2 then 1 
										else
											if t1 < t2 then -1 
											else 
												compare q1 q2
			|_ -> 0;;
		
		let string_of_code c =
			match c with 
			| l when l <> [] -> List.fold_left(fun x y -> x^y^" ") "" c
			|_ -> "";;
		
		let code_of_string s = Some (Str.split_delim (Str.regexp " ") s);;
		
		let rec combinaisonList n a =
		  match n with 
			  |x when n = 0 -> [[]]
			  |_            -> let c = combinaisonList (n-1) a in
								List.concat (List.map (fun x -> List.map(fun y  -> y@x) a) c);;

		
		let tous = combinaisonList nombre_pions (List.map(fun t -> [t]) couleurs_possibles);;
		
		
		
		let rec combinaisonTuple n m =
		  match (n,m) with
			  |(x,y) when x + y <= nombre_pions ->  [(x,y)] @ (combinaisonTuple n (m+1))
			  |(x,y) when x + y > nombre_pions  && x <= nombre_pions -> combinaisonTuple (n+1) 0
			  |_ -> [];; 
			  
		let toutes_reponses = List.filter (fun t -> (t <> (3,1))) (combinaisonTuple 0 0);;
	
	
	
	let supprime a l1 = 
		List.filter (fun t -> if List.mem t [a] then false else true) l1;;

	let rec couleur code vrai_code acc = 
			match code with 
			| (u :: v) -> if List.mem u vrai_code then couleur v (supprime u vrai_code) (acc+1) 
						else
							couleur v vrai_code acc 
			| _ -> acc;; 
	

		let bien_place code vrai_code = 
			 let (l1,l2) = List.partition ( fun t -> let (u,v) = t in if u = v then true else false) (List.combine code vrai_code) in 
				let (l3,l4) = List.split l2 in
					(List.length l1, couleur l3 l4 0);;
 

		let reponse code vrai_code = if List.mem (bien_place code vrai_code) toutes_reponses then 
										Some(bien_place code vrai_code)
									else 
										None;;
		
	end;;

