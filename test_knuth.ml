






type pion = string

		type t = pion list

		let nombre_pions = 4;;
		
		let couleurs_possibles = ["blanc";"bleu";"jaune";"noir";"rouge";"vert"];;
		
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
		

		
		let rec combinaisonList n a =
		  match n with 
			  |x when n = 0 -> [[]]
			  |_            -> let c = combinaisonList (n-1) a in
								List.concat (List.map (fun x -> List.map(fun y  -> y@x) a) c);;

		
		let tous = combinaisonList nombre_pions (List.map(fun t -> [t]) couleurs_possibles);;
		
		
		
		let rec combinaisonTuple n m =
		  match (n,m) with
			  |(x,y) when x + y <= 4 ->  [(x,y)] @ (combinaisonTuple n (m+1))
			  |(x,y) when x + y > 4  && x <= 4 -> combinaisonTuple (n+1) 0
			  |_ -> [];; 
			  
		let toutes_reponses = List.filter (fun t -> (t <> (3,1))) (combinaisonTuple 0 0);;
	
	
	
		let rec supprime_un a l1 =
			match l1 with 
				|( b :: c ) when b = a -> c
				|( b :: c )   -> b :: supprime_un a c
				|_   -> l1;;

	let rec couleur code vrai_code acc = 
			match code with 
			| (u :: v) -> if List.mem u vrai_code then couleur v (supprime_un u vrai_code) (acc+1) 
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
		|(l , Some(x,y)) -> List.fold_left ( fun acc t -> if (reponse t l) = Some(x,y) then t :: acc else acc ) [] possibles 
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
		List.fold_left(fun acc t -> let (u,v) = acc in let x = (pire_cas t (filtre essai possible) toutes_reponses) in if x < u then (x,t) else acc )(21474836471,[""]) (filtre essai possible);;
	

	
	let knuth essai possible =
		let (u,v) = trouver_code essai possible in 
			v;;
	
	

	

	let rec jouer code_secret essais possible acc =
		match essais with 
		|(l , Some(x,y)) -> if l = code_secret then  acc 
							else
								let r = knuth essais possible in 
								jouer code_secret (r, reponse code_secret r) (filtre essais possible) acc+1
		|_ -> failwith "niquetamere";;

	
	

	
	
	
	let essai1 = ["blanc";"blanc";"bleu";"bleu"];;

	jouer ["vert";"noir";"rouge";"jaune"] (essai1 , reponse ["vert";"noir";"rouge";"jaune"] essai1) (filtre (essai1 ,(reponse ["vert";"noir";"rouge";"jaune"] essai1)) tous)  1;;


	let rec moyenne x y =
		if y = 0 then x/.50. else
			moyenne (x +. float_of_int(jouer ["noir"; "rouge";"bleu";"noir"] (essai1 , reponse ["noir"; "rouge";"bleu";"noir"] essai1) (filtre (essai1 ,reponse ["noir"; "rouge";"bleu";"noir"] essai1) tous)  1)) (y-1);;


	moyenne 0. 50;;
	
	let rec temps code i = 
	match i with 
	|x when x <> 1296 -> 1 + temps (pire_cas ["noir"; "noir";"noir";"noir"] tous toutes_reponses) (i+1)
	|_ -> 0;;
	
	
	
	
	
	
	
	
	
	
	
