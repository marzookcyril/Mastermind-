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
	
	
	
	let rec filtre essais possibles = 
		match essais with
		|(l , Some(x,y)) -> List.fold_left ( fun acc t -> if (reponse t l) = Some(x,y) then t :: acc else acc ) [] possibles 
		|_ -> [];;


	
	
	let print_code code = 
		List.map ( fun t ->  print_string ((t)^" ")) code ;;

	let print_list l = 
		List.map (fun t -> print_code t);;

	
	filtre (["rouge";"bleu";"rouge";"bleu"], Some(2,2)) tous;;
