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
	
	
	
	let rec print_list c = 
	match c with
	|[] -> print_newline()
	| e :: l -> print_string e ; print_string " " ; print_list l;;

	
	
	
	
	
	let choix essais possibles = 
		let l = List.filter ( fun t -> if List.mem t essais then false else true) possibles in 
			List.nth l (Random.int (List.length l));;

			
	
	let filtre essais possibles = 
		match essais with
		|(l , Some(x,y)) -> List.fold_left ( fun acc t -> if (reponse t l) = Some(x,y) then t :: acc else acc ) [] possibles 
		|_ -> [];;


	(*["noir"; "rouge";"bleu";"noir"]*)

	let l1 = filtre (["noir"; "rouge";"bleu";"vert"] , Some(3,0)) tous;;

	choix [["noir"; "rouge";"bleu";"vert"]]  l1;;

	
	let l2 = filtre (["noir"; "rouge"; "rouge"; "vert"] , Some(2,0)) l1;;
	
	choix [["noir"; "rouge"; "rouge"; "vert"]]  l2;;

	let l3 = filtre (["noir"; "rouge"; "bleu"; "blanc"] , Some(3,0)) l2;;
	
	choix [["noir"; "rouge"; "bleu"; "blanc"]]  l3;;
	
	let tab code = 
		[code];;
	
	let rec jouer code_secret essais possible acc =
		match essais with 
		|(l , Some(x,y)) -> if l = code_secret then  acc 
							else
								let r = choix (tab l) (filtre essais possible) in 
								jouer code_secret (r, reponse code_secret r) (filtre essais possible) acc+1
		|_ -> failwith "niquetamere";;

	let essai1 = choix [] tous;;

	jouer ["noir"; "rouge";"bleu";"noir"] (essai1 , reponse ["noir"; "rouge";"bleu";"noir"] essai1) tous 1;;


	let rec moyenne x y =
		if y = 0 then x/.500. else
			moyenne (x +. float_of_int((jouer ["noir"; "noir";"noir";"noir"] (essai1 , reponse ["noir"; "noir";"noir";"noir"] essai1) tous 1))) (y-1);;
	moyenne 0. 500;;
