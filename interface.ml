open Graphics;;

(*module Interface :
	sig 
	
		val menu : unit -> unit  

	end = struct *)


let depart () = 
	open_graph "";
	set_window_title "Mastermind";
	resize_window 2000 1500;;

let k = 4 ;; (**Nombre pions*)
let c = 6 ;;(**Nombre couleurs*)
let t = 10 ;; (**Nombre tentatives*)
let l = [black;white;red;green;blue;yellow];; (**Couleurs possibles*)


let accueil () = 
	draw_rect 700 400 100 50;
	moveto 720 420;
	draw_string "Joueur vs IA" ;;


let convert n = 
	match n with 
	|a when a = black -> "noir" 
	|a when a = white -> "blanc"
	|a when a = red -> "rouge"
	|a when a = green -> "vert"
	|a when a = blue -> "bleu"
	|a when a = yellow  -> "jaune"
	|_ -> "";;
	

let invconvert n = 
	match n with 
	|a when a = "noir" -> black 
	|a when a = "blanc" -> white
	|a when a = "rouge" -> red 
	|a when a = "vert" -> green
	|a when a = "bleu" -> blue
	|a when a = "jaune" -> yellow
	|_ -> 0;;



(**Liste couleurs totale pour la partie en cours*
	*@param 	liste de couleurs possible
	*@param 	nombre de couleurs souhaité
	*@return 	liste avec le nombre de couleur souhaité
	*)
let rec liste_couleur l c  = 
	match (l,c) with
	|(h :: t, x) when x > 0 -> h :: liste_couleur t (x-1)
	|(h :: [], _) -> [h]
	|(_ ,_) -> [];;

(**Variable prenant la liste des couleurs pour la partie*)
let listecouleur = liste_couleur l c;;

(**Change la couleur d'écriture
	*@param 	liste de couleur pour la partie
	*@param 	couleur actuelle
	*@return 	nouvelle couleur à utilisé
	*)
let rec ch_couleur l couleur =
	match l with
	|(h :: []) -> if h = couleur then List.hd listecouleur else 0
	|(h :: t) -> if h = couleur then List.hd t else ch_couleur t couleur 
	|_ -> 0;;
	
(**Dessine la grille de jeu 
	*@param 	variable pour le dessin correct de la grille
	*@param 	variable pour le dessin correct de la grille
	*@param		nombre de colonne
	*)
let rec grille i j n = 
	match (i,j,n) with 
	|(x,y,z) when x < (t+1) && y < z+1  -> draw_rect  (400+(y*66)) (20+(x*66)) 66 66 ; grille x (y+1) z
	|(x,y,z) when x < (t+1) && y = z+1 -> grille (x+1) 1 z;
	|_                          -> draw_rect 10 10 1 1 ;;


(**Delimitation grille/grille de reponse*)
let grilleee () = 
	draw_rect (400 - 15*k) 86 (15*k + 60) (66*t);
	fill_rect 460 86 5 (66*t);
	draw_rect (400 - 15*k) 20 (15*k + 66*(k+1)) 66;;

(**Dessine la grille de reponse
	*@param variable pour le dessin correct de la grille de reponse
	*)
let rec grillereponse acc = 
	match (acc) with
	|(x) when x < (t+1) -> draw_rect (400 - (15*k)) (20+(x*66)) (32*k) 0; grillereponse (x+1)
	|_ -> draw_rect 10 10 1 1 ;;


(**Dessin d'un rond de la couleur voulue à l'endroit souhaité
	*@param 	couleur voulue
	*@param		première coordonnée 
	*@param 	seconde coordonnée
	*)
let rond couleur x y =
	draw_circle x y 20;
	set_color couleur;
	fill_circle x y 20;
	set_color black;;

(**Initialisation des parties cliquable
	*@rond 		cercles permettant de choisir les couleurs
	*@rect 		rectangle pour valider son choix
*)	
let rondreponse () = 
	for i = 0 to (k-1) do rond black (498 + i*66) 53 done;;


let boutonvalider () =
	draw_rect 345 33 80 40;
	moveto 350 53;
	draw_string "cliquez ici" ;
	moveto 350 43;
	draw_string "pour valider" ;;


(**Change de couleur le rond sur lequel on clique 
	*@param 	première coordonnée 
	*@param 	seconde coordonnée
	*@param 	couleur actuelle
	*)
let cliccouleur a b couleur =
	for i = 0 to k do 
	if a >= (478 + i*66) && a <= (518 + i*66) && b >= 33 && b <= 73 then 
		rond (ch_couleur listecouleur couleur) (498 +i*66) 53 
	else draw_rect 10 10 1 1 done;;

(**Valide le choix des couleurs et l'insere dans la grille 
	*@param 	première coordonnée 
	*@param 	seconde coordonnée
	*@param		variable permettant le placement correcte dans la grille
	*)
	
let valider a b add = 
	for i = 0 to (k-1) do 
	if a >= 345  && a <= 425 && b >= 33 && b <= 73 then	
		rond (point_color (498+i*66) 53) (498+i*66) (53 + (add * 66))
	else draw_rect 10 10 1 1 done;;

(**Affiche la reponse au code selectionné précedemment
	*@param 	première coordonnée 
	*@param 	seconde coordonnée
	*@param		variable permettant le placement correcte dans la grille de reponse
*)
let pionplace a b add = 
	if (add < (t+1) && add <> 0) then 
		(for i = 1 to a do fill_rect ((400 - 15*k) + 20*i) (45 + (add * 66)) 15 15 done;
		for i = 1 to b do draw_rect ((400 - 15*k) + (a+i)*20) (45 + (add * 66)) 15 15 done)
	else 
		draw_rect 10 10 1 1;;

let rec recucode temp = 
	if temp < k then
		point_color (498 + temp*66) 53 :: recucode (temp + 1)
	else
		[];;
		
let rec recucodesecret temp = 
	if temp < k then
		point_color (698 + temp*66) 363 :: recucodesecret (temp + 1)
	else
		[];;
		
let toutecreation () = 
	grilleee ();
	grille 1 1 k;
	grillereponse 1;
	rondreponse ();
	boutonvalider ();;


let chcouleurcode a b couleur = 
	for i = 0 to k do 
	if a >= (678 + i*66) && a <= (718 + i*66) && b >= 343 && b <= 383 then 
		rond (ch_couleur listecouleur couleur) (698 +i*66) 363 
	else draw_rect 10 10 1 1 done;;

let rec couleurpourlecode a tableau = 
	let bu = wait_next_event [Button_down] in
	if bu.mouse_x >= 545 && bu.mouse_x <= 625 && bu.mouse_y >= 343 && bu.mouse_y <= 383 then
		let tableau = (recucodesecret 0) in print_string (convert (List.hd tableau))
	else
		(chcouleurcode bu.mouse_x bu.mouse_y (point_color bu.mouse_x bu.mouse_y); couleurpourlecode a tableau);;

let choixducodesecret () =
	draw_rect (600 - 15*k) 334 (15*k + 66*(k+1)) 66;
	draw_rect 700 400 100 50;
	moveto 720 420;
	draw_string "Choix code" ;
	for i = 0 to (k-1) do rond black (698 + i*66) 363 done;
	draw_rect 545 344 80 40;
	moveto 550 364;
	draw_string "cliquez ici" ;
	moveto 550 354;
	draw_string "pour valider" ;
	couleurpourlecode 0 [];;

	
	
(**Boucle principale permettant de jouer tout le reste*)
let rec boucle level ad tableau = 
	if ad > t then (moveto 550 (75 * t) ; draw_string "C'est perdu.") else 
	let bu = wait_next_event [Button_down] in
	if bu.mouse_x >= 345 && bu.mouse_x <= 425 && bu.mouse_y >= 33 && bu.mouse_y <= 73 then
		(valider bu.mouse_x bu.mouse_y ad ; pionplace (Random.int 5) (Random.int 5) ad; let tableau = (recucode 0) in boucle level (ad+1) tableau) else
		(cliccouleur bu.mouse_x bu.mouse_y (point_color bu.mouse_x bu.mouse_y); boucle level ad tableau);;
		
		
let choixniveau () = 
	draw_rect 350 400 100 50;
	moveto 370 420;
	draw_string "Level 1" ;
	
	draw_rect 600 400 100 50;
	moveto 620 420;
	draw_string "Level 2" ;
	
	draw_rect 850 400 100 50;
	moveto 870 420;
	draw_string "Level 3" ;
	
	set_color red;
	fill_rect 1100 400 100 50;
	moveto 1120 420;
	set_color black;
	draw_string "Level 4" ;
	
	let bu = wait_next_event [Button_down] in
		if bu.mouse_x >= 350 && bu.mouse_x <= 450 && bu.mouse_y >= 400 && bu.mouse_y <= 450 then
			(clear_graph () ; toutecreation () ; boucle 1 0 [])
		else 
			if bu.mouse_x >= 600 && bu.mouse_x <= 700 && bu.mouse_y >= 400 && bu.mouse_y <= 450 then
				(clear_graph () ;toutecreation () ; boucle 2 0 [])
			else
				if bu.mouse_x >= 850 && bu.mouse_x <= 950 && bu.mouse_y >= 400 && bu.mouse_y <= 450 then
					(clear_graph () ; toutecreation () ; boucle 3 0 [])
				else 
					if bu.mouse_x >= 1100 && bu.mouse_x <= 1200 && bu.mouse_y >= 400 && bu.mouse_y <= 450 then
						(clear_graph () ; toutecreation () ; boucle 4 0 []);;

let rec menu () =
	depart();
	accueil();
	let bu = wait_next_event [Button_down] in
		if bu.mouse_x >= 700 && bu.mouse_x <= 800 && bu.mouse_y >= 400 && bu.mouse_y <= 450 then
			(clear_graph() ; choixniveau ())
		else menu ();;
		
depart ();;
choixducodesecret();;
(*menu ();; *)
(*end;; *)
