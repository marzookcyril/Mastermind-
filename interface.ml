open Graphics;;
open Code;;
open Ia;;
open Knuth;;
open Naif;;
open Niveau3;;
open Niveau2;;


(**Ouvre une fenetre et l'initialise*)
let depart () = 
	Random.self_init();
	open_graph "";
	set_window_title "Mastermind";
	resize_window 1980 1080;;


(**Initialisation des variables globales*)
let k = Code.nombre_pions ;; 					(**Nombre pions*)
let c = 6 ;;									(**Nombre couleurs*)
let t = int_of_string(Sys.argv.(2)) ;; 			(**Nombre tentatives*)
let l = [black;white;red;green;blue;yellow];;	(**Couleurs possibles*)
let joueur = (Sys.argv.(1));; 					(**Nom du joueur*)


(**Premier essai pour Knuth en fonction du nombre de pions*)
let essai1 = 
	match k with
	|x when x = 2 -> ["blanc";"bleu"] 
	|x when x = 3 -> ["blanc";"blanc";"bleu"] 
	|x when x = 4 -> ["blanc";"blanc";"bleu";"bleu"]
	|x when x = 5 -> ["blanc";"blanc";"blanc";"bleu";"bleu"]
	|x when x = 6 -> ["blanc";"blanc";"blanc";"bleu";"bleu";"bleu"] 
	|_ -> ["blanc";"blanc";"bleu";"bleu"];;
	
	
(**Nombre parties*)	
let nb = 
	if int_of_string(Sys.argv.(3)) mod 2 = 0 then int_of_string(Sys.argv.(3))
	else int_of_string(Sys.argv.(3)) +1 ;;


(**Choix si entrée réponses*)
let reponsepion = 
	if Sys.argv.(4) = "true" then true
	else false;;


(**Accueil avec boutons pour choix du mode*)
let accueil () = 
	draw_rect 800 500 100 50;
	moveto 815 520;
	draw_string "Joueur vs IA" ;
	draw_rect 1050 500 100 50;
	moveto 1053 520;
	draw_string "Joueur vs Joueur";;


(**Choix utilisation algorithme
	*@param		numéro d'ia
	*@param		liste des essais
	*@param		liste des possibles
	*)
let choisir_algo x essais possible = 
	match x with 
	|a when a = 1 -> Naif.choix [fst(essais)] possible
	|a when a = 2 -> Niveau3.choix [fst(essais)] (Niveau3.filtre essais possible)
	|a when a = 3 -> Niveau2.choix [fst(essais)] (Niveau2.filtre essais possible)
	|a when a = 4 -> Knuth.knuths essais possible
	|_ -> failwith(" il n'y a pas autant d'ia ");;
	

(**Converti la couleur d'int a string
	*@param 	la couleur en type unit/int
	*@return 	la couleur en type string
	*)
let convert n = 
	match n with 
	|a when a = black -> "noir" 
	|a when a = white -> "blanc"
	|a when a = red -> "rouge"
	|a when a = green -> "vert"
	|a when a = blue -> "bleu"
	|a when a = yellow  -> "jaune"
	|_ -> "";;


(**Converti la couleur de string a int
	*@param 	la couleur en type string
	*@return 	la couleur en type unit/int
	*)
let invconvert n = 
	match n with 
	|a when a = "noir" -> black 
	|a when a = "blanc" -> white
	|a when a = "rouge" -> red 
	|a when a = "vert" -> green
	|a when a = "bleu" -> blue
	|a when a = "jaune" -> yellow
	|_ -> 0;;


(**Liste couleurs totale pour la partie en cours
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


(**Dessin d'un carré de la couleur voulue à l'endroit souhaité
	*@param 	couleur voulue
	*@param		première coordonnée 
	*@param 	seconde coordonnée
	*)
let carre couleur x y =
		draw_rect (x-1) (y-1) 22 22;
		set_color couleur;
		fill_rect x y 20 20;
		set_color black;;


(**Transforme une liste de couleur en tuple (nb de noir,nb de blanc)
	*@param 	liste de couleur
	*@param		accumulateur 
	*@return 	(nb de noir,nb de blanc)
	*)
let rec listtotuple tab (x,y) = 
	match tab with 
	|(t :: q) -> if t = black then listtotuple q (x+1,y) else if t = white then listtotuple q (x,y+1) else listtotuple q (x,y)
	|_ -> (x,y);;


(**Initialisation des ronds cliquables*)	
let rondreponse () = 
	for i = 0 to (k-1) do rond black (498 + i*66) 53 done;;



(**Initialisation des carrés cliquables*)	
let carrereponse () =
	for i = 0 to (k-1) do carre red (898 + i*66) 53 done;;



(**Creation du bouton pour valider*)
let boutonvalider () =
	draw_rect 345 33 80 40;
	moveto 350 53;
	draw_string "cliquez ici" ;
	moveto 350 43;
	draw_string "pour valider" ;;

	
(**Creation du bouton pour valider avec l'ia*)
let boutonvalideria () =
	draw_rect 345 33 80 40;
	moveto 350 53;
	draw_string "votre code" ;
	moveto 350 43;
	draw_string "secret" ;;


(**Change de couleur le rond sur lequel on clique 
	*@param 	première coordonnée 
	*@param 	seconde coordonnée
	*@param 	couleur actuelle
	*)
let cliccouleur a b couleur =
	for i = 0 to (k-1) do 
	if a >= (478 + i*66) && a <= (518 + i*66) && b >= 33 && b <= 73 then 
		rond (ch_couleur listecouleur couleur) (498 +i*66) 53 
	else draw_rect 10 10 1 1 done;;


(**Change de couleur le carré sur lequel on clique 
	*@param 	première coordonnée 
	*@param 	seconde coordonnée
	*@param 	couleur actuelle
	*)
let cliccouleurcarre a b couleur =
	for i = 0 to (k-1) do 
	if a >= (878 + i*66) && a <= (918 + i*66) && b >= 33 && b <= 73 then 
		carre (ch_couleur [black;white;red] couleur) (898 +i*66) 53 
	else draw_rect 10 10 1 1 done;;


(**Valide le choix des couleurs et l'insere dans la grille 
	*@param 	première coordonnée 
	*@param 	seconde coordonnée
	*@param		variable permettant le placement correcte dans la grille
	*)
let valider add = 
	for i = 0 to (k-1) do 
		rond (point_color (498+i*66) 53) (498+i*66) (53 + (add * 66)) 
	done;;


(**Supprime le Some pour faciliter les calculs
	*@param 	OptionTuple   Some(a,b)
	*@return 	Tuple         (a,b)
	*)
let desome x = 
        match x with 
        |Some(a,b) -> (a,b)
        |_ -> failwith "";;
        
        
(**Affiche la reponse au code selectionné précedemment
	*@param 	première coordonnée 
	*@param 	seconde coordonnée
	*@param		variable permettant le placement correcte dans la grille de reponse
*)
let pionplace a add = 
	if (add < (t+1) && add <> 0) then 
		(for i = 1 to fst(a) do fill_rect ((400 - 15*k) + 20*i) (45 + (add * 66)) 15 15 done;
		for i = 1 to snd(a) do draw_rect ((400 - 15*k) + (fst(a)+i)*20) (45 + (add * 66)) 15 15 done)
	else 
		draw_rect 10 10 1 1;;


(**Recupere le code et le place dans un tableau
	*@param 	variable temporaire pour l'execution du code
	*@return 	un tableau avec le code
	*)
let rec recucode temp = 
	if temp < k then
		point_color (498 + temp*66) 53 :: recucode (temp + 1)
	else
		[];;


(**Transforme le code d'int en code de string
	*@param 	code en int
	*@return 	code en string
	*)
let rec tableau_peg t =
	match t with 
	|(h :: q) -> (convert h) :: tableau_peg q 
	|_ -> [];;
	
	
(**Transforme le code de string en code d'int
	*@param 	code en string
	*@return 	code en int
	*)
let rec tableau_invpeg t =
	match t with 
	|(h :: q) -> (invconvert h) :: tableau_invpeg q 
	|_ -> [];;
	
	
(**Recupere le code secret et le place dans un tableau
	*@param 	variable temporaire pour l'execution du code
	*@return 	un tableau avec le code secret
	*)
let rec recucodesecret temp = 
	if temp < k then
	 point_color (698 + temp*66) 363 :: recucodesecret (temp + 1) 
		
	else
		[];;
		
		
(**Recupere le code secret et le place dans un tableau
	*@param 	variable temporaire pour l'execution du code
	*@return 	un tableau avec le code secret
	*)
let rec recucodecarre temp = 
	if temp < k then
	 point_color (900 + temp*66) 56 :: recucodecarre (temp + 1) 	
	else
		[];;
	
				
(**Créer la totalite des grilles de jeu*)
let toutecreation () = 
	grilleee ();
	grille 1 1 k;
	grillereponse 1;
	rondreponse ();
	boutonvalider ();
	moveto 250 50;;	
	
(**Créer la totalite des grilles de jeu d'ia*)
let toutecreationia () = 
	grilleee ();
	grille 1 1 k;
	grillereponse 1;
	boutonvalideria ();;


(**Change de couleur le rond sur lequel on clique 
	*@param 	première coordonnée 
	*@param 	seconde coordonnée
	*@param 	couleur actuelle
	*)
let chcouleurcode a b couleur = 
	for i = 0 to (k-1) do 
	if a >= (678 + i*66) && a <= (718 + i*66) && b >= 343 && b <= 383 then 
		rond (ch_couleur listecouleur couleur) (698 +i*66) 363 
	else draw_rect 10 10 1 1 done;;


(**Dessine le code donné en second paramètre 
	*@param 	première coordonnée 
	*@param 	seconde coordonnée
	*@param 	liste de couleur
	*)
let dessinecouleur a b tab = 
	let tab1 = tableau_invpeg(tab) in 
		for i = 0 to (k-1) do
			rond (List.nth tab1 i) (a + i*66)  b 
		done;;


(**Change les couleur du tableau puis recupere le code secret dans un tableau 
	*@param 	variable temporaire
	*@return	code secret
	*)
let rec couleurpourlecode tableau = 
	let bu = wait_next_event [Button_down] in
	if bu.mouse_x >= 545 && bu.mouse_x <= 625 && bu.mouse_y >= 343 && bu.mouse_y <= 383 then
		let tableau = (tableau_peg(recucodesecret 0)) in tableau
	else
		(chcouleurcode bu.mouse_x bu.mouse_y (point_color bu.mouse_x bu.mouse_y); couleurpourlecode tableau);;


(**Ensemble des fonctions pour stocker le code secret*)
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
	couleurpourlecode [];;


(**Ecran de victoire*)	
let ecran_victoire () =
	moveto 850 500;
	draw_string "C'est gagne!";;
	

(**Ecran de defaite*)		
let ecran_defaite () = 
	moveto 850 500;
	draw_string "C'est perdu!";;


(**Ecran de defaite à cause d'une erreur*)
let ecran_defaite_erreur () = 
	clear_graph();
	moveto 800 500;
	draw_string "C'est perdu! c'est pas bien d'essayer de tricher";;
	
	
(**Initialisation des parties cliquables pour rentrer la réponse au code*)	
let validercarre () = 
	draw_rect (800 - 15*k) 33 (15*k + 66*(k+1)) 66;
	draw_rect 875 99 150 100;
	moveto 880 179;
	draw_string "reponse au code" ;
	moveto 880 159;
	draw_string "noir = bien place" ;
	moveto 880 139;
	draw_string "blanc = bonne couleur" ;
	moveto 880 119;
	draw_string "rouge = aucun des deux" ;
	draw_rect 755 46 80 40;
	moveto 760 64;
	draw_string "cliquez ici" ;
	moveto 760 54;
	draw_string "pour valider" ;
	carrereponse ();;
	

(**Permet de rentrer la réponse au code*)	
let rec entreereponse () = 
	let bu = wait_next_event [Button_down] in 
	if bu.mouse_x >= 755 && bu.mouse_x <= 835 && bu.mouse_y >= 46 && bu.mouse_y <= 84 then
		draw_rect 10 10 1 1
	else
		(cliccouleurcarre bu.mouse_x bu.mouse_y (point_color bu.mouse_x bu.mouse_y) ; entreereponse ());;
	
	
(**Boucle faisant tourner tout le jeu contre l'ia avec ia cherche le code
	*@param 	numéro de l'ia
	*@param 	code_secret
	*@param 	accumulateur/tentative
	*)	
let rec boucleia level code_secret a = 
	if reponsepion = true then 
		((dessinecouleur 498 (119+ a*66) (fst (List.nth (code_secret) a));
		validercarre());
		pionplace (listtotuple ((recucodecarre 0)) (0,0)) (a+1); 
		moveto 1150 50;
		draw_string joueur;
		entreereponse ();
		if (desome(snd (List.nth (code_secret) a))) = listtotuple ((recucodecarre 0)) (0,0) then 
			(pionplace (desome(snd (List.nth (code_secret) a))) (a+1); 
			if ((desome(snd (List.nth (code_secret) a))) = (k,0)) || a > (t-2) then fill_rect 100 100 1 1
			else boucleia level code_secret (a+1))
		else ecran_defaite_erreur())
	else 
		((dessinecouleur 498 (119+ a*66) (fst (List.nth (code_secret) a)));
		pionplace (desome(snd (List.nth (code_secret) a))) (a+1); 
		draw_rect 245 895 250 20;
		moveto 250 900 ;
		draw_string "Cliquez n'importe ou pour faire defiler";
		let u = wait_next_event [Button_down] in 
		if (((desome(snd (List.nth (code_secret) a))) = (k,0)) || a > (t-2)) then fill_rect 100 100 1 1
		else boucleia level code_secret (a+1));;
	
	
(**Boucle faisant tourner tout le jeu contre l'ia avec joueur cherche le code
	*@param 	accumulateur/tentative
	*@param		code secret
	*)
let rec boucle ad code_secret = 
	if ad > t then let p = wait_next_event [Button_down] in (moveto 550 (75 * t)) else 
	let bu = wait_next_event [Button_down] in
	if bu.mouse_x >= 345 && bu.mouse_x <= 425 && bu.mouse_y >= 33 && bu.mouse_y <= 73 then
		(valider ad ; let tableau = tableau_peg((recucode 0)) in ((pionplace (desome ((Code.reponse tableau code_secret))) ad); if desome ((Code.reponse tableau code_secret)) = (k,0) then let a = wait_next_event [Button_down] in moveto 5 5 else boucle (ad+1) code_secret)) 
	else
		(cliccouleur bu.mouse_x bu.mouse_y (point_color bu.mouse_x bu.mouse_y); boucle ad code_secret);;
		
		
(**Supprime un élement d'une liste 
	*@param 	element à supprimer 
	*@param		liste
	*@return    liste sans l'élement a
	*)		
let supprime a l1 = List.filter (fun t -> if List.mem t [a] then false else true) l1;;


(**Fonction qui fait tourner l'algo de knuth 
	  *@param		le code secret
	  *@param		l'essai
	  *@param		liste des possibles
	  *@param		accumulateur pour compter le nombre d'essai
	  *@return		liste des code avec la reponse
	  *)
let rec jouer level codesecret essais possible acc = 
			match essais with 
			|(l , Some(x,y)) when acc <= t -> if l = codesecret then [essais] else let r = choisir_algo level essais possible in 
                                                    if (level > 1) then 
                                                        essais :: (jouer level codesecret (r, Code.reponse codesecret r) (IA.filtre level essais possible) (acc+1))
                                                    else 
                                                        essais :: (jouer level codesecret (r, Code.reponse codesecret r) (supprime l possible) (acc+1))

            |_-> [];;


	
(**Dessine les carres de choix de niveau*)			
let dessin_carres_niveau () = 
	draw_rect 450 500 100 50;
	moveto 470 520;
	draw_string "Level 1" ;
	
	draw_rect 700 500 100 50;
	moveto 720 520;
	draw_string "Level 2" ;
	
	draw_rect 950 500 100 50;
	moveto 970 520;
	draw_string "Level 3" ;
	
	set_color red;
	fill_rect 1200 500 100 50;
	moveto 1220 520;
	set_color black;
	draw_rect 1200 500 100 50;
	draw_string "Level 4" ;;

	

(**Choix du niveau de l'ia puis lancement des boucles de jeu
	*@param 	accumulateur/tentative
	*)	
let rec choixniveau a  = 
	dessin_carres_niveau ();
	let bu = wait_next_event [Button_down] in
		if bu.mouse_x >= 450 && bu.mouse_x <= 550 && bu.mouse_y >= 500 && bu.mouse_y <= 550 then
			(clear_graph () ; (toutecreation () ; moveto 250 50 ; draw_string joueur ; boucle 0 (IA.choix 1 [] Code.tous))  ; clear_graph(); moveto 550 410 ; draw_string joueur ;
				let u = (choixducodesecret()) in clear_graph() ; toutecreationia(); 	dessinecouleur 498 53 u ; (boucleia 0 (jouer 1 u (essai1, (Code.reponse essai1 u)) Code.tous 0) 0)) 
		else 
			if bu.mouse_x >= 700 && bu.mouse_x <= 800 && bu.mouse_y >= 500 && bu.mouse_y <= 550 then
				(clear_graph () ; (toutecreation () ; moveto 250 50 ; draw_string joueur ; boucle 0 (IA.choix 1 [] Code.tous)) ; clear_graph(); moveto 550 410 ; draw_string joueur ;
					let u = (choixducodesecret()) in clear_graph() ; toutecreationia() ; 	dessinecouleur 498 53 u ; (boucleia 0 (jouer 3 u (essai1, (Code.reponse essai1 u)) (IA.filtre 3 (essai1,Code.reponse essai1 u) Code.tous) 0) 0))
			else
				if bu.mouse_x >= 950 && bu.mouse_x <= 1050 && bu.mouse_y >= 500 && bu.mouse_y <= 550 then
					(clear_graph () ; (toutecreation () ;moveto 250 50 ; draw_string joueur ; boucle 0 (IA.choix 1 [] Code.tous)) ; clear_graph(); moveto 550 410 ; draw_string joueur ;
							let u = (choixducodesecret()) in clear_graph() ; toutecreationia() ; 	dessinecouleur 498 53 u ; (boucleia 0 (jouer 2 u (essai1, (Code.reponse essai1 u)) (IA.filtre 2 (essai1,Code.reponse essai1 u) Code.tous) 0) 0))
				else 
					if bu.mouse_x >= 1200 && bu.mouse_x <= 1300 && bu.mouse_y >= 500 && bu.mouse_y <= 550 then
						(clear_graph () ; (toutecreation () ; moveto 250 50 ; draw_string joueur ;boucle 0 (IA.choix 1 [] Code.tous)) ; clear_graph(); moveto 550 410 ; draw_string joueur ;
							let u = (choixducodesecret()) in clear_graph() ; toutecreationia() ; 	dessinecouleur 498 53 u ; (boucleia 0 (jouer 4 u (essai1, (Code.reponse essai1 u)) (IA.filtre 4 (essai1,Code.reponse essai1 u) Code.tous) 0) 0))
					else choixniveau a;;


(**Boucle joueur contre joueur 
	*@param 	accumulateur/tentative
	*@param		code secret
	*)	
let rec bouclejcj ad code_secret =
	if reponsepion = true then 
		(validercarre();
		if ad > t then (moveto 550 (75 * t)) else 
		let bu = wait_next_event [Button_down] in
		if bu.mouse_x >= 345 && bu.mouse_x <= 425 && bu.mouse_y >= 33 && bu.mouse_y <= 73 then
			(valider (ad+1) ; entreereponse(); 
			let tableau = tableau_peg((recucode 0)) in 
			if desome ((Code.reponse tableau code_secret)) = listtotuple ((recucodecarre 0)) (0,0) then
				((pionplace (desome ((Code.reponse tableau code_secret)))) (ad+1); 
				if desome ((Code.reponse tableau code_secret)) = (k,0) then 
					moveto 5 5 
				else bouclejcj (ad+1) code_secret)
			else ecran_defaite_erreur() ; let u = wait_next_event [Button_down] in moveto 5 5 )
		else
			(cliccouleur bu.mouse_x bu.mouse_y (point_color bu.mouse_x bu.mouse_y); bouclejcj ad code_secret))
	else 
		(if ad > t then (moveto 550 (75 * t)) else 
		(let bu = wait_next_event [Button_down] in
		if bu.mouse_x >= 345 && bu.mouse_x <= 425 && bu.mouse_y >= 33 && bu.mouse_y <= 73 then
			(valider (ad+1);
			let tableau = tableau_peg((recucode 0)) in 
			((pionplace (desome ((Code.reponse tableau code_secret)))) (ad+1); 
			if desome ((Code.reponse tableau code_secret)) = (k,0) then 
				let b = wait_next_event [Button_down] in moveto 5 5
			else bouclejcj (ad+1) code_secret))
		else
			(cliccouleur bu.mouse_x bu.mouse_y (point_color bu.mouse_x bu.mouse_y); bouclejcj ad code_secret)));;


(**Lance la boucle de joueur contre joueur avec le code secret choisi par l'autre utilisateur*)
let rec jcj () = 
	moveto 550 410 ; draw_string joueur ;
	let u = choixducodesecret () in 
	(clear_graph () ; (toutecreation () ; moveto 250 50 ; draw_string "joueur 2" ; bouclejcj 0 u); clear_graph()) ; 
	moveto 550 410 ; draw_string "joueur 2" ;
	let v = choixducodesecret () in 
	(clear_graph () ; (toutecreation () ; moveto 250 50 ; draw_string joueur ; bouclejcj 0 v)) ; clear_graph();;


(**Ecran de fin*)	
let ecran_fin() = 
	moveto 850 500;
	draw_string "Termine fin de partie";;
	
	
(**Menu principal permettant de lancer le reste
	*@param		nombre de parties à jouer
	*)	
let rec menu nb =
	clear_graph();
	if nb <= 0 then (ecran_fin() ; let b = wait_next_event [Button_down] in moveto 5 5)
	else 
	(clear_graph();
	accueil();
	let bu = wait_next_event [Button_down] in
		if bu.mouse_x >= 800 && bu.mouse_x <= 900 && bu.mouse_y >= 500 && bu.mouse_y <= 550 then
			(clear_graph() ; if nb mod 2 = 0 then choixniveau 1 else choixniveau 2; let u = wait_next_event [Button_down] in menu (nb-2))
		else 
			if bu.mouse_x >= 1050 && bu.mouse_x <= 1250 && bu.mouse_y >= 500 && bu.mouse_y <= 550 then
				(clear_graph () ; jcj ();  menu (nb-2))
			else 
				(clear_graph(); menu nb));;


depart();;
menu nb;;
