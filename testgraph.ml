open Graphics;;
open_graph "";;
set_window_title "Mastermind";;
resize_window 2000 1500;;

let k = 4 ;; (**Nombre pions*)
let c = 6 ;;(**Nombre couleurs*)
let t = 10 ;; (**Nombre tentatives*)
let l = [black;white;red;green;blue;yellow;cyan;magenta];; (**Couleurs possibles*)


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

grille 1 1 k;;

(**Delimitation grille/grille de reponse*)
draw_rect (400 - 15*k) 86 (15*k + 60) (66*t);;
fill_rect 460 86 5 (66*t);;
draw_rect (400 - 15*k) 20 (15*k + 66*(k+1)) 66;;

(**Dessine la grille de reponse
	*@param variable pour le dessin correct de la grille de reponse
	*)
let rec grillereponse acc = 
	match (acc) with
	|(x) when x < (t+1) -> draw_rect (400 - (15*k)) (20+(x*66)) (32*k) 0; grillereponse (x+1)
	|_ -> draw_rect 10 10 1 1 ;;

grillereponse 1;;

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
let rondreponse = 
	for i = 0 to (k-1) do rond black (498 + i*66) 53 done;;
	
draw_rect 345 33 80 40;;
moveto 350 53;;
draw_string "cliquez ici" ;;
moveto 350 43;;
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
		else draw_rect 10 10 1 1;;

(**Boucle principale permettant de jouer tout le reste*)
let rec boucle ad = 
	if ad > t then (moveto 550 (75 * t) ; draw_string "C'est perdu.") else 
	let bu = wait_next_event [Button_down] in
	if bu.mouse_x >= 345 && bu.mouse_x <= 425 && bu.mouse_y >= 33 && bu.mouse_y <= 73 then
		(valider bu.mouse_x bu.mouse_y ad ; pionplace (Random.int 5) (Random.int 5) ad ; boucle (ad+1)) else
		(cliccouleur bu.mouse_x bu.mouse_y (point_color bu.mouse_x bu.mouse_y); boucle ad);;

boucle 0 ;;
