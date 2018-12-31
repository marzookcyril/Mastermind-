open Graphics;;
open_graph "";;
set_window_title "Mastermind";;
resize_window 2000 1500;;

let k = 4 ;; (*nombre pions*)
let c = 6 ;;(*nombre couleurs*)
let l = [black;white;red;green;blue;yellow;cyan;magenta];;


let rec liste_couleur l c  = 
	match (l,c) with
	|(h :: t, x) when x > 0 -> h :: liste_couleur t (x-1)
	|(h :: [], _) -> [h]
	|(_ ,_) -> [];;

let listecouleur = liste_couleur l c;;

let rec ch_couleur l couleur =
	match l with
	|(h :: []) -> if h = couleur then List.hd listecouleur else 0
	|(h :: t) -> if h = couleur then List.hd t else ch_couleur t couleur 
	|_ -> 0;;
	

let rec grille i j n = 
	match (i,j,n) with 
	|(x,y,z) when x < 11 && y < z+1  -> draw_rect  (400+(y*66)) (20+(x*66)) 66 66 ; grille x (y+1) z
	|(x,y,z) when x < 11 && y = z+1 -> grille (x+1) 1 z;
	|_                          -> draw_rect 10 10 1 1 ;;

grille 1 1 k;;

draw_rect 340 86 120 660;;
fill_rect 460 86 5 660;;
draw_rect 340 20 (126 + 66*k) 66;;

let rec grillee acc = 
	match (acc) with
	|(x) when x < 11 -> draw_rect 340 (20+(x*66)) 130 0; grillee (x+1)
	|_ -> draw_rect 10 10 1 1 ;;
grillee 1;;

let rond couleur x y =
	draw_circle x y 20;
	set_color couleur;
	fill_circle x y 20;
	set_color black;;
	
rond red 498 53;;
rond red 564 53;;
rond red 630 53;;
rond red 696 53;;


draw_rect 345 33 80 40;;
moveto 350 53;;
draw_string "cliquez ici" ;;
moveto 350 43;;
draw_string "pour valider" ;;


let cliccouleur a b couleur =
	if a >= 478 && a <= 518 && b >= 33 && b <= 73 then 
		rond (ch_couleur listecouleur couleur) 498 53 
	else 
	if a >= 544 && a <= 584 && b >= 33 && b <= 73 then 
		rond (ch_couleur listecouleur couleur) 564 53 	
	else
	if a >= 610 && a <= 650 && b >= 33 && b <= 73 then 
		rond (ch_couleur listecouleur couleur) 630 53 
	else
	if a >= 676 && a <= 716 && b >= 33 && b <= 73 then 
		rond (ch_couleur listecouleur couleur) 696 53 
	else draw_rect 10 10 1 1 ;;

let valider a b add = 
	if a >= 345 && a <= 425 && b >= 33 && b <= 73 then	
		(rond (point_color 498 53) 498 (53 + (add * 66));
		rond (point_color 564 53) 564 (53 + (add * 66));
		rond (point_color 630 53) 630 (53 + (add * 66));
		rond (point_color 696 53) 696 (53 + (add * 66)))
	else draw_rect 10 10 1 1 ;;

let pionplace a b add = 
	if (add < 11 && add <> 0) then 
		(for i = 1 to a do fill_rect (340 + 20*i) (45 + (add * 66)) 15 15 done;
		for i = 1 to b do draw_rect (340 + (a+i)*20) (45 + (add * 66)) 15 15 done)
		else draw_rect 10 10 1 1;;


let rec boucle ad = 
	if ad > 10 then (moveto 550 750 ; draw_string "C'est perdu.") else 
	let bu = wait_next_event [Button_down] in
	if bu.mouse_x >= 345 && bu.mouse_x <= 425 && bu.mouse_y >= 33 && bu.mouse_y <= 73 then
		(valider bu.mouse_x bu.mouse_y ad ; pionplace (Random.int 5) (Random.int 5) ad ; boucle (ad+1)) else
		(cliccouleur bu.mouse_x bu.mouse_y (point_color bu.mouse_x bu.mouse_y); boucle ad);;

boucle 0 ;;
