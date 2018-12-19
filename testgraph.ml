open Graphics;;
open_graph "";;
set_window_title "Mastermind";;
resize_window 2000 1500;;

let k = 4 (*nombre pions*)

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
rond blue 538 53;;
rond black 578 53;;
rond white 618 53;;
rond yellow 658 53;;
rond green 698 53;;

moveto 350 53;;
draw_string "cliquez ici" ;;
moveto 350 43;;
draw_string "pour choisir ->" ;;


	
