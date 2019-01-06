(** Algorithmes de recherche de code *)
open Code
open Naif1
open Naif2
open Naifc
open Knuth;;

module IA :
	sig
		(** Nombre d'algorithmes developpes *)
		val nombre_methodes : int
		
		(** Choisit un code a proposer 
			*@param methode 0 pour l'algorithme naif,
			*  				1 pour l'algorithme de KNUTH
			*				... et ainsi de suite
			*@param essais la liste des codes deja proposes
			*@param possibles la liste des codes possibles
			*@return le prochain code a essayer
			*)
		val choix  : int -> Code.t list -> Code.t list -> Code.t

		(** Filtre les codes possibles
			*@param methode 0 pour l'algorithme naif,
			*				1 pour l'algorithme de KNUTH
			*				... et ainsi de suite	
			*@param (code, rep) le code essaye et la reponse correspondante
			*@param possibles la liste de courante de codes possibles
			*@return la nouvelle liste de codes possibles
			*)
		val filtre : int -> (Code.t * (int*int) option) -> Code.t list -> Code.t list
		
	end = struct
		
		
		
		(** 1 : ia naif aléatoire 
			2 : ia plutôt forte faite par nous mêmes 
			3 : ia ne prenant en compte que les couleurs 
			4 : knuth *)
		let nombre_methodes = 4 ;; 
		
		
		(** On applique la fonction choix de chaque ia excepté celle de knuth
		  * car a chaque etape de knuth nous avons besoin de la reponse car 
		  * il faut utiliser filtre a chaque étape et il manque le parametre reponse*)
		let choix x essais possible = 
			match x with 
			|a when a = 1 -> Naif1.choix essais possible
			|a when a = 2 -> Naif2.choix essais possible
			|a when a = 3 -> Naifc.choix essais possible
			|_ -> failwith(" il n'y a pas autant d'ia ");;
		
		(** De même on applique la fonction filtre de chaque ia excepté celle de : 
		  * l'ia Naif1 car le code est choisi aléatoirement dans la fonction choix *)
		let filtre x essais possible = 
			match x with 
			|a when a = 2 -> Naif2.filtre essais possible
			|a when a = 3 -> Naifc.filtre essais possible
			|a when a = 4 -> Knuth.filtre essais possible
			|_ -> failwith(" il n'y a pas autant d'ia ");;
		
		let choisir_algo x essais possible = 
			match x with 
			|a when a = 1 -> Naif1.choix [fst(essais)] possible
			|a when a = 2 -> Naif2.choix [fst(essais)] (Naif2.filtre essais possible)
			|a when a = 3 -> Naifc.choix [fst(essais)] (Naifc.filtre essais possible)
			|a when a = 4 -> Knuth.knuths essais possible
			|_ -> failwith(" il n'y a pas autant d'ia ");;
	
		
	end;;
