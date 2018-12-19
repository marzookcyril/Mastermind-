(** Algorithmes de recherche de code *)
open Code;;
open Naif1
open Naif2;;

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
		
		let nombre_methodes = 3;; 
		
		
	
		
	end;;
