//pas besoin d'inclure les trucs même depuis des fichiers différents.

abstract class P_Piece() {
	val color; // B ou W
	val name; 
	val is_alive;
	val id; // != "0"
	// Couleur.2PremieresLettres.numéro
	val position; //doublon avec la liste des pieces dans partie ?
	def move (a,b) = {position = (a,b)} ; //change la position de la piece couple (ligne,colonne)
	//def take; -> inutile : un truc dans move gere la prise des pieces (cahnge le flag de la piece sur la case d'arivée)
	def is_legal; //renvoie liste des positions possibles pour le prochain coup
	// doit verifier si en echec et si le move peut causer l'echec avant de renvoyer la liste
}


trait Horizontal {
	def dpct_horiz((a,b)) : Liste ... //uttiliser des traits pour factoriser le code.
}
class Queen(color:Str) extends Piece {
	name = "Qu"
}

class Peon() extends Piece {
	
}

class King() extends Piece {
	def is_check; 
}

class Tower() extends Piece {
					
}

class Knight() extends Piece {
		
}

class Bishop() extends Piece {
	
}