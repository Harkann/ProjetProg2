//pas besoin d'inclure les trucs même depuis des fichiers différents.



//color de type char car la comparaison string char est fausse

abstract class Piece(color:Char,position : (Int,Int)) {
	//val color:Char; // 'B' ou 'W'
	val name:String; 
	var is_alive:Boolean;
	//val id:Int; // != "0"
	// Couleur.2PremieresLettres.numéro
	//var position:(Int,Int); //doublon avec la liste des pieces dans partie ?
	//def move(position:(Int,Int)):Unit; //change la position de la piece couple (ligne,colonne)
	//def take; -> inutile : un truc dans move gere la prise des pieces (cahnge le flag de la piece sur la case d'arivée)
	//def is_legal():List[(Int,Int)]; //renvoie liste des positions possibles pour le prochain coup
	// doit verifier si en echec et si le move peut causer l'echec avant de renvoyer la liste
}


trait Horizontal {
	def dpct_horiz(position:(Int,Int)) : List[(Int,Int)]{}  //uttiliser des traits pour factoriser le code.
}

trait Id_creation {
	def id_create(color:Char,name:String) : Int = {
		var ind=0
		for( i <- 0 to 7) {
			for( j <- 0 to 7) {
				val piece_ij = Projet.partie.matrix_pieces(i)(j)
				if ( piece_ij(0)==color )
				{ if ((piece_ij(1)+piece_ij(2))==name) {ind+=1}}
			}
		}
		return ind
	}	
}



class Queen(color:Char,position:(Int,Int)) extends Piece(color,position) with Id_creation {
	val name = "Qu"
	var is_alive= true
}

class Peon(color:Char,position:(Int,Int)) extends Piece(color,position) {
	val name="Pe"
	var is_alive=true
}

class King(color:Char,position:(Int,Int)) extends Piece(color,position) {
	val name="Ki"
	var is_alive=true
	//def is_check; 
}

class Tower(color:Char,position:(Int,Int)) extends Piece(color,position) {
	val name="To"
	var is_alive=true
}

class Knight(color:Char,position:(Int,Int)) extends Piece(color,position) {
	val name="Kn"
	var is_alive=true
}

class Bishop(color:Char,position:(Int,Int)) extends Piece(color,position) {
	val name="Bi"
	var is_alive=true

}