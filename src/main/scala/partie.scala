import Array._

class Partie() {
	var matrix_pieces = ofDim[String](8,8);
	// contient l'id des pieces à leur position. vaut 0 si pas de piece a la position.
	var player = 'W';
	var check = false; //gros 
	def is_check() = check;
	def next_turn() = {
		if (player == 'W') {player = 'B'}
		else {player = 'W'}
	}

}
=======

object Projet{
	var partie= new Partie()
}

