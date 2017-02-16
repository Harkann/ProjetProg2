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

		//definition des pieces blanches
		new Peon('W',(2,1))
		new Peon('W',(2,2))
		new Peon('W',(2,3))
		new Peon('W',(2,4))
		new Peon('W',(2,5))
		new Peon('W',(2,6))
		new Peon('W',(2,7))
		new Peon('W',(2,8))
		new Tower('W',(1,1))
		new Tower('W',(1,8))
		new Knight('W',(1,2))
		new Knight('W',(1,7))
		new Bishop('W',(1,3))
		new Bishop('W',(1,6))
		new Queen('W',(1,4))
		new King('W',(1,5))

		//definition des pieces noires
		new Peon('B',(7,1))
		new Peon('B',(7,2))
		new Peon('B',(7,3))
		new Peon('B',(7,4))
		new Peon('B',(7,5))
		new Peon('B',(7,6))
		new Peon('B',(7,7))
		new Peon('B',(7,8))
		new Tower('B',(8,1))
		new Tower('B',(8,8))
		new Knight('B',(8,2))
		new Knight('B',(8,7))
		new Bishop('B',(8,3))
		new Bishop('B',(8,6))
		new Queen('B',(8,4))
		new King('B',(8,5))
	}

}

object Projet{
	var partie= new Partie()
}

