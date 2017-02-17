import Array._

class Partie() {
	var matrix_pieces = ofDim[String](9,9); //plus grande pour pas avoir à s'enmerder avec les indices.
	for( i <- 1 to 8) {
		for( j <- 1 to 8) {
			matrix_pieces(i)(j) = "0"
		}
	}


	// contient l'id des pieces à leur position. vaut "0" si pas de piece a la position.
	var player = 'W';

	def other_player(player: Char):Char={
	if (player=='B') {return 'W'}
	else {return 'B'} 
	//petit risque de probleme si char different de 'B' ou 'W'
}
	var check = false; //gros 
	def is_check() = check;
	def next_turn() = {
		if (player == 'W') {player = 'B'}
		else {player = 'W'}
	}
	def id_piece_on_case (i:Int,j:Int):String = {
		return matrix_pieces(i)(j)
	}
	def color_from_id(id:String):Char = {
		return id(0)
	}
	
	def type_from_id(id:String):String = {
		return id.substring(1,3)
	}
	
	def get_player() = player
	
		
	def partie_init() ={
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
	partie.partie_init()
}

