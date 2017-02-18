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
	var liste_pieces :List[Piece] = List()
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
	
	def get_piece(id:String):Piece = {

		var indice = -1
		println("lenght "+liste_pieces.length)
		for( i <- 0 to liste_pieces.length-1) {
			println(i)
			if (liste_pieces(i).get_id() == id){
				indice = i
				println(indice)
			}			
		}
		println(indice)
		return liste_pieces(indice)

	}

	def in_danger_of(player: Char): List[(Int,Int)]={
		var res : List[ (Int,Int) ] = List()
		val other=other_player(player)
		for( i <- 1 to 8) {
			for( j <- 1 to 8) {
				var id_piece_ij = matrix_pieces(i)(j)
				var piece_ij=get_piece(id_piece_ij)
				if (id_piece_ij(0)==player)
				{
					var (list_move,list_attack)= piece_ij.move_piece((i,j))
					res=res++list_attack
				}
			}
		}
		return res
	}

	def is_check(player : Char) : Boolean={
		val other=other_player(player)
		var list_in_danger=in_danger_of(other)
		if (list_in_danger.contains(player+"Ki0"))
			{return true}
		else {return false}

	}

	def partie_init() ={

		//definition des pieces blanches
		liste_pieces= liste_pieces:+new Peon('W',(2,1))
		liste_pieces= liste_pieces:+new Peon('W',(2,2))
		liste_pieces= liste_pieces:+new Peon('W',(2,3))
		liste_pieces= liste_pieces:+new Peon('W',(2,4))
		liste_pieces= liste_pieces:+new Peon('W',(2,5))
		liste_pieces= liste_pieces:+new Peon('W',(2,6))
		liste_pieces= liste_pieces:+new Peon('W',(2,7))
		liste_pieces= liste_pieces:+new Peon('W',(2,8))
		liste_pieces= liste_pieces:+new Tower('W',(1,1))
		liste_pieces= liste_pieces:+new Tower('W',(1,8))
		liste_pieces= liste_pieces:+new Knight('W',(1,2))
		liste_pieces= liste_pieces:+new Knight('W',(1,7))
		liste_pieces= liste_pieces:+new Bishop('W',(1,3))
		liste_pieces= liste_pieces:+new Bishop('W',(1,6))
		liste_pieces= liste_pieces:+new Queen('W',(1,4))
		liste_pieces= liste_pieces:+new King('W',(1,5))

		//definition des pieces noires
		liste_pieces= liste_pieces:+new Peon('B',(7,1))
		liste_pieces= liste_pieces:+new Peon('B',(7,2))
		liste_pieces= liste_pieces:+new Peon('B',(7,3))
		liste_pieces= liste_pieces:+new Peon('B',(7,4))
		liste_pieces= liste_pieces:+new Peon('B',(7,5))
		liste_pieces= liste_pieces:+new Peon('B',(7,6))
		liste_pieces= liste_pieces:+new Peon('B',(7,7))
		liste_pieces= liste_pieces:+new Peon('B',(7,8))
		liste_pieces= liste_pieces:+new Tower('B',(8,1))
		liste_pieces= liste_pieces:+new Tower('B',(8,8))
		liste_pieces= liste_pieces:+new Knight('B',(8,2))
		liste_pieces= liste_pieces:+new Knight('B',(8,7))
		liste_pieces= liste_pieces:+new Bishop('B',(8,3))
		liste_pieces= liste_pieces:+new Bishop('B',(8,6))
		liste_pieces= liste_pieces:+new Queen('B',(8,4))
		liste_pieces= liste_pieces:+new King('B',(8,5))
		println("lenght "+liste_pieces.length)
		println("init done")
	}
}
object Projet{
	var partie= new Partie()
}

