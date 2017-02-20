import Array._

class Partie(){
	var matrix_pieces = ofDim[String](9,9); //plus grande pour pas avoir à s'enmerder avec les indices.
	

	// contient l'id des pieces à leur position. vaut "0" si pas de piece a la position.
	var player = 'W';
	var nb_ia = 0
	var color_ia = 'B';
	var liste_pieces :List[Piece] = List()
	var is_running = true
	var delai_ia = 100
	def stop() ={
		is_running = false
	}
	def set_delay_ia(value:Int) = {
		delai_ia = value
	}

	def other_player(player: Char):Char = {
		if (player=='B') {return 'W'}
		else {return 'B'} 
		//petit risque de probleme si char different de 'B' ou 'W'
	}
	var check = false; //gros et inutile
	def is_check() = check;
	def next_turn():Unit = {
		is_mat(other_player(player))
		if (is_running){
			if (nb_ia == 0){
				if (player == 'W') {player = 'B'}
				else {player = 'W'}
			}
			else if (nb_ia == 1){
				if (player == color_ia){
					if (player == 'W') {player = 'B'}
					else {player = 'W'}
				}
				else {
					new Thread(new IA(color_ia)).start
				}
			}
			else if (nb_ia == 2){
				if (player == 'W'){
					new Thread(new IA('W')).start
				}
				else {
					new Thread(new IA('B')).start
				}
			}
		}
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
		for( i <- 0 to liste_pieces.length-1) {
			if (liste_pieces(i).get_id() == id){
				indice = i
			}			
		}
		return liste_pieces(indice)

	}

	def allowed_moves(player:Char): List[((Int,Int),(Int,Int))] = {
		var all_moves : List[ ((Int,Int),(Int,Int)) ] = List()
		for( i <- 1 to 8) {
			for( j <- 1 to 8) {
				var id_piece_ij = matrix_pieces(i)(j)

				if (id_piece_ij(0)==player)
				{
					var piece_ij=get_piece(id_piece_ij)

					var (list_move,list_attack)= piece_ij.move_piece_check((i,j))//ici_pour l'IA
					for( move <- list_move++list_attack) {
						all_moves=all_moves:+((i,j),move)
					}
				}
			}
		}
		if (all_moves == List()){
			Interface.pat()
		}
		return all_moves
	}

	def in_danger_of(player: Char): List[(Int,Int)] = {
		var res : List[ (Int,Int) ] = List()
		val other=other_player(player)
		for( i <- 1 to 8) {
			for( j <- 1 to 8) {
				var id_piece_ij = matrix_pieces(i)(j)
				if (id_piece_ij!="0"){
				var piece_ij=get_piece(id_piece_ij)
				if (id_piece_ij(0)==player)
				{
					var (list_move,list_attack)= piece_ij.move_piece((i,j))
					res=res++list_attack
				}
				}
			}
		}
		return res
	}

	def is_check(player : Char) : Boolean = {
		val other=other_player(player)
		var list_in_danger=in_danger_of(other)
		for (pos <-list_in_danger){
			var (i,j)=pos
			var id_piece=matrix_pieces(i)(j)
			if (id_piece.substring(0,3)==player+"Ki"){
				return true
			}
		}
		return false

	}


	def is_mat(player: Char) : Unit = {
		val id_king=player+"Ki"+0
		val king=get_piece(id_king)
		var position=(1,1)
		for( i <- 1 to 8) {
			for( j <- 1 to 8) {
				var id_piece_ij=matrix_pieces(i)(j)
				if(id_king==id_piece_ij) {position=(i,j)}
			}
		}
		var (moves,attacks) =king.move_piece_check(position)
		if ((is_check(player))&& (allowed_moves(player)==List())) {Interface.perdu(player)}

	}

	def partie_two_players() = {
		nb_ia = 0
		color_ia = '0'
	}

	def partie_one_ia(color:Char) ={
		nb_ia = 1
		color_ia = color
	}

	def partie_two_ia() = {
		nb_ia = 2
		color_ia = '0'
	}

	class IA(color:Char) extends Runnable{
		override def run = {
			var moves_ia = allowed_moves(color)
			Thread.sleep(delai_ia)
			var random_move = scala.util.Random
			var random_moveInt = random_move.nextInt(moves_ia.length-1)
			var (origin,destination) = moves_ia(random_moveInt)
			var (oi,oj) = origin
			var (di,dj) = destination
			var id_piece_selected = id_piece_on_case(oi,oj)
			var id_destination = id_piece_on_case(di,dj)
			if (id_destination == "0"){
				Interface.piece_move(id_piece_selected,(oi,oj),(di,dj))
			}
			else{
				Interface.piece_take(id_piece_selected,(oi,oj),(di,dj))
			}
			if (player == 'W') {player = 'B'}
			else {player = 'W'}
			
			Projet.partie.next_turn()
		}
	}
	def start() = {
		if (color_ia == 'W'|| nb_ia == 2){
			println("start plop")
			new Thread(new IA('W')).start
		}
	}

	def partie_init() ={
		is_running = true
		for( i <- 1 to 8) {
			for( j <- 1 to 8) {
				matrix_pieces(i)(j) = "0"
			}
		}
		player = 'W'
		//nb_ia = 0
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
		println("Initialisation terminée")
	}
	
}
object Projet{

	var partie= new Partie()
}

