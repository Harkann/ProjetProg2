import Array._

class Partie(){
	/**contient l'id des pieces à leur position. vaut "0" si pas de pièce a la position.
	(plus grande que normalement, pour pas avoir a s'embêter avec les indices pour les déplacements)*/
	var matrix = ofDim[Piece](9,9); 
	/**couleur du joueur en train de jouer, 'W' ou 'B'*/
	var player = 'W';
	/**nombre d'ia, 0, 1 ou 2*/
	var nb_ia = 0
	/**couleur de l'ia, 'B','W' ou '0'*/
	var color_ia = 'B';
	/**la partie est en cours ou non */
	var is_running = true
	/**délai en ms avant le déplacement des pièces de l'ia*/
	var delai_ia = 10

	/**renvoie si la partie est finie*/
	def stop() ={
		is_running = false
	}
	/**sera utilisée pour définir la vitesse du jeu de l'IA*/
	def set_delay_ia(value:Int) = {
		delai_ia = value
	}
	/**renvoie la couleur du joueur opposé a "player"*/
	def other_player(player: Char):Char = {
		if (player=='B') {return 'W'}
		else {return 'B'} 
		//petit risque de problème si char différent de 'B' ou 'W'
	}
	/**lance le tour suivant*/
	def next_turn():Unit = {
		if (is_running){is_mat(player)}
		if (is_running){is_mat(other_player(player))}
		if (is_running){is_pat(other_player(player))}
		if (is_running){
			if (nb_ia == 0){
				player = other_player(player)
			}
			else if (nb_ia == 1){
				if (player == color_ia){
					player = other_player(player)
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
	/**renvoie l'id de la pièce a la position (i,j)*/
	def id_piece_on_case (i:Int,j:Int):String = {
		var piece_ij=matrix(i)(j)
		return piece_ij.id
	}
	/**renvoie la couleur de la pièce ayant l'id "id"*/
	def color_from_id(id:String):Char = {
		return id(0)
	}
	/**renvoie le type de la pièce ayant l'id "id"*/
	def type_from_id(id:String):String = {
		return id.substring(1,3)
	}
	/**renvoie le joueur courant*/
	def get_player() = player
	/**renvoie la pièce ayant l'id "id"*/


	/*
	def get_piece(id:String):Piece = {
		/**position de la pièce dans la liste, vaut -1 pour planter si la pièce existe pas*/
		var indice = -1
		for( i <- 0 to liste_pieces.length-1) {
			if (liste_pieces(i).get_id() == id){
				indice = i
			}			
		}
		return liste_pieces(indice)
	}
	*/


	/**renvoie la liste des mouvements possibles pour le joueur "player"
	(utilisée par l'ia)*/
	def allowed_moves(player:Char): List[((Int,Int),(Int,Int))] = {
		/**liste des mouvements possibles*/
		var all_moves : List[ ((Int,Int),(Int,Int)) ] = List()
		for( i <- 1 to 8) {
			for( j <- 1 to 8) {
				/**pièce sur la case (i,j)*/
				var piece_ij=matrix(i)(j)
				/**id de la pièce sur la case (i,j)*/
				var id_piece_ij = piece_ij.id

				if (id_piece_ij(0)==player)
				{
					/**liste des mouvements possibles de la pièce courante*/
					var (list_move,list_attack)= piece_ij.move_piece_check((i,j))
					for( move <- list_move) {
						all_moves=all_moves:+((i,j),move)
					}
				}
			}
		}
		return all_moves
	}
	/**teste si le joueur "player" est pat et affiche pat*/
	def is_pat(player:Char) = {
		if (allowed_moves(player) == List()){
			Interface.pat()
		}
	}
	/**renvoie la liste des pièces du joueur "player" qui sont attaquées par les pièces de l'autre joueur.*/
	def in_danger_of(player: Char): List[(Int,Int)] = {
		/**liste des pièces attaquées*/
		var res : List[ (Int,Int) ] = List()
		/**autre joueur*/
		val other=other_player(player)
		for( i <- 1 to 8) {
			for( j <- 1 to 8) {
				var piece_ij=matrix(i)(j)
				var id_piece_ij = piece_ij.id
				if (id_piece_ij!="0"){
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
	/**renvoie si le joueur "player" est en échec*/
	def is_check(player : Char) : Boolean = {
		/**autre joueur*/
		val other=other_player(player)
		/**liste des pièces attaquées*/
		var list_in_danger=in_danger_of(other)
		for (pos <-list_in_danger){
			var (i,j)=pos
			var piece= matrix(i)(j)
			var id_piece=piece.id
			if (id_piece.substring(0,3)==player+"Ki"){
				return true
			}
		}
		return false

	}

	/**renvoie si le joueur "player" est mat*/
	def is_mat(player: Char) : Unit = {
		/**id du roi*/
		val id_king=player+"Ki"+0
		/**position du roi*/
		var position=(1,1)
		var king = null 
		for( i <- 1 to 8) {
			for( j <- 1 to 8) {
				var piece_ij=matrix(i)(j)
				/**id de la pièce sur la case (i,j)*/
				var id_piece_ij = piece_ij.id
				if(id_king==id_piece_ij) {
					position=(i,j)
					king = matrix(i)(j)
				}
			}
		}
		/***/
		var (moves,attacks) =king.move_piece_check(position)
		if ((is_check(player))&& (allowed_moves(player)==List())) {Interface.perdu(player)}

	}
	/**défini les paramètres de la partie pour deux joueurs*/
	def partie_two_players() = {
		nb_ia = 0
		color_ia = '0'
	}
	/**défini les paramètres de la partie pour un joueur et une ia de couleur "color"*/
	def partie_one_ia(color:Char) ={
		nb_ia = 1
		color_ia = color
	}
	/**défini les paramètres de la partie pour deux joueurs*/
	def partie_two_ia() = {
		nb_ia = 2
		color_ia = '0'
	}
	/**compte le nombre de tours*/
	var nb_tours = 0
	
	/**démarre la partie*/
	def start() = {
		if (color_ia == 'W'|| nb_ia == 2){
			if (nb_ia == 1){player = 'B'}
			new Thread(new IA('W')).start
		}
	}
	/**initialise la partie*/
	def partie_init() = {
		is_running = true
		for( i <- 1 to 8) {
			for( j <- 1 to 8) {
				matrix(i)(j) = null
			}
		}
		player = 'W'
		nb_tours = 0
		matrix(2)(1) = new Peon('W',(2,1))
		matrix(2)(2) = new Peon('W',(2,2))
		matrix(2)(3) = new Peon('W',(2,3))
		matrix(2)(4) = new Peon('W',(2,4))
		matrix(2)(5) = new Peon('W',(2,5))
		matrix(2)(6) = new Peon('W',(2,6))
		matrix(2)(7) = new Peon('W',(2,7))
		matrix(2)(8) = new Peon('W',(2,8))
		matrix(1)(1) = new Tower('W',(1,1))
		matrix(1)(8) = new Tower('W',(1,8))
		matrix(1)(2) = new Knight('W',(1,2))
		matrix(1)(7) = new Knight('W',(1,7))
		matrix(1)(3) = new Bishop('W',(1,3))
		matrix(1)(6) = new Bishop('W',(1,6))
		matrix(1)(4) = new Queen('W',(1,4))
		matrix(1)(5) = new King('W',(1,5))

		//definition des pieces noires
		matrix(7)(1) = new Peon('B',(7,1))
		matrix(7)(2) = new Peon('B',(7,2))
		matrix(7)(3) = new Peon('B',(7,3))
		matrix(7)(4) = new Peon('B',(7,4))
		matrix(7)(5) = new Peon('B',(7,5))
		matrix(7)(6) = new Peon('B',(7,6))
		matrix(7)(7) = new Peon('B',(7,7))
		matrix(7)(8) = new Peon('B',(7,8))
		matrix(8)(1) = new Tower('B',(8,1))
		matrix(8)(8) = new Tower('B',(8,8))
		matrix(8)(2) = new Knight('B',(8,2))
		matrix(8)(7) = new Knight('B',(8,7))
		matrix(8)(3) = new Bishop('B',(8,3))
		matrix(8)(6) = new Bishop('B',(8,6))
		matrix(8)(4) = new Queen('B',(8,4))
		matrix(8)(5) = new King('B',(8,5))
		println("Initialisation terminée")
	}
	
}
object Projet{
	/**stocke tous les paramètres de la partie*/
	var partie= new Partie()
}

/**permet de lancer l'ia sous forme de thread*/
	class IA(color:Char) extends Runnable{
		/**lance le thread du tour de l'ia*/
		override def run = {
			nb_tours=nb_tours+1
			var moves_ia = allowed_moves(color)
			Thread.sleep(delai_ia)
			/**objet random*/
			var random_move = scala.util.Random
			/**entier random permettant de choisir un mouvement*/
			var random_moveInt = random_move.nextInt(moves_ia.length)
			/**origine et destination de la pièce*/
			var (origin,destination) = moves_ia(random_moveInt)
			/**coordonnées de l'origine*/
			var (oi,oj) = origin
			/**coordonnées de la destination*/
			var (di,dj) = destination
			/**id de la pièce de départ*/
			var id_piece_selected = id_piece_on_case(oi,oj)
			/**id de la pièce sur la case de destination*/
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