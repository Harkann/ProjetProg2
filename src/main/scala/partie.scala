import Array._
import scala.collection.mutable.ArrayBuffer

class Partie(){
	/**contient l'id des pieces à leur position. vaut "0" si pas de pièce a la position.
	(plus grande que normalement, pour pas avoir a s'embêter avec les indices pour les déplacements)*/
	var matrix = ofDim[Piece](9,9); 
	
	var pieces_B = Array(0,0,0,0,0,0)
	var pieces_W = Array(0,0,0,0,0,0)

	def modif_piece(color:Char,num:Int,modif:Int){
		if (color == 'B') {pieces_B(num) += modif
			println("+1")
		}
		else if (color == 'W'){pieces_W(num) +=modif 
			println("+1")
		}
	}

	var dplct_save : ArrayBuffer[Dpct]= ArrayBuffer()
	/**couleur du joueur en train de jouer, 'W' ou 'B'*/
	var player = 'W';
	/**nombre d'ia, 0, 1 ou 2*/
	var nb_ia = 0
	/**couleur de l'ia, 'B','W' ou '0'*/
	var color_ia = 'B';
	/**la partie est en cours ou non */
	var is_running = true
	/**délai en ms avant le déplacement des pièces de l'ia*/
	var delai_ia = Config.delai_ia
	/**l'interface peut deplacer des pieces*/
	var is_interface= true
	/**renvoie si la partie est finie*/
	def stop() ={
		is_running = false
	}
	/**renvoie la couleur du joueur opposé a "player"*/
	def other_player(player: Char):Char = {
		if (player=='B') {return 'W'}
		else {return 'B'} 
		//petit risque de problème si char différent de 'B' ou 'W'
	}
	/**lance le tour suivant*/
	def next_turn():Unit = {
		//if (is_running){is_mat(player)}
		//if (is_running){is_mat(other_player(player))}
		//if (is_running){is_pat(other_player(player))}
		if (is_running){
			if (nb_ia == 0){
				player = other_player(player)
			}
			else if (nb_ia == 1){
				player = other_player(player)
				if (player == color_ia){
					new Thread(new IA(color_ia,this)).start
				} 
			}
			else {
				player = other_player(player)
				new Thread(new IA(color_ia,this)).start

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
	def get_color(i:Int,j:Int):Char = {
		var piece_ij=matrix(i)(j)
		if (piece_ij != null){return piece_ij.color}
		else {return '0'}
	}
	/**renvoie le type de la pièce ayant l'id "id"*/
	def type_from_id(id:String):String = {
		return id.substring(1,3)
	}
	/**renvoie le joueur courant*/
	def get_player() = player
	/**renvoie la pièce ayant l'id "id"*/

	def get_piece(i:Int,j:Int):Piece = {
		/**position de la pièce dans la liste, vaut -1 pour planter si la pièce existe pas*/
		return matrix(i)(j)
	}

/*
	chantier de réflexions :
	- Prise en passant : 
	 besoin du dernier mouvement de l'adversaire.
	- Stocker les parties :
	 tableau des changement de positions et potentielles prises de piece ?
	definir un type special pour ça position de depart position d'arrive prise de qqch= null ou some(piece)
	- Promotion : 
	 variable globale pour lancer le choix la promotion ? NON
	 move : redécomposition -> déplacement de la tour dans le roque dans une fonction a part parce que là c'est moche.
							-> même chose pour le declanchement de la promotion.
	- 50 coups -> compteur reinitialisé a chaque prise de piece ou déplacement de pions.
	- Imposibilitée de mater : avoir un tableau qui garde le nombre de piece pour chaque couleur : genre en_jeu[0]=nb pion en jeux.
	- triple repetition de la position : rejouer la partie depuis le début ? :/ je penses que y a moyen d'être plus subtil.
	- perte au temps : timer


	Mika beaucoup d'interface à gerer...

	/* LA  PROMOTION PEUT METTRE E ECHEC BRUTALEMENT LE ROI ET IL N'AFFICHE PLUS MAT C'EST NORMAL ?*/



	*/
	def nothing_but_pat(tab_color:Array[Int],tab_other_color:Array[Int]) {
		if (
			((tab_color.deep == Array(0,0,0,0,0,1).deep) && (tab_other_color.deep == Array(0,0,0,0,0,1).deep)) ||
			((tab_color.deep == Array(0,0,0,1,0,1).deep) && (tab_other_color.deep == Array(0,0,0,0,0,1).deep)) ||
			((tab_color.deep == Array(0,0,0,1,0,1).deep) && (tab_other_color.deep == Array(0,0,0,1,0,1).deep)) ||		
			((tab_color.deep == Array(0,0,0,0,0,1).deep) && (tab_other_color.deep == Array(0,0,1,0,0,1).deep)) 

			){
			pat()
		}
	}

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
			pat()
		}
	}

	def pat(){
		this.stop()
		println("PAT")
		//Interface.RootWindow.interface_partie.pat()
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
				if (piece_ij != null ){
					var id_piece_ij = piece_ij.id
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
		var king = matrix(1)(1)
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
		if ((is_check(player))&& (allowed_moves(player)==List())) {
			this.stop()
			println("MAT")
			//Interface.RootWindow.interface_partie.perdu(player)
		}

	}
	def partie_nb_ia(nbIA:Int,colorIA:Char) = {
		nb_ia = nbIA
		color_ia=colorIA
	}
	/**compte le nombre de tours*/
	var nb_turn = 0
	
	/**démarre la partie*/
	def start() = {
		if (nb_ia == 1 && color_ia == 'W'){
			println("plop")
			new Thread(new IA('W',this)).start
		}
		else if (nb_ia == 2){
			println("plop")
			new Thread(new IA('W',this)).start
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
		nb_turn = 0
		matrix(2)(1) = new Peon('W',(2,1),this)
		matrix(2)(2) = new Peon('W',(2,2),this)
		matrix(2)(3) = new Peon('W',(2,3),this)
		matrix(2)(4) = new Peon('W',(2,4),this)
		matrix(2)(5) = new Peon('W',(2,5),this)
		matrix(2)(6) = new Peon('W',(2,6),this)
		matrix(2)(7) = new Peon('W',(2,7),this)
		matrix(2)(8) = new Peon('W',(2,8),this)
		matrix(1)(1) = new Tower('W',(1,1),this)
		matrix(1)(8) = new Tower('W',(1,8),this)
		matrix(1)(2) = new Knight('W',(1,2),this)
		matrix(1)(7) = new Knight('W',(1,7),this)
		matrix(1)(3) = new Bishop('W',(1,3),this)
		matrix(1)(6) = new Bishop('W',(1,6),this)
		matrix(1)(4) = new Queen('W',(1,4),this)
		matrix(1)(5) = new King('W',(1,5),this)

		//definition des pieces noires
		matrix(7)(1) = new Peon('B',(7,1),this)
		matrix(7)(2) = new Peon('B',(7,2),this)
		matrix(7)(3) = new Peon('B',(7,3),this)
		matrix(7)(4) = new Peon('B',(7,4),this)
		matrix(7)(5) = new Peon('B',(7,5),this)
		matrix(7)(6) = new Peon('B',(7,6),this)
		matrix(7)(7) = new Peon('B',(7,7),this)
		matrix(7)(8) = new Peon('B',(7,8),this)
		matrix(8)(1) = new Tower('B',(8,1),this)
		matrix(8)(8) = new Tower('B',(8,8),this)
		matrix(8)(2) = new Knight('B',(8,2),this)
		matrix(8)(7) = new Knight('B',(8,7),this)
		matrix(8)(3) = new Bishop('B',(8,3),this)
		matrix(8)(6) = new Bishop('B',(8,6),this)
		matrix(8)(4) = new Queen('B',(8,4),this)
		matrix(8)(5) = new King('B',(8,5),this)
	}

}

