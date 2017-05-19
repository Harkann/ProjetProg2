/**permet de lancer l'ia sous forme de thread*/
class IA(color:Char,partie:Partie) extends Runnable{
	/**lance le thread du tour de l'ia*/
	override def run = {
		var moves_ia = partie.allowed_moves(color)
		Thread.sleep(Current_Config.delai_ia)
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
		var piece_selected = partie.get_piece(oi,oj)
		partie.is_interface = true
		piece_selected.move(destination)
		
	}
}

object IA_promote {
	def promote(posi:(Int,Int),piece:Piece,partie:Partie) = {
		partie.waiting = true
		var random_prom = scala.util.Random
		var random_promInt = random_prom.nextInt(Current_Config.possible_proms.length)
		piece.asInstanceOf[Peon].promo(posi,Current_Config.possible_proms(random_promInt),partie)
		partie.waiting = false
	}
}


class Smart_IA(color:Char,partie:Partie,depth:Int) extends Runnable with Evaluation {
	/**lance le thread du tour de l'ia*/
	override def run = {
		Thread.sleep(Current_Config.delai_ia)
		/**origine et destination de la pièce*/
		var (origin,destination) = choice_dpct(1)
		/**coordonnées de l'origine*/
		var (oi,oj) = origin
		/**coordonnées de la destination*/
		var (di,dj) = destination
		/**id de la pièce de départ*/
		var piece_selected = partie.get_piece(oi,oj)
		
		piece_selected.move(destination)
		partie.is_interface = true
	}

	def choice_dpct(depth:Int) : ((Int,Int),(Int,Int)) = {
		var partie_aux = new Partie()
		partie_aux.matrix = copy_of(partie.matrix)
		val possible_moves = partie.allowed_moves(color)
		var score_max = -100000000
		val alpha = -100000000
		val beta = 100000000
		var move_max = possible_moves(0)
		var maximals_moves : List[((Int,Int),(Int,Int))]= List(move_max)

		for( move <- possible_moves) {
			
			//var partie_aux = new Partie()
			//partie_aux.matrix = copy_of(partie.matrix)
			//partie_aux.matrix_save = copy_of(partie.matrix_save)
			//partie_aux.dplct_save = partie.dplct_save.clone
			//partie_aux.last_important_change = partie.last_important_change

			var (beg,end) = move

			var dpct = new Dpct(beg,end,partie)

			dpct.do_dpct(partie.matrix)
			val score = alphabeta(other_player(color),partie,alpha,beta,depth,false) +avoid_repetition(partie,move)
			dpct.undo_dpct(partie.matrix)

			if (score == score_max) {
				maximals_moves = maximals_moves :+ move
			}
			if (score > score_max) {
				maximals_moves = List(move)
				score_max = score
				move_max = move
			}
			//println("piece : " + dpct.piece.color +" "+ dpct.piece.name + " " + beg + " -> " + end + " score = " + score)
		}
		
		var random_move = scala.util.Random
		var random_moveInt = random_move.nextInt(maximals_moves.length)
		return maximals_moves(random_moveInt)
		//return move_max
	}

	def has_already_been_move(mv : ((Int,Int),(Int,Int))) : Int = {
		if (partie.dplct_save.length <= 1) return 0
		var last_dpct = partie.dplct_save(partie.dplct_save.length-2)
		if (last_dpct.posi_begin == mv._2 ){
			return -30
		}
		else return 0
	}


	def avoid_repetition(partie: Partie, mv : ((Int,Int),(Int,Int))) : Int = {
		var matrix_intermediate = copy_of(partie.matrix_save)
		var nb_repetition = 0
		if ((partie.last_important_change == 0) && (equal(partie.matrix,matrix_intermediate))){
				nb_repetition +=1
			}
		for( i <- partie.last_important_change+1 to partie.dplct_save.length) {
			var dpct = partie.dplct_save(i-1)
			dpct.do_dpct(matrix_intermediate)
			if (equal(partie.matrix,matrix_intermediate)){
				nb_repetition +=1
			}
		}
		if (nb_repetition == 0){
			return 0
		}
		else if (nb_repetition == 1){
			return -1000
		}
		else {
			return -1000000
		}
	}
	/*def choice_dpct(depth:Int) : ((Int,Int),(Int,Int)) = {
		var partie_aux = new Partie()
		var beta = 100000000
		var alpha = -100000000
		
		var (score,mv) = alphabetaMax(color,partie,alpha,beta,depth)
		
		if (mv == List()){
			mv = partie.allowed_moves(color)
		}
		/**objet random*/
		var random_move = scala.util.Random
		/**entier random permettant de choisir un mouvement*/
		var random_moveInt = random_move.nextInt(mv.length)
		//println("le resulat de (2,8).position : "+ partie.matrix(2)(8).position)
		return mv(random_moveInt)
		//return (end,beg)
	}*/
}
