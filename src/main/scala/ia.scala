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
		var (origin,destination) = choice_dpct(depth)
		/**coordonnées de l'origine*/
		var (oi,oj) = origin
		/**coordonnées de la destination*/
		var (di,dj) = destination
		/**id de la pièce de départ*/
		var piece_selected = partie.get_piece(oi,oj)
		
		piece_selected.move(destination)
	}

	def choice_dpct(depth:Int) : ((Int,Int),(Int,Int)) = {
		println("choice_dpct")
		var partie_aux = new Partie()
		val possible_moves = partie.allowed_moves(color)
		var score_max = -100000000
		var move_max = possible_moves(0)
		for( move <- possible_moves) {
			/* appliquer le move */
			println("dpclt")
			partie_aux.matrix = copy_of(partie.matrix)
			var (beg,end) = move
			var dcpt = new Dpct(beg,end,partie_aux)
			dcpt.do_dpct(partie_aux.matrix)
			val score = alphabetaMax(color,partie_aux,0,0,depth-1)
			if (score >= score_max) {
				score_max = score
				move_max = move
			}
		}
		return move_max
	}
}