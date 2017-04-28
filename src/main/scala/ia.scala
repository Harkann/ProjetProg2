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

class Smart_IA(color:Char,partie:Partie) extends Runnable{
	/**lance le thread du tour de l'ia*/
	override def run = {
		var moves_ia = partie.allowed_moves(color)
		Thread.sleep(Current_Config.delai_ia)
		
		/* TO DO */


		/**origine et destination de la pièce*/
		var (origin,destination) = moves_ia(0) /* TO DO */
		/**coordonnées de l'origine*/
		var (oi,oj) = origin
		/**coordonnées de la destination*/
		var (di,dj) = destination
		/**id de la pièce de départ*/
		var piece_selected = partie.get_piece(oi,oj)
		
		piece_selected.move(destination)
	}
	/**
	def alphabetaMax(alpha : Int, beta : Int , depth : Int) : Int = {
		if (depth == 0){
			return evaluation()
		}
		var var_alpha = alpha
		var ver_beta = beta
		val possible_moves = partie.allowed_moves(color)
		for( move <- possible_moves) {
			val score = alphabetaMin(var_alpha,var_beta,depth-1)
			if (score >= var_beta) {
				return var_beta
			}
			if (score > var_alpha) {
				var_alpha = score
			}
		}
		return var_alpha
	}

	def alphabetaMin(alpha : Int, beta : Int , depth : Int) : Int = {
		if (depth == 0){
			return -evaluation()
		}
		var var_alpha = alpha
		var ver_beta = beta
		val possible_moves = partie.allowed_moves(color)
		for( move <- possible_moves) {
			/*appliquer le move*/
			val score = alphabetaMax(var_alpha,var_beta,depth-1)
			if (score <= var_alpha) {
				return var_alpha
			}
			if (score < var_beta) {
				var_beta = score
			}
			/* restaurer la sauvegarde */
		}
		return var_beta
	}

	def evaluation() : Int = {
		var score = 0
		for( i <- 1 to 8) {

		}
		return 0
	}
	*/
}
