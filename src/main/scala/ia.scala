/**permet de lancer l'ia sous forme de thread*/
class IA(color:Char,partie:Partie) extends Runnable{
	/**lance le thread du tour de l'ia*/
	override def run = {
		var moves_ia = partie.allowed_moves(color)
		Thread.sleep(Config.delai_ia)
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
		var random_promInt = random_prom.nextInt(Config.possible_proms.length)
		piece.asInstanceOf[Peon].promo(posi,Config.possible_proms(random_promInt),partie)
		partie.waiting = false
	}
}