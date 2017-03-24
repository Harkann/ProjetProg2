
class Dpct(p_begin:(Int,Int),p_end:(Int,Int),partie:Partie){
	val posi_begin = p_begin
	val posi_end = p_end
	var (i,j) = p_begin
	var (x,y) = p_end
	val piece = partie.matrix(i)(j)
	val piece_met = partie.matrix(x)(y)
}

trait Save {
	def return_back(partie:Partie) = {
	val dpct = partie.dplct_save.remove(partie.nb_turn)
	partie.matrix(dpct.i)(dpct.j)= dpct.piece
	partie.matrix(dpct.x)(dpct.y)= dpct.piece_met
	partie.nb_turn -=1
	}

	def last_move(partie:Partie) : Dpct = {
		//println("longueur : "+partie.dplct_save.length)
		return partie.dplct_save(partie.nb_turn-1)
	}
	
}