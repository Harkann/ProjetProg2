
class Dpct(p_begin:(Int,Int),p_end:(Int,Int),partie:Partie){
	val posi_begin = p_begin
	val posi_end = p_end
	var (i,j) = p_begin
	var (x,y) = p_end
	val piece = partie.matrix(i)(j)
	val piece_met = partie.matrix(x)(y)
}

trait Return {
	def return_back(partie:Partie) = {
		val dpct = partie.dplct_save.remove(partie.nb_turn)
	}
	
}