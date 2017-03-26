
class Dpct(p_begin:(Int,Int),p_end:(Int,Int),partie:Partie){
	val posi_begin = p_begin
	val posi_end = p_end
	var (i,j) = p_begin
	var (x,y) = p_end
	val piece = partie.matrix(i)(j)
	val piece_met = partie.matrix(x)(y)
	var optional_other_dpct : Dpct = null

	def do_dpct(matrix:Array[Array[Piece]]){
		matrix(i)(j) = null
		matrix(x)(y) = piece
		if (optional_other_dpct != null){
			optional_other_dpct.do_dpct(matrix)
		}
	}

	def undo_dpct(matrix:Array[Array[Piece]]){
		matrix(i)(j) = piece
		matrix(x)(y) = piece_met
		if (optional_other_dpct != null) {
			optional_other_dpct.undo_dpct(matrix)
		}
	}
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

trait Moves_50 {
	def moves_50_check(partie: Partie) {
		if (partie.nb_turn-partie.last_important_change > 50) {
			println("50 coups sans changements")
			partie.pat()
		}
	}
}

trait Repetions_3 extends Standard {
	def repetitions_3_check(partie:Partie) {
		var matrix_intermediate = copy_of(partie.matrix_save)
		var nb_repetition = 0
		if (equal(partie.matrix,matrix_intermediate)){
				nb_repetition +=1
				//println(matrix_intermediate.deep.mkString("\n"))
			}
		for( i <- partie.last_important_change+1 to partie.dplct_save.length) {
			var dpct = partie.dplct_save(i-1)
			dpct.do_dpct(matrix_intermediate)
			if (equal(partie.matrix,matrix_intermediate)){
				nb_repetition +=1
				//println(matrix_intermediate.deep.mkString("\n"))
			}
		}
		if(nb_repetition>=3){
			println("3 répetitions")
			partie.pat()
		}
	}
	
}