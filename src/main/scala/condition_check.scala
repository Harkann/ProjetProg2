
trait condition_check {
	def incremente_cpt_nb_piece(partie:Partie,piece_met:Piece){
		// prise d'une piece
		if ((piece_met != null) &&  (piece_met.is_promotion)) {
			partie.modif_piece(piece_met.color,piece_met.num_type,-1)
			partie.modif_lost_piece(piece_met.color,0,-1)
		}
		else if (piece_met != null){
			partie.modif_piece(piece_met.color,piece_met.num_type,-1)
			partie.modif_lost_piece(piece_met.color,piece_met.num_type,-1)
		}
	} 

	def roque_check(dpct:Dpct,partie:Partie){

		var (i,j)=dpct.posi_begin
		var (x,y)=dpct.posi_end
		val piece = dpct.piece

		if ((piece != null) && (piece.name == "Ki") && (piece.nb_turn==0) && (y==7)) {
			dpct.optional_other_dpct = new Dpct((i,8),(i,6),partie)
			dpct.is_roque = "O-O"
			val T = partie.matrix(i)(8)
			T.position = (i,6)
			partie.matrix(i)(6) = T
			partie.matrix(i)(8) = null
			T.nb_turn+=1
		}
		if ((piece != null) && (piece.name == "Ki") && (piece.nb_turn==0) && (y==3)) {
			dpct.optional_other_dpct = new Dpct((i,1),(i,4),partie)
			dpct.is_roque = "O-O-O"
			val T = partie.matrix(i)(1)
			T.position=(i,4)
			T.nb_turn+=1
			partie.matrix(i)(4) = partie.matrix(i)(1)
			partie.matrix(i)(1) = null
			
		}
	}

	def promotion_check(dpct:Dpct,partie:Partie){
		var (i,j) = dpct.posi_begin
		var (x,y) = dpct.posi_end
		val piece = partie.matrix(x)(y)
		if ((piece != null) && (piece.name == "Pe") && ((x == 8) || (x == 1))){
			if (partie.nb_ia == 0 || (partie.nb_ia == 1 && partie.player != partie.color_ia)){
				partie.game_window.head_up_bar.notif.promote(dpct.posi_end,piece.color,piece)
				partie.modif_piece(piece.color,0,-1)
			}
			else {
				IA_promote.promote(dpct.posi_end,piece,partie)
			}
			dpct.promotion = piece.PGN_name
			partie.matrix(x)(y).is_promotion = true
		}
	}

	def prise_en_passant_check(dpct:Dpct,partie:Partie){
		val (i,j) = dpct.posi_begin
		val (x,y) = dpct.posi_end
		val piece = dpct.piece
		val piece_met = dpct.piece_met
		if ((piece != null) && (piece.name == "Pe") && (j != y) && (piece_met == null)){
			dpct.optional_other_dpct = new Dpct((0,0),(i,y),partie)
			val color = partie.matrix(i)(y).color
			partie.matrix(i)(y)=null
			partie.modif_piece(color,0,-1)
			partie.modif_lost_piece(color,0,-1)
		}
	}
	
	def nothing_but_pat_check(partie:Partie,tab_color:Array[Int],tab_other_color:Array[Int]) {
		if (
			((tab_color.deep == Array(0,0,0,0,0,1).deep) && (tab_other_color.deep == Array(0,0,0,0,0,1).deep)) ||
			((tab_color.deep == Array(0,0,0,1,0,1).deep) && (tab_other_color.deep == Array(0,0,0,0,0,1).deep)) ||
			((tab_color.deep == Array(0,0,0,1,0,1).deep) && (tab_other_color.deep == Array(0,0,0,1,0,1).deep)) ||		
			((tab_color.deep == Array(0,0,0,0,0,1).deep) && (tab_other_color.deep == Array(0,0,1,0,0,1).deep)) 

			){
			partie.pat("nulle")
		}
	}
}


trait Moves_50 {
	def moves_50_check(partie: Partie) {
		if (partie.nb_turn-partie.last_important_change > 50) {
			partie.pat("50")
		}
	}
}

trait Repetions_3 extends Standard {
	def repetitions_3_check(partie:Partie) {
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
		if(nb_repetition >= 3){
			partie.pat("3")
		}
	}
	
}