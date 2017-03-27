
trait condition_check {

	def roque_check(dpct:Dpct,partie:Partie){

		var (i,j)=dpct.posi_begin
		var (x,y)=dpct.posi_end
		val piece = dpct.piece

		if ((piece != null) && (piece.name == "Ki") && (piece.nb_turn==0) && (y==7)) {
			dpct.optional_other_dpct = new Dpct((i,8),(i,6),partie)
			val T = partie.matrix(i)(8)
			T.position = (i,6)
			partie.matrix(i)(6) = T
			partie.matrix(i)(8) = null
			T.nb_turn+=1
		}
		if ((piece != null) && (piece.name == "Ki") && (piece.nb_turn==0) && (y==3)) {
			dpct.optional_other_dpct = new Dpct((i,8),(i,4),partie)
			val T = partie.matrix(i)(1)
			T.position=(i,4)
			T.nb_turn+=1
			partie.matrix(i)(4) = partie.matrix(i)(1)
			partie.matrix(i)(1) = null
			
		}
	}

	def promotion_check(dpct:Dpct,partie:Partie){
		//println("pr check")
		var (i,j) = dpct.posi_begin
		var (x,y) = dpct.posi_end
		val piece = partie.matrix(x)(y)
		if ((piece != null) && (piece.name == "Pe") && ((x == 8) || (x == 1))){
			if (partie.nb_ia == 0 || (partie.nb_ia == 1 && partie.player != partie.color_ia)){
				partie.game_window.head_up_bar.notif.promote(dpct.posi_end,piece.color,piece)
			}
			else {
				IA_promote.promote(dpct.posi_end,piece,partie)
			}
		}
	}

	def prise_en_passant_check(dpct:Dpct,partie:Partie){
		val (i,j) = dpct.posi_begin
		val (x,y) = dpct.posi_end
		val piece = dpct.piece
		val piece_met = dpct.piece_met
		if ((piece != null) && (piece.name == "Pe") && (j != y) && (piece_met == null)){
			dpct.optional_other_dpct = new Dpct((0,0),(i,y),partie)
			partie.matrix(i)(y)=null
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