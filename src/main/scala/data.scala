trait Evaluation extends Values with Squares with Standard {

	def alphabetaMax(color : Char, partie : Partie, alpha : Int, beta : Int , depth : Int) : Int = {
		println("alphabetaMax")
		if (depth == 0){
			return evaluation(color,partie)
		}
		var var_alpha = alpha
		var var_beta = beta
		var partie_aux = new Partie()
		val possible_moves = partie.allowed_moves(color)
		for( move <- possible_moves) {
			/* appliquer le move */
			partie_aux.matrix = copy_of(partie.matrix)
			partie_aux.dplct_save = partie.dplct_save.clone()
			var (beg,end) = move
			var dcpt = new Dpct(beg,end,partie_aux)
			dcpt.do_dpct(partie_aux.matrix)
			 val score = alphabetaMin(color,partie_aux,var_alpha,var_beta,depth-1)
			if (score >= var_beta) {
				return var_beta
			}
			if (score > var_alpha) {
				var_alpha = score
			}
		}
		return var_alpha
	}

	def alphabetaMin(color : Char,partie : Partie,alpha : Int, beta : Int , depth : Int) : Int = {
		println("alphabetaMin")
		if (depth == 0){
			return -evaluation(color,partie)
		}
		var var_alpha = alpha
		var var_beta = beta
		var partie_aux = new Partie()
		val possible_moves = partie.allowed_moves(color)
		for( move <- possible_moves) {
			/* appliquer le move */
			partie_aux.matrix = copy_of(partie.matrix)
			partie_aux.dplct_save = partie.dplct_save.clone()
			var (beg,end) = move
			var dcpt = new Dpct(beg,end,partie_aux)
			dcpt.do_dpct(partie_aux.matrix)
			val score = alphabetaMax(color,partie_aux,var_alpha,var_beta,depth-1)
			if (score <= var_alpha) {
				return var_alpha
			}
			if (score < var_beta) {
				var_beta = score
			}
		}
		return var_beta
	}

	def evaluation(color:Char,partie : Partie) : Int = {
		var score_player = 0
		var score_other = 0
		for( i <- 1 to 8) {
			for( j <- 1 to 8){
				var piece= partie.matrix(i)(j)
				if (piece != null){
					if (piece.color == color){
						score_player += value(piece)
					}
					else{
						score_other += value(piece)
					}
				}
			}
		}
		return score_player-score_other
	}

	def value(piece :Piece):Int = {
		val (x,y) = piece.position
		val i = i_color(x,piece.color)
		val j = j_color(y,piece.color)
		if (piece.name == "Pe"){
			return (val_peon+square_peon(i)(j))
		}
		if (piece.name == "Kn"){
			return (val_knigth+square_knigt(i)(j))
		}
		if (piece.name == "Bi"){
			return (val_bishop + square_bishop(i)(j))
		}
		if (piece.name == "To"){
			return (val_tower + square_tower(i)(j))			
		}
		if (piece.name == "Qu") {
			return ( val_queen + square_queen(i)(j))
		}
		if (piece.name == "Kn") {
			return ( val_king + square_king_middle(i)(j))
		}
		else{
			return 0
		}

	}


	def i_color(x : Int,color : Char) : Int = {
		if (color == 'W'){
			return x-1
		}
		else {
			return 8-x
		}
	}

	def j_color(y : Int,color : Char) : Int = {
		return y-1
	}
}

trait Values{
	val val_peon = 100
	val val_knigth = 320
	val val_bishop = 330
	val val_tower = 500
	val val_queen = 900
	val val_king = 20000
}


trait Squares{

val square_peon = Array (
 Array(0,  0,  0,  0,  0,  0,  0,  0),
 Array(50, 50, 50, 50, 50, 50, 50, 50),
 Array(10, 10, 20, 30, 30, 20, 10, 10),
 Array(5,  5, 10, 25, 25, 10,  5,  5),
 Array(0,  0,  0, 20, 20,  0,  0,  0),
 Array(5, -5,-10,  0,  0,-10, -5,  5),
 Array(5, 10, 10,-20,-20, 10, 10,  5),
 Array(0,  0,  0,  0,  0,  0,  0,  0)
)


val square_knigt = Array(
Array(-50,-40,-30,-30,-30,-30,-40,-50),
Array(-40,-20,  0,  0,  0,  0,-20,-40),
Array(-30,  0, 10, 15, 15, 10,  0,-30),
Array(-30,  5, 15, 20, 20, 15,  5,-30),
Array(-30,  0, 15, 20, 20, 15,  0,-30),
Array(-30,  5, 10, 15, 15, 10,  5,-30),
Array(-40,-20,  0,  5,  5,  0,-20,-40),
Array(-50,-40,-30,-30,-30,-30,-40,-50)
)

val square_bishop = Array(
Array(-20,-10,-10,-10,-10,-10,-10,-20),
Array(-10,  0,  0,  0,  0,  0,  0,-10),
Array(-10,  0,  5, 10, 10,  5,  0,-10),
Array(-10,  5,  5, 10, 10,  5,  5,-10),
Array(-10,  0, 10, 10, 10, 10,  0,-10),
Array(-10, 10, 10, 10, 10, 10, 10,-10),
Array(-10,  5,  0,  0,  0,  0,  5,-10),
Array(-20,-10,-10,-10,-10,-10,-10,-20) )

val square_tower = Array(
Array(0,  0,  0,  0,  0,  0,  0,  0),
Array(5, 10, 10, 10, 10, 10, 10,  5),
Array(-5,  0,  0,  0,  0,  0,  0, -5),
Array(-5,  0,  0,  0,  0,  0,  0, -5),
Array(-5,  0,  0,  0,  0,  0,  0, -5),
Array(-5,  0,  0,  0,  0,  0,  0, -5),
Array(-5,  0,  0,  0,  0,  0,  0, -5),
Array(0,  0,  0,  5,  5,  0,  0,  0)
)

val square_queen = Array(
Array(-20,-10,-10, -5, -5,-10,-10,-20),
Array(-10,  0,  0,  0,  0,  0,  0,-10),
Array(-10,  0,  5,  5,  5,  5,  0,-10),
Array( -5,  0,  5,  5,  5,  5,  0, -5),
Array(  0,  0,  5,  5,  5,  5,  0, -5),
Array(-10,  5,  5,  5,  5,  5,  0,-10),
Array(-10,  0,  5,  0,  0,  0,  0,-10),
Array(-20,-10,-10, -5, -5,-10,-10,-20)
)

val square_king_middle =  Array(
Array(-30,-40,-40,-50,-50,-40,-40,-30),
Array(-30,-40,-40,-50,-50,-40,-40,-30),
Array(-30,-40,-40,-50,-50,-40,-40,-30),
Array(-30,-40,-40,-50,-50,-40,-40,-30),
Array(-20,-30,-30,-40,-40,-30,-30,-20),
Array(-10,-20,-20,-20,-20,-20,-20,-10),
Array(20, 20,  0,  0,  0,  0, 20, 20),
Array(20, 30, 10,  0,  0, 10, 30, 20) 
)

val square_king_end = Array(
Array(-50,-40,-30,-20,-20,-30,-40,-50),
Array(-30,-20,-10,  0,  0,-10,-20,-30),
Array(-30,-10, 20, 30, 30, 20,-10,-30),
Array(-30,-10, 30, 40, 40, 30,-10,-30),
Array(-30,-10, 30, 40, 40, 30,-10,-30),
Array(-30,-10, 20, 30, 30, 20,-10,-30),
Array(-30,-30,  0,  0,  0,  0,-30,-30),
Array(-50,-30,-30,-30,-30,-30,-30,-50)
)


}

