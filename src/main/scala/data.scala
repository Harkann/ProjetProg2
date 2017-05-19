
trait Evaluation extends Values with Squares with Standard {

	def amelioration(color : Char, partie : Partie, alpha :Int, beta :Int) : Int = {
		var var_alpha = alpha
		var var_beta = beta
		val score = evaluation(color,partie)
		//var partie_aux = new Partie()
		//partie_aux.matrix = copy_of(partie.matrix)
		//partie_aux.dplct_save = partie.dplct_save.clone()

		//println("ici is ok")
		val li_danger = partie.list_in_danger_of(other_player(color))
		//println("ici normalement ça coince ")
		for (move <- li_danger){
			var (beg,end) = move
			var dcpt = new Dpct(beg,end,partie)

			dcpt.do_dpct(partie.matrix)
			val value = amelioration(other_player(color),partie,-var_beta,-var_alpha)
			dcpt.undo_dpct(partie.matrix)
			if (value >= var_beta){
				return var_beta
			}
			if (value > var_alpha){
				var_alpha = value
			}
		}
		return var_alpha
	}


	 def alphabeta(color : Char, partie : Partie, alpha : Int, beta : Int , depth : Int, is_max : Boolean,amelio : Boolean) : Int = {
	 	var var_alpha = alpha
		var var_beta = beta
		//var partie_aux = new Partie()
		//partie_aux.matrix = copy_of(partie.matrix)
		//partie_aux.dplct_save = partie.dplct_save.clone()
		var possible_moves = partie.allowed_moves(color)
		if ((depth == 0)||(possible_moves == List())){
			var score_final = evaluation(color,partie)
			
			if (amelio){
				return amelioration(color,partie,alpha,beta)
			}
			else{
				return score_final
			}
		}

		if (is_max) {
			var score = -100000000
			for( move <- possible_moves) {

				var (beg,end) = move
				var dcpt = new Dpct(beg,end,partie)

				dcpt.do_dpct(partie.matrix)
				score = score max alphabeta(other_player(color),partie,var_alpha,var_beta,depth-1,false,amelio)
				dcpt.undo_dpct(partie.matrix)

				var_alpha = var_alpha max score
				if (var_alpha >= var_beta) {
					return score
				}
			}
			return score
		}
		else{
			
			var score = 100000000
			for( move <- possible_moves) {

				var (beg,end) = move
				var dcpt = new Dpct(beg,end,partie)

				dcpt.do_dpct(partie.matrix)
				score =  score min  alphabeta(other_player(color),partie,var_alpha,var_beta,depth-1,true,amelio)
				dcpt.undo_dpct(partie.matrix)

				var_beta = var_beta min score
				if (var_alpha >= var_beta) {
					return score
				}
			}
			return score
		}

	 }


	/*def alphabetaMax(color : Char, partie : Partie, alpha : Int, beta : Int , depth : Int) : (Int,List[((Int,Int),(Int,Int))]) = {
		//println("alphabetaMax profondeur : "+depth)
		if (depth == 0){
			var score_final = evaluation(color,partie)
			println("Min Evaluation finale : "+score_final)
			return (score_final,List())
		}
		println ("___________________________ BEGIN ALPHA BETA MAX " +depth +" ____________________________")
		var var_alpha = alpha
		var var_beta = beta
		var partie_aux = new Partie()
		val possible_moves = partie.allowed_moves(color)
		var move_res : List[((Int,Int),(Int,Int))] = List()
		if (possible_moves == List()){
			println("NO FUCKING MOVE")
			var score_final = evaluation(color,partie)
			println("Min Evaluation finale : "+score_final)
			return (score_final,List())
		}
		for( move <- possible_moves) {
			/* appliquer le move */
			//println("alpha max debut : le resulat de (2,8).position : "+ partie.matrix(2)(8).position)
			partie_aux.matrix = copy_of(partie.matrix)
			partie_aux.dplct_save = partie.dplct_save.clone()
			//println("alpha max : le resulat de (2,8).position : "+ partie.matrix(2)(8).position)
			var (beg,end) = move
			var dcpt = new Dpct(beg,end,partie_aux)
			dcpt.do_dpct(partie_aux.matrix)
			val score = alphabetaMin(other_player(color),partie_aux,var_alpha,var_beta,depth-1)._1
			dcpt.undo_dpct(partie_aux.matrix)//changement ici
			//println("alpha max fin : le resulat de (2,8).position : "+ partie.matrix(2)(8).position)
			if (score >= var_beta) {
				println("MAX score>= var_beta, beta = "+var_beta+" score = "+score)
				return (var_beta,move_res)
			}
			if (score > var_alpha) {
				var_alpha = score
				move_res = move_res :+ move
				println("MAX score>= var_beta, beta = "+var_beta+" score = "+score)
			}
		}
		return (var_alpha,move_res)
	}

	def alphabetaMin(color : Char,partie : Partie,alpha : Int, beta : Int , depth : Int) : (Int,List[((Int,Int),(Int,Int))]) = {
		//println("alphabetaMin pronfondeur : "+depth)
		println("pouet?")
		if (depth == 0){
			var score_final = -evaluation(color,partie)
			println("Min Evaluation finale : "+score_final)
			return (score_final,List())
		}
		println ("___________________________ BEGIN ALPHA BETA MIN " +depth +" ____________________________")
		var var_alpha = alpha
		var var_beta = beta
		var partie_aux = new Partie()
		val possible_moves = partie.allowed_moves(color)
		var move_res : List[((Int,Int),(Int,Int))] = List()
		if (possible_moves == List()){
			println("NO FUCKING MOVE")
			var score_final = - evaluation(color,partie)
			println("Min Evaluation finale : "+score_final)
			return (score_final,List())
		}
		for( move <- possible_moves) {
			/* appliquer le move */
			partie_aux.matrix = copy_of(partie.matrix)
			partie_aux.dplct_save = partie.dplct_save.clone()
			//println(" alpha min : le resulat de (2,8).position : "+ partie.matrix(2)(8).position)
			var (beg,end) = move
			var dcpt = new Dpct(beg,end,partie_aux)
			dcpt.do_dpct(partie_aux.matrix)
			val score = alphabetaMax(other_player(color),partie_aux,var_alpha,var_beta,depth-1)._1
			println("score = "+score)
			dcpt.undo_dpct(partie_aux.matrix)//changement ici
			//println("alpha min fin : le resulat de (2,8).position : "+ partie.matrix(2)(8).position)
			if (score <= var_alpha) {
				println("MIN score<= var_alpha, alpha= "+var_alpha+" score = "+score)
				return (var_alpha,move_res)
			}
			if (score < var_beta) {
				var_beta = score
				move_res = move_res :+ move
				println("MIN score < var_beta, beta = "+var_beta+" score = "+score)
			}
		}
		return (var_beta,move_res)
	}*/

	def other_player(color:Char):Char = {
		if (color == 'W'){
			return 'B'
		}
		else {
			return 'W'
		}
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
		else {
			return ( val_king + square_king_middle(i)(j))
		}

	}

	def borw_matrix(col:Char, i : Int) : Int = {
		if (col == 'W'){
			return i
		}
		else{
			return (7-i)
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

