import Array._


/*
***************************************************************************************************************
____________________________ DÉFINITION DES TRAITS GENERAUX DE DEPLACEMENT ___________________________________

***************************************************************************************************************
*/


trait Standard {
	def matrix(position:(Int,Int),partie:Partie) : Piece = {
		var (i,j) = position
		return partie.matrix(i)(j)
		}

	def copy_of(matrix : Array[Array[Piece]]) : Array[Array[Piece]] = {
		var res = ofDim[Piece](9,9)
		for( i <- 1 to 8) {
			for( j <- 1 to 8) {
				res(i)(j)=matrix(i)(j)
			}	
		}
		return res
	}
	def equal (matrix1 : Array[Array[Piece]],matrix2 :Array[Array[Piece]]) : Boolean = {
		for(i<- 1 to 8){
			for(j<- 1 to 8){
				val piece1= matrix1(i)(j)
				val piece2=matrix2(i)(j)
				if (((piece1 == null) && (piece2 != null)) || ((piece1 != null) && (piece2 == null))){
					return false
				}
				else if ((piece1 == null) && (piece2 == null)){}
				else if ((piece1.name != piece2.name) || (piece1.color != piece2.color)){
					return false
				}	
			}
		}
		return true
	}
}


trait Id_creation extends Standard {
	/**crée un Id*/
	def id_create(color:Char,name:String,partie:Partie) : Int = {
		var ind=0
		for( i <- 1 to 8) {
			for( j <- 1 to 8) {
				var piece_ij = matrix((i,j),partie)
				if ((piece_ij != null) && (piece_ij.color ==color) )
				{ if (piece_ij.id.substring(1,3)==name) {ind+=1}} 
			}
		}
		return ind
	}	
}


/**definition des deplacements plus générale pour eviter la redondance de code*/
 trait Dplct_directions extends Standard{
	def dpct_direction (position:(Int,Int),direction:(Int,Int),partie:Partie) : (List[(Int,Int)],List[(Int,Int)]) = {
		var (i,j) = position
		var (a,b) = direction //on se deplace selon cette direction
		val piece= matrix(position,partie)
		i = i + a
		j = j + b
		var res : List[ (Int,Int) ] = List()
		var attack_list: List[ (Int,Int) ] = List()
		while 
		//tant qu'on est dans l'échequier et qu'on a pas croisé de pièce
			((1<=i) && (i<=8) &&
			(1<=j) && (j<=8) &&
			(partie.matrix(i)(j)==null)) 
		{
			res=res:+(i,j)
			//on se déplace selon le vecteur (a,b)
			i=i+a
			j=j+b
		}
		if 	// à t'on croisé une pièce si oui, peut on la prendre?
			((1<=i) && (i<=8) && 
			(1<=j) && (j<=8) ){
				val piece_met = matrix((i,j),partie)
				if (piece.color != piece_met.color){
					res=res:+(i,j);attack_list=attack_list:+(i,j)
				}
			}
		return (res,attack_list)

	} 

	/** fonction qui map sur une liste la premiere fonction dpct_direction **/
	def dpct_direction_list(position:(Int,Int),direction_list:List[(Int,Int)],partie:Partie) : (List[(Int,Int)],List[(Int,Int)]) = {
		var res : List[ (Int,Int) ] = List()
		var attack_list: List[ (Int,Int) ] = List()
		for( direction <- direction_list) {
			var (intermediare_move,intermediare_attacks) = dpct_direction(position,direction,partie)
			res = res ++ intermediare_move
			attack_list = attack_list ++ intermediare_attacks
		}
		return (res,attack_list)
	}
}


/** donne les déplacements et les attaques possible a partir d'une liste de position relatives ou l'on peut aller **/
trait Dplct_positions extends Standard {

	def dpct_positions(position:(Int,Int),movement_list:List[(Int,Int)],partie:Partie) : (List[(Int,Int)],List[(Int,Int)]) = {
		var (i,j) = position 
		var attack_list: List[ (Int,Int) ] = List()
		val piece= matrix(position,partie)
		var res : List[ (Int,Int) ] = List()
		for( dplct <- movement_list) {
			var (x,y) = dplct
			if ( (i+x >=1) && (i+x <=8) && (j+y <=8) && (j+y >=1) )
			{
				var piece_met = matrix((i+x,j+y),partie)
				if (piece_met == null)  
					{res=res:+(i+x,j+y)}
				else if (piece_met.color != piece.color )
					{res=res:+(i+x,j+y);attack_list=attack_list:+(i+x,j+y)}
			}
		}
		return (res,attack_list)
	}

	/** Ici on ne permet de considerer le mouvement que si c'est une attaque  **/
	def dpct_pos_attack_only(position:(Int,Int),movement_list:List[(Int,Int)],partie:Partie) : (List[(Int,Int)],List[(Int,Int)]) = {
		var (i,j) = position 
		var attack_list: List[ (Int,Int) ] = List()
		val piece= matrix((i,j),partie)
		var res : List[ (Int,Int) ] = List()
		for( dplct <- movement_list) {
			var (x,y) = dplct
			if ( (i+x >=1) && (i+x <=8) && (j+y <=8) && (j+y >=1) )
			{
				var piece_met = matrix((i+x,j+y),partie)
				if ((piece_met != null) && (piece_met.color != piece.color ))
					{res=res:+(i+x,j+y);attack_list=attack_list:+(i+x,j+y)}
			}
		}
		return (res,attack_list)
	}

	def dpct_pos_dpct_only(position:(Int,Int),movement_list:List[(Int,Int)],partie:Partie) : (List[(Int,Int)],List[(Int,Int)]) = {
		var (i,j) = position 
		var attack_list: List[ (Int,Int) ] = List()
		val piece= matrix((i,j),partie)
		var res : List[ (Int,Int) ] = List()
		for( dplct <- movement_list) {
			var (x,y) = dplct
			if ( (i+x >=1) && (i+x <=8) && (j+y <=8) && (j+y >=1) )
			{
				var piece_met = matrix((i+x,j+y),partie)
				if (piece_met == null)  
					{res=res:+(i+x,j+y)}
			}
		}
		return (res,attack_list)
	}
}

/**déplacement diagonal (fous)*/
trait Diagonal extends Dplct_directions {
	def dpct_diag(position:(Int,Int),partie:Partie) : (List[(Int,Int)],List[(Int,Int)]) = {
		var direction_list : List[(Int,Int)] = List((1,1),(-1,-1),(1,-1),(-1,1))
		return dpct_direction_list(position,direction_list,partie)	}
}

/**déplacement horizontal et vertical (tours et reines)*/
trait Horizontal_Vertical extends Dplct_directions {
	def dpct_horizon_vertic(position:(Int,Int),partie:Partie) : (List[(Int,Int)],List[(Int,Int)]) = {
		var direction_list : List[(Int,Int)] = List((0,1),(0,-1),(1,0),(-1,0))
		return dpct_direction_list(position,direction_list,partie)	}
}

/**déplacement des cavaliers*/
trait Jump extends Dplct_positions {
	def jump(position:(Int,Int),partie:Partie) : (List[(Int,Int)],List[(Int,Int)]) = {
		/**liste des déplacements possibles relatifs à la position initiale*/
		val movement_list : List[(Int,Int)] = List((1,2),(-1,2),(2,1),(2,-1),(-2,-1),(-2,1),(1,-2),(-1,-2)) 
		return (dpct_positions(position,movement_list,partie))
	}
}



trait Roque extends Standard {
	def roque_line(pos_K:(Int,Int),pos_T:(Int,Int),partie:Partie) : Boolean = {
		var (i_K,j_K) = pos_K
		val K = matrix(pos_K,partie)
		var (i_T,j_T) = pos_T
		val T = matrix(pos_T,partie)
		if (T == null) return false
		if ((K.nb_turn != 0) || (T.nb_turn != 0)) {
			return false
		}
		for ( j <- ((j_K min j_T)+1) to ((j_K max j_T))-1){
			if (partie.matrix(i_K)(j) != null) {
				return false
			}
		}
		return true
	}
	def roque(pos:(Int,Int),partie:Partie) : List[(Int,Int)] = {
		var (i,j) = pos
		var res : List[(Int,Int)] = List()
		if (roque_line(pos,(i,8),partie)) {
			res = res:+(i,7)
		}
		if (roque_line(pos,(i,1),partie)) {
			res = res:+(i,3)
		}
		return res
	}
}



trait Promotion extends Standard {

	def promo(position:(Int,Int),new_type:String,partie:Partie){
		val (i,j) = position
		val piece = matrix(position,partie)
		//val new_type = "Queen" // TEMPORAIRE
		partie.modif_piece(piece.color,0,-1)
		if ((new_type == "Qu") || (new_type == "Q")) {
			partie.matrix(i)(j) = new Queen (piece.color, position,partie)
		}
		else if ((new_type == "To")||(new_type == "R")) {
			partie.matrix(i)(j) = new Tower (piece.color, position,partie)
		}
		else if ((new_type == "Kn")||(new_type == "N")) {
			partie.matrix(i)(j) = new Knight (piece.color, position,partie)
		}
		else {
			partie.matrix(i)(j) = new Bishop (piece.color, position,partie)
		}
		partie.matrix(i)(j).is_promotion = true
		if (partie.nb_turn < partie.dplct_save.length){
		partie.dplct_save(partie.nb_turn).promotion = partie.matrix(i)(j).PGN_name}
		partie.game_window.plateau.reset_all()

	}
}

trait Prise_en_passant {
	def prise_en_passant(position:(Int,Int),partie:Partie,c:Int) : List[(Int,Int)] = {
		if (partie.dplct_save.length == 0) {return List ()}
		val dpct = partie.last_move(partie)
		if (dpct != null){
		val (i,j)=position
		if ((Math.abs(dpct.j-j) == 1) &&
			(dpct.x == i) && (dpct.i == i+c*2) &&
			(dpct.piece.color != partie.matrix(i)(j).color)){
			return List((i+c,dpct.j))
		}
		else{
			return List()
		}
		}
		else{
			return List()
		}

	}
}



/*
***************************************************************************************************************
____________________________ DÉFINITION DES TRAITS SPECIFIQUES DE DEPLACEMENT _________________________________

***************************************************************************************************************
*/



/**déplacement des pions*/
trait Peon_move extends Dplct_positions with Prise_en_passant {
	/**déplacement du pion blanc, avance vers le haut*/
	def dpct_peon_w_or_b(position:(Int,Int),partie:Partie, c:Int) : (List[(Int,Int)],List[(Int,Int)]) = {
		var movement_list : List[(Int,Int)] = List((1*c,0))

		var (i,j) = position
		val peon=partie.matrix(i)(j)
		var (moves,attacks) = dpct_pos_dpct_only(position,movement_list,partie)
		if ((peon.nb_turn == 0) &&  (moves != List()) ) {
			movement_list = List((1*c,0),(2*c,0)) 
		 	var (moves_int,attacks_int) = dpct_pos_dpct_only(position,movement_list,partie)
		 	moves = moves_int
		}

		movement_list = List((1*c,1),(1*c,-1))
		var (moves_att,attacks_att) = dpct_pos_attack_only(position,movement_list,partie)
		var prise_passant = prise_en_passant(position,partie,c)
		moves = moves ++ prise_passant
		attacks = attacks ++ prise_passant
		return (moves++moves_att,attacks++attacks_att)
	}


	/**déplacement global*/
	def dpct_peon(position:(Int,Int),partie:Partie) : (List[(Int,Int)],List[(Int,Int)])={
		var (i,j) = position
		val peon=partie.matrix(i)(j)
		if (peon.color =='B') {return dpct_peon_w_or_b(position,partie,-1)}
		else {return dpct_peon_w_or_b(position,partie,1)}
	}

}

/**déplacement du roi*/
trait King_move extends Dplct_positions with Roque {
	def dpct_king(position:(Int,Int),partie:Partie) : (List[(Int,Int)],List[(Int,Int)]) = { //déplacemnt du roi
		val movement_list : List[(Int,Int)] = List((1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1))
		var (mv,att) = dpct_positions(position,movement_list,partie)
		return (mv ++ roque(position,partie),att)
	}
}
