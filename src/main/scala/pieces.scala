import javax.swing.ImageIcon


/*REPORT PROBLEM Pion qui pense pouvoir prendre ce quil y a directement devant lui......*/





/*
***************************************************************************************************************
______________________________DÉFINITION DE LA CLASSE ABSTRAITE PIECE  _______________________________________

***************************************************************************************************************
*/




//color de type char car la comparaison string char est fausse
/**Superclasse abstraite contenant toutes les pièces,
color : 'W' ou 'B'*/
abstract class Piece(col:Char,var position : (Int,Int),var partie:Partie) extends Standard {
	val color = col;
	/**nom de la pièce*/
	val name:String; 
	/** un numreo attribué a chaque type de piece **/
	val num_type:Int;
	/**statut en vie ou non de la pièce*/
	var is_alive:Boolean;
	/**id de la pièce, l'id "0" désigne une case vide*/
	val id:String;
	val image:ImageIcon; 
	/**renvoie l'id*/
	def get_id() = id
	/**renvoie la liste les positions atteignables par la pièces depuis "position" sans tenir compte du risque d'échec*/
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]);

	/**nombre de déplacements de la pièce*/
	var nb_turn = 0
	/** permet la prise en compte de la nouvelle piece dans les tableaux pieces_W et pieces_B **/
	partie.modif_piece(color,num_type,1)



	/**déplace la pièce vers "posi"*/
	def move(posi:(Int,Int)) = {
		/**coordonnées actuelles de la pièce*/
		var (i,j)=position
		/**coordonnées de la destination*/
		var (x,y)=posi
		val piece = matrix(position,partie)
		val piece_met = matrix(posi,partie)

		// prise d'une piece
		if (piece_met != null) {partie.modif_piece(piece_met.color,piece_met.num_type,-1)}
		roque_check(posi)

		position = (x,y)
		partie.matrix(x)(y)=piece
		partie.matrix(i)(j)=null
		nb_turn+=1
		promotion_check(posi)
		partie.next_turn()
	}

	def roque_check(posi:(Int,Int)){

		var (i,j)=position
		var (x,y)=posi
		val piece = matrix(position,partie)

		if ((piece != null) && (piece.name == "Ki") && (piece.nb_turn==0) && (y==7)) {
			println("on effectut le roque")
			val T = partie.matrix(i)(8)
			T.position = (i,6)
			partie.matrix(i)(6) = T
			partie.matrix(i)(8) = null
			T.nb_turn+=1
			promotion_check(posi)
		}
		if ((piece != null) && (piece.name == "Ki") && (piece.nb_turn==0) && (y==3)) {
			println("on effectut le roque")
			val T = partie.matrix(i)(1)
			T.position=(i,4)
			T.nb_turn+=1
			partie.matrix(i)(4) = partie.matrix(i)(1)
			partie.matrix(i)(1) = null
			println("deplacement de la tour fait normalement...")
		}
	}

	def promotion_check(posi:(Int,Int)){
		var (x,y) = posi
		val piece = matrix(posi,partie)
		println( "check_promotion : " + piece )
		if ((piece != null) && (piece.name == "Pe") && ((x == 8) || (x == 1))){
			piece.asInstanceOf[Peon].promo(posi,partie)
		}
	}

	def full_verif(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		/**déplacements possibles (avec ou sans prise)*/
		var res_moves : List[ (Int,Int) ] = List()
		/**prises possibles*/
		var	res_attacks : List[ (Int,Int) ] = List()
		val (i,j) = position
		val piece=matrix(position,partie)
		var (moves,attacks) = move_piece(position)
			for (mv <-moves) {
				var (x,y)= mv
				var save = matrix(mv,partie)
				partie.matrix(x)(y) = piece
				partie.matrix(i)(j) = null

				if (partie.is_check(piece.color)) {
					partie.matrix(x)(y)=save
					partie.matrix(i)(j)=piece
				}
				else {
					partie.matrix(x)(y)=save
					partie.matrix(i)(j)=piece
					res_moves=res_moves:+mv
					if (attacks.contains(mv)) {res_attacks=res_attacks:+mv}
				}
			}
		return (res_moves,res_attacks)
	}

	/**renvoie la liste des cases atteignables par la pièce située en "position" en tenant compte de la mise en échec*/
	def move_piece_check(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		/**coordonnée de la pièce*/
		var (i,j) = position
		/** id de la pièce sur la case*/
		var piece = matrix((i,j),partie)
		/**pièce sur la case*/
		var id=piece.id
		if (!(piece.is_alive)) {return (List(),List())}
		/**autre joueur*/
		val other=partie.other_player(id(0))
		if (id.substring(1,3)=="Ki") {
			return full_verif(position)
		}
		else {
			var (moves,attacks) = move_piece(position)
			if ((partie.in_danger_of(other).contains(position))||
				partie.is_check(id(0))){
				return full_verif(position)
			}
			else {
				return (moves,attacks)
			}
		}
	}
}






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
			println("prolème de nombre de tour")
			return false
		}
		for ( j <- ((j_K min j_T)+1) to ((j_K max j_T))-1){
			if (partie.matrix(i_K)(j) != null) {
				//println("prolème d'une case non vide")
				return false
			}
		}
		return true
	}
	def roque(pos:(Int,Int),partie:Partie) : List[(Int,Int)] = {
		var (i,j) = pos
		var res : List[(Int,Int)] = List()
		if (roque_line(pos,(i,8),partie)) {
			//println("il y a un roque!")
			res = res:+(i,7)
		}
		if (roque_line(pos,(i,1),partie)) {
			//println("il y a un roque!")
			res = res:+(i,3)
		}
		return res
	}
}



trait Promotion extends Standard {

	def promo(position:(Int,Int),partie:Partie){
		val (i,j) = position
		val piece = matrix(position,partie)
		//val new_type = click_promotion() // qui permettrais de savoir ce que le joueur prefere
		val new_type = "Queen" // TEMPORAIRE
		Projet.partie.modif_piece(piece.color,0,-1)
		if (new_type == "Queen") {
			partie.matrix(i)(j) = new Queen (piece.color, position,partie)
		}
		else if (new_type == "Tower") {
			partie.matrix(i)(j) = new Tower (piece.color, position,partie)
		}
		else if (new_type == "Knight") {
			partie.matrix(i)(j) = new Knight (piece.color, position,partie)
		}
		else {
			partie.matrix(i)(j) = new Bishop (piece.color, position,partie)
		}

	}
}








/*
***************************************************************************************************************
____________________________ DÉFINITION DES TRAITS SPECIFIQUES DE DEPLACEMENT _________________________________

***************************************************************************************************************
*/



/**déplacement des pions*/
trait Peon_move extends Dplct_positions  {
	/**déplacement du pion blanc, avance vers le haut*/
	def dpct_peon_white(position:(Int,Int),partie:Partie) : (List[(Int,Int)],List[(Int,Int)]) = {
		var movement_list : List[(Int,Int)] = List((1,0))
		var (i,j) = position
		val peon=partie.matrix(i)(j)
		var (moves,attacks) = dpct_positions(position,movement_list,partie)
		if ((peon.nb_turn == 0) &&  (moves != List()) ) {
			movement_list = List((1,0),(2,0)) 
		 	var (moves_int,attacks_int) = dpct_positions(position,movement_list,partie)
		 	moves = moves_int
		}

		movement_list = List((1,1),(1,-1))
		var (moves_att,attacks_att) = dpct_pos_attack_only(position,movement_list,partie)
		/*var att_prise_passant = prise_en_passant(position,movement_list)
		moves = moves ++ att_prise_passant
		attacks = attacks ++ att_prise_passant*/
		return (moves++moves_att,attacks++attacks_att)
	}


	/**déplacement du pion blanc, avance vers le bas*/
	def dpct_peon_black(position:(Int,Int),partie:Partie) : (List[(Int,Int)],List[(Int,Int)]) = {
		var movement_list : List[(Int,Int)] = List((-1,0))
		var (i,j) = position
		val peon=partie.matrix(i)(j)
		var (moves,attacks) = dpct_positions(position,movement_list,partie)
		if ((peon.nb_turn == 0) &&  (moves != List()) ) {
			movement_list = List((-1,0),(-2,0)) 
		 	var (moves_int,attacks_int) = dpct_positions(position,movement_list,partie)
		 	moves = moves_int
		 	attacks = attacks_int
		}
		movement_list = List((-1,1),(-1,-1))
		var (moves_att,attacks_att) = dpct_pos_attack_only(position,movement_list,partie)
		/*var att_prise_passant = prise_en_passant(position,movement_list)
		moves = moves ++ att_prise_passant
		attacks = attacks ++ att_prise_passant*/
		return (moves++moves_att,attacks++attacks_att)
	}

	/**déplacement global*/
	def dpct_peon(position:(Int,Int),partie:Partie) : (List[(Int,Int)],List[(Int,Int)])={
		var (i,j) = position
		val peon=partie.matrix(i)(j)
		if (peon.color =='B') {return dpct_peon_black(position,partie)}
		else {return dpct_peon_white(position,partie)}
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




/*
***************************************************************************************************************
______________________________DÉFINITION DES DIFFERENTES CLASSES DE PIECES ___________________________________

***************************************************************************************************************
*/




class Peon(color:Char,pos:(Int,Int),partie:Partie) extends Piece(color,pos,partie)
with Id_creation with Peon_move with Promotion {
	val num_type = 0
	val name="Pe"
	val image = new ImageIcon(getClass.getResource(color+name+".PNG"))
	var is_alive=true
	val id=color+name+id_create(color,name,partie)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		return dpct_peon(position,partie)
	}

	def promotion(position:(Int,Int)) { promo(position,partie) }
	var (i,j) = position
	//Projet.partie.matrix_pieces(i)(j)=id 

}



class Tower(color:Char,pos:(Int,Int),partie:Partie) extends Piece(color,pos,partie) 
with Id_creation with Horizontal_Vertical{
	val name = "To"
	val num_type = 1
	val image = new ImageIcon(getClass.getResource(color+name+".PNG"))
	var is_alive=true
	val id=color+name+id_create(color,name,partie)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		return (dpct_horizon_vertic(position,partie))
	}
	var (i,j) = position
	//Projet.partie.matrix_pieces(i)(j)=id
}

class Knight(color:Char,pos:(Int,Int),partie:Partie) extends Piece(color,pos,partie) 
with Id_creation with Jump{
	val name="Kn"
	val num_type = 2
	val image = new ImageIcon(getClass.getResource(color+name+".PNG"))
	var is_alive=true
	val id=color+name+id_create(color,name,partie)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = jump(position,partie)
	var (i,j) = position
	//Projet.partie.matrix_pieces(i)(j)=id
}

class Bishop(color:Char,position:(Int,Int),partie:Partie) extends Piece(color,position,partie) 
with Id_creation with Diagonal{
	val name="Bi"
	val num_type = 3
	val image = new ImageIcon(getClass.getResource(color+name+".PNG"))
	var is_alive=true
	val id=color+name+id_create(color,name,partie)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		return (dpct_diag(position,partie))
	}
	var (i,j) = position
	//Projet.partie.matrix_pieces(i)(j)=id
}

class Queen(color:Char,pos:(Int,Int),partie:Partie) extends Piece(color,pos,partie) 
with Id_creation with Diagonal with Horizontal_Vertical{ 
	//si jamais on remet "position" et pas un autre nom soit "pos" position est considéré constante
	val name = "Qu"
	val num_type = 4
	val image = new ImageIcon(getClass.getResource(color+name+".PNG"))
	var is_alive= true
	val id=color+name+id_create(color,name,partie)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		var (v_h_moves,v_h_attacks)=dpct_horizon_vertic(position,partie)
		var (diag_moves,diag_attacks)=dpct_diag(position,partie)
		return (v_h_moves++diag_moves,v_h_attacks++diag_attacks)
	}
	var (i,j) = position
	//position normalement libre
	//Projet.partie.matrix(i)(j)=id
}



class King(color:Char,pos:(Int,Int),partie:Partie) extends Piece(color,pos,partie) 
with Id_creation with King_move{
	val name="Ki"
	val num_type = 5
	val image = new ImageIcon(getClass.getResource(color+name+".PNG"))
	var is_alive=true
	val id=color+name+id_create(color,name,partie)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		return dpct_king(position,partie)
	}
	var (i,j) = position
	//Projet.partie.matrix_pieces(i)(j)=id
}
