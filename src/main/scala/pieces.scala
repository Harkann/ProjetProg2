//pas besoin d'inclure les trucs même depuis des fichiers différents.

//MATRIX_PIECE A SUPPRIMER ET REECRIRE DE CE FICHIER !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

//color de type char car la comparaison string char est fausse
/**Superclasse abstraite contenant toutes les pièces,
color : 'W' ou 'B'*/
abstract class Piece(col:Char,var position : (Int,Int)) {
	val color = col;
	/**nom de la pièce*/
	val name:String; 
	/**statut en vie ou non de la pièce*/
	var is_alive:Boolean;
	/**id de la pièce, l'id "0" désigne une case vide*/
	val id:String;
	/**renvoie l'id*/
	def get_id() = id
	/**renvoie la liste les positions atteignables par la pièces depuis "position" sans tenir compte du risque d'échec*/
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]);

	/**nombre de déplacements de la pièce*/
	var nb_turn = 0
	/**prend la pièce à la position posi*/
	def delete(posi:(Int,Int)) = {
		/**coordonnées de la pièce*/
		var(i,j)=position
		/**coordonnées de la pièce prise*/
		var (x,y)=posi
		/**id de la pièce prise*/
		var piece_deleted=Projet.partie.matrix(x)(y)
		/**pièce supprimée*/
		piece_deleted.is_alive=false
		move(posi)
	}
	/**déplace la pièce vers "posi"*/
	def move(posi:(Int,Int)) = {
		/**coordonnées actuelles de la pièce*/
		var (i,j)=position
		position=posi
		/**coordonnées de la destination*/
		var (x,y)=posi
		Projet.partie.matrix(x)(y)=Projet.partie.matrix(i)(j)
		Projet.partie.matrix(i)(j)=null
		
		nb_turn+=1
	}

	def matrix(position:(Int,Int)) : Piece = {
		var (i,j) = position
		return Projet.partie.matrix(i)(j)
	}

	def full_verif(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		/**déplacements possibles (avec ou sans prise)*/
		var res_moves : List[ (Int,Int) ] = List()
		/**prises possibles*/
		var	res_attacks : List[ (Int,Int) ] = List()
		val (i,j) = position
		val piece=matrix(position)
		var (moves,attacks) = move_piece(position)
			for (mv <-moves) {
				var (x,y)= mv
				var save = matrix(mv)
				Projet.partie.matrix(x)(y) = piece
				Projet.partie.matrix(i)(j) = null

				if (Projet.partie.is_check(piece.color)) {
					Projet.partie.matrix(x)(y)=save
					Projet.partie.matrix(i)(j)=piece
				}
				else {
					Projet.partie.matrix(x)(y)=save
					Projet.partie.matrix(i)(j)=piece
					res_moves=res_moves:+mv}
			}
			for (at <-attacks) {
				var (x,y)= at
				var save = Projet.partie.matrix(x)(y)
				Projet.partie.matrix(x)(y)=piece
				Projet.partie.matrix(i)(j)=null

				if (Projet.partie.is_check(id(0))) {
					Projet.partie.matrix(x)(y)=save
					Projet.partie.matrix(i)(j)=piece
				}
				else {
					Projet.partie.matrix(x)(y)=save
					Projet.partie.matrix(i)(j)=piece
					res_attacks=res_attacks:+at}
			}
		return (res_moves,res_attacks)
	}

	/**renvoie la liste des cases atteignables par la pièce située en "position" en tenant compte de la mise en échec*/
	def move_piece_check(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		/**coordonnée de la pièce*/
		var (i,j)=position
		/** id de la pièce sur la case*/
		var piece=Projet.partie.matrix(i)(j)
		/**pièce sur la case*/
		var id=piece.id
		if (!(piece.is_alive)) {return (List(),List())}
		/**autre joueur*/
		val other=Projet.partie.other_player(id(0))
		if (id.substring(1,3)=="Ki") {
			return full_verif(position)
		}
		else {
			var (moves,attacks) = move_piece(position)
			if ((Projet.partie.in_danger_of(other).contains(position))||
				Projet.partie.is_check(id(0))){
				return full_verif(position)
			}
			else {
				return (moves,attacks)
			}
		}
	}
}




/**definition des deplacements plus générale pour eviter la redondance de code*/
 trait Dplct_directions {
	def dpct_direction (position:(Int,Int),direction:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		var (i,j) = position
		var (a,b) = direction //on se deplace selon cette direction
		val piece= Projet.partie.matrix(i)(j)
		i = i + a
		j = j + b
		var res : List[ (Int,Int) ] = List()
		var attack_list: List[ (Int,Int) ] = List()
		while 
		//tant qu'on est dans l'échequier et qu'on a pas croisé de pièce
			((1<=i) && (i<=8) &&
			(1<=j) && (j<=8) &&
			(Projet.partie.matrix(i)(j)==null)) 
		{
			res=res:+(i,j)
			//on se déplace selon le vecteur (a,b)
			i=i+a
			j=j+b
		}
		val piece_met = Projet.partie.matrix(i)(j)
		if 	// à t'on croisé une pièce si oui, peut on la prendre?
			((1<=i) && (i<=8) && 
			(1<=j) && (j<=8) && 
			(piece.color != piece_met.color)) 
			{res=res:+(i,j);attack_list=attack_list:+(i,j)}
		return (res,attack_list)

	} 
	def dpct_direction_list(position:(Int,Int),direction_list:List[(Int,Int)]) : (List[(Int,Int)],List[(Int,Int)]) = {
		var res : List[ (Int,Int) ] = List()
		var attack_list: List[ (Int,Int) ] = List()
		for( direction <- direction_list) {
			var (intermediare_move,intermediare_attacks) = dpct_direction(position,direction)
			res = res ++ intermediare_move
			attack_list = attack_list ++ intermediare_attacks
		}
		return (res,attack_list)
	}
}

trait Dplct_positions{
	def dpct_positions(position:(Int,Int),movement_list:List[(Int,Int)]) : (List[(Int,Int)],List[(Int,Int)]) = {
		var (i,j) = position 
		var attack_list: List[ (Int,Int) ] = List()
		val piece= Projet.partie.matrix(i)(j)
		var res : List[ (Int,Int) ] = List()
		for( dplct <- movement_list) {
			var (x,y) = dplct
			if ( (i+x >=1) && (i+x <=8) && (j+y <=8) && (j+y >=1) )
			{
				var piece_met = Projet.partie.matrix(i+x)(j+y)
				if (piece_met == null)  
					{res=res:+(i+x,j+y)}
				else if (piece_met.color == piece.color )
					{res=res:+(i+x,j+y);attack_list=attack_list:+(i+x,j+y)}
			}
		}
		return (res,attack_list)
	}
}

trait Passing_take{
	def prise_en_passant(position:(Int,Int),movement_list:List[(Int,Int)]) : List[(Int,Int)] = {
		var (i,j) = position 
		var res : List[ (Int,Int) ] = List()
		val piece = Projet.partie.matrix(i)(j)
		for (dplct <- movement_list) {
			var (x,y) = dplct
			if ( (i+x >=1) && (i+x <=8) && (j+y <=8) && (j+y >=1) )
			{
				var piece_met = Projet.partie.matrix(i+x)(j+y)
				if (piece_met == null)  
					{}
				else if (piece_met.color == piece.color )
					{res=res:+(i+x,j+y)}
			}
		}
		return res
	}
}

/**déplacement diagonal (fous)*/
trait Diagonal extends Dplct_directions {
	def dpct_diag(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		var direction_list : List[(Int,Int)] = List((1,1),(-1,-1),(1,-1),(-1,1))
		return dpct_direction_list(position,direction_list)	}
}

/**déplacement horizontal et vertical (tours et reines)*/
trait Horizontal_Vertical extends Dplct_directions {
	def dpct_horizon_vertic(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		var direction_list : List[(Int,Int)] = List((0,1),(0,-1),(1,0),(-1,0))
		return dpct_direction_list(position,direction_list)	}
}

/**déplacement des cavaliers*/
trait Jump extends Dplct_positions {
	def jump(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		/**liste des déplacements possibles relatifs à la position initiale*/
		val movement_list : List[(Int,Int)] = List((1,2),(-1,2),(2,1),(2,-1),(-2,-1),(-2,1),(1,-2),(-1,-2)) 
		return (dpct_positions(position,movement_list))
	}
}


/**déplacement des pions*/
trait Peon_move extends Dplct_positions with Passing_take {
	/**déplacement du pion blanc, avance vers le haut*/
	def dpct_peon_white(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		var movement_list : List[(Int,Int)] = List((1,0))
		var (i,j) = position
		val peon=Projet.partie.matrix(i)(j)
		var (moves,attacks) = dpct_positions(position,movement_list)
		if ((peon.nb_turn == 0) &&  (moves != List()) ) {
			movement_list = List((1,0),(2,0)) 
		 	var (moves_int,attacks_int) = dpct_positions(position,movement_list)
		 	moves = moves_int
		 	attacks = attacks_int
		}
		movement_list = List((1,1),(1,-1))
		var att_prise_passant = prise_en_passant(position,movement_list)
		moves = moves ++ att_prise_passant
		attacks = attacks ++ att_prise_passant
		return (moves,attacks)
	}


	/**déplacement du pion blanc, avance vers le bas*/
	def dpct_peon_black(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		var movement_list : List[(Int,Int)] = List((-1,0))
		var (i,j) = position
		val peon=Projet.partie.matrix(i)(j)
		var (moves,attacks) = dpct_positions(position,movement_list)
		if ((peon.nb_turn == 0) &&  (moves != List()) ) {
			movement_list = List((-1,0),(-2,0)) 
		 	var (moves_int,attacks_int) = dpct_positions(position,movement_list)
		 	moves = moves_int
		 	attacks = attacks_int
		}
		movement_list = List((-1,1),(-1,-1))
		var att_prise_passant = prise_en_passant(position,movement_list)
		moves = moves ++ att_prise_passant
		attacks = attacks ++ att_prise_passant
		return (moves,attacks)
	}

	/**déplacement global*/
	def dpct_peon(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)])={
		var (i,j) = position
		val peon=Projet.partie.matrix(i)(j)
		if (peon.color =='B') {return dpct_peon_black(position)}
		else {return dpct_peon_white(position)}
	}

}
/**déplacement du roi*/
trait King_move extends Dplct_positions {
	def dpct_king(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = { //déplacemnt du roi
		val movement_list : List[(Int,Int)] = List((1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1))
		return (dpct_positions(position,movement_list))
	}
}



trait Id_creation {
	/**crée un Id*/
	def id_create(color:Char,name:String) : Int = {
		var ind=0
		for( i <- 1 to 8) {
			for( j <- 1 to 8) {
				var piece_ij = (Projet.partie.matrix(i)(j)).id
				if ( piece_ij(0)==color )
				{ if (piece_ij.substring(1,3)==name) {ind+=1}} 
			}
		}
		return ind
	}	
}




class Queen(color:Char,pos:(Int,Int)) extends Piece(color,pos) 
with Id_creation with Diagonal with Horizontal_Vertical{ 
	//si jamais on remet "position" et pas un autre nom soit "pos" position est considéré constante
	val name = "Qu"
	var is_alive= true
	val id=color+name+id_create(color,name)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		var (v_h_moves,v_h_attacks)=dpct_horizon_vertic(position)
		var (diag_moves,diag_attacks)=dpct_diag(position)
		return (v_h_moves++diag_moves,v_h_attacks++diag_attacks)
	}
	var (i,j) = position
	//position normalement libre
	//Projet.partie.matrix(i)(j)=id
 
}

class Peon(color:Char,pos:(Int,Int)) extends Piece(color,pos) 
with Id_creation with Peon_move{
	val name="Pe"
	var is_alive=true
	val id=color+name+id_create(color,name)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		return dpct_peon(position)
	}
	var (i,j) = position
	//Projet.partie.matrix_pieces(i)(j)=id 

}

class King(color:Char,pos:(Int,Int)) extends Piece(color,pos) 
with Id_creation with King_move{
	val name="Ki"
	var is_alive=true
	val id=color+name+id_create(color,name)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		return dpct_king(position)
	}
	var (i,j) = position
	//Projet.partie.matrix_pieces(i)(j)=id
}

class Tower(color:Char,pos:(Int,Int)) extends Piece(color,pos) 
with Id_creation with Horizontal_Vertical{
	val name="To"
	var is_alive=true
	val id=color+name+id_create(color,name)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		return (dpct_horizon_vertic(position))
	}
	var (i,j) = position
	//Projet.partie.matrix_pieces(i)(j)=id
}

class Knight(color:Char,pos:(Int,Int)) extends Piece(color,pos) 
with Id_creation with Jump{
	val name="Kn"
	var is_alive=true
	val id=color+name+id_create(color,name)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = jump(position)
	var (i,j) = position
	//Projet.partie.matrix_pieces(i)(j)=id
}

class Bishop(color:Char,position:(Int,Int)) extends Piece(color,position) 
with Id_creation with Diagonal{
	val name="Bi"
	var is_alive=true
	val id=color+name+id_create(color,name)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		return (dpct_diag(position))
	}
	var (i,j) = position
	//Projet.partie.matrix_pieces(i)(j)=id
}
