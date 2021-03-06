import javax.swing.ImageIcon


/* roque meillleurs regles configuration de pendule sauvegarde de partie  chargement sauvegarde mettre une classe joueur bien def */

/*
***************************************************************************************************************
______________________________DÉFINITION DE LA CLASSE ABSTRAITE PIECE  _______________________________________

***************************************************************************************************************
*/




//color de type char car la comparaison string char est fausse
/**Superclasse abstraite contenant toutes les pièces,
color : 'W' ou 'B'*/
abstract class Piece(col:Char,var position : (Int,Int),var partie:Partie) extends Standard with condition_check with Conversion_to_PGN {
	val color = col;
	/**nom de la pièce*/
	val name:String; 
	/**nom au format PGN de la piece**/
	val PGN_name:String; 
	/** un numreo attribué a chaque type de piece **/
	val num_type:Int;
	/**statut en vie ou non de la pièce*/
	var is_alive:Boolean;
	/** **/
	var is_promotion= false;
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
	/**déplace la pièce vers "posi"*/
	def move(posi:(Int,Int)) = {
		/**coordonnées actuelles de la pièce*/
		var (i,j)=position

		//println("position : "+position)
		//println("posi : "+posi)

		/**coordonnées de la destination*/
		var (x,y)=posi
		val piece = matrix(position,partie)
		//println("piece (2,8) : "+partie.matrix(2)(8))
		//println("piece (4,8) : "+partie.matrix(4)(8))
		//println(piece)
		val piece_met = partie.matrix(x)(y)

		incremente_cpt_nb_piece(partie,piece_met)


		var dpct= new Dpct(position,posi,partie)
		roque_check(dpct,partie)
		prise_en_passant_check(dpct,partie)
		position = (x,y)
		partie.matrix(x)(y)=piece
		partie.matrix(i)(j)=null

		nb_turn+=1
		if ((piece_met!=null)||(name=="Pe")){
			partie.last_important_change=partie.nb_turn
			partie.matrix_save = copy_of(partie.matrix)
		}
		if(partie.is_mat(partie.other_player(piece.color))) {
			dpct.echec_other_player = "#"
		}
		else if(partie.is_check(partie.other_player(piece.color))) {
			dpct.echec_other_player = "+"
		}
		


		promotion_check(dpct,partie)
		partie.dplct_save += dpct
		partie.game_window.plateau.reset_all()
		//nothing_but_pat_check(partie,partie.pieces_B,partie.pieces_W)
		//nothing_but_pat_check(partie,partie.pieces_W,partie.pieces_B)
		nothing_but_pat_check(partie,partie.lost_pieces_B,partie.lost_pieces_W)
		nothing_but_pat_check(partie,partie.lost_pieces_W,partie.lost_pieces_B)		
		partie.moves_50_check(partie)
		partie.repetitions_3_check(partie)
		//read_test()
		if (partie.player_type(partie.other_player(partie.currently_playing)) == 'G' && partie.player_type(partie.currently_playing) != 'G'){
			partie.gnuchess.move_and_write(i,j,x,y)
			/**TODO : renvoie le mouvement à gnuchess*/
		}
		//println("pieces_B: "+partie.pieces_B.deep+" pieces_W: "+partie.pieces_W.deep)
		//println("lost_pieces_B: "+partie.lost_pieces_B.deep+" lost_pieces_W: "+partie.lost_pieces_W.deep)
		if (partie.waiting == false){
			partie.next_turn()
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
		if (piece.name == "Ki"){
			res_moves=res_moves ++ piece.asInstanceOf[King].roque(position,partie)
		}
		return (res_moves,res_attacks)
	}

	/**renvoie la liste des cases atteignables par la pièce située en "position" en tenant compte de la mise en échec*/
	def move_piece_check(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		
		/**coordonnée de la pièce*/
		var (i,j) = position
		//println ("position : "+i+" "+j)
		/** id de la pièce sur la case*/
		var piece = matrix((i,j),partie)
		if ((piece == null) || !(piece.is_alive)) {
			return (List(),List())
		}
		/**pièce sur la case*/
		var id=piece.id
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
______________________________DÉFINITION DES DIFFERENTES CLASSES DE PIECES ___________________________________

***************************************************************************************************************
*/





class Peon(color:Char,pos:(Int,Int),partie:Partie) extends Piece(color,pos,partie)
with Id_creation with Peon_move with Promotion {
	val num_type = 0
	val name="Pe"
	val PGN_name=""
	val image = Tools.icon_resized(color+name+".PNG",Tools.min_size/20,Tools.min_size/20)
	var is_alive=true
	val id=color+name+id_create(color,name,partie)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		return dpct_peon(position,partie)
	}

	//def promotion(position:(Int,Int)) { promo(position,partie) }
	var (i,j) = position
	partie.modif_piece(color,num_type,1)
	//Projet.partie.matrix_pieces(i)(j)=id 

}

class Tower(color:Char,pos:(Int,Int),partie:Partie) extends Piece(color,pos,partie) 
with Id_creation with Horizontal_Vertical{
	val name = "To"
	val PGN_name="R"
	val num_type = 1
	val image = Tools.icon_resized(color+name+".PNG",Tools.min_size/20,Tools.min_size/20)
	var is_alive=true
	val id=color+name+id_create(color,name,partie)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		return (dpct_horizon_vertic(position,partie))
	}
	var (i,j) = position
	partie.modif_piece(color,num_type,1)
	//Projet.partie.matrix_pieces(i)(j)=id
}

class Knight(color:Char,pos:(Int,Int),partie:Partie) extends Piece(color,pos,partie) 
with Id_creation with Jump{
	val name="Kn"
	val PGN_name="N"
	val num_type = 2
	val image = Tools.icon_resized(color+name+".PNG",Tools.min_size/20,Tools.min_size/20)
	var is_alive=true
	val id=color+name+id_create(color,name,partie)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = jump(position,partie)
	var (i,j) = position
	partie.modif_piece(color,num_type,1)
	//Projet.partie.matrix_pieces(i)(j)=id
}

class Bishop(color:Char,position:(Int,Int),partie:Partie) extends Piece(color,position,partie) 
with Id_creation with Diagonal{
	val name="Bi"
	val PGN_name="B"
	val num_type = 3 // Pink Fluffy unicorns Dancing on Rainbooows
	val image = Tools.icon_resized(color+name+".PNG",Tools.min_size/20,Tools.min_size/20)
	var is_alive=true
	val id=color+name+id_create(color,name,partie)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		return (dpct_diag(position,partie))
	}
	var (i,j) = position
	partie.modif_piece(color,num_type,1)
	//Projet.partie.matrix_pieces(i)(j)=id
}

class Queen(color:Char,pos:(Int,Int),partie:Partie) extends Piece(color,pos,partie) 
with Id_creation with Diagonal with Horizontal_Vertical{ 
	//si jamais on remet "position" et pas un autre nom soit "pos" position est considéré constante
	val name = "Qu"
	val PGN_name="Q"
	val num_type = 4
	val image = Tools.icon_resized(color+name+".PNG",Tools.min_size/20,Tools.min_size/20)
	var is_alive= true
	val id=color+name+id_create(color,name,partie)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		var (v_h_moves,v_h_attacks)=dpct_horizon_vertic(position,partie)
		var (diag_moves,diag_attacks)=dpct_diag(position,partie)
		return (v_h_moves++diag_moves,v_h_attacks++diag_attacks)
	}
	var (i,j) = position
	partie.modif_piece(color,num_type,1)
	//position normalement libre
	//Projet.partie.matrix(i)(j)=id
}



class King(color:Char,pos:(Int,Int),partie:Partie) extends Piece(color,pos,partie) 
with Id_creation with King_move with Roque{
	val name="Ki"
	val PGN_name="K"
	val num_type = 5
	val image = Tools.icon_resized(color+name+".PNG",Tools.min_size/20,Tools.min_size/20)
	var is_alive=true
	var has_been_check = false
	val id=color+name+id_create(color,name,partie)
	partie.modif_piece(color,num_type,1)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		return dpct_king(position,partie)
	}
	var (i,j) = position
	//Projet.partie.matrix_pieces(i)(j)=id
}
