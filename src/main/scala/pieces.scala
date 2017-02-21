//pas besoin d'inclure les trucs même depuis des fichiers différents.



//color de type char car la comparaison string char est fausse
/**Superclasse abstraite contenant toutes les pièces,
color : 'W' ou 'B'*/
abstract class Piece(color:Char,var position : (Int,Int)) {
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
		var id_piece_deleted=Projet.partie.matrix_pieces(x)(y)
		/**pièce supprimée*/
		var piece_deleted=Projet.partie.get_piece(id_piece_deleted)
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
		Projet.partie.matrix_pieces(x)(y)=get_id()
		Projet.partie.matrix_pieces(i)(j)="0"
		
		nb_turn+=1
	}

	/**renvoie la liste des cases atteignables par la pièce située en "position" en tenant compte de la mise en échec*/
	def move_piece_check(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		/**coordonnée de la pièce*/
		var (i,j)=position
		/** id de la pièce sur la case*/
		var id=Projet.partie.matrix_pieces(i)(j)
		/**pièce sur la case*/
		var piece=Projet.partie.get_piece(id)
		if (!(piece.is_alive)) {return (List(),List())}
		/**déplacements possibles (avec ou sans prise)*/
		var res_moves : List[ (Int,Int) ] = List()
		/**prises possibles*/
		var	res_attacks : List[ (Int,Int) ] = List()
		/**autre joueur*/
		val other=Projet.partie.other_player(id(0))
		if (id.substring(1,3)=="Ki") {
			var (moves,attacks) = move_piece(position)
			for (mv <-moves) {
				var (x,y)= mv
				var save = Projet.partie.matrix_pieces(x)(y)
				Projet.partie.matrix_pieces(x)(y)=id
				Projet.partie.matrix_pieces(i)(j)="0"

				if (Projet.partie.is_check(id(0))) {
					Projet.partie.matrix_pieces(x)(y)=save
					Projet.partie.matrix_pieces(i)(j)=id
				}
				else {
					Projet.partie.matrix_pieces(x)(y)=save
					Projet.partie.matrix_pieces(i)(j)=id
					res_moves=res_moves:+mv}
			}
			for (at <-attacks) {
				var (x,y)= at
				var save = Projet.partie.matrix_pieces(x)(y)
				Projet.partie.matrix_pieces(x)(y)=id
				Projet.partie.matrix_pieces(i)(j)="0"

				if (Projet.partie.is_check(id(0))) {
					Projet.partie.matrix_pieces(x)(y)=save
					Projet.partie.matrix_pieces(i)(j)=id
				}
				else {
					Projet.partie.matrix_pieces(x)(y)=save
					Projet.partie.matrix_pieces(i)(j)=id
					res_attacks=res_attacks:+at}
			}
		}
		else {
			var (moves,attacks) = move_piece(position)
			if ((Projet.partie.in_danger_of(other).contains(position))||
				Projet.partie.is_check(id(0))){
				for (mv <-moves) {
					var (x,y)= mv
					var save = Projet.partie.matrix_pieces(x)(y)
					Projet.partie.matrix_pieces(x)(y)=id
					Projet.partie.matrix_pieces(i)(j)="0"

					if (Projet.partie.is_check(id(0))) {
						Projet.partie.matrix_pieces(x)(y)=save
						Projet.partie.matrix_pieces(i)(j)=id
					}
					else {
						Projet.partie.matrix_pieces(x)(y)=save
						Projet.partie.matrix_pieces(i)(j)=id
						res_moves=res_moves:+mv}
				}
				for (at <-attacks) {
					var (x,y)= at
					var save = Projet.partie.matrix_pieces(x)(y)
					Projet.partie.matrix_pieces(x)(y)=id
					Projet.partie.matrix_pieces(i)(j)="0"

					if (Projet.partie.is_check(id(0))) {
						Projet.partie.matrix_pieces(x)(y)=save
						Projet.partie.matrix_pieces(i)(j)=id
					}
					else {
						Projet.partie.matrix_pieces(x)(y)=save
						Projet.partie.matrix_pieces(i)(j)=id
						res_attacks=res_attacks:+at}
				}
			}
			else {
				res_moves=moves
				res_attacks=attacks
			}
		}
		return (res_moves,res_attacks)
	}
}



/**déplacement horizontal et vertical (tours et reines)*/
trait Horizontal_Vertical {

	def dpct_horiz (position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = { 
		var (i,j) = position 
		val id = Projet.partie.matrix_pieces(i)(j)
		var res : List[ (Int,Int) ] = List()
		var attack_list: List[ (Int,Int) ] = List()
		var n = 1
		while ((j+n<=8) && (Projet.partie.matrix_pieces(i)(j+n)=="0")) {res=res:+(i,j+n);n+=1}
		if ((j+n<=8) && (Projet.partie.matrix_pieces(i)(j+n)(0)==Projet.partie.other_player(id(0))))
				{res=res:+(i,j+n);attack_list=attack_list:+(i,j+n)}
		n=1
		while ((j-n>=1) && (Projet.partie.matrix_pieces(i)(j-n)=="0")) {res=res:+(i,j-n);n+=1}
		if ((j-n>=1) && (Projet.partie.matrix_pieces(i)(j-n)(0)==Projet.partie.other_player(id(0))))
				{res=res:+(i,j-n);attack_list=attack_list:+(i,j-n)}
		return (res,attack_list)}


	def dpct_verti (position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = { 
		var (i,j) = position 
		var res : List[ (Int,Int) ] = List()
		var attack_list: List[ (Int,Int) ] = List()
		var n = 1
		val id= Projet.partie.matrix_pieces(i)(j)
		while ((i+n<=8) && (Projet.partie.matrix_pieces(i+n)(j)=="0")) {res=res:+(i+n,j);n+=1}

		if ((i+n<=8) && (Projet.partie.matrix_pieces(i+n)(j)(0)==Projet.partie.other_player(id(0))))
			{res=res:+(i+n,j);attack_list=attack_list:+(i+n,j)}
		n=1
		while ((i-n>=1) && (Projet.partie.matrix_pieces(i-n)(j)=="0")) {res=res:+(i-n,j);n+=1}
		if (i-n>=1 && (Projet.partie.matrix_pieces(i-n)(j)(0)==Projet.partie.other_player(id(0))))
			{res=res:+(i-n,j);attack_list=attack_list:+(i-n,j)}
		return (res,attack_list)}
}

/**déplacement diagonal (fous)*/
trait Diagonal {

	def dpct_diag_R (position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = { //diagonale allant vers la droite
	var (i,j) = position 
	var res : List[ (Int,Int) ] = List()
	var attack_list: List[ (Int,Int) ] = List()
	var n = 1
	val id= Projet.partie.matrix_pieces(i)(j)
	while ((i+n<=8) && (j+n<=8) && (Projet.partie.matrix_pieces(i+n)(j+n)=="0")) {res=res:+(i+n,j+n);n+=1}
	if ((i+n<=8) && (j+n<=8) && 
		(Projet.partie.matrix_pieces(i+n)(j+n)(0)==Projet.partie.other_player(id(0)))) //avance
		{res=res:+(i+n,j+n);attack_list=attack_list:+(i+n,j+n)}
	n=1
	while ((i-n>=1) && (j-n>=1) && (Projet.partie.matrix_pieces(i-n)(j-n)=="0")) {res=res:+(i-n,j-n);n+=1} //recule
	if ((i-n>=1) && (j-n>=1) && (Projet.partie.matrix_pieces(i-n)(j-n)(0)==Projet.partie.other_player(id(0))))
		{res=res:+(i-n,j-n);attack_list=attack_list:+(i-n,j-n)}
	return (res,attack_list)}




	def dpct_diag_L (position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = { //diagonale allant vers la gauche
	var (i,j) = position 
	var res : List[ (Int,Int) ] = List()
	var n=1
	var attack_list: List[ (Int,Int) ] = List()
	val id= Projet.partie.matrix_pieces(i)(j)
	while ((i+n<=8) && (j-n>=1) && (Projet.partie.matrix_pieces(i+n)(j-n)=="0")) {res=res:+(i+n,j-n);n+=1} //avance
	if ((i+n<=8) && (j-n>=1) && (Projet.partie.matrix_pieces(i+n)(j-n)(0)==Projet.partie.other_player(id(0))))
		{res=res:+(i+n,j-n);attack_list=attack_list:+(i+n,j-n)}
	n=1
	while ((i-n>=1) && (j+n<=8) && (Projet.partie.matrix_pieces(i-n)(j+n)=="0")) {res=res:+(i-n,j+n);n+=1} //recule
	if ((i-n>=1) && (j+n<=8) && (Projet.partie.matrix_pieces(i-n)(j+n)(0)==Projet.partie.other_player(id(0))))
		{res=res:+(i-n,j+n);attack_list=attack_list:+(i-n,j+n)}

	return (res,attack_list) }
}
/**déplacement des cavaliers*/
trait Jump{
	def jump(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		/**liste des déplacements possibles relatifs à la position initiale*/
		val movement_list : List[(Int,Int)] = List((1,2),(-1,2),(2,1),(2,-1),(-2,-1),(-2,1),(1,-2),(-1,-2)) 
		var (i,j) = position 
		var attack_list: List[ (Int,Int) ] = List()
		val id= Projet.partie.matrix_pieces(i)(j)
		var res : List[ (Int,Int) ] = List()
		for( dplct <- movement_list) {
			var (x,y) = dplct
			if ( (i+x >=1) && (i+x <=8) && (j+y <=8) && (j+y >=1) )
			{

				if (Projet.partie.matrix_pieces(i+x)(j+y)=="0")  
					{res=res:+(i+x,j+y)}
				if (Projet.partie.matrix_pieces(i+x)(j+y)(0)==Projet.partie.other_player(id(0)))
					{res=res:+(i+x,j+y);attack_list=attack_list:+(i+x,j+y)}
			}
		}
		return (res,attack_list)
	}
}
/**déplacement des pions*/
trait Peon_move{
	/**déplacement du pion blanc, avance vers le haut*/
	def dpct_peon_white(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		var (i,j) = position
		var res : List[ (Int,Int) ] = List()
		val id= Projet.partie.matrix_pieces(i)(j)
		val peon=Projet.partie.get_piece(id)
		val other='B'
		var attack_list: List[ (Int,Int) ] = List()
		if ((peon.nb_turn==0) && (i+2<=8) && (Projet.partie.matrix_pieces(i+2)(j))=="0")
			{res=res:+(i+2,j)}
		if ((i+1<=8) && (Projet.partie.matrix_pieces(i+1)(j))=="0")
			{res=res:+(i+1,j)}
		if ((i+1<=8) && (j+1<=8) && (Projet.partie.matrix_pieces(i+1)(j+1))(0)==other)
			{res=res:+(i+1,j+1);attack_list=attack_list:+(i+1,j+1)}
		if ((i+1<=8) && (j-1>=1) && (Projet.partie.matrix_pieces(i+1)(j-1))(0)==other)
			{res=res:+(i+1,j-1);attack_list=attack_list:+(i+1,j-1)}
		return (res,attack_list)
	}
	/**déplacement du pion blanc, avance vers le bas*/
	def dpct_peon_black(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		var (i,j) = position
		var res : List[ (Int,Int) ] = List()
		val id= Projet.partie.matrix_pieces(i)(j)
		val peon=Projet.partie.get_piece(id)
		val other='W'
		var attack_list: List[ (Int,Int) ] = List()
		if ((peon.nb_turn==0) && (i-2<=8) && (Projet.partie.matrix_pieces(i-2)(j))=="0")
		{res=res:+(i-2,j)}
		if ((i-1>=1) && (Projet.partie.matrix_pieces(i-1)(j))=="0")
			{res=res:+(i-1,j)}
		if ((i-1>=1) && (j+1<=8) && (Projet.partie.matrix_pieces(i-1)(j+1))(0)==other)
			{res=res:+(i-1,j+1);attack_list=attack_list:+(i-1,j+1)}
		if ((i-1>=1) && (j-1>=1) && (Projet.partie.matrix_pieces(i-1)(j-1))(0)==other)
			{res=res:+(i-1,j-1);attack_list=attack_list:+(i-1,j-1)}
		return (res,attack_list)
	}
	/**déplacement global*/
	def dpct_peon(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)])={
		var (i,j) = position
		val id= Projet.partie.matrix_pieces(i)(j)
		if (id(0)=='B') {return dpct_peon_black(position)}
		else {return dpct_peon_white(position)}
	}

}
/**déplacement du roi*/
trait King_move{
	def dpct_king(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = { //déplacemnt du roi
		val movement_list : List[(Int,Int)] = List((1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1))
		var (i,j) = position 
		var attack_list: List[ (Int,Int) ] = List()
		val id= Projet.partie.matrix_pieces(i)(j)
		var res : List[ (Int,Int) ] = List()
		for( dplct <- movement_list) {
			var (x,y) = dplct

			if ( (i+x >=1) && (i+x <=8) && (j+y <=8) && (j+y >=1) )
			{
				if (Projet.partie.matrix_pieces(i+x)(j+y)=="0")  
					{res=res:+(i+x,j+y)}
				if (Projet.partie.matrix_pieces(i+x)(j+y)(0)==Projet.partie.other_player(id(0)))
					{res=res:+(i+x,j+y);attack_list=attack_list:+(i+x,j+y)}
			}
		}
		return (res,attack_list)
	}
}



trait Id_creation {
	/**crée un Id*/
	def id_create(color:Char,name:String) : Int = {
		var ind=0
		for( i <- 1 to 8) {
			for( j <- 1 to 8) {
				var piece_ij = Projet.partie.matrix_pieces(i)(j)
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
		var (vert1,vert2)= dpct_verti(position)
		var (hori1,hori2)= dpct_horiz(position)
		var (dL1,dL2)=dpct_diag_L(position)
		var (dR1,dR2)=dpct_diag_R(position)
		return (vert1++hori1++dL1++dR1,vert2++hori2++dL2++dR2)
	}
	var (i,j) = position
	//position normalement libre
	Projet.partie.matrix_pieces(i)(j)=id
 
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
	Projet.partie.matrix_pieces(i)(j)=id 

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
	Projet.partie.matrix_pieces(i)(j)=id
}

class Tower(color:Char,pos:(Int,Int)) extends Piece(color,pos) 
with Id_creation with Horizontal_Vertical{
	val name="To"
	var is_alive=true
	val id=color+name+id_create(color,name)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		var (vert1,vert2)= dpct_verti(position)
		var (hori1,hori2)= dpct_horiz(position)
		return (vert1++hori1,vert2++hori2)
	}
	var (i,j) = position
	Projet.partie.matrix_pieces(i)(j)=id
}

class Knight(color:Char,pos:(Int,Int)) extends Piece(color,pos) 
with Id_creation with Jump{
	val name="Kn"
	var is_alive=true
	val id=color+name+id_create(color,name)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = jump(position)
	var (i,j) = position
	Projet.partie.matrix_pieces(i)(j)=id
}

class Bishop(color:Char,position:(Int,Int)) extends Piece(color,position) 
with Id_creation with Diagonal{
	val name="Bi"
	var is_alive=true
	val id=color+name+id_create(color,name)
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		var (dL1,dL2)=dpct_diag_L(position)
		var (dR1,dR2)=dpct_diag_R(position)
		return (dL1++dR1,dL2++dR2)
	}
	var (i,j) = position
	Projet.partie.matrix_pieces(i)(j)=id
}
