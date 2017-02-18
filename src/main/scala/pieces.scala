//pas besoin d'inclure les trucs même depuis des fichiers différents.



//color de type char car la comparaison string char est fausse

abstract class Piece(color:Char,var position : (Int,Int)) {
	//val color:Char; // 'B' ou 'W'
	val name:String; 
	var is_alive:Boolean;
	val id:String; // != "0"
	def get_id() = id
	def move_piece(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]);
	var nb_turn = 0
	def delete(posi:(Int,Int))={
		var(i,j)=position
		var (x,y)=posi
		var id_piece_deleted=Projet.partie.matrix_pieces(x)(y)
		var piece_deleted=Projet.partie.get_piece(id_piece_deleted)
		piece_deleted.is_alive=false
		move(posi)

	}
	def move(posi:(Int,Int))={
		var (i,j)=position
		position=posi
		var (x,y)=posi
		Projet.partie.matrix_pieces(x)(y)=get_id()
		Projet.partie.matrix_pieces(i)(j)="0"
		Projet.partie.next_turn()
		nb_turn+=1
	}
	// Couleur.2PremieresLettres.numéro
	//var position:(Int,Int); //doublon avec la liste des pieces dans partie ?
	//def move(position:(Int,Int)):Unit; //change la position de la piece couple (ligne,colonne)
	//def take; -> inutile : un truc dans move gere la prise des pieces (change le flag de la piece sur la case d'arivée)
	//def is_legal():List[(Int,Int)]; //renvoie liste des positions possibles pour le prochain coup
	// doit verifier si en echec et si le move peut causer l'echec avant de renvoyer la liste
}




trait Horizontal_Vertical {  //utiliser des traits pour factoriser le code.

	def dpct_horiz (position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)])={ 
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


	def dpct_verti (position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)])={ 
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


trait Diagonal {  //utiliser des traits pour factoriser le code.

	def dpct_diag_R (position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)])={ 
	var (i,j) = position 
	var res : List[ (Int,Int) ] = List()
	var attack_list: List[ (Int,Int) ] = List()
	var n = 1
	val id= Projet.partie.matrix_pieces(i)(j)
	while ((i+n<=8) && (j+n<=8) && (Projet.partie.matrix_pieces(i+n)(j+n)=="0")) {res=res:+(i+n,j+n);n+=1}
	if ((i+n<=8) && (j+n<=8) && 
		(Projet.partie.matrix_pieces(i+n)(j+n)(0)==Projet.partie.other_player(id(0))))
		{res=res:+(i+n,j+n);attack_list=attack_list:+(i+n,j+n)}
	n=1
	while ((i-n>=1) && (j-n>=1) && (Projet.partie.matrix_pieces(i-n)(j-n)=="0")) {res=res:+(i-n,j-n);n+=1}
	if ((i-n>=1) && (j-n>=1) && (Projet.partie.matrix_pieces(i-n)(j-n)(0)==Projet.partie.other_player(id(0))))
		{res=res:+(i-n,j-n);attack_list=attack_list:+(i-n,j-n)}
	return (res,attack_list)}




	def dpct_diag_L (position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)])={ 
	var (i,j) = position 
	var res : List[ (Int,Int) ] = List()
	var n=1
	var attack_list: List[ (Int,Int) ] = List()
	val id= Projet.partie.matrix_pieces(i)(j)
	while ((i+n<=8) && (j-n>=1) && (Projet.partie.matrix_pieces(i+n)(j-n)=="0")) {res=res:+(i+n,j-n);n+=1}
	if ((i+n<=8) && (j-n>=1) && (Projet.partie.matrix_pieces(i+n)(j-n)(0)==Projet.partie.other_player(id(0))))
		{res=res:+(i+n,j-n);attack_list=attack_list:+(i+n,j-n)}
	n=1
	while ((i-n>=1) && (j+n<=8) && (Projet.partie.matrix_pieces(i-n)(j+n)=="0")) {res=res:+(i-n,j+n);n+=1}
	if ((i-n>=1) && (j+n<=8) && (Projet.partie.matrix_pieces(i-n)(j+n)(0)==Projet.partie.other_player(id(0))))
		{res=res:+(i-n,j+n);attack_list=attack_list:+(i-n,j+n)}

	return (res,attack_list) }
}

trait Jump{
	def jump(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)])={
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

trait Peon_move{//ici pb deplacement pion noir en arriere !!!!!!
	def dpct_peon_white(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)])={
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
	def dpct_peon_black(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)])={
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
		if ((i-1>=1) && (j+1<=8) && (Projet.partie.matrix_pieces(i+1)(j+1))(0)==other)
			{res=res:+(i-1,j+1);attack_list=attack_list:+(i-1,j+1)}
		if ((i-1>=1) && (j-1>=1) && (Projet.partie.matrix_pieces(i-1)(j-1))(0)==other)
			{res=res:+(i-1,j-1);attack_list=attack_list:+(i-1,j-1)}
		return (res,attack_list)
	}
	def dpct_peon(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)])={
		var (i,j) = position
		val id= Projet.partie.matrix_pieces(i)(j)
		if (id(0)=='B') {return dpct_peon_black(position)}
		else {return dpct_peon_white(position)}
	}

}

trait King_move{
	def dpct_king(position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)])={
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

//ce soir coder le "en danger" 
/*
trait In_danger{
def in_danger_of(player: Char): List[(Int,Int)]={
	var res : List[ (Int,Int) ] = List()
	val other=Projet.partie.other_player(player)
	for( i <- 1 to 8) {
		for( j <- 1 to 8) {
			var piece_ij = Projet.partie.matrix_pieces(i)(j)
			if (piece_ij(0)==player)
			{
				var (list_move,list_attack)=move_piece(Projet.partie.get_piece(piece_ij))
				res=res++list_attack
			}
		}
	}
	return list_attack
}
}
*/

/*
def is_check(player : Char): Bool{
	val other=Projet.partie.other_player(player)
	var list_in_danger=in_danger_of(other)


}

*/

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
	//def is_check;  -> idée de jobic -> un fonction determinant les pieces attaquables -> est ce que le roi est dedans? 
	//ndt : savoir si un moove n'induit ne pas d'echec-> est t'il attaqué si oui on verifit plus
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
