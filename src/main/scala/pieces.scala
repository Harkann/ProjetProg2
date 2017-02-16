//pas besoin d'inclure les trucs même depuis des fichiers différents.



//color de type char car la comparaison string char est fausse

abstract class Piece(color:Char,var position : (Int,Int)) {
	//val color:Char; // 'B' ou 'W'
	val name:String; 
	var is_alive:Boolean;
	val id:String; // != "0"
	// Couleur.2PremieresLettres.numéro
	//var position:(Int,Int); //doublon avec la liste des pieces dans partie ?
	//def move(position:(Int,Int)):Unit; //change la position de la piece couple (ligne,colonne)
	//def take; -> inutile : un truc dans move gere la prise des pieces (cahnge le flag de la piece sur la case d'arivée)
	//def is_legal():List[(Int,Int)]; //renvoie liste des positions possibles pour le prochain coup
	// doit verifier si en echec et si le move peut causer l'echec avant de renvoyer la liste
}


trait Horizontal_Vertical {  //utiliser des traits pour factoriser le code.
	def dpct_horiz(position:(Int,Int)) : List[(Int,Int)]={
	var (i,j) = position 
	var res : List[ (Int,Int) ] = List()
	for( new_j <- (j+1) to 8) { res=res:+(i,new_j)} //est ce qu'on gere le fait de surveiller s'il n'y a pas de pieces ici ?????????????????????????????????????????????????????????????????
	for( new_j <- 1 to (j-1)) { res=res:+(i,new_j)}
	return res
	}

	def dpct_verti(position:(Int,Int)) : List[(Int,Int)]={
	var (i,j) = position 
	var res : List[ (Int,Int) ] = List()
	for( new_i <- (i+1) to 8) { res=res:+(new_i,j)} 
	for( new_i <- 1 to (i-1)) { res=res:+(new_i,j)}
	return res
	}
}

trait Diagonal_ {  //utiliser des traits pour factoriser le code.
	def dpct_diag_R(position:(Int,Int)) : List[(Int,Int)]={
	var (i,j) = position 
	var res : List[ (Int,Int) ] = List()
	if (i>=j) {
	for( dpclt <- 1 to 8-i) { res=res:+(i+dpclt,j+dpclt)}
	for( dpclt <- 1 to j-1) { res=res:+(i-dpclt,j-dpclt)}
	}
	else {
	for( dpclt <- 1 to 8-j) { res=res:+(dpclt+i,j+dpclt)}
	for( dpclt<- 1 to (i-1)) { res=res:+(i+dpclt,j+dpclt)}
	}
	return res
	}

	def dpct_diag_L(position:(Int,Int)) : List[(Int,Int)]={
	var (i,j) = position 
	var res : List[ (Int,Int) ] = List()
	if (i+j<=9) {
	for( dpclt <- 1 to j-1) { res=res:+(i+dpclt,j-dpclt)}
	for( dpclt <- 1 to i-1) { res=res:+(i-dpclt,j+dpclt)}
	}
	else {
	for( dpclt <- 1 to 8-i) { res=res:+(dpclt+i,j-dpclt)}    //je me méfie de ma programation des deplacements....
	for( dpclt<- 1 to (8-j)) { res=res:+(i-dpclt,j+dpclt)}
	}
	return res
	}
}



trait Id_creation {
	def id_create(color:Char,name:String) : Int = {
		var ind=0
		for( i <- 0 to 7) {
			for( j <- 0 to 7) {
				val piece_ij = Projet.partie.matrix_pieces(i)(j)
				if ( piece_ij(0)==color )
				{ if (piece_ij.substring(1,3)==name) {ind+=1}} 
			}
		}
		return ind
	}	
}



//Je mets tout mes commentaires qui sont valables pour toute pièce sur la reine uniquement

class Queen(color:Char,pos:(Int,Int)) extends Piece(color,pos) with Id_creation { //si jamais on remet "position" et pas un autre nom soit "pos" position est considéré constante
	val name = "Qu"
	var is_alive= true
	val id=color+name+id_create(color,name)

	var (i,j) = position
	// est ce qu'on devrait pas faire atention à ce que la position ne soit pas dejà prise?
	Projet.partie.matrix_pieces(i)(j)=id
 
}

class Peon(color:Char,pos:(Int,Int)) extends Piece(color,pos) with Id_creation{
	val name="Pe"
	var is_alive=true
	val id=color+name+id_create(color,name)

	var (i,j) = position
	Projet.partie.matrix_pieces(i)(j)=id 
}

class King(color:Char,pos:(Int,Int)) extends Piece(color,pos) with Id_creation{
	val name="Ki"
	var is_alive=true
	val id=color+name+id_create(color,name)
	//def is_check; 

	var (i,j) = position
	Projet.partie.matrix_pieces(i)(j)=id
}

class Tower(color:Char,pos:(Int,Int)) extends Piece(color,pos) with Id_creation{
	val name="To"
	var is_alive=true
	val id=color+name+id_create(color,name)
}

class Knight(color:Char,pos:(Int,Int)) extends Piece(color,pos) with Id_creation{
	val name="Kn"
	var is_alive=true
	val id=color+name+id_create(color,name)

	var (i,j) = position
	Projet.partie.matrix_pieces(i)(j)=id
}

class Bishop(color:Char,position:(Int,Int)) extends Piece(color,position) with Id_creation{
	val name="Bi"
	var is_alive=true
	val id=color+name+id_create(color,name)

	var (i,j) = position
	Projet.partie.matrix_pieces(i)(j)=id
}