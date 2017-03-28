
class Blitz_4(){
	var partie_G = new Partie
	var partie_D = new Partie
	var dispo_G_W = partie_D.lost_pieces_W
	var dispo_G_B = partie_D.lost_pieces_B
	var dispo_D_W = partie_G.lost_pieces_W
	var dispo_D_B = partie_G.lost_pieces_B
	
	def arrive_possible(type_piece:String,partie:Partie):List[(Int,Int)] = {
		var res:List[(Int,Int)]= List()
		if (type_piece == "Pe"){
			for(i<-2 to 7){
				for(j<-1 to 8){
					if (partie.matrix(i)(j) == null){
						res = res:+(i,j)
					}
				}
			}
		}
		else{
			for(i<-1 to 8){
				for(j<-1 to 8){
					if (partie.matrix(i)(j) == null){
						res = res:+(i,j)
					}
				}
			}
		}
		return res
	}

	def arrive(type_piece:String,color:Char,position:(Int,Int),partie:Partie){
		var piece:Piece = null
		type_piece match {
			case "Pe" => piece = new Peon(color,position,partie)
			case "To" => piece = new Tower(color,position,partie)
			case "Kn" => piece = new Knight(color,position,partie)
			case "Bi" => piece = new Bishop(color,position,partie)
			case "Qu" => piece = new Queen(color,position,partie)
		}
		
		var (i,j) = position
		partie.matrix(i)(j) = piece
	}
}