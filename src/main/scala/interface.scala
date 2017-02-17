import swing._
import Array._
import javax.swing.ImageIcon

object Interface extends SimpleSwingApplication {
	var id_piece_selected = "0"
	var Cells = ofDim[Button](8,8)
	def initColors(i:Int,j:Int) = {
		if((i+j)%2 == 0){
			Cells(i)(j).background = java.awt.Color.BLACK
		}
		else{
			Cells(i)(j).background = java.awt.Color.WHITE
		}
	}
	def resetColors()  = {
		for( i <- 7 to 0 by -1) {
			for( j <- 0 to 7) {
				if((i+j)%2 == 0){
					Cells(i)(j).background = java.awt.Color.BLACK
				}
				else{
					Cells(i)(j).background = java.awt.Color.WHITE
				}
			}
		}
	}
	def select_case(i:Int,j:Int) ={
		Cells(i)(j).background = java.awt.Color.GREEN
	}
	def select_case_move(i:Int,j:Int) ={
		Cells(i)(j).background = java.awt.Color.BLUE
	}
	def select_case_take(i:Int,j:Int) ={
		Cells(i)(j).background = java.awt.Color.RED
	}

	def id_piece_on_case (i:Int,j:Int):String = {
		return Projet.partie.id_piece_on_case(i+1, j+1)
	}

	def color_from_id (id:String):Char = Projet.partie.color_from_id(id)
	def type_from_id (id:String):String = Projet.partie.type_from_id(id)
	def get_player() = Projet.partie.get_player()
	def piece_move(id:String,position:(Int,Int)) : (List[(Int,Int)],List[(Int,Int)]) = {
		var piece = Projet.partie.get_piece(id)	
		return piece.move_piece(position)
	} 

	for( i <- 7 to 0 by -1) {
		for( j <- 0 to 7) {
			Cells(i)(j)= new Button {
				var piece_id = id_piece_on_case(i,j)
				action = Action("") {
					piece_id = id_piece_on_case(i,j)
					if (id_piece_selected == "0"){
						if (piece_id != "0" && color_from_id(piece_id) == get_player() && piece_id != id_piece_selected){
							id_piece_selected = piece_id
							resetColors()
							select_case(i,j)
							var (moves, prises) = piece_move(piece_id,(i+1,j+1))
							for( (i,j) <- moves) {
								select_case_move(i-1,j-1)
							}
							for( (i,j) <- prises) {
								select_case_take(i-1,j-1)
							}
						}
					}	
					else {
						if (piece_id == id_piece_selected){
							id_piece_selected = "0"
							resetColors()
						}
						else if (piece_id == "0" && moves.contents((i,j))) {

						}			
					}
				}

				if (piece_id !="0"){
					icon = new ImageIcon( getClass.getResource(color_from_id(piece_id)+type_from_id(piece_id)+".PNG"))
				}
			}
			initColors(i,j)
		}		
		
	}




	def top = new MainFrame {
		title = "Chess"
		contents = new GridPanel(8, 8) {
			for( i <- 7 to 0 by -1) {
				for( j <- 0 to 7) {
					contents += (Cells(i)(j))
				}
			}
		}
	}
}
