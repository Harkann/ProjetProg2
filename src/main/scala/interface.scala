import swing._
import Array._
import javax.swing.ImageIcon
import java.awt.Color
object Interface extends SimpleSwingApplication {
	var id_piece_selected = "0"
	var moves:List[(Int,Int)] = List()
	var prises:List[(Int,Int)] = List()
	var origin_pos = (0,0)
	var Cells = ofDim[Button](8,8)
	val myGreen = new Color (48, 163, 115)
	val myBlue = new Color (0, 3, 112)
	val myRed = new Color (112, 0, 0)
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

		Cells(i)(j).background = myGreen
	}
	def select_case_move(i:Int,j:Int) ={
		Cells(i)(j).background = myBlue
	}
	def select_case_take(i:Int,j:Int) ={
		Cells(i)(j).background = myRed
	}

	def id_piece_on_case (i:Int,j:Int):String = {
		return Projet.partie.id_piece_on_case(i+1, j+1)
	}

	def color_from_id (id:String):Char = Projet.partie.color_from_id(id)
	def type_from_id (id:String):String = Projet.partie.type_from_id(id)
	def get_player() = Projet.partie.get_player()
	def piece_allowed_move(id:String,position:(Int,Int)) : List[(Int,Int)] = {
		var piece = Projet.partie.get_piece(id)	
		var (move,i) = piece.move_piece(position)
		return move
	} 
	def piece_allowed_take(id:String,position:(Int,Int)) : List[(Int,Int)] = {
		var piece = Projet.partie.get_piece(id)	
		var (i,take) = piece.move_piece(position)
		return take
	}
	def piece_move(id:String,origin:(Int,Int),destination:(Int,Int)) ={
		var piece = Projet.partie.get_piece(id)
		piece.move(destination)
		var (i,j) = origin
		println("plop1")
		Cells(i-1)(j-1).icon = null
		var (i2,j2) = destination
		println("plop2")
		Cells(i2-1)(j2-1).icon = new ImageIcon( getClass.getResource(color_from_id(id)+type_from_id(id)+".PNG"))
		println("plop3")

	}


	

	var game_one_player = new Button{
		
	}
	val game_two_players = new Button{
		action = Action("Player vs. Playe") {
			spawn_game()
		}
	}
	var game_two_ia = new Button{

	}
	val box = new BoxPanel(Orientation.Vertical) {
		contents+= game_two_players
	}

	def top = new MainFrame {
		title = "Chess"
		contents = box
		
	}
	def spawn_game():Unit = {
		Projet.partie.partie_init()
		for( i <- 7 to 0 by -1) {
			for( j <- 0 to 7) {
				Cells(i)(j)= new Button {
					var piece_id = id_piece_on_case(i,j)


					action = Action("") {
						piece_id = id_piece_on_case(i,j)
						if (id_piece_selected == "0"){
							if (piece_id != "0" && color_from_id(piece_id) == get_player() && piece_id != id_piece_selected){
								id_piece_selected = piece_id
								origin_pos = (i+1,j+1)
								resetColors()
								select_case(i,j)
								moves = piece_allowed_move(piece_id,(i+1,j+1))
								prises = piece_allowed_take(piece_id,(i+1,j+1))
								println("moves "+moves)
								for( (i,j) <- moves) {
									select_case_move(i-1,j-1)

								}
								for( (i,j) <- prises) {
									select_case_take(i-1,j-1)
								}
							}
						}	
						else {
							println("re moves "+moves)
							if (piece_id == id_piece_selected){
								id_piece_selected = "0"
								resetColors()
							}
							else if (piece_id == "0" && moves.contains((i+1,j+1))) {
								println(moves)
								piece_move(id_piece_selected,origin_pos,(i+1,j+1)) 
								resetColors()
								id_piece_selected = "0"
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
		box.contents -= game_two_players
		box.contents += new GridPanel(8, 8) {
			for( i <- 7 to 0 by -1) {
				for( j <- 0 to 7) {
					contents += (Cells(i)(j))
				}
			}
		}
		box.revalidate()
		box.repaint()
	}
}
