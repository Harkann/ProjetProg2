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
		var (move,i) = piece.move_piece_check(position)//ici
		return move
	} 
	def piece_allowed_take(id:String,position:(Int,Int)) : List[(Int,Int)] = {
		var piece = Projet.partie.get_piece(id)	
		var (i,take) = piece.move_piece_check(position)//ici
		return take
	}
	def piece_move(id:String,origin:(Int,Int),destination:(Int,Int)):Unit ={
		var piece = Projet.partie.get_piece(id)
		piece.move(destination)
		var (i,j) = origin
		Cells(i-1)(j-1).icon = null
		var (i2,j2) = destination
		Cells(i2-1)(j2-1).icon = new ImageIcon( getClass.getResource(color_from_id(id)+type_from_id(id)+".PNG"))


	}
	def piece_take(id:String,origin:(Int,Int),destination:(Int,Int)):Unit ={
		var piece = Projet.partie.get_piece(id)
		piece.delete(destination)
		var (i,j) = origin
		Cells(i-1)(j-1).icon = null
		var (i2,j2) = destination
		Cells(i2-1)(j2-1).icon = new ImageIcon( getClass.getResource(color_from_id(id)+type_from_id(id)+".PNG"))

		
	}
	def perdu(player:Char) = {
		Projet.partie.stop()
		println("perdu")
		box.contents+= new Label (player+" a perdu")
	}

	def pat() = {
		Projet.partie.stop()
		println("pat")
		box.contents+= new Label ("pat")
	}
	val back_menu = new Button{
		action = Action("Back to main menu"){
			Projet.partie.stop()
			set_menu()
		}

	}

	val game_one_player_white = new Button{
		action = Action("Player White vs. IA Black") {
			Projet.partie.partie_one_ia('B')
			spawn_game()
		}
	}

	val game_one_player_black = new Button{
		action = Action("Player Black vs. IA White") {
			Projet.partie.partie_one_ia('W')
			spawn_game()
		}
	}

	val game_two_players = new Button{
		action = Action("Player vs. Player") {
			Projet.partie.partie_two_players()
			spawn_game()
		}
	}
	val game_two_ia = new Button{
		action = Action("IA vs. IA") {
			Projet.partie.partie_two_ia()
			spawn_game()
		}

	}

	val box = new BoxPanel(Orientation.Vertical) 
	val grid_menu = new GridPanel(4,1)
	val grid_game = new GridPanel(8,8)
	def set_menu() = {
		grid_game.contents.clear()
		box.contents.clear()
		box.contents += grid_menu
		grid_menu.contents+= game_two_players
		grid_menu.contents+= game_one_player_black
		grid_menu.contents+= game_one_player_white
		grid_menu.contents+= game_two_ia
		box.revalidate()
		box.repaint()
	}

	def top = new MainFrame {
		title = "Chess"
		contents = box
		set_menu()
		
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
							else if (piece_id == "0" && moves.contains((i+1,j+1))) {
								piece_move(id_piece_selected,origin_pos,(i+1,j+1)) 
								resetColors()
								id_piece_selected = "0"
								Projet.partie.next_turn()
							}
							else if (piece_id != "0" && prises.contains((i+1,j+1))) {
								piece_take(id_piece_selected,origin_pos,(i+1,j+1)) 
								resetColors()
								id_piece_selected = "0"
								Projet.partie.next_turn()
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
		box.contents.clear()
		box.contents +=grid_game
		for( i <- 7 to 0 by -1) {
			for( j <- 0 to 7) {
				grid_game.contents += (Cells(i)(j))
			}
		}
		box.contents+= back_menu
		box.revalidate()
		box.repaint()
		Projet.partie.start()
	}
}
