import swing._
import Array._
import javax.swing.ImageIcon
import java.awt.Color

object Interface extends SimpleSwingApplication {
	/**id de la npièce sélectioné pour le déplacement*/
	var id_piece_selected = "0"
	/**liste des mouvements possibles pour la pièce sélectionnée*/
	var moves:List[(Int,Int)] = List()
	/**liste des prises possibles pour la pièce sélectionnée*/
	var prises:List[(Int,Int)] = List()
	/**position de départ de la pièce*/
	var origin_pos = (0,0)
	/**liste des cases sous*/
	var Cells = ofDim[Button](8,8)
	/** couleur verte (pièce sélectionnée)*/
	val myGreen = new Color (48, 163, 115)
	/** couleur verte (déplacement possible)*/
	val myBlue = new Color (0, 3, 112)
	/** couleur verte (prise possible)*/
	val myRed = new Color (112, 0, 0)
	/**Initialise la couleur de la case i,j
	(utilisé au lancement de la partie)*/
	def initColors(i:Int,j:Int) = {
		if((i+j)%2 == 0){
			Cells(i)(j).background = java.awt.Color.BLACK
		}
		else{
			Cells(i)(j).background = java.awt.Color.WHITE
		}
	}
	/**Permet de réinitialiser les couleurs du plateau*/
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
	/**colore la case i,j en vert*/
	def select_case(i:Int,j:Int) ={
		Cells(i)(j).background = myGreen
	}
	/**colore la case i,j en bleu*/
	def select_case_move(i:Int,j:Int) ={
		Cells(i)(j).background = myBlue
	}
	/**colore la case i,j en rouge*/
	def select_case_take(i:Int,j:Int) ={
		Cells(i)(j).background = myRed
	}
	/**Voir Partie*/
	def id_piece_on_case (i:Int,j:Int):String = {
		return Projet.partie.id_piece_on_case(i+1, j+1)
	}
	/**Voir Partie*/
	def color_from_id (id:String):Char = Projet.partie.color_from_id(id)
	/**Voir Partie*/
	def type_from_id (id:String):String = Projet.partie.type_from_id(id)
	/**Voir Partie*/
	def get_player() = Projet.partie.get_player()
	/**récupère la liste des mouvements possibles 
	de la pièce*/
	def piece_allowed_move(id:String,position:(Int,Int)) : List[(Int,Int)] = {
		/**objet pièce correspondant à l'id*/
		var piece = Projet.partie.get_piece(id)	
		/**move : mouvements possibles de la pièce*/
		var (move,i) = piece.move_piece_check(position)
		return move
	} 
	/**récupère la liste des pièces pouvant être 
	prises par la pièce*/
	def piece_allowed_take(id:String,position:(Int,Int)) : List[(Int,Int)] = {
		/**objet pièce correspondant à l'id*/
		var piece = Projet.partie.get_piece(id)	
		/**take : prises possibles de la pièce*/
		var (i,take) = piece.move_piece_check(position)
		return take
	}
	/**déplace la pièce "id" de la case "origin" vers "destination"*/
	def piece_move(id:String,origin:(Int,Int),destination:(Int,Int)):Unit ={
		/**objet pièce correspondant à l'id*/
		var piece = Projet.partie.get_piece(id)
		piece.move(destination)
		var (i,j) = origin
		Cells(i-1)(j-1).icon = null
		var (i2,j2) = destination
		Cells(i2-1)(j2-1).icon = new ImageIcon( getClass.getResource(color_from_id(id)+type_from_id(id)+".PNG"))


	}
	/**prend la pièce sur la case "destination" avec la pièce "id" de la case "origin"*/
	def piece_take(id:String,origin:(Int,Int),destination:(Int,Int)):Unit ={
		/**objet pièce correspondant à l'id*/
		var piece = Projet.partie.get_piece(id)
		piece.delete(destination)
		var (i,j) = origin
		Cells(i-1)(j-1).icon = null
		var (i2,j2) = destination
		Cells(i2-1)(j2-1).icon = new ImageIcon( getClass.getResource(color_from_id(id)+type_from_id(id)+".PNG"))

		
	}
	/**affiche quand le joueur "player" a perdu*/
	def perdu(player:Char) = {
		Projet.partie.stop()
		println("perdu")
		box.contents+= new Label (player+" a perdu")
	}
	/**affiche quand il y a pat*/
	def pat() = {
		Projet.partie.stop()
		println("pat")
		box.contents+= new Label ("pat")
	}
	/**bouton retour au menu*/
	val back_menu = new Button{
		action = Action("Back to main menu"){
			Projet.partie.stop()
			set_menu()
		}

	}
	/**bouton qui lance une partie avec un seul joueur blanc*/
	val game_one_player_white = new Button{
		action = Action("Player White vs. IA Black") {
			Projet.partie.partie_one_ia('B')
			spawn_game()
		}
	}
	/**bouton qui lance une partie avec un seul joueur noir*/
	val game_one_player_black = new Button{
		action = Action("Player Black vs. IA White") {
			Projet.partie.partie_one_ia('W')
			spawn_game()
		}
	}
	/**bouton qui lance une partie avec deux joueurs*/
	val game_two_players = new Button{
		action = Action("Player vs. Player") {
			Projet.partie.partie_two_players()
			spawn_game()
		}
	}
	/**bouton qui lance une partie avec deux ia*/
	val game_two_ia = new Button{
		action = Action("IA vs. IA") {
			Projet.partie.partie_two_ia()
			spawn_game()
		}

	}
	/**bouton qui ferme l'interface*/
	val quit_program = new Button{
		action = Action("Quit Game"){
			top.closeOperation()
		}

	}
	/**Contient l'interface*/
	val box = new BoxPanel(Orientation.Vertical) 
	/**contient le menu principal*/
	val grid_menu = new GridPanel(5,1)
	/**contient le plateau*/
	val grid_game = new GridPanel(8,8)
	/**affiche le menu principal*/
	def set_menu():Unit = {
		grid_game.contents.clear()
		box.contents.clear()
		box.contents += grid_menu
		grid_menu.contents+= game_two_players
		grid_menu.contents+= game_one_player_black
		grid_menu.contents+= game_one_player_white
		grid_menu.contents+= game_two_ia
		grid_menu.contents+= quit_program
		box.revalidate()
		box.repaint()
	}
	/**initialise la fenêtre avec le menu principal*/
	def top = new MainFrame {
		title = "Chess"
		contents = box
		set_menu()
		
	}
	/**lance la partie*/
	def spawn_game():Unit = {
		Projet.partie.partie_init()
		for( i <- 7 to 0 by -1) {
			for( j <- 0 to 7) {
				Cells(i)(j)= new Button {
					/**récupère l'id de la pièce sur la case*/
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
		box.contents+= new GridPanel(1,2){
			contents+= back_menu
			contents+= quit_program
		}
		box.revalidate()
		box.repaint()
		Projet.partie.start()
	}
}
