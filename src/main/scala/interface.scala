import swing._
import Array._
import javax.swing.ImageIcon
import java.awt.Color

object Interface extends SimpleSwingApplication{
	/*
	/**id de la npièce sélectioné pour le déplacement*/
	var id_piece_selected = "0"
	/**liste des mouvements possibles pour la pièce sélectionnée*/
	var moves:List[(Int,Int)] = List()
	/**liste des prises possibles pour la pièce sélectionnée*/
	var prises:List[(Int,Int)] = List()
	/**position de départ de la pièce*/
	var origin_pos = (0,0)
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
	*/
	
	/**Boutons permettant de lancer la partie*/
	class PartieButton(text:String,nbIA:Int,colorIA:Char,window:MainWindow) extends Button{
		action = Action(text){
			//Projet.partie.partie_nb_ia(nbIA,colorIA)
			interface_partie = new EcranPartie(8,8,window)
			interface_partie.spawn_game()
		}
	}
	class QuitButton(window:MainWindow) extends Button{
		action = Action("Quit Game"){
			window.closeOperation()
		}
	}
	/**Menu principal*/
	class MainMenu(i:Int,j:Int,window:MainFrame) extends GridPanel(i,j){

		/**bouton qui lance une partie avec un seul joueur blanc*/
		val game_one_player_white = new PartieButton("Player White vs. IA Black",1,'B',window)
		/**bouton qui lance une partie avec un seul joueur noir*/
		val game_one_player_black = new PartieButton("Player Black vs. IA White",1,'W',window)
		/**bouton qui lance une partie avec deux joueurs*/
		val game_two_players = new PartieButton("Player vs. Player",0,'0',window)
		/**bouton qui lance une partie avec deux ia*/
		val game_two_ia = new PartieButton("IA vs. IA",2,'0',window)
		/**bouton qui ferme l'interface*/
		val quit_program = new QuitButton
		/**affiche le menu principal*/
		def set_menu():Unit = {
			contents.clear()
			contents+= game_two_players
			contents+= game_one_player_black
			contents+= game_one_player_white
			contents+= game_two_ia
			contents+= quit_program
			revalidate()
			repaint()
			println("Menu set\n"+contents+"\n")
		}
	}

	class MainWindow() extends MainFrame{
		val box = new BoxPanel(Orientation.Vertical) 
		val menu_principal = new MainMenu(5,1,this)
		val grid_game = new GridPanel(8,8)
		//init()
		
		def init_menu()={
			title = "Chess"
			contents = box
			box.contents.clear()
			box.contents+=menu_principal
			menu_principal.set_menu()
			println(menu_principal.contents+"\n")
			println(box.contents)
			box.revalidate()
			box.repaint()
		}
	}

	/**Case de l'échiquier*/
	class Case(i:Int,j:Int) extends Button{
		val myGreen = new Color (48, 163, 115)
		val myBlue = new Color (0, 3, 112)
		val myRed = new Color (112, 0, 0)
		var piece = Projet.partie.matrix(i)(j)

		def init_colors() = {
			if((i+j)%2 == 0){ background = java.awt.Color.BLACK }
			else{ background = java.awt.Color.WHITE }
		}
		def colorie(couleur:String) ={
			if (couleur == "green"){ Cells(i)(j).background = myGreen }
			else if (couleur == "red"){ Cells(i)(j).background = myRed }
			else if (couleur == "blue"){ Cells(i)(j).background = myBlue }
		}
		initColors(i,j)

		action = Action(""){
			if (Projet.partie.is_running()){

			}
		}

	}
	/**Grille de taille i,j contenant les différentes cases*/
	class Echiquier(i:Int,j:Int,window:MainWindow) extends GridPanel(i,j){
		
		var Cells = ofDim[Button](8,8)
		for (i <- 8 to 1 by -1){
			for( j <- 1 to 8){
				Cells(i)(j) = new Case(i,j)
				Cells(i)(j).initColors()
			}
		}

		def reset_colors()  = {
			for( i <- 9 to 1 by -1) {
				for( j <- 1 to 8) {
					Cells(i)(j).initColors()
				}
			}
		}

	}
	/**Ecran de jeu contenant l'échiquier de taille i,j*/
	class EcranPartie(i:Int,j:Int,window:MainFrame) extends BoxPanel(Orientation.Vertical){

		var plateau = new Echiquier(i,j,window)
		val back_menu = new Button{
			action = Action("Back to main menu"){
				//Projet.partie.stop()
				window.init()
			}
		}
		val quit_program = new QuitButton

		def pat() = {
			//Projet.partie.stop()
			println("pat")
			box.contents+= new Label ("pat")
		}

		def perdu(player:Char) = {
			//Projet.partie.stop()
			println("perdu")
			box.contents+= new Label (player+" a perdu")
		}

		def spawn_game():Unit = {
			Projet.partie.start()
			window.contents = this
			this.contents+=plateau
			this.contents+= new GridPanel(1,2){
				contents+= back_menu
				contents+= quit_program
			}
			revalidate()
			repaint()
		}	
	}


	/*
	/**lance la partie*/
	def spawn_game():Unit = {
		Projet.partie.partie_init()
		for( i <- 7 to 0 by -1) {
			for( j <- 0 to 7) {
				Cells(i)(j)= new Button {
					/**récupère l'id de la pièce sur la case*/
					var piece_id = id_piece_on_case(i,j)
					action = Action("") {
						if (Projet.partie.is_running){
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
					}

					if (piece_id !="0"){
						icon = new ImageIcon( getClass.getResource(color_from_id(piece_id)+type_from_id(piece_id)+".PNG"))
					}
				}
				initColors(i,j)

			}		

		}
		*/



		val RootWindow = new MainWindow()
		def top = RootWindow
		RootWindow.init_menu()
	}