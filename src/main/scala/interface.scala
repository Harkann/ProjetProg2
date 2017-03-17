import swing._
import Array._
import javax.swing.ImageIcon
import java.awt.Color

object Interface extends SimpleSwingApplication{

	/*
	/**liste des mouvements possibles pour la pièce sélectionnée*/
	var moves:List[(Int,Int)] = List()
	/**liste des prises possibles pour la pièce sélectionnée*/
	var prises:List[(Int,Int)] = List()
	/**position de départ de la pièce*/
	var origin_pos = (0,0)
	
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
	var is_button_clicked = false
	var button_clicked_i = 0
	var button_clicked_j = 0
	var piece_selected:Piece = null
	var piece_allowed_move:List[(Int,Int)] = List()
	var piece_allowed_take:List[(Int,Int)] = List()
	/**Boutons permettant de lancer la partie*/
	class PartieButton(text:String,nbIA:Int,colorIA:Char,window:MainWindow) extends Button{
		action = Action(text){
			//Projet.partie.partie_nb_ia(nbIA,colorIA)
			var interface_partie = new EcranPartie(8,8,window)
			interface_partie.spawn_game()
		}
	}
	class QuitButton(window:MainWindow) extends Button{
		action = Action("Quit Game"){
			window.closeOperation()
		}
	}
	/**Menu principal*/
	class MainMenu(i:Int,j:Int,window:MainWindow) extends GridPanel(i,j){

		/**bouton qui lance une partie avec un seul joueur blanc*/
		val game_one_player_white = new PartieButton("Player White vs. IA Black",1,'B',window)
		/**bouton qui lance une partie avec un seul joueur noir*/
		val game_one_player_black = new PartieButton("Player Black vs. IA White",1,'W',window)
		/**bouton qui lance une partie avec deux joueurs*/
		val game_two_players = new PartieButton("Player vs. Player",0,'0',window)
		/**bouton qui lance une partie avec deux ia*/
		val game_two_ia = new PartieButton("IA vs. IA",2,'0',window)
		/**bouton qui ferme l'interface*/
		val quit_program = new QuitButton(window)
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
			//println("Menu set\n"+contents+"\n")
		}
	}

	class MainWindow() extends MainFrame{
		val box = new BoxPanel(Orientation.Vertical) 
		val menu_principal = new MainMenu(5,1,this)
		val interface_partie = new EcranPartie(8,8,this)
		//init()
		
		def init_menu()={
			title = "Chess"
			contents = box
			box.contents.clear()
			box.contents+=menu_principal
			menu_principal.set_menu()
			//println(menu_principal.contents+"\n")
			//println(box.contents)
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
		var is_clicked = false

		def set_is_clicked(value:Boolean){
			is_clicked = value
		}

		def clic(){
			button_clicked_i = i
			button_clicked_j = j
			is_clicked = true
			is_button_clicked = true
		}

		def get_image(){
			piece = Projet.partie.matrix(i)(j)
			if (piece != null){icon = piece.image}
		}

		def unclic(){
			button_clicked_i = 0
			button_clicked_j = 0
			is_clicked = false
			is_button_clicked = false
			Interface.RootWindow.interface_partie.plateau.reset_colors()
		}

		def select_piece(){
			var piece_selected = Projet.partie.get_piece(i,j)
			colorie("green")
			var(piece_move,piece_take) = Projet.partie.get_piece(i,j).move_piece_check(i,j)
			piece_allowed_move = piece_move
			piece_allowed_take = piece_take
			for ((i,j) <- piece_allowed_move){
				Interface.RootWindow.interface_partie.plateau.Cells(i)(j).colorie("blue")
			}
			for ((i,j) <- piece_allowed_take){
				Interface.RootWindow.interface_partie.plateau.Cells(i)(j).colorie("red")
			}
		}

		def init_colors() = {
			if((i+j)%2 == 0){ background = java.awt.Color.BLACK }
			else{ background = java.awt.Color.WHITE }
		}
		def colorie(couleur:String) ={
			if (couleur == "green"){ background = myGreen }
			else if (couleur == "red"){ background = myRed }
			else if (couleur == "blue"){ background = myBlue }
		}
		init_colors()
		get_image()
		action = Action(""){
			get_image()
			if (Projet.partie.is_running && Projet.partie.is_interface){
				if (is_clicked){
					unclic()
				}
				else if (is_button_clicked){

					if (piece_allowed_move.contains(i,j)){
						piece_selected.move(i,j)
						unclic()
					}
					else if (piece_allowed_take.contains(i,j)){
						piece_selected.delete(i,j)
						unclic()
					}
				}
				else if (Projet.partie.get_color(i,j) == Projet.partie.player){
					select_piece()
					clic()
				}
				else {
					//il se passe rien
				}
				
			}
			get_image()
		}

	}
	/**Grille de taille i,j contenant les différentes cases*/
	class Echiquier(i:Int,j:Int,window:MainWindow) extends GridPanel(i,j){
		
		var Cells = ofDim[Case](9,9)
		for (i <- 8 to 1 by -1){
			for( j <- 1 to 8){
				Cells(i)(j) = new Case(i,j)
				Cells(i)(j).init_colors()
				Cells(i)(j).get_image()
				this.contents+=Cells(i)(j)
			}
		}

		def reset_colors()  = {
			for( i <- 8 to 1 by -1) {
				for( j <- 1 to 8) {
					Cells(i)(j).init_colors()
				}
			}
		}
		def set_images() ={
			for( i <- 8 to 1 by -1) {
				for( j <- 1 to 8) {
					Cells(i)(j).get_image()
				}
			}
		}


	}
	/**Ecran de jeu contenant l'échiquier de taille i,j*/
	class EcranPartie(i:Int,j:Int,window:MainWindow) extends BoxPanel(Orientation.Vertical){

		var plateau = new Echiquier(i,j,window)
		val back_menu = new Button{
			action = Action("Back to main menu"){
				//Projet.partie.stop()
				window.init_menu()
			}
		}
		val quit_program = new QuitButton(window)

		def pat() = {
			println("pat")
			this.contents+= new Label ("pat")
		}

		def perdu(player:Char) = {
			//Projet.partie.stop()
			println("perdu")
			this.contents+= new Label (player+" a perdu")
		}

		def spawn_game():Unit = {
			Projet.partie.start()
			Projet.partie.partie_init()
			plateau.set_images()
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
		//il faudrait tester la resolution minimale sur d'autres ordis...
		top.minimumSize = new Dimension(700, 1000) //schwoon 1300*700
		RootWindow.init_menu()
	}