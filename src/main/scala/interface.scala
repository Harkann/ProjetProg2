import swing._
import Array._
import javax.swing.ImageIcon
import java.awt.Color

object Interface extends SimpleSwingApplication{

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
		}
	}

	class MainWindow() extends MainFrame{
		var box = new BoxPanel(Orientation.Vertical) 
		var menu_principal = new MainMenu(5,1,this)
		var interface_partie = new EcranPartie(8,8,this)
		//init()
		
		def init_menu()={
			title = "Chess"
			contents = box
			box.contents.clear()
			box.contents+=menu_principal
			menu_principal.set_menu()
			box.revalidate()
			box.repaint()
		}
	}

	/**Case de l'échiquier*/
	class Case(i:Int,j:Int,plateau:Echiquier) extends Button{
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
			else {icon = null}
			this.revalidate()
			this.repaint()
		}

		def unclic(){
			var a = button_clicked_i
			var b = button_clicked_j
			button_clicked_i = 0
			button_clicked_j = 0
			if (a>0 && b>0){plateau.Cells(a)(b).unclic()}
			is_clicked = false
			is_button_clicked = false
			background = java.awt.Color.RED
			get_image()
			plateau.reset_colors()
		}

		def select_piece(){
			piece_selected = Projet.partie.get_piece(i,j)
			colorie("green")
			var(piece_move,piece_take) = Projet.partie.get_piece(i,j).move_piece_check(i,j)
			piece_allowed_move = piece_move
			piece_allowed_take = piece_take
			for ((a,b) <- piece_allowed_move){ plateau.Cells(a)(b).colorie("blue") }
			for ((a,b) <- piece_allowed_take){ plateau.Cells(a)(b).colorie("red") }
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
			if (Projet.partie.is_running && Projet.partie.is_interface){
				if (is_clicked){ unclic() }
				else if (is_button_clicked){
					if (piece_allowed_take.contains(i,j)){
						println ("je veux attaquer ")
						piece_selected.delete(i,j)
						println ("j'ai pris la piece normalement ")
						unclic()
					}
					else if (piece_allowed_move.contains(i,j)){
						piece_selected.move(i,j)
						unclic()
					}
				}
				else if (Projet.partie.get_color(i,j) == Projet.partie.player){
					select_piece()
					clic()
				}
				else {
					//il ne se passe rien
				}
				
			}
		}

	}
	/**Grille de taille i,j contenant les différentes cases*/
	class Echiquier(i:Int,j:Int,window:MainWindow) extends GridPanel(i,j){
		
		var Cells = ofDim[Case](9,9)
		for (i <- 8 to 1 by -1){
			for( j <- 1 to 8){
				Cells(i)(j) = new Case(i,j,this)
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
			is_button_clicked = false
			button_clicked_i = 0
			button_clicked_j = 0
			piece_selected = null
			piece_allowed_move = List()
			piece_allowed_take = List()
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
	var RootWindow = new MainWindow()
	def top = RootWindow
	//il faudrait tester la resolution minimale sur d'autres ordis...
	top.minimumSize = new Dimension(700, 1000) //schwoon 1300*700
	RootWindow.init_menu()
}