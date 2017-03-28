import swing._
import Array._
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
			window.box.contents.clear()
			var partie = new Partie()
			var interface_partie = new EcranPartie(8,8,window,partie)
			partie.partie_nb_ia(nbIA,colorIA,interface_partie)
			interface_partie.spawn_game()
		}
	}

	class VarPartieButton(text:String,nbIA:Int,colorIA:Char,window:MainWindow) extends Button{
		action = Action(text){
			window.box.contents.clear()
			var partie1 = new Partie()
			var partie2 = new Partie()
			var interface_partie1 = new EcranPartie(8,8,window,partie1)
			var interface_partie2 = new EcranPartie(8,8,window,partie2)
			partie1.partie_nb_ia(nbIA,colorIA,interface_partie1)
			partie2.partie_nb_ia(nbIA,colorIA,interface_partie2)
			interface_partie1.spawn_game()
			interface_partie2.spawn_game()
		}
	}
	class QuitButton(window:MainWindow,partie:Partie) extends Button{
		action = Action("Quit Game"){
			window.closeOperation()
			if (partie != null){
				partie.stop()
			}
		}
	}

	class SettingsButton(window:MainWindow) extends Button{
		action = Action("Settings"){
			//window.closeOperation()
			//if (partie != null){
				//	partie.stop()
				//}
			}
		}
		/**Menu principal*/
		class MainMenu(window:MainWindow) extends GridPanel(7,1){

			/**bouton qui lance une partie avec un seul joueur blanc*/
			val game_one_player_white = new PartieButton("Player White vs. IA Black",1,'B',window)
			/**bouton qui lance une partie avec un seul joueur noir*/
			val game_one_player_black = new PartieButton("Player Black vs. IA White",1,'W',window)
			/**bouton qui lance une partie avec deux joueurs*/
			val game_two_players = new PartieButton("Player vs. Player",0,'0',window)
			/**bouton qui lance une partie avec deux ia*/
			val game_two_ia = new PartieButton("IA vs. IA",2,'0',window)

			val game_var = new VarPartieButton("VAR",2,'0',window)
			/** */
			val settings_butt = new SettingsButton(window)
			/**bouton qui ferme l'interface*/
			val quit_program = new QuitButton(window,null)
			/**affiche le menu principal*/
			def set_menu():Unit = {
				contents.clear()
				contents+= game_two_players
				contents+= game_one_player_black
				contents+= game_one_player_white
				contents+= game_two_ia
				contents+= game_var
				contents+= settings_butt
				contents+= quit_program
				revalidate()
				repaint()
			}
		}

		class SettingsMenu(window:MainWindow) extends BoxPanel(Orientation.Vertical){

		}

		class MainWindow() extends MainFrame{
			var meta_box = new BoxPanel(Orientation.Vertical) 
			var box = new BoxPanel(Orientation.Horizontal) 
			var under_box = new BoxPanel(Orientation.Horizontal) 
			var menu_principal = new MainMenu(this)		
			def init_menu()={
				title = "Chess"
				contents = meta_box
				meta_box.contents.clear()
				meta_box.contents+= box 
				meta_box.contents+= under_box
				under_box.contents.clear()
				under_box.revalidate()
				under_box.repaint()
				box.contents.clear()
				box.contents+= menu_principal
				menu_principal.set_menu()
				box.revalidate()
				box.repaint()
				meta_box.revalidate()
				meta_box.repaint()
			}
		}

		/**Case de l'échiquier*/
		class Case(i:Int,j:Int,plateau:Echiquier,partie:Partie) extends Button{
			val myGreen = new Color (48, 163, 115)
			val myBlue = new Color (0, 3, 112)
			val myRed = new Color (112, 0, 0)
			var piece = partie.matrix(i)(j)
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
				piece = partie.matrix(i)(j)
				if (piece != null){icon = piece.image}
				else {icon = null}
				this.revalidate()
				this.repaint()
			}

			def select_piece(){
				piece_selected = partie.get_piece(i,j)
				colorie("green")
				var(piece_move,piece_take) = partie.get_piece(i,j).move_piece_check(i,j)
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

			get_image()
			action = Action(""){
				if (partie.is_running && partie.is_interface){
					if (is_clicked){ 
						plateau.reset_all()
					}
					else if (is_button_clicked){
						if (piece_allowed_move.contains(i,j)){
							piece_selected.move((i,j))
							//partie.next_turn()
							plateau.reset_all()
						}
					}
					else if (partie.get_color(i,j) == partie.player){
						select_piece()
						clic()
					}
					else {
						//il ne se passe rien
					}

				}
			}
			this.maximumSize = new Dimension(100,100)
		}
		/**Grille de taille i,j contenant les différentes cases*/
		class Echiquier(i:Int,j:Int,window:MainWindow,partie:Partie) extends GridPanel(i,j){

			var Cells = ofDim[Case](9,9)
			for (i <- 8 to 1 by -1){
				for( j <- 1 to 8){
					Cells(i)(j) = new Case(i,j,this,partie)
					this.contents+=Cells(i)(j)
				}
			}
			reset_all()
			def reset_all() = {
				for( i <- 8 to 1 by -1) {
					for( j <- 1 to 8) {
						Cells(i)(j).is_clicked = false
						Cells(i)(j).init_colors()
						Cells(i)(j).get_image()
					}
				}
				button_clicked_i = 0
				button_clicked_j = 0
				is_button_clicked = false
			}
		}

		class PieceButton(posi:(Int,Int),color:Char,piece:Piece,piece_type:String,notif:Notification,partie:Partie) extends Button {
			this.action = Action(""){
				piece.asInstanceOf[Peon].promo(posi,piece_type,partie)
				partie.waiting = false
				partie.next_turn()
				notif.initial()
			}
		}

		class PiecePanel(posi:(Int,Int),color:Char,piece:Piece,notif:Notification,partie:Partie) extends GridPanel(1,4) {
			val queen = new PieceButton(posi,color,piece,"Qu",notif,partie)
			this.contents+= queen
			queen.icon = Tools.icon_resized(color+"Qu.PNG",30,30)
			val bishop = new PieceButton(posi,color,piece,"Bi",notif,partie)
			this.contents+=bishop
			bishop.icon = Tools.icon_resized(color+"Bi.PNG",30,30)
			val knight = new PieceButton(posi,color,piece,"Kn",notif,partie)
			this.contents+=knight
			knight.icon = Tools.icon_resized(color+"Kn.PNG",30,30)
			val tower = new PieceButton(posi,color,piece,"To",notif,partie)
			this.contents+=tower
			tower.icon = Tools.icon_resized(color+"To.PNG",30,30)
			this.revalidate()
			this.repaint()
		}
		class TextAreaEnd(color:Char,type_end:String,complement:String) extends GridPanel(2,1){
			println(color+" "+type_end+" "+complement)
			type_end match {
				case "MAT" => {
					this.background = java.awt.Color.RED
					color match {
						case 'W' => this.contents+= new Label(){text = "Le joueur Blanc a perdu\n" } 
						case 'B' => this.contents+= new Label(){text = "Le joueur Noir a perdu\n" } 
					}
				}
				case "PAT" => {
					this.background = java.awt.Color.PINK
					this.contents+= new Label(){text = "PAT\n" } 
				}
				case _ => {"\n"}
			}

			complement match {

				case "50" => this.contents+= new Label(){text = "Cause : règle des 50 coups" }  
				case "3" => this.contents+= new Label(){text = "Cause : 3 fois la même position" }  
				case "temps" => this.contents+= new Label(){text = "Cause : temps écoulé" }
				case "nulle" => this.contents+= new Label(){text = "Cause : impossibilité de mater"}
				case "Pat" =>this.contents+= new Label(){text = "Cause : plus aucun mouvements possibles"}
				case _ => {" "}
			}
			this.revalidate()
			this.repaint()
		}
		class Notification(partie:Partie) extends GridPanel(3,1){
			def initial():Unit = {
				this.contents.clear()
				if (Config.return_allowed){
					var retour = new Button(){
						action = Action("Return"){
							partie.return_back(partie)
						}
					}
					this.contents+= new FlowPanel(retour)
				}
				var save_game = new Button(){
					action = Action("Save_game"){
						// SAVE FUNCTION
					}
				}
				this.contents+= new FlowPanel(save_game)
				text_init()
				this.revalidate()
				this.repaint()
			}
			initial()
			val notif = this
			if (Config.timer){
				var start_time = new Button(){
					action = Action("Start Timer"){
						partie.is_interface = true
						partie.start()
						partie.white_timer.interrupt()
						notif.initial()
					}
				}
				this.contents+=start_time
				this.revalidate()
				this.repaint()
			}


			def promote(posi:(Int,Int),color:Char,piece:Piece) = {
				partie.waiting = true
				this.contents+= new FlowPanel(new PiecePanel(posi,color,piece,this,partie))
				this.revalidate()
				this.repaint()
			}
			def text_init():Unit = {
				this.contents+= new FlowPanel(new TextAreaEnd('0',"",""))
				this.revalidate()
				this.repaint()
			}
			def text_end(color:Char,type_end:String,complement:String):Unit = {
				initial()
				partie.type_end = (type_end,color)
				println(partie.type_end)
				this.contents+= new FlowPanel(new TextAreaEnd(color,type_end,complement))
				this.revalidate()
				this.repaint()
			}

		}
		class TimerDisplay(color:Char) extends Label {
			def set(time:(Int,Int,Int)) = {
				var (hour,min,sec) = time
				this.text = color+": "+hour+":"+min+":"+sec
				this.revalidate()
				this.repaint()
			}
		}
		class HeadUpBar(partie:Partie) extends GridPanel(3,1) {
			var white_timer = new TimerDisplay('W')
			this.contents+= white_timer
			var notif = new Notification(partie)
			this.contents+= notif
			var black_timer = new TimerDisplay('B')
			this.contents+= black_timer
			this.revalidate()
			this.repaint()

			def edit_timer(color:Char,time:(Int,Int,Int)) = {
				color match {
					case 'W' => white_timer.set(time)
					case 'B' => black_timer.set(time)
				}
			}
		}
		/**Ecran de jeu contenant l'échiquier de taille i,j*/
		class EcranPartie(i:Int,j:Int,window:MainWindow,partie:Partie) extends FlowPanel(){
			var head_up_bar = new HeadUpBar(partie)
			var plateau = new Echiquier(i,j,window,partie)
			val back_menu = new Button{
				action = Action("Back to main menu"){
					partie.stop()
					window.init_menu()

				}
			}

			val quit_program = new QuitButton(window,partie)
			def spawn_game():Unit = {
				is_button_clicked = false
				button_clicked_i = 0
				button_clicked_j = 0
				piece_selected = null
				piece_allowed_move = List()
				piece_allowed_take = List()
				partie.partie_init()
				if (!Config.timer){
					partie.is_interface = true
					partie.start()
				}
				plateau.reset_all()
				window.box.contents += this
				window.under_box.contents.clear()
				window.under_box.contents += new GridPanel(1,2){
					contents+= back_menu
					contents+= quit_program
				}
				this.contents+= new BoxPanel(Orientation.Vertical){
					contents+= new FlowPanel(head_up_bar)
					contents+= new FlowPanel(plateau)
				}
				revalidate()
				repaint()
			}	
		}
		var RootWindow = new MainWindow()
		def top = RootWindow
		//il faudrait tester la resolution minimale sur d'autres ordis...
		top.preferredSize = new Dimension(Config.res_x,Config.res_y) //schwoon 1300*700
		RootWindow.init_menu()
	}
