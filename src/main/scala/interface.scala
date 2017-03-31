import swing._
import Array._
import java.awt.Color

object Interface extends SimpleSwingApplication{
	/**un bouton a été actionné*/
	var is_button_clicked = false
	/** */
	var is_ba4_button = false
	/** */
	var type_ba4_button = ""
	/**coordonnées du bouton actionné, ou 0*/
	var button_clicked_i = 0
	/**coordonnées du bouton actionné, ou 0*/
	var button_clicked_j = 0
	/**Objet piece sélectionnée, ou null*/
	var piece_selected:Piece = null
	/**Mouvements possibles de l'objet pièce*/
	var piece_allowed_move:List[(Int,Int)] = List()
	/**Prises possibles de l'objet pièce*/
	var piece_allowed_take:List[(Int,Int)] = List()
	/**Partie 1 dans le cas d'un Ba4 */
	var piece_allowed_arrive:List[(Int,Int)] = List()
	var partie1:Partie = null
	/**Partie 1 dans le cas d'un Ba4 */
	var partie2:Partie = null
	/** infos sur la variante*/
	var ba4:Blitz_4 = null
	/**Boutons permettant de lancer la partie classique*/
	class PartieButton(text:String,nbIA:Int,colorIA:Char,window:MainWindow) extends Button{
		action = Action(text){
			Current_Config.type_partie = ""
			window.middle_box.contents.clear()
			/**Partie crée*/
			var partie = new Partie()
			/**Interface de la partie crée*/
			var interface_partie = new EcranPartie(8,8,window,partie)
			partie.partie_nb_ia(nbIA,colorIA,interface_partie)
			interface_partie.spawn_game()
		}
	}
	/**Boutons permettant de lancer la partie de Ba4*/
	class VarPartieButton(text:String,nbIA:Int,colorIA:Char,window:MainWindow) extends Button{
		action = Action(text){
			Current_Config.type_partie = "var"
			window.middle_box.contents.clear()
			partie1 = new Partie()
			partie1.numero = 1
			partie2 = new Partie()
			partie2.numero = 2
			ba4 = new Blitz_4(partie1,partie2)
			/**Interface de la partie1 crée*/
			var interface_partie1 = new EcranPartie(8,8,window,partie1)
			/**Interface de la partie2 crée*/
			var interface_partie2 = new EcranPartie(8,8,window,partie2)
			partie1.partie_nb_ia(nbIA,colorIA,interface_partie1)
			partie2.partie_nb_ia(nbIA,colorIA,interface_partie2)
			interface_partie1.spawn_game()
			interface_partie2.spawn_game()
		}
	}
	/**Bouton quittant la fenetre @window et stoppant @partie */
	class QuitButton(window:MainWindow,partie:Partie) extends Button{
		action = Action("Quit Game"){
			window.closeOperation()
			if (partie != null){
				partie.stop()
			}
			if (partie1 != null && partie2 != null){
				partie1.stop
				partie2.stop
			}
		}
	}
	/**NOT_IMPLEMENTED*/
	class SettingsButton(window:MainWindow) extends Button{
		action = Action("Settings"){}
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
		/**bouton qui lance une partie de Blitz à 4 */
		val game_var = new VarPartieButton("VAR",0,'0',window)
		/**bouton qui permet d'accéder aux paramètre NOT_IMPLEMENTED*/
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
	/**NOT IMPLEMENTED */
	class SettingsMenu(window:MainWindow) extends BoxPanel(Orientation.Vertical){

	}
	/**Ecran principal (contiendra l'integralité de l'interface) */
	class MainWindow() extends MainFrame{
		/**contient la totalité de l'interface */
		var meta_box = new BoxPanel(Orientation.Vertical) 
		/**contient le menu ou l'ecran de partie */
		var box = new BoxPanel(Orientation.Horizontal)
		var middle_box = new BoxPanel(Orientation.Horizontal)
		var left_box = new BoxPanel(Orientation.Vertical) 
		var right_box = new BoxPanel(Orientation.Vertical)
		box.contents+=left_box
		box.contents+=middle_box
		box.contents+=right_box
		box.revalidate
		box.repaint
		/**contient les boutons inferieurs en partie */
		var under_box = new BoxPanel(Orientation.Horizontal) 
		/**contient les notifications en partie */
		var upper_box = new BoxPanel(Orientation.Horizontal) 
		var mup_box = new BoxPanel(Orientation.Horizontal) 
		var lup_box = new BoxPanel(Orientation.Horizontal) 
		var rup_box = new BoxPanel(Orientation.Horizontal) 
		upper_box.contents+=lup_box
		upper_box.contents+=mup_box
		upper_box.contents+=rup_box
		upper_box.revalidate
		upper_box.repaint
		/**menu/ecran d'accueil */
		var menu_principal = new MainMenu(this)		
		/**affiche le menu principal*/
		def init_menu()={
			title = "Chess"
			contents = meta_box
			meta_box.contents.clear()
			meta_box.contents+= upper_box
			meta_box.contents+= new FlowPanel(box) 
			meta_box.contents+= under_box
			under_box.contents.clear()
			under_box.revalidate()
			under_box.repaint()
			lup_box.contents.clear()
			rup_box.contents.clear
			mup_box.contents.clear
			upper_box.revalidate()
			upper_box.repaint()
			middle_box.contents.clear()
			right_box.contents.clear()
			left_box.contents.clear()
			middle_box.contents+= new FlowPanel(menu_principal)
			menu_principal.set_menu()
			middle_box.revalidate()
			middle_box.repaint()
			meta_box.revalidate()
			meta_box.repaint()
		}
	}

	/**Case de l'échiquier*/
	class Case(i:Int,j:Int,plateau:Echiquier,partie:Partie) extends Button{
		/**couleur piece selectionnée*/
		val myGreen = new Color (48, 163, 115)
		/**couleur dpct possible*/
		val myBlue = new Color (0, 3, 112)
		/**couleur prise possible*/
		val myRed = new Color (112, 0, 0)
		/**objet piece a la position i,j*/
		var piece = partie.matrix(i)(j)
		/**bouton actionné*/
		var is_clicked = false
		/**clic sur la case*/
		def clic(){
			button_clicked_i = i
			button_clicked_j = j
			is_clicked = true
			is_button_clicked = true
		}
		/**recupere l'image de la piece sur la case*/
		def get_image(){
			piece = partie.matrix(i)(j)
			if (piece != null){icon = piece.image}
			else {icon = null}
			this.revalidate()
			this.repaint()
		}
		/**selectionne l'objet piece sur la case*/
		def select_piece(){
			piece_selected = partie.get_piece(i,j)
			colorie("green")
			var(piece_move,piece_take) = partie.get_piece(i,j).move_piece_check(i,j)
			piece_allowed_move = piece_move
			piece_allowed_take = piece_take
			for ((a,b) <- piece_allowed_move){ plateau.Cells(a)(b).colorie("blue") }
			for ((a,b) <- piece_allowed_take){ plateau.Cells(a)(b).colorie("red") }
		}
		/**initialise la couleur de la case(noir ou blanc)*/
		def init_colors() = {
			if((i+j)%2 == 0){ background = java.awt.Color.BLACK }
			else{ background = java.awt.Color.WHITE }
		}
		/**set la couleur de la case*/
		def colorie(couleur:String) ={
			if (couleur == "green"){ background = myGreen }
			else if (couleur == "red"){ background = myRed }
			else if (couleur == "blue"){ background = myBlue }
		}

		get_image()
		action = Action(""){
			if (partie.is_running && partie.is_interface){
				if (is_clicked){plateau.reset_all()}
				else if (is_ba4_button){
					if (piece_allowed_arrive.contains(i,j)){
						
						partie.numero match {
							case 1 => {
								type_ba4_button match {
									case "To" => {partie2.modif_lost_piece(partie.player,1,1)} 
									case "Bi" => {partie2.modif_lost_piece(partie.player,3,1)}
									case "Qu" => {partie2.modif_lost_piece(partie.player,4,1)}
									case "Pe" => {partie2.modif_lost_piece(partie.player,0,1)}
									case "Kn" => {partie2.modif_lost_piece(partie.player,2,1)}
								}
							}
							case 2 => {
								type_ba4_button match {
									case "To" => {partie1.modif_lost_piece(partie.player,1,1)} 
									case "Bi" => {partie1.modif_lost_piece(partie.player,3,1)}
									case "Qu" => {partie1.modif_lost_piece(partie.player,4,1)}
									case "Pe" => {partie1.modif_lost_piece(partie.player,0,1)}
									case "Kn" => {partie1.modif_lost_piece(partie.player,2,1)}
								}
							}
						}
						ba4.arrive(type_ba4_button,partie.player,(i,j),partie)
						plateau.reset_all()
						partie.game_window.pieces_W.reset_buttons
						partie.game_window.pieces_B.reset_buttons
					}
				}
				else if (is_button_clicked){
					if (piece_allowed_move.contains(i,j)){
						piece_selected.move((i,j))
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
				if (Current_Config.type_partie == "var"){
					println("plop")
					partie1.game_window.pieces_W.update_numbers
					partie1.game_window.pieces_B.update_numbers
					partie2.game_window.pieces_W.update_numbers
					partie2.game_window.pieces_B.update_numbers
				}

			}
		}

	}
	/**Grille de taille i,j contenant les différentes cases*/
	class Echiquier(i:Int,j:Int,window:MainWindow,partie:Partie) extends GridPanel(i,j){
		/**matrice des cases*/
		var Cells = ofDim[Case](9,9)
		for (i <- 8 to 1 by -1){
			for( j <- 1 to 8){
				Cells(i)(j) = new Case(i,j,this,partie)
				this.contents+=Cells(i)(j)
			}
		}
		reset_all()
		/**reinitialise les couleurs/images/selections*/
		def reset_all() = {
			for( i <- 8 to 1 by -1) {
				for( j <- 1 to 8) {
					Cells(i)(j).is_clicked = false
					Cells(i)(j).init_colors()
					Cells(i)(j).get_image()
				}
			}
			is_ba4_button = false
			type_ba4_button = ""
			button_clicked_i = 0
			button_clicked_j = 0
			is_button_clicked = false
		}
		this.maximumSize = new Dimension(Current_Config.res_x,Current_Config.res_y)
	}
	/**bouton pour promotion*/
	class PieceButton(posi:(Int,Int),color:Char,piece:Piece,piece_type:String,notif:Notification,partie:Partie) extends Button {
		this.action = Action(""){
			piece.asInstanceOf[Peon].promo(posi,piece_type,partie)
			partie.waiting = false
			partie.is_interface = true
			partie.next_turn()
			notif.initial()
		}
	}
	/**interface pour la promotion*/
	class PiecePanel(posi:(Int,Int),color:Char,piece:Piece,notif:Notification,partie:Partie) extends GridPanel(1,4) {
		/**promotion en reine*/
		val queen = new PieceButton(posi,color,piece,"Qu",notif,partie)
		this.contents+= queen
		queen.icon = Tools.icon_resized(color+"Qu.PNG",Tools.min_size/20,Tools.min_size/20)
		/**promotion en fou*/
		val bishop = new PieceButton(posi,color,piece,"Bi",notif,partie)
		this.contents+=bishop
		bishop.icon = Tools.icon_resized(color+"Bi.PNG",Tools.min_size/20,Tools.min_size/20)
		/**promotion en cavalier*/
		val knight = new PieceButton(posi,color,piece,"Kn",notif,partie)
		this.contents+=knight
		knight.icon = Tools.icon_resized(color+"Kn.PNG",Tools.min_size/20,Tools.min_size/20)
		/**promotion en tour*/
		val tower = new PieceButton(posi,color,piece,"To",notif,partie)
		this.contents+=tower
		tower.icon = Tools.icon_resized(color+"To.PNG",Tools.min_size/20,Tools.min_size/20)
		this.revalidate()
		this.repaint()
	}
	/**notification de fin de partie*/
	class TextAreaEnd(color:Char,type_end:String,complement:String,numero:Int) extends GridPanel(3,1){
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
			case _ => this.contents+= new Label(){text = " "}
		}
		complement match {
			case "50" => this.contents+= new Label(){text = "Cause : règle des 50 coups" }  
			case "3" => this.contents+= new Label(){text = "Cause : 3 fois la même position" }  
			case "temps" => this.contents+= new Label(){text = "Cause : temps écoulé" }
			case "nulle" => this.contents+= new Label(){text = "Cause : impossibilité de mater"}
			case "Pat" =>this.contents+= new Label(){text = "Cause : plus aucun mouvements possibles"}
			case _ => this.contents+= new Label(){text = " "}
		}
		if (Current_Config.type_partie == "var"){
			numero match {
				case 1 => this.contents+= new Label(){text = "Echiquier : 1"}
				case 0 => this.contents+= new Label(){text = "Echiquier : 2"}
				case _ => this.contents+= new Label(){text = " "}
			}
		}
		this.revalidate()
		this.repaint()
	}
	/**NOT IMPLEMENTED*/
	class DispoButton(color:Char,piece_type:String,container:Pieces_dispo,partie:Partie) extends Button {
		var number = 0
		var is_clicked = false
		this.background = java.awt.Color.WHITE
		action = Action("") {
			if (partie.is_running){
				if (partie.is_interface && partie.player == color){
					if (is_clicked == false){
						is_clicked = true
						is_ba4_button = true
						this.background = new Color (48, 163, 115)
						type_ba4_button = piece_type
						piece_allowed_arrive = ba4.arrive_possible(piece_type,partie)
						for ((x,y) <- piece_allowed_arrive) {partie.game_window.plateau.Cells(x)(y).colorie("blue")} 
						set_number
					}
					else {
						is_clicked = false
						this.background = java.awt.Color.WHITE
					}
				}
			}
		}
		this.icon = Tools.icon_resized(color+piece_type+".PNG",Tools.min_size/30,Tools.min_size/30)
		this.text = number.toString()
		def set_number() = {
			println("plop3")
			piece_type match {
				case "To" => {number = container.liste_pieces(1)} 
				case "Bi" => {number = container.liste_pieces(3)}
				case "Qu" => {number = container.liste_pieces(4)}
				case "Pe" => {number = container.liste_pieces(0)}
				case "Kn" => {number = container.liste_pieces(2)}
			}
			this.text = number.toString()
			println(number)
			if (number == 0){this.enabled = false}
			else{this.enabled = true}
			this.revalidate
			this.repaint
		}
		set_number

	}
	/**Pieces disponibles sur l'echiquier*/
	class Pieces_dispo(partie:Partie,color:Char) extends GridPanel(5,1){
		var liste_pieces:Array[Int] = null
		def set_liste() = {
			(partie.numero,color) match {
				case (1,'W') =>{liste_pieces = ba4.dispo_G_W}
				case (1,'B') =>{liste_pieces = ba4.dispo_G_B}
				case (2,'W') =>{liste_pieces = ba4.dispo_D_W}
				case (2,'B') =>{liste_pieces = ba4.dispo_D_B}
			}
		}
		set_liste
		val queen = new DispoButton(color,"Qu",this,partie)
		this.contents+= queen
		val bishop = new DispoButton(color,"Bi",this,partie)
		this.contents+=bishop
		val knight = new DispoButton(color,"Kn",this,partie)
		this.contents+=knight
		val tower = new DispoButton(color,"To",this,partie)
		this.contents+=tower
		val peon = new DispoButton(color,"Pe",this,partie)
		this.contents+=peon
		this.revalidate()
		this.repaint()
		def reset_buttons = {
			queen.is_clicked = false
			bishop.is_clicked = false
			knight.is_clicked = false
			tower.is_clicked = false
			peon.is_clicked = false
			queen.background = java.awt.Color.WHITE
			bishop.background = java.awt.Color.WHITE
			knight.background = java.awt.Color.WHITE
			tower.background = java.awt.Color.WHITE
			peon.background = java.awt.Color.WHITE
		}
		def update_numbers() = {
			ba4.update_listes
			set_liste
			println("plop2 "+color+liste_pieces(0)+liste_pieces(1)+liste_pieces(2)+liste_pieces(3)+liste_pieces(4)+liste_pieces(5))
			queen.set_number
			bishop.set_number
			knight.set_number
			tower.set_number
			peon.set_number
		}
	}
	/**Notifications*/
	class Notification(partie:Partie) extends BoxPanel(Orientation.Vertical){
		/**le timer est-il activé ?*/
		var timer = true
		/**this*/
		var notif = this
		/**initialise les notifications et les boutons en fonction des parametres*/
		def initial():Unit = {
			this.contents.clear()
			if (Current_Config.type_partie != "var"){
				/**annule le dernier mouvement*/
				var retour = new Button(){
					action = Action("Return"){partie.return_back(partie)}
					enabled = Current_Config.return_allowed
				}
				this.contents+= new FlowPanel(retour)
				/**sauvegarde la partie au format PGN*/
				var save_game = new Button(){
					action = Action("Save_game"){partie.save_to_PGN(partie,"*",partie.player,"save.txt")}
				}
				this.contents+= new FlowPanel(save_game)
				/**demarre le timer*/
				var start_time = new Button(){
					action = Action("Start Timer"){
						partie.is_interface = true
						partie.start()
						partie.white_timer.interrupt()
						timer = false
						notif.initial()
						notif.revalidate()
						notif.repaint()
					}
					enabled = Current_Config.timer && notif.timer
				}
				this.contents+= new FlowPanel(start_time)
			}
			else{
				/**demarre le timer (partie de Ba4)*/
				var start_time = new Button(){
					action = Action("Start Timer"){
						partie1.is_interface = true
						partie2.is_interface = true
						partie1.start()
						partie2.start()
						partie1.white_timer.interrupt()
						partie2.white_timer.interrupt()
						timer = false
						notif.initial()
						notif.revalidate()
						notif.repaint()
					}
					enabled = Current_Config.timer && notif.timer
				}
				this.contents+= new FlowPanel(start_time)
			}
		}
		initial()
		this.revalidate()
		this.repaint()
		/**affiche l'interface de promotion*/
		def promote(posi:(Int,Int),color:Char,piece:Piece) = {
			partie.waiting = true
			partie.is_interface = false
			this.contents+= new FlowPanel(new PiecePanel(posi,color,piece,this,partie))
			this.revalidate()
			this.repaint()
		}
		/**affiche le texte de fin de partie @type_end : "MAT" ou "PAT", @complement : raison de la fin de partie*/
		def text_end(color:Char,type_end:String,complement:String,num:Int):Unit = {
			initial()
			partie.type_end = (type_end,color)
			partie.save_to_PGN(partie,type_end,color,"save.txt")
			if (Current_Config.type_partie != "var"){this.contents+= new FlowPanel(new TextAreaEnd(color,type_end,complement,0))}
			else {
				if (partie.numero == 1){partie2.game_window.head_up_bar.notif.text_end(color,type_end,complement,1)}
				else{this.contents+= new FlowPanel(new TextAreaEnd(color,type_end,complement,num))}
				this.revalidate()
				this.repaint()
				partie1.stop()
				partie2.stop()
			}
			this.revalidate()
			this.repaint()
		}
	}
	/**affichage du temps restant*/
	class TimerDisplay(color:Char,bar:HeadUpBar) extends GridPanel(2,1) {
		color match {
			case 'W' => this.contents+= new FlowPanel(new Label(){text = "Blanc"})
			case 'B' => this.contents+= new FlowPanel(new Label(){text = "Noir"})
		}
		var temps = new Label()
		this.contents+=new FlowPanel(temps)
		def set(time:(Int,Int,Int)) = {
			/**temps restant*/
			var (hour,min,sec) = time
			color match {
				case 'W' => temps.text = hour+":"+min+":"+sec
				case 'B' => temps.text = hour+":"+min+":"+sec
			}

			temps.revalidate
			temps.repaint
		}
	}
	/**contient boutons + notifs + timers*/
	class HeadUpBar(partie:Partie) extends BoxPanel(Orientation.Horizontal) {
		/**timer blanc*/
		var white_timer = new TimerDisplay('W',this)
		this.contents+= new FlowPanel(white_timer)
		/**notifications*/
		var notif = new Notification(partie)
		var promotion:PiecePanel = null
		if (Current_Config.type_partie != "var"){this.contents+= new FlowPanel(notif)}
		/**timer noir*/
		var black_timer = new TimerDisplay('B',this)
		this.contents+= new FlowPanel(black_timer)
		/**met a jour le temps d'un timer*/
		def edit_timer(color:Char,time:(Int,Int,Int)) = {
			color match {
				case 'W' => white_timer.set(time)
				case 'B' => black_timer.set(time)
			}
		}
	}
	/**Ecran de jeu contenant l'échiquier de taille i,j*/
	class EcranPartie(i:Int,j:Int,window:MainWindow,partie:Partie) extends FlowPanel(){
		var pieces_W:Pieces_dispo = null
		var pieces_B:Pieces_dispo = null
		var head_up_bar = new HeadUpBar(partie)
		var plateau = new Echiquier(i,j,window,partie)
		val back_menu = new Button{
			action = Action("Back to main menu"){
				partie.stop()
				if (partie1 != null && partie2 != null){
					partie1.stop
					partie2.stop
				}
				window.init_menu()
			}
		}
		if (Current_Config.type_partie == "var"){
			pieces_W = new Pieces_dispo(partie,'W')
			pieces_B = new Pieces_dispo(partie,'B')
		}
		val quit_program = new QuitButton(window,partie)
		/**initialise l'interface, la partie et lance la partie*/
		def spawn_game():Unit = {
			is_button_clicked = false
			button_clicked_i = 0
			button_clicked_j = 0
			piece_selected = null
			piece_allowed_move = List()
			piece_allowed_take = List()
			partie.partie_init()
			if (!Current_Config.timer){
				partie.is_interface = true
				partie.start()
			}
			plateau.reset_all()
			window.middle_box.contents += this
			window.mup_box.contents.clear()
			window.lup_box.contents.clear
			window.rup_box.contents.clear
			if (Current_Config.type_partie == "var"){
				window.lup_box.contents += new FlowPanel(partie1.game_window.head_up_bar)
				window.lup_box.revalidate
				window.lup_box.repaint
				window.rup_box.contents += new FlowPanel(partie2.game_window.head_up_bar)
				window.rup_box.revalidate
				window.rup_box.repaint
				window.mup_box.contents += new FlowPanel(partie2.game_window.head_up_bar.notif)
				window.mup_box.revalidate
				window.mup_box.repaint
				partie.numero match {
					case 1 => {
						window.left_box.contents.clear
						window.left_box.contents += new FlowPanel(pieces_W)
						window.left_box.contents += new FlowPanel(pieces_B)
					}
					case 2 => {
						window.right_box.contents.clear
						window.right_box.contents += new FlowPanel(pieces_W)
						window.right_box.contents += new FlowPanel(pieces_B)
					}
				}
			}
			else {
				window.mup_box.contents += new FlowPanel(head_up_bar)
			}

			window.under_box.contents.clear()
			window.under_box.contents += new GridPanel(1,2){
				contents+= back_menu
				contents+= quit_program
			}
			this.contents+= new FlowPanel(plateau)
			revalidate()
			repaint()
		}	
	}

	var RootWindow = new MainWindow()
	def top = RootWindow
	Current_Config.init_config()
	top.preferredSize = new Dimension(Current_Config.res_x,Current_Config.res_y) //schwoon 1300*700
	RootWindow.init_menu()
}