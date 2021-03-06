import java.io._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source


class Dpct(p_begin:(Int,Int),p_end:(Int,Int),partie:Partie){
	val posi_begin = p_begin
	var posi_end = p_end
	var (i,j) = p_begin
	var (x,y) = p_end
	val piece = partie.matrix(i)(j)
	val piece_met = partie.matrix(x)(y)
	var optional_other_dpct : Dpct = null
	var is_roque = ""
	var promotion = ""
	var echec_other_player = ""

	def do_dpct(matrix:Array[Array[Piece]]){
		matrix(i)(j) = null
		matrix(x)(y) = piece
		if (piece != null){piece.position =(x,y)}
		if (optional_other_dpct != null){
			optional_other_dpct.do_dpct(matrix)
		}
		if ((piece != null)&&(promotion != "")&&(piece.name=="Pe")){
			piece.asInstanceOf[Peon].promo(p_end,promotion,partie)
		}
	}

	def undo_dpct(matrix:Array[Array[Piece]]){
		matrix(i)(j) = piece
		matrix(x)(y) = piece_met
		if (piece != null){piece.position =(i,j)}
		if (piece_met != null){piece_met.position =(x,y)}
		if (optional_other_dpct != null) {
			optional_other_dpct.undo_dpct(matrix)
		}
	}
}

trait Save {
	val pgn = Array("","R","N","B","Q","K")
	def pgn_to_num(promotion:String):Int = {
		for(i<-0 to 5){
			if (pgn(i) == promotion){
				return i
			}
		}
		return 6
	}

	def return_back(partie:Partie) = {
	val dpct = partie.dplct_save.remove(partie.nb_turn-1)
	dpct.piece.nb_turn -=1
	dpct.piece.position = dpct.posi_begin
	val piece = partie.matrix(dpct.x)(dpct.y)
	partie.matrix(dpct.i)(dpct.j)= dpct.piece
	partie.matrix(dpct.x)(dpct.y)= dpct.piece_met
	//println("piece = "+piece.name)
	if (dpct.promotion != "") {
		//println("promotion")
		partie.modif_piece(dpct.piece.color,pgn_to_num(dpct.promotion),-1)
		partie.modif_piece(dpct.piece.color,0,1)
	}
	else if (dpct.piece_met != null){
		partie.modif_piece(dpct.piece_met.color,dpct.piece_met.num_type,1)
		partie.modif_lost_piece(dpct.piece_met.color,dpct.piece_met.num_type,1)
	}
	if (dpct.optional_other_dpct != null){
		val other_dpct = dpct.optional_other_dpct
		if (other_dpct.piece != null){
			other_dpct.piece.position = other_dpct.posi_begin
			other_dpct.piece.nb_turn -=1}
		if(other_dpct.piece_met != null){
			partie.matrix(other_dpct.i)(other_dpct.j)= other_dpct.piece
			partie.matrix(other_dpct.x)(other_dpct.y)= other_dpct.piece_met
			partie.modif_piece(other_dpct.piece_met.color,other_dpct.piece_met.num_type,1)
			partie.modif_lost_piece(other_dpct.piece_met.color,other_dpct.piece_met.num_type,1)
		}

	}
	partie.nb_turn -=2
	//partie.is_running = true
	//partie.game_window.head_up_bar.notif.initial()
	partie.next_turn()
	partie.game_window.plateau.reset_all()
	}

	def last_move(partie:Partie) : Dpct = {
		return partie.dplct_save(partie.dplct_save.length-1)
	}
	
}


trait Conversion_to_PGN {
	val lettre = Array('z','a','b','c','d','e','f','g','h')
	def save_to_PGN(partie:Partie,type_end:String,color:Char,file_name:String) = {
		val writer = new PrintWriter(new File(file_name ))
		var texte = ""
		//println(partie.dplct_save)
		for( i <- 0 to partie.dplct_save.length-1) {
			//println("chips?")
			if (i%2 == 0) { texte+=((i/2+1)+". ")}
			val dpct = partie.dplct_save(i)
			//println(dpct)
			if (dpct != null){
			if (dpct.is_roque != ""){
				texte += dpct.is_roque
			}
			else if (dpct.promotion != ""){
				texte += (lettre(dpct.x))+""+dpct.y+"="+dpct.promotion
			}
			else {
				//println("save_to_PGN : "+dpct.piece)
				texte += dpct.piece.PGN_name
				//println("chips?")
				if (dpct.piece_met != null) {
					texte += "x"
				}
				texte += ""+(lettre(dpct.i))+dpct.j+(lettre(dpct.x))+dpct.y
			}
			texte+=" "
			if (dpct.echec_other_player != "") {
				texte += dpct.echec_other_player+ " "
			}}
		}
		//println("c'est fini!")
		type_end match {
				case "MAT" => {
					color match {
						case 'W' => {texte += " 0-1 " } 
						case 'B' => {texte += " 1-0 " } 
					}
				}
				case "PAT" => {
					texte += " 1/2-1/2 "
				}
				case _ =>  {texte+=" * "}
			}
		writer.write(texte)
		writer.close()
	}
	
	def load(partie:Partie,texte:String) = {
		val len = texte.length
		var index = 0
		var dpct :Dpct = null 
		var is_comment = false
		while (index < len) {
			texte(index) match {
				case '[' | '{' => {
					is_comment = true
					index +=1
				}
				case ']' | '}' => {
					is_comment = false
					index +=1
				}
				case _ if (is_comment) => { index += 1 }

				case '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' =>{
					var (new_index1,new_nb_turn) = get_nb_turn(texte,index)
					partie.nb_turn = 2*new_nb_turn
					var (dpct_W,new_index2) = read_move(texte,new_index1,'W',partie)
					dpct_W.do_dpct(partie.matrix)
					partie.dplct_save += dpct_W
					partie.nb_turn +=1
					var (dpct_B,new_index3) = read_move(texte,new_index2,'B',partie)
					dpct_B.do_dpct(partie.matrix)
					partie.dplct_save += dpct_B
					index = new_index3
				}
				case '*' => index = len
				case _ => index+=1 

			}
		}
	}

	def get_nb_turn(texte:String,beginning_nbturn:Int):(Int,Int) ={
		var index = beginning_nbturn
		var res = 0
		while (texte(index) !='.'){
			res = res*10 + texte(index).toInt -48
			index+=1
		}
		return (index+2,res)
	}

	def read_dpct(texte:String,index:Int) : (Int,Int,Int,Int,Int) = {
		texte(index) match {
			case  '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8' =>{
				var i = 0
				var j = texte(index).toInt-48
				var x = line_to_int(texte(index+1))
				var y = texte(index+2).toInt-48
				var new_index = index+3
				return (i,j,x,y,new_index)
			}
			case  'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h' =>{
				texte(index+1) match {
					case 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h' => {
						var i = line_to_int(texte(index))
						var j = 0
						var x = line_to_int(texte(index+1))
						var y = texte(index+2).toInt-48
						var new_index = index+3
						return (i,j,x,y,new_index)
					}
					case '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8' =>{
						texte(index+2) match {
							case 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h' =>{
								var i = line_to_int(texte(index))
								var j = texte(index+1).toInt-48
								var x = line_to_int(texte(index+2))
								var y = texte(index+3).toInt-48
								var new_index = index+4
								return (i,j,x,y,new_index)
							}
							case ' ' =>{
								var i = 0
								var j = 0
								var x = line_to_int(texte(index))
								var y = texte(index+1).toInt-48
								var new_index = index+2
								return (i,j,x,y,new_index)

							} 
							case '=' => {
								var i = 0
								var j = 0
								var x = line_to_int(texte(index))
								var y = texte(index+1).toInt-48
								var new_index = index+2
								return (i,j,x,y,new_index)
							}
						}
						
					} 

				}
			} 
			case _ => read_dpct(texte,index+1)
		}
	}


	def read_move(texte:String,index:Int,player:Char,partie:Partie) : (Dpct,Int) = {
		var dpct : Dpct = null
		var n_index = 0
		texte(index) match {
			case 'O' => {
				return read_roque(texte,index,player,partie)
			} 
			case 'K'|'Q'|'B'|'N'|'R' => {
				println("chipster")
				var (i,j,x,y,new_index) = read_dpct(texte,index+1)
				var piece = read_find(texte(index).toString,i,j,x,y,partie)
				dpct= new Dpct(piece.position,(x,y),partie)
				println("read_move : "+dpct.piece)
				n_index = new_index

				return (dpct,n_index)

			}
			case  'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h' =>{
				var (i,j,x,y,new_index) = read_dpct(texte,index)
				var piece = read_find("",i,j,x,y,partie)
				println(piece)
				dpct= new Dpct(piece.position,(x,y),partie)
				println("read_move : "+dpct.piece)
				if (texte(new_index) == '='){
					dpct.promotion=texte(new_index+1).toString
				}
				n_index = new_index+2

				return (dpct,n_index)
			}
			case _ =>return read_move(texte,index+1,player,partie) 
		}
	}

	def read_roque(texte:String,i:Int,player:Char,partie:Partie):(Dpct,Int) = {
		var ligne = 0
		if (player == 'B') {ligne = 8} else {ligne=1}
		var dpct : Dpct = null
		var new_index = i
		if (texte(i+3) == '-'){
			dpct = new Dpct((ligne,5),(ligne,3),partie)
			dpct.optional_other_dpct = new Dpct((i,1),(i,4),partie)
			dpct.is_roque = "O-O-O"
			new_index = i+5
		}
		else {
			dpct = new Dpct((ligne,5),(ligne,7),partie)
			dpct.optional_other_dpct = new Dpct((i,8),(i,6),partie)
			dpct.is_roque = "O-O"
			new_index = i+3
		}
		return (dpct,new_index)
	}


	def int_to_line(i:Int) : Char = {
		return (97+i-1).toChar
	}

	def line_to_int(line:Char) : Int = {
		return line.toInt-96
	}

	def read_find(piece_type:String,i:Int,j:Int,x:Int,y:Int,partie:Partie) : Piece = {
		println("i: "+i+" j: "+j+" x:"+x+" y: "+y)
		println(piece_type)
		for (ligne <- 1 to 8 ){
			for (colonne <- 1 to 8){
				var piece = partie.matrix(ligne)(colonne)
				//println(piece)
				if (piece != null){
				var (moves,attacks) = piece.move_piece_check((ligne,colonne))
				//println("ligne "+ligne+" colonne "+colonne)
				//println("i "+i+" j "+j)
				//println(piece.PGN_name+"|"+piece_type)
				println(moves)
				if ((piece.PGN_name == piece_type) &&
					((i==0)||(ligne==i))&&
					((j==0)||(colonne==j))&&
					(moves.contains((x,y)))){
					println("J'ai trouvé la piece")
					return piece
				}
				}

			}
		}
		return null
	}

	def read_test() {

	var partie = new Partie
	partie.partie_init()
    
    for (line <- Source.fromFile("load.txt").getLines){
    	load(partie,line)
    }
    save_to_PGN(partie,"*",'W',"save_load.txt")
	}
} 


