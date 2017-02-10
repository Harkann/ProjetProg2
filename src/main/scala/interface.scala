import swing._
import Array._


object Interface extends SimpleSwingApplication {
	var Cells = ofDim[Button](8,8)
	def initColors(i:Int,j:Int) = {
		if((i+j)%2 == 0){
			Cells(i)(j).background = java.awt.Color.BLACK
		}
		else{
			Cells(i)(j).background = java.awt.Color.WHITE
		}
	}
	def resetColors(i:Int,j:Int)  = {
		for( i2 <- 7 to 0 by -1) {
			for( j2 <- 0 to 7) {
				if (i2!=i || j2!=j){
					if((i2+j2)%2 == 0){
						Cells(i2)(j2).background = java.awt.Color.BLACK
					}
					else{
						Cells(i2)(j2).background = java.awt.Color.WHITE
					}
				}
			}
		}
	}

	def select_case(i:Int,j:Int) ={
		if (Cells(i)(j).background == java.awt.Color.GREEN){
			if((i+j)%2 == 0){
				Cells(i)(j).background = java.awt.Color.BLACK
			}
			else{
				Cells(i)(j).background = java.awt.Color.WHITE
			}
		}
		else {
			Cells(i)(j).background = java.awt.Color.GREEN
		}
	}


	def is_piece_on_case (i:Int,j:Int) = {
		Projet.partie.matrix_pieces(i)(j)
	}

	for( i <- 7 to 0 by -1) {
		for( j <- 0 to 7) {
			Cells(i)(j)= new Button {
				action = Action((i+1)+","+(j+1)) {
					val piece_id = is_piece_on_case(i,j)
					if (piece_id != "0"){
						resetColors(i,j)
						select_case(i,j)

					}
					else {
						resetColors(i,j)
					}
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