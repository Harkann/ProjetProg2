import swing._
import Array._

object Interface extends SimpleSwingApplication {
	var Cells = ofDim[Button](8,8)
	for( i <- 7 to 0 by -1) {
		for( j <- 0 to 7) {
			Cells(i)(j)= new Button {
				action = Action((i+1)+","+(j+1)) {
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
					if (background == java.awt.Color.GREEN){
						if((i+j)%2 == 0){
							background = java.awt.Color.BLACK

						}
						else{
							background = java.awt.Color.WHITE
						}
					}
					else {
						background = java.awt.Color.GREEN
					}
				}
				
				if((i+j)%2 == 0){
					background = java.awt.Color.BLACK

				}
				else{
					background = java.awt.Color.WHITE
				}
			}
		}
	}

	def top = new MainFrame {
		title = "Chess Game"
		contents = new GridPanel(8, 8) {
			for( i <- 7 to 0 by -1) {
				for( j <- 0 to 7) {
					contents += (Cells(i)(j))
				}
			}
		}
	}
}