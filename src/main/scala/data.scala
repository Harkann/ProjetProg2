trait evaluation extends Values with Squares {
	def value(piece :Piece):Int ={
		val (x,y) = piece.position
		val i = borw_matrix(piece.color,x-1)
		val j = y-1
		if (piece.name == "Pe"){
			return (val_peon+square_peon(i)(j))
		}
		if (piece.name == "Kn"){
			return (val_knigth+square_knigt(i)(j))
		}
		if (piece.name == "Bi"){
			return (val_bishop + square_bishop(i)(j))
		}
		if (piece.name == "To"){
			return (val_tower + square_tower(i)(j))			
		}
		if (piece.name == "Qu") {
			return ( val_queen + square_queen(i)(j))
		}
		else {
			return ( val_king + square_king_middle(i)(j))
		}

	}

	def borw_matrix(col:Char, i : Int) : Int = {
		if (col == 'W'){
			return i
		}
		else{
			return (7-i)
		}
	}
}

trait Values{
	val val_peon = 100
	val val_knigth = 320
	val val_bishop = 330
	val val_tower = 500
	val val_queen = 900
	val val_king = 20000
}

trait Squares{

val square_peon = Array (
 Array(0,  0,  0,  0,  0,  0,  0,  0),
 Array(50, 50, 50, 50, 50, 50, 50, 50),
 Array(10, 10, 20, 30, 30, 20, 10, 10),
 Array(5,  5, 10, 25, 25, 10,  5,  5),
 Array(0,  0,  0, 20, 20,  0,  0,  0),
 Array(5, -5,-10,  0,  0,-10, -5,  5),
 Array(5, 10, 10,-20,-20, 10, 10,  5),
 Array(0,  0,  0,  0,  0,  0,  0,  0)
)


val square_knigt = Array(
Array(-50,-40,-30,-30,-30,-30,-40,-50),
Array(-40,-20,  0,  0,  0,  0,-20,-40),
Array(-30,  0, 10, 15, 15, 10,  0,-30),
Array(-30,  5, 15, 20, 20, 15,  5,-30),
Array(-30,  0, 15, 20, 20, 15,  0,-30),
Array(-30,  5, 10, 15, 15, 10,  5,-30),
Array(-40,-20,  0,  5,  5,  0,-20,-40),
Array(-50,-40,-30,-30,-30,-30,-40,-50)
)

val square_bishop = Array(
Array(-20,-10,-10,-10,-10,-10,-10,-20),
Array(-10,  0,  0,  0,  0,  0,  0,-10),
Array(-10,  0,  5, 10, 10,  5,  0,-10),
Array(-10,  5,  5, 10, 10,  5,  5,-10),
Array(-10,  0, 10, 10, 10, 10,  0,-10),
Array(-10, 10, 10, 10, 10, 10, 10,-10),
Array(-10,  5,  0,  0,  0,  0,  5,-10),
Array(-20,-10,-10,-10,-10,-10,-10,-20) )

val square_tower = Array(
Array(0,  0,  0,  0,  0,  0,  0,  0),
Array(5, 10, 10, 10, 10, 10, 10,  5),
Array(-5,  0,  0,  0,  0,  0,  0, -5),
Array(-5,  0,  0,  0,  0,  0,  0, -5),
Array(-5,  0,  0,  0,  0,  0,  0, -5),
Array(-5,  0,  0,  0,  0,  0,  0, -5),
Array(-5,  0,  0,  0,  0,  0,  0, -5),
Array(0,  0,  0,  5,  5,  0,  0,  0)
)

val square_queen = Array(
Array(-20,-10,-10, -5, -5,-10,-10,-20),
Array(-10,  0,  0,  0,  0,  0,  0,-10),
Array(-10,  0,  5,  5,  5,  5,  0,-10),
Array( -5,  0,  5,  5,  5,  5,  0, -5),
Array(  0,  0,  5,  5,  5,  5,  0, -5),
Array(-10,  5,  5,  5,  5,  5,  0,-10),
Array(-10,  0,  5,  0,  0,  0,  0,-10),
Array(-20,-10,-10, -5, -5,-10,-10,-20)
)

val square_king_middle =  Array(
Array(-30,-40,-40,-50,-50,-40,-40,-30),
Array(-30,-40,-40,-50,-50,-40,-40,-30),
Array(-30,-40,-40,-50,-50,-40,-40,-30),
Array(-30,-40,-40,-50,-50,-40,-40,-30),
Array(-20,-30,-30,-40,-40,-30,-30,-20),
Array(-10,-20,-20,-20,-20,-20,-20,-10),
Array(20, 20,  0,  0,  0,  0, 20, 20),
Array(20, 30, 10,  0,  0, 10, 30, 20) 
)

val square_king_end = Array(
Array(-50,-40,-30,-20,-20,-30,-40,-50),
Array(-30,-20,-10,  0,  0,-10,-20,-30),
Array(-30,-10, 20, 30, 30, 20,-10,-30),
Array(-30,-10, 30, 40, 40, 30,-10,-30),
Array(-30,-10, 30, 40, 40, 30,-10,-30),
Array(-30,-10, 20, 30, 30, 20,-10,-30),
Array(-30,-30,  0,  0,  0,  0,-30,-30),
Array(-50,-30,-30,-30,-30,-30,-30,-50)
)



}

