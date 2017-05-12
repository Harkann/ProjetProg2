import java.io.PrintWriter
import scala.io.Source
import scala.util.matching.Regex
class Gnuchess(partie:Partie) {
	val proc = Runtime.getRuntime.exec(Array("gnuchess","-xe"))
	val out = new PrintWriter(proc.getOutputStream)
	def write(command:String) = {
		val writer = new Thread(){
			override def run() = {
				out.println(command)
				out.flush()
			}
		}
		writer.start()
	}
	def letter_to_int(letter:Char) = {
		letter match {
			case 'a' => 1
			case 'b' => 2
			case 'c' => 3
			case 'd' => 4
			case 'e' => 5
			case 'f' => 6
			case 'g' => 7
			case 'h' => 8
		}
	}
	def int_to_letter(value:Int) = {
		value match {
			case 1 => 'a'
			case 2 => 'b'
			case 3 => 'c'
			case 4 => 'd'
			case 5 => 'e'
			case 6 => 'f'
			case 7 => 'g'
			case 8 => 'h'
		}
	}
	def move_and_write(oi:Int,oj:Int,di:Int,dj:Int) = {
		write(int_to_letter(oj)+""+((oi+48).toChar)+" "+int_to_letter(dj)+""+((di+48).toChar))
	}

	def parse_and_move(line:String) = {
		val pattern = new Regex("[a-z][1-8]")
		var coo = (pattern findAllIn line).mkString("")	
		println(coo)
		println(coo.charAt(0))
		partie.get_piece(coo.charAt(1).toInt-48,letter_to_int(coo.charAt(0))).move(coo.charAt(3).toInt-48,letter_to_int(coo.charAt(2)))
	}
	def stop() = {
		write("quit")
		out.close()
	}
	val output = new Thread(){
		override def run() = {
			for (line <- Source.fromInputStream(proc.getInputStream).getLines){
				println(line)
				if (line.containsSlice("My move is")){
					parse_and_move(line)
					partie.is_interface = true 
				}
			}
		}
	}
	output.start()

}	
