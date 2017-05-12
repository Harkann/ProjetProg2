import java.io.PrintWriter
import scala.io.Source
import scala.util.matching.Regex
class Gnuchess(partie:Partie) {
	val proc = Runtime.getRuntime.exec(Array("gnuchess","-x"))
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
	def parse_and_move(line:String) = {
		val pattern = new Regex("[a-z][1-8]")
		var coo = (pattern findAllIn line).mkString("")	
		println(coo)
		println(coo.charAt(0))
		println(letter_to_int(coo.charAt(0))+""+(coo.charAt(1).toInt-48))
		println(letter_to_int(coo.charAt(2))+(coo.charAt(3).toInt-48))
		partie.get_piece(coo.charAt(1).toInt-48,letter_to_int(coo.charAt(0))).move(coo.charAt(3).toInt-48,letter_to_int(coo.charAt(2)))
	}

	val output = new Thread(){
		override def run() = {
			for (line <- Source.fromInputStream(proc.getInputStream).getLines){
				println(line)
				if (line.containsSlice("My move is")){
					parse_and_move(line) 
				}
				/*TODO : Parse le retour et applique le move correspondant*/
			}
		}
	}
	output.start()
}	
