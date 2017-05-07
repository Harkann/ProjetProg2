import java.io.PrintWriter
import scala.io.Source
class Gnuchess() {
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

	val output = new Thread(){
		override def run() = {
			for (line <- Source.fromInputStream(proc.getInputStream).getLines){
				println(line)
				/*TODO : Parse le retour et applique le move correspondant*/
			}
		}
	}
	output.start()
}	
