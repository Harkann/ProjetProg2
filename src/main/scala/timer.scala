class Timer(duration:Int,color:Char,partie:Partie) extends Runnable(){

	val end_time = System.currentTimeMillis + duration

	var is_running = true 
	override def run() = {
		while (is_running){
			if (System.currentTimeMillis < end_time){
				println(System.currentTimeMillis+" "+end_time)
				Thread.sleep(1000)
			}
			else {
				println("fin au temps")
				is_running = false
				partie.perdu(color,"temps")
			}

		}
	}
}