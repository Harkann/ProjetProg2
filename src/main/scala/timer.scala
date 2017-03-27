class TimerClock(duration:Int,color:Char,partie:Partie) extends Runnable(){
	val end_time = System.currentTimeMillis + duration
	var is_running = false 
	override def run() = {
		partie.game_window.head_up_bar.edit_timer(color,display(duration))
		waiting()
	}
	def display(milisec:Int):(Int,Int,Int) = {
		var hour = (milisec/1000)/3600
		var min = ((milisec/1000)%3600)/60
		var sec = ((milisec/1000)%3600)%60
		return(hour,min,sec)
	}
	def running():Unit = {
		while (is_running && partie.is_running){
			if (System.currentTimeMillis < end_time){
				println(System.currentTimeMillis+" "+end_time)
				try {
					partie.game_window.head_up_bar.edit_timer(color,display(end_time.toInt-System.currentTimeMillis.toInt))
					Thread.sleep(100)
				}
				catch {
					case e: InterruptedException => {
						is_running = !is_running
						waiting()
					}
				}
			}
			else {
				println("fin au temps")
				is_running = false
				partie.perdu(color,"temps")
			}

		}
	}
	def waiting():Unit = {
		while (!is_running && partie.is_running){
			if(Thread.interrupted){
				println("interrupted")
				running()
			}
			else {
				try {
					Thread.sleep(100)
					println("wait "+color)
				}
				catch {
					case e :InterruptedException => {
						is_running = !is_running
						running()
					}

				}
			}
		}
	}
}