class TimerClock(color:Char,partie:Partie) extends Runnable(){
	var current_period = 1
	var current_duration = Config.temps_cadences(current_period-1)
	var current_coups = Config.nb_coups(current_period-1)
	var current_increment = Config.increment_cadences(current_period-1)
	var current_time_left = current_duration
	var is_running = false 
	override def run() = {
		partie.game_window.head_up_bar.edit_timer(color,display(current_time_left))
		waiting()
	}
	def next_period() = {
		current_period+=1
		if (current_coups < partie.nb_turn/2 && current_period <= Config.nb_periods){
			current_duration = Config.temps_cadences(current_period-1)
			current_coups = Config.nb_coups(current_period-1)
			current_increment = Config.increment_cadences(current_period-1)
			current_time_left = current_duration
		}
		else {
			println("fin au temps")
			is_running = false
			partie.perdu(color,"temps")
		}
	}
	def display(milisec:Int):(Int,Int,Int) = {
		var hour = (milisec/1000)/3600
		var min = ((milisec/1000)%3600)/60
		var sec = ((milisec/1000)%3600)%60
		return(hour,min,sec)
	}
	def running():Unit = {
		while (is_running && partie.is_running){
			if (current_time_left > 0){
				try {
					partie.game_window.head_up_bar.edit_timer(color,display(current_time_left))
					Thread.sleep(10)
					current_time_left -=10
				}
				catch {
					case e: InterruptedException => {
						current_time_left+=current_increment
						partie.game_window.head_up_bar.edit_timer(color,display(current_time_left))
						is_running = !is_running
						waiting()
					}
				}
			}
			else {next_period()}
		}
	}
	def waiting():Unit = {
		while (!is_running && partie.is_running){
			if(Thread.interrupted){running()}
			else {
				try {Thread.sleep(100)}
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