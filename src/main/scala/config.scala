object Config {
	val delai_ia = 1
	val res_x = 700
	val res_y = 1000
	val possible_proms = List("Qu","Bi","Kn","To")
	val return_allowed = true

	// Gestion du temps 
	val timer = true
	val nb_periods = 1
	val nb_coups = List(0) //0 -> KO
	val temps_cadences = List(3663000) //milisecondes
	val increment_cadences = List(30000)
}


object Check_Config {
	if (Config.nb_periods != Config.nb_coups.length){
		println("ERROR : nb_coups length does not match nb_periods")
	}
	if (Config.nb_periods != Config.temps_cadences.length){
		println("ERROR : temps_cadences length does not match nb_periods")
	}
	if (Config.nb_periods != Config.increment_cadences.length){
		println("ERROR : increment_cadences length does not match nb_periods")
	}
}