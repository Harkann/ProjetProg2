import Array._
object Config {
	val delai_ia = 1 //milisecondes
	val res_x = 1300 //pixels
	val res_y = 700 //pixels
	val possible_proms = List("Qu","Bi","Kn","To")
	val return_allowed = true
	
	// Gestion du temps 
	val timer = true
	val nb_periods = 1
	val nb_coups = List(0) //0 -> KO
	val temps_cadences = List((0*3600+2*60+0)*1000) //milisecondes
	val increment_cadences = List(0)

	val type_partie = ""
	val player_blanc = '0'
	val player_noir = '0'
	val ia_depth = 1
	val ia_repetitions_avoid = true
	val ia_amelioration = true
	val ia_random = true

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

object Current_Config {
	var delai_ia = Config.delai_ia
	var	res_x = Config.res_x
	var	res_y = Config.res_y
	var	possible_proms = Config.possible_proms
	var	return_allowed = Config.return_allowed
	var	timer = Config.timer
	var	nb_periods = Config.nb_periods
	var	nb_coups = Config.nb_coups
	var	temps_cadences = Config.temps_cadences
	var	increment_cadences = Config.increment_cadences
	var	type_partie = Config.type_partie
	var player_noir = Config.player_noir
	var player_blanc = Config.player_blanc
	var ia_depth = Config.ia_depth
	var ia_repetitions_avoid = Config.ia_repetitions_avoid
	var ia_amelioration = Config.ia_amelioration
	var ia_random = Config.ia_random

	def init_config() = {
		delai_ia = Config.delai_ia
		res_x = Config.res_x
		res_y = Config.res_y
		possible_proms = Config.possible_proms
		return_allowed = Config.return_allowed
		timer = Config.timer
		nb_periods = Config.nb_periods
		nb_coups = Config.nb_coups
		temps_cadences = Config.temps_cadences
		increment_cadences = Config.increment_cadences
		type_partie = Config.type_partie
		player_noir = Config.player_noir
		player_blanc = Config.player_blanc
		ia_depth = Config.ia_depth
		ia_repetitions_avoid = Config.ia_repetitions_avoid
		ia_amelioration = Config.ia_amelioration
		ia_random = Config.ia_random
	}
}