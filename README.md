# ProjetProg2
Jeu d'echec Programation 2 ENS CACHAN L3 2017

Le plateau a été représenté par une matrice contenant l'identitée des pieces.
L'identité d'une piece est crée ainsi :
un caractère pour la couleur de la piece,
deux caractères pour le nom de la pièce
un chiffre pour le numéro de la piece 
Ainsi le quatrième pion noir a pour id : "BPe3"

La gestion des déplacements a été faite à l'aide de traits un par type de déplacement:
- horizontaux et verticaux pour la tour et la reine
- diagonaux pour le fou et la reine
- jump pour les cavaliers 
- move_peon pour les pions
- move_king pour le roi

Pour les IA, on avait besoin de la liste des deplacements disponibles celà ce fait en deux temps:
On créée une liste des déplacements possibles sans faire attention à la gestion de la mise en échec.
On examine chaque déplacement pour voir s'il met ou laisse le roi en échec.

#PAD crans

https://pad.crans.org/p/projetProgRGM
