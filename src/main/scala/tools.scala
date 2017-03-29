import javax.swing.ImageIcon
import java.time.Instant.now
/** http://stackoverflow.com/questions/6714045/how-to-resize-jlabel-imageicon
	Merci à Rémi Dupré pour avoir trouvé ce bout de code*/
object Tools {
	
	/**redimensionne l'image @file aux dimensions spécifiées*/
	def icon_resized(file : String, width : Integer, height : Integer) : ImageIcon = {
		val imageIcon = new ImageIcon(getClass.getResource(file)); // load the image to a imageIcon
		val image = imageIcon.getImage(); // transform it 
		val newimg = image.getScaledInstance(width, height,  java.awt.Image.SCALE_SMOOTH); // scale it the smooth way  
		return new ImageIcon(newimg);  // transform it back
	}
	/**renvoie le minimum du x/y de la résolution*/
	def min_size = {
		if (Current_Config.res_x < Current_Config.res_y){Current_Config.res_x}
		else {Current_Config.res_y}
	}
}