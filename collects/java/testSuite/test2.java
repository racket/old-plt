package tracking;
 
import java.awt.*;

/** Cette classe élimine le bug de IE 1.0.2 qui aligne
  incorrectemnt les label. La valeur corrective est dans la 
  classe tracking.
  */

public class myLabel extends Label {

  /** Crée un label avec une chaîne. */

  public myLabel(String s){
    super(s);
  }


  /** Crée un label avec une chaîne et un alignement. */

  public myLabel(String s, int alignement){
    super(s, alignement);
  }

  /** Positionne le label avec sa hauteur et sa largeur */

  public void reshape(int x, int y, int largeur, int hauteur){
    super.reshape(x - tracking.ajxLabelIE,  
                  y, 
                  largeur + tracking.ajxLabelIE, 
                  hauteur);
  }
  
}

