package tracking;
 
import java.awt.*;

/** Cette classe �limine le bug de IE 1.0.2 qui aligne
  incorrectemnt les label. La valeur corrective est dans la 
  classe tracking.
  */

public class myLabel extends Label {

  /** Cr�e un label avec une cha�ne. */

  public myLabel(String s){
    super(s);
  }


  /** Cr�e un label avec une cha�ne et un alignement. */

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

