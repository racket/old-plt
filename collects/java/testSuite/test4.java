
import java.awt.*;

/** 
  Les glyc�mies, les insulines et les options sont gard�es dans
  cette classe.  Le constructeur permet de sp�cifier un certain nombre
  de jours et de p�riodes, mais seulement 7 jours et 4 p�riodes sont affich�s.

  L'algorithme de recommandation des modifications de doses
  d'insulines se trouve ici, incluant le calcul des moyennes.

  Les valeurs par d�faut des param�tres de l'algorithme de
  recommandation des insulines peuvent �tre facilement modifi�s par
  le param�tre � options � de la page HTML. Voir la classe tracking.

 */


public class Glycemies {

  public int insulinesI = 0;
  public Glycemie glycemies[][];
  public double moyennes[][];        // Les colonnes sont pour ac pc.
  public IndicateurCanvas moyennesBullet[][];
  private myLabel labelMoyennes[][];
  public Numerique acSeuilHypo;
  public Numerique acSeuilHyper;
  public Numerique acSeuilTresHyper;

  public Numerique pcSeuilHypo;
  public Numerique pcSeuilHyper;
  public Numerique pcSeuilTresHyper;

  // Les glyc�mies sont toujours stock�es en mmol/L; mais
  // l'affichage et l'entr�e des glyc�mies peuvent �tre en mmol/L ou en mg/dl

  public boolean mmolL = true; // si true il faut consid�rer l'entr�e en mmol/L

  // Nombre de glyc�mies maximum pour le calcul de la moyenne
  public Numerique nbMaxGlyMoyennes;   

  // Nombre de glyc�mies minimum pour le calcul de la moyenne   
  public Numerique nbMinGlyMoyennes;   

  public Glycemies(int nbJours, int nbPeriodes){
    glycemies = new Glycemie[nbJours][nbPeriodes];
    moyennes = new double[nbPeriodes][2];
    labelMoyennes = new myLabel[nbPeriodes][2];
    moyennesBullet = new IndicateurCanvas[nbPeriodes][2];

    // Les valeurs par d�faut des param�tres.
    // Ceux-ci peuvent �tre facilement modifi�s par le param�tre
    // � options � de la page HTML.

    nbMinGlyMoyennes = new Numerique(2);
    nbMaxGlyMoyennes = new Numerique(3);
    acSeuilTresHyper = new Numerique(14.0);
    acSeuilHyper     = new Numerique(7.0);
    acSeuilHypo      = new Numerique(4.0);
    pcSeuilTresHyper = new Numerique(20.0);
    pcSeuilHyper     = new Numerique(10.0);
    pcSeuilHypo      = new Numerique(7.0);

    for(int i=0; i < nbJours; i++){
      for(int j=0; j < nbPeriodes; j++){
        glycemies[i][j] = new Glycemie(this, i, j);
      }
    }

    for(int j=0; j < nbPeriodes; j++){
      moyennes[j][0] = 0.0;
      moyennes[j][1] = 0.0;
    }
  }

  /** Passer � travers toutes les bo�tes de texte et prendre
     les nouvelles valeurs. Cela peut entra�ner des questions
     (bo�tes dialogues) pour d�terminer si les glyc�mies sont
     pertinentes.
     */

  public void determineLesGlycemies(){
    for(int i=0; i < glycemies.length; i++){
      for(int periode=0; periode < glycemies[i].length; periode++){
        // Avant le repas

        if(glycemies[i][periode].acGlyTextField != null){
          String t = glycemies[i][periode].acGlyTextField.getText();

          // La valeur a �t� modifi�e ?
          if(glycemies[i][periode].acGlyTextField.estModifie){
            if(tracking.estNumerique(t)){
              double x = Float.valueOf(t).doubleValue();
              glycemies[i][periode].setGlyExt(x, true);
            }
            else glycemies[i][periode].setGlyExt(-1, true);
          }
        }
        // Apr�s le repas

        if(glycemies[i][periode].pcGlyTextField != null){
          String t = glycemies[i][periode].pcGlyTextField.getText();
          
          // La valeur a �t� modifi�e ?
          if(glycemies[i][periode].pcGlyTextField.estModifie){
            if(tracking.estNumerique(t)){
              double x = Float.valueOf(t).doubleValue();
              glycemies[i][periode].setGlyExt(x, false);
            }
            else glycemies[i][periode].setGlyExt(-1, false);
          }
        }
      }
    }
  }

  /** Afficher toutes les glyc�mies et les moyennes */

  public void rafraichirValeurGlycemies(){
    for(int i=0; i < this.glycemies.length; i++){
      for(int j=0; j < this.glycemies[i].length; j++){
        if( this.glycemies[i][j].acGlyTextField != null ) 
          this.glycemies[i][j].acGlyTextField.rendreVisible ();
        if( this.glycemies[i][j].pcGlyTextField != null ){ 
          this.glycemies[i][j].pcGlyTextField.rendreVisible ();
        }
      }
    }
    rafraichirMoyennes();
  }

  public void rafraichirMoyennes(){
    for(int j=0; j < moyennes.length; j++){
      setMoyenne(j, moyennes[j][0], true);
      setMoyenne(j, moyennes[j][1], false);
    }
  }

  public void rafraichirInsulins(){
    for(int j=0; j < 4; j++){
      double x = glycemies[insulinesI][j].quantBasal.getValeurDouble();
      glycemies[0][j].basaLabel.setText(tracking.formatFloat(x));
      x = glycemies[insulinesI][j].quantPrandial.getValeurDouble();
      glycemies[0][j].pranLabel.setText(tracking.formatFloat(x));
    }
  }

  public void calculMoyennes(){
    for(int periode=0; periode < glycemies[0].length; periode++){
      calculMoyenne(periode, true);
      calculMoyenne(periode, false);
    }
  }

  /** Le calcul de la moyenne utilise au moins � nbMinGlyMoyennes � valeurs
    glyc�miques et au maximum � nbMaxGlyMoyennes � valeurs glyc�miques.
    
    Ces deux param�tres sont modifiables par l'utilisateur directement
    par l'interface dans le cas du c�d�rom, mais modifiable seulement
    dans la page HTML pour l'H�tel-Dieu. 

   */

  public void calculMoyenne(int periode, boolean estAc){
    double s = 0.0; 
    int nb = 0;

    for(int i=0; i < glycemies.length && nb < nbMaxGlyMoyennes.getValeurInt();
        i++){
    if( glycemies[i][periode].getGly(estAc) > 0 
        &&  glycemies[i][periode].getPertinent(estAc) ){
        s += glycemies[i][periode].getGly(estAc);
        nb++;
      }
    }

    if(nb >= nbMinGlyMoyennes.getValeurInt() && nb > 0) { 
      setMoyenne(periode, s / nb, estAc);
    }
    else {
      setMoyenne(periode, -1, estAc);
    }
  }

  public void setLabelMoyenne(myLabel labelMoyenne, int j, boolean estAc){
    labelMoyennes[j][estAc?0:1] = labelMoyenne;
  }

  public String moyenneExt(int periode, boolean estAc){
    if(moyennes[periode][estAc?0:1] >= 0.0)
      return tracking.formatGlyExt(moyennes[periode][estAc?0:1],mmolL);
    else return "";
  }

  public void setMoyenne(int periode, double x, boolean estAc){
    moyennes[periode][estAc?0:1] = x;

    if(x > 0){
      setTextMoyenne(periode,
                     moyenneExt(periode, estAc),
                     estAc);

      if(estHyper(x, estAc) || estHypo(x, estAc)){
        if(moyennesBullet[periode][estAc?0:1]!=null)
          moyennesBullet[periode][estAc?0:1].setBullet();
      } else
        if(moyennesBullet[periode][estAc?0:1]!=null)
          moyennesBullet[periode][estAc?0:1].unsetBullet();
    }
    else if(x == -2){
      setTextMoyenne(periode, "**", estAc);
      if(moyennesBullet[periode][estAc?0:1]!=null)
        moyennesBullet[periode][estAc?0:1].unsetBullet();
    }
    else if(x == -1){
      setTextMoyenne(periode, "N/A", estAc);
      if(moyennesBullet[periode][estAc?0:1]!=null)
        moyennesBullet[periode][estAc?0:1].unsetBullet();
    }
  }

  private void setTextMoyenne(int j, String s, boolean estAc){
    if(labelMoyennes[j][estAc?0:1] != null){
      labelMoyennes[j][estAc?0:1].setText(s);
      labelMoyennes[j][estAc?0:1].setAlignment(Label.CENTER);
      // labelMoyennes[j][estAc?0:1].resize(labelMoyennes[j][estAc?0:1].preferredSize());
      labelMoyennes[j][estAc?0:1].validate();
    }
  }

  public double getMoyenne(int p, boolean estAc){
    return moyennes[p][estAc?0:1];
  }

  public boolean estAnormale(double x, boolean estAc){
    return estHypo(x, estAc) || estHyper(x, estAc);
  }

  // x est en mmol/L
  public boolean estHypo(double x, boolean estAc){
    return (x < ((estAc)?acSeuilHypo.getValeurDouble():
                 pcSeuilHypo.getValeurDouble()) 
            && x > 0.0);
  }

  // x est en mmol/L
  public boolean estHyper(double x, boolean estAc){
    return (x > ((estAc)?acSeuilHyper.getValeurDouble():
                 pcSeuilHyper.getValeurDouble()));
  }
       
  // x est en mmol/L
  public boolean estTresHyper(double x, boolean estAc){
    return (x > ((estAc)?acSeuilTresHyper.getValeurDouble():
                 pcSeuilTresHyper.getValeurDouble()));
  }

  /** 
    Recommandation pour la modification des insulines.

    L'algorithme complet de la modification des insulines est aussi d�crit dans
    un document word.

   */

  /** Calculer de combien il faut modifier l'insuline.

    Il faut r�f�rencer la quantit� d'insuline actuelle pour
    correctement �valuer la modification.

    @param periode : le num�ro de la p�riode o� il y a une glyc�mie probl�me.
                     periode == 0 => basal
                     periode > 0  => prandial
    @param hyper : true => pour hyperglyc�mie, false => pour hypoglyc�mie */
  
  public PaireDouble quantModInsuline(int periode, boolean hyper){
    if( periode == 0 ) {   /* C'est basal */
      double q = quantTotaleBasal(0); 
      if(q > 10.0) return new PaireDouble (2.0, q+(hyper?2.0:-2.0));
      else return new PaireDouble (1.0, q+(hyper?1.0:-1.0));
    }
    else{  
      /* C'est prandial, il faut r�f�rencer l'insuline de la p�riode 
         pr�c�dente. */
      double q = glycemies[0][periode-1].quantPrandial.getValeurDouble(); 
      if(q > 0.5)  return new PaireDouble(0.2, q+(hyper? 0.2:-0.2));
      else return new PaireDouble(0.1, q+(hyper?0.1:-0.1));
    }
  }

  /** Recherche si deux hypoglyc�mies cons�cutives existent pour une journ�e.
    Retourne un entier jour donnant la deuxi�me journ�e o�
    une hypoglyc�mie s'est produite.
   */

  public int deuxHypoCons(int periode){
    int jour1, jour;
    
    /* On traite avant le repas => true */
    
    // Cherche premi�re glyc�mie pertinente et > 0
    jour1 = -1;
    for(jour =0; jour < 6; jour++) 
      if(glycemies[jour][periode].getPertinent(true) &&
         glycemies[jour][periode].getGly(true) > 0.0) {
        /* Premi�re valeur trouv�e */
        jour1 = jour;
        break;
      }

    if(jour1 == -1 || !glycemies[jour1][periode].estHypoPertinent(true)) 
      return -1;

    // La premi�re glyc�mie pertinente est hypo : jour1
    // Cherche si la prochaine glyc�mie pertinente est hypo

    for(jour = jour1+1; jour < 7; jour++){
      if(glycemies[jour][periode].getPertinent(true) &&
         glycemies[jour][periode].getGly(true) > 0.0) {
        
        /* Premi�re valeur trouv�e */
        if( glycemies[jour][periode].estHypoPertinent(true) )
            
          // Deux hypo cons�cutives pertinentes trouv�es.
          return jour1;
        return -1;
      }
    }

    return -1;
  }

  /** Cherche une p�riode o� trois hypoglyc�mies (trois jours) se sont
    produites dont la premi�re est la premi�re glyc�mie pertinente non
    z�ro.  Retourne le num�ro du premier jour si cela existe, sinon
    -1.  */

  public int troisHypo(int periode){
    int jour, jour1;
    int hypo;

    // Trouve premi�re glyc�mie pertinente et plus grande que z�ro

    jour1 = -1;
    for(jour=0; jour < 5; jour++)
      if(glycemies[jour][periode].getPertinent(true) 
         && glycemies[jour][periode].getGly(true) > 0.0){
        jour1 = jour;
        break;
      }

    // Si pas hypo passe � la prochaine periode

    if(jour1 == -1 || !glycemies[jour1][periode].estHypoPertinent(true)) 
      return -1;
      
    // La premi�re glyc�mie pertinente non z�ro est hypoglyc�mique
    
    hypo = 1;
    for(jour = jour1+1; jour < 7; jour++){
      if( glycemies[jour][periode].estHypoPertinent(true) ) hypo++;
      if(hypo == 3) return jour1;
    }

    // Rien trouv�, retourne faux
    return -1;
  }


  /**
    V�rifie s'il existe une moyenne hyperglyc�mique : retourne la p�riode.
    Retourne -1 si aucune moyenne hyperglyc�mique.
    */

  private int modMoyenneHyper(boolean estAc){
    int periode;

    for(periode=0; periode < 4; periode++)
      if( estHyper(getMoyenne(periode, estAc), estAc ) ) 
        return periode;

    return -1;
  }

  /** Retourne vrai si x = y approximativement (moins de 0.01) */

  public boolean egalAppx(double x, double y){
    return Math.abs(x - y) < 1.0E-2;
  }
  
  /** Pour la p�riode p, cherche la journ�e j o� il y a eu changement
    d'insuline ou retourne -1.
   */

  private int changInsPrandial(int p){
    int j;

    for(j = 0; j < 6; j++)
      if( !egalAppx(glycemies[j][p].quantPrandial.getValeurDouble(),
                    glycemies[j+1][p].quantPrandial.getValeurDouble()) ) 
        return j;
    
    return -1;
  }

  /** Retourne une paire qui indique qu'un changement d'insuline
    prandiale a eu lieu il y a moins de DEUX jours. Retourne une paire
    � faux � autrement.  */

  private PaireInt changInsPrandialDeuxJours(){
    int jour, periode;
    for(periode=0; periode<4; periode++){

      jour = changInsPrandial(periode);
      if(jour != -1 && jour < 2) 
        return new PaireInt(jour, periode);
    }
    return PaireInt.paireFaux();
  }

  /* Retourne l'indice de la premi�re journ�e o� il y a eu
     un changement de la somme des insulines basales pour une journ�e.
     
     Pour l'insuline basale, il faut faire la somme de toutes les
     insulines pour la journ�e actuelle (journ�e 0).
     
     */

  private int changInsBasal(){
    int j;

    for(j = 0; j < 6; j++)
      if( !egalAppx(quantTotaleBasal(j), quantTotaleBasal(j+1)) )
        return j;

    return -1;
  }

  private double quantTotaleBasal(int jour){
    double sommeBasal = 0;
    int i;
    for(i=0; i < 4; i++){
      if(glycemies[jour][i].quantBasal.getValeurDouble() > 0)
	sommeBasal += glycemies[jour][i].quantBasal.getValeurDouble();
    }

    return sommeBasal;
  }

  /**  Retourne une paire qui indique qu'un changement d'insuline basale
    a eu lieu il y a moins de TROIS jours. Retourne une paire � faux � 
    autrement.
    */

  private PaireInt changInsBasaleTroisJours(){
    int jour, periode;

    jour = changInsBasal();
    if(jour != -1 && jour < 3) return new PaireInt(jour, 0);
    else return PaireInt.paireFaux();
  }


  /** Retourne une cha�ne de caract�res donnant le nom de la
    p�riode � modifier. Cela ne fonctionne que pour prandiale car
    pour basale on ne conna�t pas le moment o� le patient prend
    cette insuline. (Bien que dans la plupart des cas l'insuline
    basale se prend le soir. De plus, le patient pourrait prendre
    l'insuline prandiale en deux moments de la journ�e. C'est donc
    la t�che du patient � choisir le moment � modifier.)
    */

  public String periodeAModifier(int periode){
    if( periode == 0 ) return "";
    else return tracking.periodes[periode-1];
  }

  /** Fonction principale pour l'ajustement des insulines.
    Cette fonction retourne un vecteur de cha�nes de caract�res 
    d�crivant l'ajustement  � effectuer.

    Cette description donne la raison, la quantit� d'insuline, 
    le type d'insuline et la p�riode.
   */

  public String[] recommandationInsuline(){
    PaireInt pHypo, pHyper;
    int hypo, hyper;
    int periode, jour;
    int hypoPeriode;
    int hyperPeriode;

    /* V�rifie si l'insuline prandiale a �t� modifi�e, pour une p�riode,
       il y a moins de deux jours.
     */

    boolean modInsPrandiale = !(changInsPrandialDeuxJours().paireEstFaux());

    /* V�rifie si l'insuline basale a �t� modifi�e, pour une p�riode,
       il y a moins de trois jours.
     */

    boolean modInsBasale = !(changInsBasaleTroisJours().paireEstFaux());

    // CAS 1 : si les deux insulines ont �t� modifi�es il ne sert �
    //         rien de poursuivre. Ce cas ne devrait pas se produire
    //         mais on le traite au cas o�.

    if(modInsPrandiale && modInsBasale){
      String[] reponse = 
      { "Il y a deux jours ou moins un changement de dose d'insuline prandiale,", 
        "pour la p�riode '" + tracking.periodes[changInsPrandialDeuxJours().cdr()] + "', a �t� effectu�.",
        "De plus, il y a trois jours ou moins un changement d'insuline basale,", 
        "pour la p�riode '" + tracking.periodes[changInsBasaleTroisJours().cdr()] + "', a �t� effectu�. ",
        "Vous ne devez pas modifier vos deux insulines en m�me temps."
      };

      return reponse;
    }

    /** Les cas d'un changement d'une seule insuline */

    // Cas 2

    if(modInsPrandiale){
      String[] reponse = 
      { "Il y a deux jours ou moins un changement de dose d'insuline prandiale,", 
        "pour la p�riode '" + tracking.periodes[changInsPrandialDeuxJours().cdr()] + "', a �t� effectu�. ",
        "Vous ne devriez pas modifier vos prochaines doses d'insulines prandiales."
      };

      return reponse;
    }

    // Cas 3

    if(modInsBasale){
      String[] reponse = 
      { "Il y a trois jours ou moins un changement de dose d'insuline basale",
        "a �t� effectu�. ",
        "Vous ne devriez pas modifier votre prochaine dose d'insuline basale."
      };

      return reponse;
    }

    /* V�rifier tous les cas d'hypo d'abord pour chaque p�riode */

    for(periode=0; periode < 4; periode++){
      /* V�rifie le cas de deux hypos */

      jour = deuxHypoCons(periode);
      if( jour >= 0 ){
        PaireDouble qi = quantModInsuline(periode, false);
        String[] reponse =
        { "Vous avez eu deux hypoglyc�mies cons�cutives sans changement",
          "d'insuline pour la p�riode '" + tracking.periodes[periode] + "'.", 

          "Vous devriez diminuer votre insuline " + 
          (periode == 0 ? "basale " : "prandiale ") +
          periodeAModifier(periode),

          "de " + tracking.formatFloat(qi.car()) + " unit�(s)"+
          (periode != 0 ? " par 10 grammes de glucides." : "."),

          qi.cdr() >=0 ?
          "Votre insuline serait de " +
          tracking.formatFloat(qi.cdr()) + " unit�(s)" +
          (periode !=0 ? " par 10 grammes de glucides." : ".")
          :
          "Toutefois, cela n'est pas possible. Consultez votre m�decin." 
          
        };
        
        return reponse;
      }

      jour = troisHypo(periode);
      if( jour >= 0 )
        {
          PaireDouble qi = quantModInsuline(periode, false);
          String[] reponse = 
          { "Vous avez eu trois hypoglyc�mies sans changement ",
            "d'insuline pour la p�riode '" + tracking.periodes[periode] + "'", 
            
            "Vous devriez diminuer votre insuline " +
            (periode == 0 ? "basale " : "prandiale ") +
            periodeAModifier(periode),
            "de " + tracking.formatFloat(qi.car()) + " unit�(s)"+
            (periode != 0 ? " par 10 grammes de glucides." : "."),
            
            qi.cdr() >=0 ?
            "Votre insuline serait de " +
            tracking.formatFloat(qi.cdr()) + " unit�(s)" +
            (periode !=0 ? " par 10 grammes de glucides." : ".")  
            :
            "Toutefois, cela n'est pas possible. Consultez votre m�decin." 
          };
          
          return reponse;
        }

    
      /* Le cas utilisant les moyennes : v�rifier les hypos en premier */
    
      if( estHypo(getMoyenne(periode, true), true) )
        {
          PaireDouble qi = quantModInsuline(periode, false);
          String[] reponse =
          { "Vous avez une moyenne hypoglyc�mique pour la p�riode " + 
            tracking.periodes[periode] + ".",
            
            "Vous devriez diminuer votre insuline " + 
            (periode ==0 ? "basale " : "prandiale ") +
            periodeAModifier(periode),
          
            "de " + tracking.formatFloat(qi.car()) + " unit�(s)"+
            (periode !=0 ? " par 10 grammes de glucides." : "."),
            
            // V�rifier s'il est possible de diminuer l'insuline sans
            // tomber sous la valeur z�ro!
            
            qi.cdr() >=0 ?
            "Votre insuline serait de " +
            tracking.formatFloat(qi.cdr()) + " unit�(s)" +
            (periode !=0 ? " par 10 grammes de glucides." : ".")
            :
            
            // Cas exceptionel o� il n'y a pas assez d'insuline.
            
            "Toutefois, cela n'est pas possible. Consultez votre m�decin." 
          };
          
          return reponse;
        }
      
    }
    /* Le cas hyper suit les cas hypos */
    
    periode = modMoyenneHyper(true);
    if( periode >= 0 )
      {
        PaireDouble qi = quantModInsuline(periode, true);
        String[] reponse =
        { "Vous avez une moyenne hyperglyc�mique pour la p�riode " + 
          tracking.periodes[periode] + ".",
          
          "Vous devriez augmenter votre insuline " + 
          (periode ==0 ? "basale " : "prandiale ") +
          periodeAModifier(periode),
          "de " + tracking.formatFloat(qi.car()) + " unit�(s)"+
          (periode != 0 ? " par 10 grammes de glucides." : "."),
          
          // V�rifier s'il est possible de diminuer l'insuline sans
          // tomber sous la valeur z�ro!
          
          "Votre insuline serait de " +
          tracking.formatFloat(qi.cdr()) + " unit�(s)" +
          (periode !=0 ? " par 10 grammes de glucides." : ".")
        };
        
        return reponse;
      }
    
    /* Si aucune r�gle ne s'applique, indiquer ce r�sultat */

    String[] reponse = {
      "Aucune recommandation de modifications pour vos prochaines insulines."
    };
    return reponse;
  }

}
