import java.awt.*;

/** La classe principale de tout l'affichage de l'applet.
  Le paneau principal est créé par cette classe. Elle contient,
  entre autres, les boîtes textes pour entrer les glycémies.

  
  */

public class TrackingPanel extends Panel{
  static final boolean debug = false;

  Button bMiseAjourM;
  Button bOptions;
  Button bInsuline;
  Scrollbar bChangeInsuline;
  myLabel labelDateInsuline;

  Glycemies glycemies;
  trackingOptions dTrackingOptions;
  InsulineDialogue idia;
  private diaDate dateChoisie;    // La date courante choisie 
  private diaDate dateInsuline;  

  /** La création du panel va permettre l'affichage de toutes les
    composantes qui permettent un communication avec l'utilisateur.

    @param glycemies : l'objet qui va contenir toutes les glycémies affichées.
    @param ijour     : numéro du jour de la date (0=dimanche, 1=lundi, etc.)
    @param dateChoisie : la date envoyé en paramètre de l'applet
    */

  public TrackingPanel(Glycemies glycemies, int ijour, String dateChoisie){

    this.dateChoisie = diaDate.toDiaDate(dateChoisie, 
                                          tracking.francais ? diaDate.FRANCAIS 
                                          : diaDate.ANGLAIS);
    this.dateInsuline = diaDate.toDiaDate(dateChoisie,
                                          tracking.francais ? diaDate.FRANCAIS 
                                          : diaDate.ANGLAIS);
   
    GridBagLayout gridBag = new GridBagLayout();
    setLayout(gridBag);
    this.glycemies = glycemies;
    myLabel label;

    dTrackingOptions = null;
    idia = null;

    int largeur = 300;
    int hauteur = 200;

    int hauteurBoite = 22;
    int largeurBoite = 33;
    int hauteurJour  = hauteurBoite;
    int largeurJour  = 35;
    int largeurMoyenne = 27;
    int largeurPeriode = 2*largeurBoite;
    int largeurPertinent = 10;
    int hauteurPeriode = 22;
    int espVertBoite = 5;
    int espHoriBoite = 2*largeurPertinent;
    int espVertJour = espVertBoite;
    int espHoriPeriode = 0;

    int orgxJours  = 5;
    int orgyJours  = 50;
    int orgxGrille = orgxJours + largeurJour + largeurPertinent + 5;
    int orgyGrille = orgyJours;
    int orgxPeriodes = orgxGrille;
    int orgyPeriodes = 5;

    /* Sans Layout manager cela permet un contrôle précis  */

    setLayout( null );

    setForeground( new Color( 0, 0, 0 ) );
    setBackground( new Color( 255, 255, 255 ) );

    // resize( (insets().left + insets().right + largeur), (insets().top + insets().bottom + hauteur) );

    /* les noms des périodes */

    for(int j=0; j<4; j++){
      label = new myLabel( tracking.periodes[j] );
      label.setFont(tracking.labelFont);
      label.setAlignment(Label.CENTER);
      label.reshape( (insets().left + orgxPeriodes + j*(2*largeurBoite+espHoriBoite) ), 
                     (insets().top + orgyPeriodes), 
                     largeurPeriode, hauteurPeriode);
      add( label );

      label = new myLabel( "ac" );
      label.setFont( tracking.labelFont );
      label.setAlignment( Label.CENTER );
      label.reshape( (insets().left + orgxPeriodes + j*(2*largeurBoite+espHoriBoite)) , 
                     (insets().top + orgyPeriodes + hauteurPeriode), 
                     largeurBoite, hauteurPeriode);
      add( label );

      label = new myLabel( "pc" );
      label.setFont( tracking.labelFont );
      label.setAlignment( Label.CENTER );
      label.reshape( (insets().left + orgxPeriodes + j*(2*largeurBoite+espHoriBoite)) + largeurBoite , 
                     (insets().top + orgyPeriodes + hauteurPeriode), 
                     largeurBoite, hauteurPeriode);
      add( label );
    }

    /* La grille des boîtes pour entrer les glycémies. 
       Cette grille est affichée à partir du bas de l'écran en remontant. 
     */

    for(int i=0; i< tracking.nbJoursConserves; i++){

      /* les noms des jours */

      label = new myLabel(tracking.nomsJours[(i+ijour)%7]);
      label.setFont(tracking.labelFont);
      label.setAlignment(Label.RIGHT);
      add( label );
      label.reshape( (insets().left + orgxJours) , 
                     (insets().top + orgyJours + (6-i)*(espVertJour+ hauteurJour)), 
                     largeurJour, hauteurJour); 

      /* les champs pour entrer les glycémies */

      for(int j=0; j<4; j++){
        if(debug) System.out.println("champ glycémique "+ i + " " + j);

        // ------- Pour avant le repas ---------

        GlyTextField tf = new GlyTextField(5, 
                                           glycemies.glycemies[i][j], 
                                           glycemies, j, true);
        glycemies.glycemies[i][j].acGlyTextField = tf;

        add( tf );
        tf.reshape((insets().left+ orgxGrille + j*(2*largeurBoite+espHoriBoite)),
                   (insets().top + orgyGrille+(6-i)*(hauteurBoite+espVertBoite)),
                   largeurBoite, hauteurBoite);
        
        // L'indicateur pour les glycémies non pertinentes 

        IndicateurCanvas bCanvas = new IndicateurCanvas(false, Color.yellow, true);

        bCanvas.reshape((insets().left+orgxGrille + 
                         j*(2*largeurBoite+espHoriBoite)) - largeurPertinent,
                        (insets().top+orgyGrille+(6-i)*(hauteurBoite+espVertBoite)),
                        largeurPertinent, 10);
        add( bCanvas );
        glycemies.glycemies[i][j].acIndPertinent = bCanvas;

        // Force la mise à jour de l'indicateur 
        
        glycemies.glycemies[i][j].rafraichirIndPertinent(true);

        // ---------- Pour après le repas ------------

        tf = new GlyTextField(5, glycemies.glycemies[i][j], glycemies, j, false);
        glycemies.glycemies[i][j].pcGlyTextField = tf;

        add( tf );
        tf.reshape((insets().left+ orgxGrille + 
                    j*(2*largeurBoite+espHoriBoite)) + largeurBoite,
                   (insets().top + orgyGrille+(6-i)*(hauteurBoite+espVertBoite)),
                   largeurBoite, hauteurBoite);
        
        // L'indicateur pour les glycémies non pertinentes 

        bCanvas = new IndicateurCanvas(false, Color.yellow, true);

        bCanvas.reshape((insets().left+orgxGrille + 
                         j*(2*largeurBoite+espHoriBoite)) + 2*largeurBoite,
                        (insets().top+orgyGrille+(6-i)*(hauteurBoite+espVertBoite)),
                        largeurPertinent, 10);
        add( bCanvas );
        glycemies.glycemies[i][j].pcIndPertinent = bCanvas;

        // Force la mise à jour de l'indicateur 
        
        glycemies.glycemies[i][j].rafraichirIndPertinent(false);
      }
    }

    label = new myLabel(tracking.francais ? "Moy" : "Avg");
    label.setFont(tracking.labelFont);
    label.setAlignment(Label.RIGHT);
    label.reshape(insets().left+orgxJours ,
                  insets().top+orgyGrille+7*(hauteurBoite+espVertBoite),
                  largeurJour, hauteurJour);
    add( label );

    // Les étiquettes pour les moyennes 

    for(int j=0; j < glycemies.glycemies[0].length; j++){
      // ------ Pour avant le repas ------

      label = new myLabel("   ");
      glycemies.setLabelMoyenne(label, j, true);
      label.reshape((insets().left+orgxGrille+
                     j*(2*largeurBoite+espHoriBoite)) + 4 ,
                    (insets().top+orgyGrille+7*(hauteurBoite+espVertBoite)),
                    largeurBoite, hauteurBoite);
      add( label );

      IndicateurCanvas bc = new IndicateurCanvas(false, Color.red, false);
      glycemies.moyennesBullet[j][0] = bc;

      // placer en dessous de la moyenne
      bc.reshape((insets().left+orgxGrille + 
                  j*(2*largeurBoite+espHoriBoite)),
                 (insets().top+orgyGrille+7*(hauteurBoite+espVertBoite))+hauteurBoite,
                 largeurBoite, 4);
      bc.repaint();
      add( bc );

      // -------  Pour après le repas ------

      label = new myLabel("  ");
      glycemies.setLabelMoyenne(label, j, false);
      label.reshape((insets().left+orgxGrille+
                     j*(2*largeurBoite+espHoriBoite)) + largeurBoite ,
                    (insets().top+orgyGrille+7*(hauteurBoite+espVertBoite)),
                    largeurBoite, hauteurBoite);
      add(label);

      bc = new IndicateurCanvas(false, Color.red, false);
      glycemies.moyennesBullet[j][1] = bc;
      bc.reshape((insets().left+orgxGrille + j*(2*largeurBoite+espHoriBoite))+largeurBoite,
                 (insets().top+orgyGrille+7*(hauteurBoite+espVertBoite))+hauteurBoite,
                 largeurBoite, 4);
      bc.repaint();
      add( bc );
    }

    /* Le bouton pour les options (seulement pour le cédérom) */

    if(tracking.cederom){
      bOptions = new Button(tracking.francais ? "Options" : "Setup options");
      bOptions.reshape((insets().left+orgxGrille+4*(2*largeurBoite+espHoriBoite))- espHoriBoite + largeurPertinent,
                       (insets().top+orgyGrille - (hauteurBoite+espVertBoite)),
                       110, 28);
      add( bOptions );
    }

    /* Affichage de la date courante */

    for(int i=0; i<7; i++){
      diaDate dateAAfficher = diaDate.toDiaDate(dateChoisie,
                                                tracking.francais ? diaDate.FRANCAIS 
                                                : diaDate.ANGLAIS);
      dateAAfficher.addJour(-i);
      label = new myLabel(dateAAfficher.toString());
      label.setFont(tracking.labelFont);
      label.setAlignment(Label.LEFT);
      label.reshape((insets().left+orgxGrille+4*(2*largeurBoite+espHoriBoite)) - espHoriBoite  + largeurPertinent,
                    (insets().top+orgyGrille+(6-i)*(hauteurBoite+espVertBoite)),
                    120, 22);
      add( label );
    }

    /* Le bouton de mise à jour des moyennes */

    bMiseAjourM = new Button(tracking.francais ? "MAJ Moyennes" : "Update averages");
    bMiseAjourM.reshape((insets().left+orgxGrille+4*(2*largeurBoite+espHoriBoite)) - espHoriBoite + largeurPertinent,
                        (insets().top+orgyGrille+7*(hauteurBoite+espVertBoite)),
                        110, 28);
    add( bMiseAjourM );

    // Les champs textes pour les insulines 

    int orgyIns = (insets().top+orgyGrille+8*(hauteurBoite+espVertBoite));
    int orgxIns = orgxGrille + 20;
    
    label = new myLabel( tracking.francais ?  "Insulines :" : "Insulins:"  );
    label.setFont(tracking.labelFont);
    label.setAlignment(Label.LEFT);
    label.reshape(insets().left+ orgxJours ,
                  insets().top+orgyIns,
                  90, hauteurJour);
    add( label );

    label = new myLabel(tracking.francais ? "Prandial" : "Bolus");
    label.setFont(tracking.labelFont);
    label.setAlignment(Label.RIGHT);
    label.reshape(insets().left+orgxJours ,
                  insets().top+orgyIns + (hauteurJour+espVertJour),
                  55, hauteurJour);
    add( label );

    label = new myLabel(tracking.francais ? "Basal" : "Basal");
    label.setFont(tracking.labelFont);
    label.setAlignment(Label.RIGHT);
    label.reshape(insets().left+orgxJours ,
                  insets().top+orgyIns+ 2*(hauteurJour+espVertJour),
                  55, hauteurJour);
    add( label );

    // prandials     

    for(int j=0; j < 4; j++){
      myLabel tf = new myLabel(tracking.formatFloat(glycemies.glycemies[0][j].quantPrandial.getValeurDouble()));
      glycemies.glycemies[0][j].pranLabel = tf;
      tf.setFont(tracking.labelFont);
      tf.reshape((insets().left+ orgxIns + j*(2*largeurBoite+espHoriBoite)),
                 (insets().top + orgyIns + (hauteurBoite+espVertBoite)),
                 largeurBoite, hauteurBoite);
      add( tf );
    }

    // basals 
    
    for(int j=0; j < 4; j++){
      myLabel t = new myLabel(tracking.formatFloat(glycemies.glycemies[0][j].quantBasal.getValeurDouble()));
    
      glycemies.glycemies[0][j].basaLabel = t;
      t.setFont(tracking.labelFont);
      t.reshape(insets().left+ orgxIns + j*(2*largeurBoite+espHoriBoite),
                insets().top + orgyIns + 2*(hauteurBoite+espVertBoite),
                largeurBoite, hauteurBoite);      
      add( t );
    }

    //add(label = new myLabel(tracking.version));
    //label.reshape(insets().left+ orgxIns + (2*largeurBoite+espHoriBoite),
    //              insets().top + orgyIns + 3*(hauteurBoite+espVertBoite),
    //              250, 25);   

    // Le bouton pour modifier les insulines

    bInsuline = new Button(tracking.francais ? 
                           "Modifier insulines" : "Modify insulins");
    bInsuline.reshape((insets().left+orgxGrille+
                       4*(2*largeurBoite+espHoriBoite)) - 
                      espHoriBoite + largeurPertinent,
                      (insets().top+orgyIns+ (hauteurJour+espVertJour)),
                      110, 28);    
    add( bInsuline );

    /* La barre de contrôle pour changer les insulines visualisées */

    bChangeInsuline = new Scrollbar(Scrollbar.VERTICAL, 0, 0, 0, 6);
    bChangeInsuline.reshape((insets().left+orgxGrille+
                             4*(2*largeurBoite+espHoriBoite)) - 
                            espHoriBoite + largeurPertinent - 18,
                            (insets().top+orgyIns + (hauteurJour+espVertJour)),
                            15, 60
                            );
    add( bChangeInsuline );

    /* La date pour les insulines */

    labelDateInsuline = new myLabel(dateInsuline.toString());
    labelDateInsuline.reshape((insets().left+orgxGrille+
                               4*(2*largeurBoite+espHoriBoite)) - 
                              espHoriBoite + largeurPertinent,
                              (insets().top+orgyIns+ (hauteurJour+espVertJour+35)),
                              110, 28
                              );
    labelDateInsuline.setFont(tracking.labelFont);
    add( labelDateInsuline );

  }

  static public String jourToString(int ijour){
    return tracking.nomsJours[ijour];
  }

  static public String periodeToString(int periode){
    return tracking.periodes[periode];
  }

  public boolean handleEvent (Event evt){
    if(evt.target == bChangeInsuline) {
      switch(evt.id){
      case Event.SCROLL_LINE_UP:
      case Event.SCROLL_PAGE_UP:
      case Event.SCROLL_ABSOLUTE:
      case Event.SCROLL_LINE_DOWN:
      case Event.SCROLL_PAGE_DOWN:
        bChangeInsuline.setValue( bChangeInsuline.getValue() );
        break;        
      default: break;
      }

      dateInsuline.addJour(glycemies.insulinesI - bChangeInsuline.getValue());

      labelDateInsuline.setText(dateInsuline.toString());

      glycemies.insulinesI = bChangeInsuline.getValue();
      glycemies.rafraichirInsulins();
      return true;
    }
    return super.handleEvent(evt);
  }

  /** Les boutons sont gérés par cette fonction :
    - pour la mise à jour des moyennes 
    - pour le setup des options.
  */

  public boolean action(Event evt, Object what){
    if(debug) System.out.println("tracking panel " + evt);
    String name = (String) what;
    if(debug) System.out.println("action "+name);
    
    if(evt.target == bMiseAjourM && evt.id == Event.ACTION_EVENT){
      if(debug) System.out.println("bMiseAjour calculMoyennes "+evt.id);
      glycemies.determineLesGlycemies();
      glycemies.calculMoyennes();
      return true;
    } else 
      if(evt.target == bOptions && evt.id == Event.ACTION_EVENT){
        if(debug) System.out.println("nouveau trackingOptions"+evt.id+evt.arg);
        Frame f = tracking.leParent(this);
        if(dTrackingOptions != null) {
          dTrackingOptions.dispose();
          dTrackingOptions.hide();
        }

        dTrackingOptions = new trackingOptions(f, glycemies);
        dTrackingOptions.show();
        return true;
      }
      else if( evt.target == bInsuline && evt.id == Event.ACTION_EVENT ){
        //  modifierInsulines();
        
        // Forcer un nouveau calcul des moyennes
        glycemies.determineLesGlycemies();
        glycemies.calculMoyennes();        
        
        Frame f = tracking.leParent(this);
        if( idia != null ){
          idia.dispose();
          idia.hide();
        }
        
        idia = new InsulineDialogue(f, glycemies);
        idia.show();
        return true;
      }
    return false;
  }

}
