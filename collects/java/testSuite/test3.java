package tracking;

import java.util.*;

/** 
  Classe diaDate permet de gérer des dates. Utile pour avancer
  et reculer une date d'un ou plusieurs jours.

  Il y a une fonction pour créer un objet date à partir d'une chaîne
  de caractères et une fonction pour faire l'inverse. (Selon le format
  francais ou anglais.)

  Les autres fonctions permettent de diminuer ou d'augmenter d'une
  ou plusieurs journées la date courante.

 */ 

class diaDate extends Object {
  static final int FRANCAIS = 1;
  static final int ANGLAIS  = 2;

  int annee;  // Annee courante
  int mois;   // mois courant
  int jour;   // jour courant
  int langue; // format de la représentation de cette date

  /** Constructeur d'une date. Langue est le format et devrait être
    l'une des deux valeurs FRANCAIS, ANGLAIS.

   */

  public diaDate(int jour, int mois, int annee, int langue){
    this.jour  = jour;
    this.mois  = mois;
    this.annee = annee;
    this.langue = langue;
  }

  /** À partir d'une chaîne de caractères et du format voulu (langue)
    générer un objet date.
    */

  static diaDate toDiaDate(String date, int langue){
    StringTokenizer p = new StringTokenizer(date, " /#,()");
    String jour;
    String mois;

    if(langue == FRANCAIS){
      jour = p.nextToken();
      mois = p.nextToken();
    } 
    else{
      mois = p.nextToken();
      jour = p.nextToken();
    }

    String annee = p.nextToken();
    return new diaDate(Integer.parseInt(jour),
                       Integer.parseInt(mois) , 
                       Integer.parseInt(annee), 
                       langue);
  }

  /** Retourne une chaîne de caractères représentant la date. 
      C'est donc l'inverse de toDiaDate.
   */

  public String toString(){
    if(langue == FRANCAIS) return jour+"/"+mois+"/"+annee;
    else return mois+"/"+jour+"/"+annee;
  }

  /* Additionne n journées à la date courante. (n peut être négatif.)
     Cette addition va modifier le mois et l'année si nécessaire.
   */
  
  public void addJour(int n){
    int i;

    if(n>0)
      for(i=0; i<n; i++) add1Jour();
    else 
      for(i=0; i<-n; i++) sub1Jour();
  }

  /** Soustraire une journée à la date courante. 
    Cette soustraction va modifier le mois et l'année si nécessaire.

   */

  public void sub1Jour(){
    if(jour > 1) jour--;
    else if( mois > 1 ){
      mois--;
      jour = nbJoursMois();
    }
    else {
      annee--;
      mois = 12;
      jour = 31;
    }
  }

  /** Additionne une journée à la date courante.
    Cette addition va modifier le mois et l'année si nécessaire.
   */

  public void add1Jour(){
    if(jour < nbJoursMois()) jour++;
    else if( mois < 12 ){
      mois++;
      jour = 1;
    }
    else {
      annee++;
      mois = 1;
      jour = 1;
    }
  }

  /** Indique si c'est une année bissextile. */

  private boolean bissextile(){
    return (annee % 4 == 0) && (annee % 100 != 0) || (annee % 400 == 0);
  }

  /** Retourne le nombre de jours pour le mois courant. */

  private int nbJoursMois(){
    switch( mois ){
    case 2 : return bissextile() ? 29 : 28;
    case 4: case 6 : case 9: case 11 : return 30;
    default :  return 31;
    }
  }

}
