
public class testFor {

    static int testFor1(int n){
	int somme1, somme2, i;
	somme1 = 0;
	for(int i=0; i<n; i++){
	    somme1 = somme1 + i;
	}

	somme2 = 0;
	for(i=n-1; i>0; i--){
	    somme2 = somme2 + i;
	}
	if(somme1!=somme2){
	    System.out.println("Error in testFor1");
	    System.out.println(somme1);
	    System.out.println(somme2);
	}
	return somme1;
    }
}
