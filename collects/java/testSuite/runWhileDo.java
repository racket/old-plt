// Test While and Do

public class runWhileDo {

    public static void main(String argv[]){
	int somme1, somme2, i;

	somme1 = 0;
	i = 100;
	while(i>0){
	    somme1 += i;
	    i--;
	}

	somme2 = 0;
	i = 100;
	do{
	    somme2 = somme2 + i;
            i--;
	} while(i>0);

	if(somme1!=somme2){
	    System.out.println("Error in runWhileDo");
	    System.out.println(somme1);
	    System.out.println(somme2);
	}
	else System.out.println("Correct result");
    }
}
