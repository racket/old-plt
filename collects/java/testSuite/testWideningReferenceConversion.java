

public class testWideningReferenceConversion {

    void main (String args[]){
	int B[] = new int[10];
	int C[] = new int[10];
	long D[] = new long[10];
	int E[][] = new int[10][10];

	B = C;

	// Error, int is not long.
	D = C;
	// Error, long is not int.
	C = D;

	// Error, int is different from int[].
	E = B;
    }

}
