
public class testLineNumber {

    public static void main(String argv[]){
	int[] A = new int[5];

	A[0] = -2;
	A[1] = 3;

	// This bumps and give an error message.
	A[5] = 6;
	
    }
}
