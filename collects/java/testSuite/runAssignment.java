// Run several tests on the instruction vs expression.  For example
// tests to see that a[++i] is compiled differently than ++i.

public class runAssignment{

    static public void main(String[] args){
	int A[] = new int[10];
	int i;

	i = 0;
	A[i++] = 3;
	A[++i] = 4;

	System.out.println(A[0]);
	System.out.println(A[2]);
	System.out.println(i);	
	++i;
	System.out.println(i);	
	A[2]++;
	System.out.println(A[2]);	
	A[++A[2]] = 100;
	System.out.println(A[A[2]]);
	//
	i = i = 2;
	System.out.println(i);
	i = A[i] = 2;
	System.out.println(A[i]);

	System.out.println(i+++2);
	System.out.println( i+(i=100));

	i += 2;
	System.out.println(i);
	i = 1;
	A[i++] += A[i];
	System.out.println(i);
	System.out.println(A[i]);

	i >>>= 1;
	System.out.println(i);
    }
}
