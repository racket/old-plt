// Test execution of initializers.

public class runInitializer implements runInitializerI {

    public int f(int n){
	System.out.println("Value of n is: "+n);
	return n;
    }

    static void main(String args[]){
	double d, x[] = {3.0,4.0};
	int i = 1; int k = 2, j = 3; Integer I = new Integer(1);
	int V2[] = {(int)999.333, (byte)1000, (int)x[1]};
	int V[] = {10,11}; 
	int Empty[] = {};

	Integer V3[] = {null, new Integer(99)};
	
	V2[0] = 2;
	System.out.println(Empty.length);
	System.out.println(i);
	System.out.println(k);
	System.out.println(j);
	System.out.println(I.toString());
	System.out.println(x[0]);
	System.out.println(x[1]);
	System.out.println(V[0]);
	System.out.println(V[1]);
	System.out.println(V2[0]);
	System.out.println(V2[1]);
	System.out.println(V2[2]);
	System.out.println(V3[1].toString());

	// The initializer should use the current value.
	int a[] = {V2[0]};
	System.out.println(a[0]);

	System.out.println(y);	
    }
}

interface runInitializerI {
    int v[] = {1,2};
    int x = Math.abs(v[0]+v[1]);
    int y = new runInitializer().f(v[0]);
}
