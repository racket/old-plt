// Test semantic analysis of initializers.

public class testInitializer {
 
    static int f(int[] a){
	return a[0];
    }

    static void main(String args[]){
	// The followings are correct.
	int i = 1; 
	int k = 2, j = 3; 
	Integer I = new Integer(1);
	int V1[]={10,11}; 
	final int z = 100; 
	int V2[][] = {{-1,-2},{-3,-4,-5}};
	int V3[][] = {{1}, new int[100]};

	// There should be an initializer in Java 1.0. But this is correct in Java 1.1
	final int m;

	// The followings are incorrect.

	// Incorrect, x not an array.
	double x = {3.0,4.0};

	int W1[] = {{-1,-2},{-3,-4,-5}};
	int W2[][] = {{-1,-2},{-3,-4,-5},7};

    }

}

interface testInitializerI {

    int x = 3;
    //
    int v [] = {2};
    // No static function allowed in interfaces.
    static int f(int n);
    // You can't call a non-static function in a static initializer.
    int y = g(2);
    int g(int n);
}
