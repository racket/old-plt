// Test all boolean operators in branhing context and non branching
// context.

public class runBoolean {

    static void main(String args[]){
	char c='a'; byte b=2; short s=100; int i=400; double d=2.3; 
	float f=1.2f;
	boolean b2=true, b3=false;

	// These are constant expressions.

	System.out.println(!true);
	System.out.println(!false);
	
	System.out.println(!true || !true);
	System.out.println(0 | 0);
	System.out.println(1 | 4);
	System.out.println(-1 | -4);
	//System.out.println(-2147483648 | -2147483648);
	//System.out.println(-2147483648 | 0);
	System.out.println(2147483647 | 2147483647);
	System.out.println(2147483647 | 0);
	System.out.println('b' | 'a');
	System.out.println('b' | 2);

	// This prints -2147483648.
	System.out.println(2147483647 + 1);

	System.out.println(2147483647 * 4);
	// This prints 4 since it overflows the int type.
	System.out.println(2147483647 * 2147483647 * 4);

	// These prints -17179869180. * is associative to the left.
	System.out.println(2147483647 * 2147483647L * 4);
	System.out.println((2147483647 * 2147483647L) * 4);
	System.out.println(2147483647 * (2147483647L * 4));

	// These prints -8589934588 
	System.out.println(4 * 2147483647 * 2147483647L);
	System.out.println((4 * 2147483647) * 2147483647L);
	// This prints -17179869180 
	System.out.println(4 * (2147483647 * 2147483647L));

	// Even though they are long integers, the expression is evaluated
	// as an int and gives -17179869180.
	System.out.println(2147483647L * 2147483647L * 4L);

    }
}


