// Test the ternary ? operator execution.

public class runQuestion {

    static void main(String args[]){
	char c; byte b; short s; int i; double d; float f;
	boolean b2, b3;

	i = 100;
	d = 2.0;
	// This is a float in the constant pool(not double)
	// So we should NOT generate a double in the constant pool
	// and then use d2f.
	f = (float)16.1122; 
	s = 100;
	c = 'a';
	b = -3;
	b3 = true;
	b2 = false;

	System.out.println(b3 ? i : i);
	System.out.println(b2 ? d : f);
	System.out.println(b3 ? d : f);

	System.out.println(b2 ? i : f);
	System.out.println(b3 ? i : f);

	System.out.println(b2 ? 2 : 3);

	System.out.println(b2 ? b : s);
	System.out.println(b2 ? b : c);
	System.out.println(b2 ? c : s);
	System.out.println(b2 ? b : i);

	System.out.println(b2 ? b : 127);

	// The ? operator is right associative.
	System.out.println(b2 ? b : b3 ? d : 127); // Prints d

	System.out.println(b2 ? new Integer(1) : new Integer(2));
    }
}
