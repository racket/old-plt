// Test the accessibility modifiers 

class testAccessA {

    public int a;
    private int b;
    protected int c;
    int d;
}


public class testAccessibility {

    public static void main(String args[]) {
	testAccessA x = new testAccessA();

	System.out.println(x.a);
	System.out.println(x.b);
	System.out.println(x.c);
	System.out.println(x.d);

    }
}
