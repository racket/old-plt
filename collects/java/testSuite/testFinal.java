

class A {
    final int x = 2;
}

class B extends A {
    final int x =3;
}

public class testFinal {
    static void main(String args[]) {
	A a1 = new B();
	B a2 = null;
	
	// Prints 2, does not look into a1.
	System.out.println(a1.x);
	// Prints 3.
	System.out.println(a2.x);
    }
    
}
