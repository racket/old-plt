// Testing the modifier private

class B {
    private int a;
    public int b;
}

interface I {

    public int b=3;
}

interface J {
    public int c=2;
}

class A extends B  implements I,J {

    public int a;
    private int b;
    int c;
    protected int p;

    public A(){
	m1();
    }

    private void m1(){
	// Error, b is not static.
	a = A.b;
	a = b;
	A x = new A();
	// a is accessible.
	A.a = 2;
    }
}


public class testPrivate {

    static int w;
    
    public static void main(String args[]) {
	int i;
	A o = new A();
	A v[] = new A[3];

	v[0] = new A();
	i = w;
	// Cannot access a as a static field since a is not static.
	i = A.a;
	i = v[0].a;
	i = o.a;

	// Error m1 is not accessible.
	o.m1();
	// Error, b is private.
	i = o.b;
	i = o.c;
	o.p = i;
    }
}
