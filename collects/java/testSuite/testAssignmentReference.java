/* This test the assignment and cast rules of reference types.

 */

interface I {
    int i=2;
}

interface J {
    int b=2;
}

interface H extends I, J {
    int c=3;

}

interface K {
    int c=3;
    int m1(int x);
    int m2(float y);
}

interface L {
    int r=4;
    int m1(int x);
    int m2(float y);
}

interface M {
    float m1(int x);
    int m2(float y);
}

interface O {
    int m2(float y);
}

class A {
    int x;
}

class B extends A {
    int w;
}

class C implements H {
    static final int r=2;
}

public class testReferenceAssignment {
    
    A fa;
    Object fo;

    public void m(Object o){

	// Error, since java.lang.Object is not a subtype of A.
	fa  = o;

	fo  = o;
	fo  = fa;

	// Error, since java.lang.Object is not a subtype of A.
	fa  = fo;

	// These casts are correct.
	fa  = (A)fo;
	fo  = (Object)fa;

    }

    static public void main(String [] argv){
	A a = new A();
	B b = new B();
	C c = new C();
	L l; K k; M m; O o; H h;

	a = b;

	a = (A)b;

	a = (B)b;

	// Correct since c implements h.
	h = c;
	
	// Casts error, since C is not related to A.
	
	a = (A)c;
	c = (C)a;

	// Error
	b = a;

	b = (B)a;

	// Error
	b = (A)a;

	
	// The cast is Ok, the methods have the same signature and return types.
	// The cast is necessary.
	l = (L)k;

	// Casts in error, since the methods m1 have different return types.
	// The pizza compiler misses this error, but sun do detect it.
	// Furthermore, they may not be initialized.
	m = (M)k;
	k = (K)m;

	// Cast is correct, although m1 missing from O.
	m = (M)o;

	// Cast is correct. 
	o = (O)m;

	// Cast is correct.
	l = (L)o;
    }
}

