interface I1 {
    int a=1;
    int b=11;
    int c=5;
}

interface I2 {
    int a=2;
    int b=10;
    int c=5;
}


interface I3 extends I1, I2 {
    int b=3;
}

class A {
    final int a =1;
}

class B extends A {
    final int a=2;
}

// 

interface In1 {
    int aa=1;
}

interface In2 {
    int aa=2;
}

interface In3 extends In1, In2 {
    int aa=3; 
}

interface In4 extends In3 {
    int bb=4;
}

// 

interface X1 {
    int e=1;
}

interface X2 extends X1 {
    int w=2;
}

interface X3 extends X1, X2 {
    int e=3; 
}

interface X4 extends X3, X2 {
    int w=4;
}

// There are two paths to the same constant.

interface Y2 extends Y3 {
    int h=2;
}

interface Y3  {
    int i=3; 
}

interface Y4 extends Y3, Y2 {
    int h=4;
}


// Ambiguous inheritance of a constant from a class and a interface

class T1 {
    static int q = 2;
}

interface T2 {
    int q=3;
}

class T3 extends T1 implements T2 {
    int w = 3;
}


public class testExtensionInterface extends T3 implements I3, In4, X4, Y4 {
    static public void main(String args[]){
	B x = new B();

	// There is an ambiguity on a.
	System.out.println(a);
	// There is an ambiguity on c. So the initial value is not used to
	// realize that it is the same object.
	System.out.println(c);
	// There is no ambiguity on b, since it is in I3.
	System.out.println(b);
	// There is no ambiguity on x.a, since in B it overrides the one in A.
	System.out.println(x.a);
	// There is no ambiguity on aa, since in In3 it overrides them in In1 and In2.
	System.out.println(aa);
	// This is ambiguous.
	System.out.println(e);
	// Two paths to the same constant. This is not ambiguous.
	System.out.println(i);
	// The following case is different from the pizza compiler and the sun
	// compiler. It is ambiguous in the sun compiler but not the pizza
	// compiler. I think it should be considered ambiguous.
	System.out.println(q);
    }
}
