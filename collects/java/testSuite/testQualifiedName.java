package P;

import B.*;

class A {
    static int a=2;
}

class B {
    static int A;
}

class f{
    static int g;
}

class h{
    static int w;
}

class Z{
    static int w;
}


class C extends B {

    static int f;
    static int Z;

    static void main (String[] argv){
	// Reuse a type name as a local variable.
	// This is not an error, and B can still be used as a type name where
	// it is interpreted as a type.
	int B;

	// This is correct since B can only be seen as a type.
	B r;

	// This is correct, since the context says that Z is a type.
	Z z;

	// This is an error, since B is not accessible as a type but int.
	System.out.println(B.A);

	// This is an error, since Z is not accessible as a type in general.
	System.out.println(Z.w);

	// Accessing A in B, not the field a in A.
	// This is an error, since A is not a reference type.
	System.out.println(A.a);

	// You can use P to refer to A as a type.
	System.out.println(P.A.a);

	// This is correct
	System.out.println(C.A);

	// Accessing the function.
	System.out.println(f(2));

	// Accessing the field. It is not the method. So the context decide.
	System.out.println(f);

	// Accessing the class f is not possible because of field f as int.
	System.out.println(f.g);

	// Correct by using the package name.
	System.out.println(P.f.g);

	// Accessing the class h is possible even though there is a method
	// named h locally.
	System.out.println(h.w);

	// Accessing the function h is possible even though there is a class
	// named h.
	System.out.println(h(2));

	// Correct, referring to the field.
	System.out.println(A);

	// Incorrect, since P.A is a class not a variable.
	System.out.println(P.A);

	// Incorrect, since P is not a variable.
	System.out.println(P);
    }

    // A name can be a method, a field or a type
    static public int f(int x){
	return x;
    }

    // A name can be a method and a type.
    static public int h(int x){
	return x;
    }

}
