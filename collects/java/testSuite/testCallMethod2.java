/*
  Testing some rare cases in method invocation.

 */

import java.lang.*;

class S {
    static    int f1(int i, int j) {return 1;}
    protected int f1(char c1, char c2) {return 2;}
}

class S2 extends S {
    int f1(int i, char c) {return 3;}
}


public class testCallMethod2{

    public static void main(String args[]) {
	S s = new S2();
	System.out.println(s.f1(1,1));      // prints 1
	System.out.println(s.f1('a',1));    // prints 1
	System.out.println(s.f1('a', 'b')); // prints 2
	System.out.println(s.f1(1, 'b'));   // prints 1

	S2 s2 = new S2();
	System.out.println(s2.f1(1,1));      // prints 1
	System.out.println(s2.f1('a',1));    // prints 1
	// System.out.println(s2.f1('a', 'b')); // Ambiguous

	// With the pizza compiler
	// prints 1, prints 3 if S.f1 is declared non static, but cannot find 
	// in the specification book a rule to justify that.
	System.out.println(s2.f1(1, 'b'));   
	
    }    
}

