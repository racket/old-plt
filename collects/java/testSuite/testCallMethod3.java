/*
  Testing some rare cases in method invocation.

 */

import java.lang.*;

class S {
    protected int f1(int i, int j) {return 1;}
    private int f1(char c1, char c2)    {return 2;}
}

class S2 extends S {
    int f1(int i, char c) {return 3;}
}


public class testCallMethod3{

    public static void main(String args[]) {
	S s = new S2();
	System.out.println(s.f1(1,1));      // prints 1
	System.out.println(s.f1('a',1));    // prints 1
	System.out.println(s.f1('a', 'b')); // prints 1
	System.out.println(s.f1(1, 'b'));   // prints 1

	S2 s2 = new S2();
	System.out.println(s2.f1(1,1));      // prints 1
	System.out.println(s2.f1('a',1));    // prints 1
	System.out.println(s2.f1('a', 'b')); // prints 3
	System.out.println(s2.f1(1, 'b'));   // prints 3
	
    }    
}
