// There can be some strange situation when to look up some
// names. Here are some.


// The class java could fool things up. It should not. Indeed the sun compiler
// complain, but I believe it is an error. See section 6.8.1 bottom paragraph.
class java { static int x; }
class A implements I {}

interface java {}

class B {}

public class testTableLookup {

    static void main(String args[]){
	  A a = new A();
	  I i = new A();
	  // This is an error, since type B is incompatible with I.
	  I e = new B();
	  
	  int j = java.x;

	  Object o = new Object();
	  
    }}
