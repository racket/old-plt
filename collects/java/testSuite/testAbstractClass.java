
abstract class C1 {
    int x;

    abstract int f1(int a);
    abstract int f2(int b);
}

// Error, the class should be declared abstract.

class C2 extends C1 {

    int f1(int a) { return a;}
}

class C3 extends C2 {

    int f2(int b) { return b+1;}
}

interface I2 {int x = 2;}
interface I3 {int y = 3;}

interface I1 extends Object { int x = 3; }

public class testAbstractClass {

    public static void main(String args[]) {
	// Error, C1 is abstract.
	C1 c1 = new C1();
	C1[] vc1 = new C1[]; 
	// I1 is not a class.
	C1 c1b = new I1();
	I1 vc1b = new I1[];

	// Error, C2 is still abstract.
	C2 c2 = new C2();
	// Correct.
	C3 c3 = new C3();
    }

}
