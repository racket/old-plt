// Testing synchronized semantic errors.

public class testSynchronized {

    public static void main(String args[]) {
	int a=2; int b=1;
	// OK
	synchronized(new Integer(1)) { a=3; b=4;}

	// Not a reference type.
	synchronized(1) { }
	// Not a reference type.
	synchronized(a+b) { }
	// This is an error since null is not an object.
	synchronized(null) { }
    }
}


