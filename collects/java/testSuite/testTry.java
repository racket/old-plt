// Testing the try statement with catches and finally

public class testTry {

    public static void main(String args[]) {
	int i=2;
	
	try {
	    i = 3;
	}
	catch(NullPointerException e) {
	    System.out.println(e);
	}
	catch(RuntimeException e) {
	    System.out.println(e);
	}

	try {
	}
	// Error, since Number is not a subclass of java.lang.Throwable
	catch(Number e) {};

    }
}
