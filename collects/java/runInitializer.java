// To test the initialization of fields and blocks

class runInitializer {

    static int a;

    static {

	a = 1;
	System.out.println("Start block 1");
    }

    static {
	System.out.println("Start block 2");
	System.out.println("a should be 1: "+a);
	a = 2;
    }

    static {
	System.out.println("Start block 3");
	System.out.println("a should be 2: "+a);
    }

    static void main(String args[]){


	System.out.println("This should come last.");
    }
}
