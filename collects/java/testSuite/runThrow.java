// A simple throw statement.

class runThrow {

    static void main(String args[]){

	try {
	    throw new RuntimeException();
	}
	catch(RuntimeException e){
	    System.out.println("Did catch "+e);
	}

	try {
	    throw new RuntimeException();
	}
	catch(RuntimeException e){
	    System.out.println("Did catch "+e);
	    throw new RuntimeException();
	}
	finally {
	    System.out.println("Must print this");
	}
    }
}
