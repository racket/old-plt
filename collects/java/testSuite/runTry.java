// Testing the try statement with catches and finally

public class runTry {

    public static int f(int i){
	int y,z,x = i; 
	y=2; z=3;
	try {
	    try {
		// return the value of 
		return x+y+z;
	    }
	    finally {
		x = x+1;
		}
	} finally {
	    x = x+1;
	}
    }

    public static void main(String args[]) {
	int i=2;
	
	// prints 7.
	System.out.println(f(2));
	
	try {
	    i = 3;
	}
	catch(NullPointerException e) {
	    System.out.println(e);
	}
	catch(RuntimeException e) {
	    System.out.println(e);
	}
	/*
	// Special case with empty blocks for try
	// There should not be any code for that one.
	try { } catch(RuntimeException e){};
	try {i=4; } catch(RuntimeException e){};	
	// There should not be any code for that one.
	try { } catch(RuntimeException e){i=4;};	


	try {
	    Integer o = new Integer(1);
	    o = null;
	    i = o.intValue();
	}
	catch(NullPointerException e) {
	    System.out.print("Hit the null exception .");
	}
	catch(RuntimeException e) {
	    System.out.println(e);
	}
	finally{
	    System.out.println(".. and the finally.");
	}

	// Prints 3
	System.out.println(i);

	int j;
	j = 0;
	for (i = 0; i < 5; i++) {
	    j = j+1;
	    j = j * 2;
	    // It goes directly out of the for statement.
	    if (i > 1) break;
	}
	// This prints 14
	System.out.println(j);

	j = 0;
	for (i = 0; i < 5; i++) {
	    try {
		j = j + 1;
	    } finally {
		j = j * 2;
		// It goes directly out of the for statement.
		if (i > 1) break;
	    }
	}

	// This prints 14
	System.out.println(j);

	j = 0;
	for (i = 0; i < 5; i++) {
	    try{
		try {
		    j = j + 1;
		} finally {
		    j = j * 2;
		    // It goes directly out of the for statement.
		    if (i > 1) break;
		}
	    } finally { if(i>1) j = j *2; }
	}
	// It prints 28.
	System.out.println(j);

	j = 0;
	for (i = 0; i < 5; i++) {
	    try{
		try {
		    j = j * 2;
		    // It goes directly out of the for statement
		    // through two finally blocks. This is done by two
		    // jsr instructions.
		    if (i == 1) break;
		    j = j + 1;
		} finally { j = j+1; }
	    } finally { if(i==1) j = j *2; }
	}
	// It prints 10.
	System.out.println(j);

	for(;;)
	try{
	    break;
	}
	finally {
	    System.out.println("Should print this once.");
	}

	for(i=0;i<10;)
	try{
	    continue;
	}
	finally {
	    i = 10;
	    System.out.println("Should print that once.");
	}

    l1: for(i=0;i<10;)
	try{
	    break l1;
	}
	finally {
	    i = 10;
	    System.out.println("Should print loop once.");
	}


	// On the following code the pizza compiler
	// generates code that loops forever. But Sun compiler
	// generates correct code that print the message only once.


    l1:
	try{
	    break l1;
	}
	finally {
	    i = 10;
	    System.out.println("Should print 10: "+i+" once.");
	}

    l1:
	try{
	    System.out.print("First this, ");
	    try {
		System.out.print("then this, ");
		// This breaks into the two finally.
		break l1;
	    }
	    finally {
		System.out.print("ending by three dots");
	    }}
	    finally {
		System.out.println("... ");
	    }
	

	try {
	    Integer a = null;
	    i = a.intValue();
	}
	catch(NullPointerException e) {
	    System.out.print("Should print this for "+e);
	}
	catch(RuntimeException e) {
	    System.out.println(e);
	}
	finally {
	    System.out.println(" exception.");
	}
	
	// A break in a catch.

	l1: try {
	    j = 10;
	    Integer a = null;
	    i = a.intValue();
	}
	catch(NullPointerException e){
	    if(j==10) break l1;
	    j = 11;
	}
	finally{
	    System.out.println("The value of j should be 10: "+j);
	}
	*/
    }
}
