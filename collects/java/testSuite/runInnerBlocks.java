/* This test blocks inside blocks and variable access. */

public class runInnerBlocks {
    
    static public void main(String [] argv){
	int i;
	i= 100;

	if(i==100) {
	    int x;

	    x = 3;
	    System.out.println("Should print 3: "+x);
	    {
		int y = 4;
		System.out.println("Should print 4: "+y);
	    }
	}


	if(i==100) {
	    int y;

	    y = 4;
	    System.out.println("Should print 4: "+y);
	    {
		int x = 3;
		System.out.println("Should print 3: "+x);
	    }
	}

	for(int j=0; j<10; j++) System.out.print(j);
	System.out.println();

	// Definition of j can be repeated.
	for(int j=0; j<10; j++) System.out.print(j);
	System.out.println();
	// Definition of j can be repeated.
	int j=2;
	System.out.println(j);
    }
}

