// Test field declaration as well as access.

// Field in interfaces must have an initializer which need not be
// a constant expression.

interface SpecialPair {
    int w = 2;

    //double x = 3.3;
    //Pair special = new Pair(-1,null);
    //int V1[] = {1,2,3,4,5};
    //Pair V2[] = {new Pair(1, new Pair (1, null))};
}

class Pair {
    // Error, double declaration.
    double car;
    Pair cdr;
    int car;
    static final Pair NIL=null;

    public Pair(int x, Pair y){
	this.car = x;
	cdr = y;
    }

    public int car(){
	return car;
    }

    public Pair cdr(){
	return this.cdr;
    }


}

public class testFieldAccess {

    public static void main(String args[]) {
	Pair a;

	// Legal
	a = new Pair(2,Pair.NIL);

	System.out.println(SpecialPair.w);
    }

}
