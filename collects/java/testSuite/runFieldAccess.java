// Test running access to fields.

// Field in interfaces must have an initializer which need not be
// a constant expression.

interface SpecialPair {
    int w = 2;
    double x = 3.3;
    Pair special = new Pair(-1,null);
    int V1[] = {1,2,3,4,5};
    Pair V2[] = {new Pair(1, new Pair (1, null))};
    int tt = 2;
}

// There is no constructor in this class. Test the insertion of the
// initializer for such a case.

class A {
    static int []a = {100,200};
    static int w;

    int[] z= {77,66};

    public static int fa(){
	w = 5;
	return a[0]+a[1];
    }

    public int fb(){
	return z[0]+z[1];
    }
}

class Pair implements SpecialPair {
    int car;
    Pair cdr;

    static int V3 [] = {-3,-4};

    // Test calling a class method as an initializer.
    int ww = car();
    // Test calling a static method as an initializer.
    int yy = f(44);

    // The null constant does not generate an attribute in the class
    // file. This field is initialized by the initializer <clinit> of 
    // the class. NIL is not a constant expression since null is not
    // a constant.

    public static final Pair NIL = null;
 
    public Pair(int x, Pair y){
	this.car = x;
	cdr = y;
    }

    public Pair(int x, int y){
	this.car = x;
	cdr = new Pair(y, null);
    }

    public int car(){
	return car;
    }

    public Pair cdr(){
	int i;
	i = car();
	return this.cdr;
    }
    
    static int f(int x){
	return x+1;
    }

    // Test an initializer using field of this class.
    int V4[] = {ww, -3, yy};
}

public class runFieldAccess {

    public static void main(String args[]) {
	Pair p;

	p = new Pair(2, new Pair (88, null));

	System.out.println(p.car());
	System.out.println(p.cdr().car());
	System.out.println(p.ww);
	System.out.println(p.yy);
	System.out.println(p.car);
	System.out.println(p.cdr.car);
	System.out.println(p.V4[0]);
	System.out.println(p.V4[1]);
	System.out.println(p.V3[0]);
	System.out.println(p.V3[1]);
	
	System.out.println(SpecialPair.w);
	System.out.println(SpecialPair.x);
	
	System.out.println(SpecialPair.V1[0]);
	System.out.println(SpecialPair.V1.length);
	
	System.out.println(A.fa());
	
	A a = new A();
	A[] va = new A[2];
	System.out.println(a.fb());
	System.out.println(a.w);
	// A strange case, should we generate code for va[0]?
	// System.out.println(va[0].w);
    }
}
