// Test semantic error messages for instanceof.
 
interface I {
    int x = 2;
}

class A implements I {
    public int m() { return x; }
}

public class testInstanceof {

    static void main(String args[]){
	A a = new A();
        I i = new A();
	int j=2;

	System.out.println(j instanceof a);
	System.out.println(j instanceof int);
	System.out.println(j instanceof Object);
	System.out.println(a instanceof Number);
	System.out.println(a instanceof null);
    }
}
