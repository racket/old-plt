interface I {
    int x = 2;
}

class A implements I {
    public int m() { return x; }
}

public class runInstanceof {

    static void main(String args[]){
	A a = new A();
        I i = new A();
	Object o = new Object();

	System.out.println(i instanceof Object);
	System.out.println(a instanceof Object);
	System.out.println(o instanceof Object);
	System.out.println(a instanceof A);
	System.out.println(a instanceof I);
    }
}
