public class testCallMethod6 {

    static public void main(String[] args){
	I a = new A();
	// It should detect f correctly through the interface I.
	System.out.println(a.f());
    }

}

interface I {
    int f();
}

class A implements I {
    // According to 8.6.8 this is not accessible outside the
    // class.
    private A(){}
    public int f(){return 0;};
}

