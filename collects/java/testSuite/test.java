public class test {

    static public void main(String[] args){
	int i=245;
	I a = new A();

	System.out.println(a.f());
    }

}

interface I {
    int f();
}

class A implements I {
    public int f(){return 77;};
}
