
class B {
    public B(){
	super();
	return;
    }
}

class C extends B {
    int x, y; C c;

    public C(int x, int y){
	this.x = x;
	this.y = y;
    }
}

public class testClassAlloc {

    static C c, c2;

    static void f1(){
	C a; I i;

	c = new C(2,3);

	c.c = c;
	System.out.println(c.x);
	c.c.c.x = 4;
	System.out.println(c.c.c.x);
	
    }

}

