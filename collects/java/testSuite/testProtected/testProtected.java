package testProtected;

public class testProtected {

    protected int a;
    public int b;
    protected int d;

    void m(){
	// accessible, it is in the same class.
	a++;
    }

    // A protected constructor.
    protected testProtected(){
	a = 0;
    }
}


class testProtected2 extends testProtected {

    void f(){
	// constructor is accessible.
	testProtected x = new testProtected();
	// accessible, same package and subclass.
	a++;
    }
}

class testProtectedB {

    void m2(){
	testProtected x = new testProtected();
	// accessible, same package.
	x.a++;
    }
}
