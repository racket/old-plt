class A {
    int x = 2;
}

class B  {
    A a;
}

class C {
    B b;
}

class testQualifiedReference {

    public static void main(String argv[]){
	C c = new C();
	    
	System.out.println(c.b.a.x);
	return;
    }
}
