// Various tests on super and this.

class T1 {
    public int x; 
}

class T2 extends T1 {

    T2 a = this;
    
    // Error.
    static T2 b = this;

    public T2(){
	// Error, cannot refer to super as a variable.
	f(super);
	// You cannot assign to this.
	this = this;
    }

    static int f(T2 a){
	System.out.println(a.x);

	// Error, cannot refer to this for a static method.
	System.out.println(this.x);

	// Error, since the method is static.
	System.out.println(super.x);
	return 2;
    }
}

public class testSuperThis {

   static public void main(String args[]){
       T2 t2;
       t2 = new T2();
    }

}
