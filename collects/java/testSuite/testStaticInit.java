public class testStaticInit {

    int a;
    // Ok, this can be used.
    int b = this.a;

    static int x;
    static int z = y;

    // error, no this not allowed on static field.
    static int w = this.a;
    // error, no super not allowed on static field.
    static int h = super.a;


    static { 
	int i;
	// f is known, this is not an error
	x = f(2);
	// error, forward reference on y
	y = f(3);
    }

    static int y;

    static public int f(int x){ return x; }
}


interface II {

    int z;
    // error, e is static, no this allowed.
    int e = this.z;
    // error, f is static, no super allowed.
    int f = super.z;

    { System.out.println("Interface II"); }

}
