import java.util.*;

class A {
    static int m(){
	return 1;
    }
    
    public int m2(){
	return m();
    }
}

class B extends A {
    static int m(){
	return 2;
    }
    static int m2(int x){
	return 7;
    }
}

public class testMethodCalled{

    public int nextInt(int x){
	return x;
    }

    static private void mStatic(){
	return;
    }

    static public void main(String[] args){
	B a = new B(); 
	testMethodCalled x = new testMethodCalled();
	int i;
	int V[];

	V = new int[2];
	Random r = new Random();
	StringTokenizer s = new StringTokenizer("allo");

	r.nextInt();
	r.nextLong();
	// Error since nextInt() is not static.
	i = nextInt(i);

	// Not an Error.
	mStatic();
	// Not an Error, even though it looks like an instance call.
	x.mStatic();
	
	// Error
	i = nextInt(2,3);
	
	// Error since Random with double does not exist.
	System.out.println(r.Random(2.0));
	System.out.println(a.m2(2));
	System.out.println(a.m2());

	// Error, m3 does not exist.
	System.out.println(a.m3());

	// Error, cannot have two arguments.
	System.out.println(2,a);

	// These are malformed calls.
	//nextInt()();
	//V[1]();
    }
}
