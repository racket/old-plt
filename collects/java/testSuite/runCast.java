
// Test Checkcast and basic primitive types conversion.

public class runCast {

    static private Number f1(int n){
	// This is a case where Pizza does not catch a type error
	// as reported by the sun compiler.
	// return (n==1) ? new Integer(1) : new Long(n);  
	
	if(n==1) return new Integer(1); else return new Long(n);
    }

    static private Number[] f2(){
	return new Long[10];
    }

    static void main(String args[]){
	
	int v1[], v2[]; Long v3[]; 
	byte b; short s; char c; int i; long l; 
        float f; double x; Object o; Long L;
	i = 65537;
	v2 = new int[12];

	// The following casts will not fail at run time.

	i = (int)i;
	b = (byte)i;
	s = (short)i;
	l = (long)i;
	f = (float)i;
	x = (double)i;
	System.out.println(i);
	System.out.println(b);
	System.out.println(s);
	System.out.println(l);
	System.out.println(f);
	System.out.println(x);

	System.out.println("Byte");
	System.out.println((byte)23.4);
	System.out.println((byte)-23.4);
	System.out.println((byte)128.4);
	b = (byte)1000;
	System.out.println(b);
	b = (byte)-129;
	System.out.println(b);
	System.out.println("Char");
	c = (char)-1;
	s = (short)c;
	System.out.println(c);
	System.out.println(s);
	c = (char)65536;
	s = (short)c;
	System.out.println(c);
	System.out.println(s);
	c = (char)300;
	s = (short)c;
	System.out.println(c);
	System.out.println(s);
	System.out.println("Int");
	System.out.println((int)23.4);
	System.out.println((int)1E38);
	System.out.println((int)-1E38);
	System.out.println((int)123456789);

	System.out.println("Long");
	l = (long)1E38;
	System.out.println(l);
	l = (long)-1E38;
	System.out.println(l);
	System.out.println((long)123456789);

	v1 = v2;
	o = new Integer(2);
	v1 = (int[])v2.clone();
	v3 = (Long[])f2();

	// The following cast will fail.
	
	L = (Long)f1(1);
    }
}
