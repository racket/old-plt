/*

  Running test for associativity and precedence of operators.  It also
  test mixing of numerical types.

 */

public class runArithmetic {

    static final byte b = 127;
    static final int a = 3;
    static final double x = 99.2;
    static final float f = 2.33f;
    static final char c = 'a';
    static final boolean bool = true;
    static final short s = 2;
    static final long l = 2L;

    final double y = 100.1;
    double z = 55.222;

    public static void main(String args[]){

	int vi; double vd; short vs; byte vb;

	vd = 2.4; vi = 1000; vs = 25; vb = 100;

	System.out.println("Associativity of operators ");
	System.out.println(vi - vi - vi);
	System.out.println(vd - vd - vd);
	System.out.println(vi/vb/2);
	System.out.println(vd/vd/vd);
	System.out.println(vi% vb % 2);
	System.out.println(vi<<vb<<2);
	System.out.println(vi>>vb>>2);
	System.out.println(vi>>>vb>>>2);
	System.out.println(vi & vi & vi);
	System.out.println(vi | vi | vi);
	System.out.println(vi ^ vi ^ vi);
	System.out.println(vd = vd = vd);
	System.out.println(vd /= vd /= vd);
	vd = 2.4;
	System.out.println(vd -= vi -= vi);
	vi = 1000;
	System.out.println(vi <<= vi <<= 2);

	// Special case of assignment operators with different types.
	System.out.println("Assignment with different types");	
	vi = 1000;
	System.out.println(vi += vd);
	vi = 1000;
	System.out.println(vi |= (1L << 3));

	System.out.println("Precedence of operators");
	System.out.println(vd+vd*vd+vd*vd/vd);
	System.out.println(vi+vi%10);
	System.out.println(vi*vi % 10);
	System.out.println(vi & vi | vb + vb);

	System.out.println("Shift operators ");
	System.out.println(vi << vi+vi);
	System.out.println(vi*vi << vi+vi);


	System.out.println(vi/vi/vi);
	System.out.println(vi-vi-vi);

	System.out.println("Mixing chars and strings with numeric values");
	System.out.println('a'+2);
	System.out.println('a'+2.1f);
	System.out.println('a'+2.1);

	System.out.println('a'+" llo");
	System.out.println(2+" allo");
	System.out.println(2.1+" allo");
	System.out.println(true+" allo");
	System.out.println(false+" allo");
	System.out.println((short)3+" allo");
	System.out.println(3L+" allo");
	System.out.println(2.3f+" allo");
	
	System.out.println('a'+2+" allo");
	System.out.println(('a'+2)+" allo");
	System.out.println('a'+(2+" allo"));
	
	System.out.println(a+" allo");
	System.out.println(x+" allo");

    } 

}
