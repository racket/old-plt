public class testConstExpr {

    public static void main(String args[]) {

	// Cannot compare strings like this
	//final boolean e = "s" < "j";

	// The resulting value is 0 but the numbers are larger than an
	// int so this is an error.
	//final int n1 = 1000000000000 - 1000000000000;

	// This is corect
	//final int n2 = 1000000000000L - 1000000000000L;

	// This is still incorrect
	//final long n3 = 1000000000000 - 1000000000000;

	// This is correct
	//final long n4 = 1000000000000L - 1000000000000L;


	// This is not a compile error. Only the five lower bits of
	// 40 are used.
	final int n5 = (1<<40);

	// error, value out of range for short.
	final short n6 = 32767+1; 

       	// error, value out of range for char.
	final char c2 = 65536;

	// Not an error.
	final byte by1 = 256-256;

	// Errors, values out of range.
	final byte by5 = -1<<8;
	final byte by6 = -1>>>8;

	// Not an error.
	final short s1 = 32768 - 32768;

	// Error, since 2^17 is too large
	final short s2 = 1<<17;
	// The following is correct since the value 2^16*2^16*2=2^33
	// is truncated to 0.
	final short s3 = (65536*65536*2);
	final short s4 = (65536*65536*4);
	final short s5 = (65536*65536*5);
    }

}
