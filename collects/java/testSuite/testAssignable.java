// Testing assignment conversion and cast for Primitive types and String.

public class testAssignable {


    public static void main(String args[]) {
	int i=0; float f=0.0F; double d=0.0; char c='e'; boolean b=true;
	String s= "s"; short h=0; long l=0; byte v=0;

	// All the following assigments are legal

	d = f;
	d = i;
	d = c;
	d = h;
	d = l;
	d = 'a';
	d = 2.0;
	d = 2.0D;
	
	f = h;
	f = v;
	f = c;
	f = i;
	f = l;
	f = 12;
	f = 'a';
	f = (float)d;
	f = (float)i; // The cast is redundant.

	i = c;
	i = v;
	i = i;
	i = 'a';
	i = (int)l;
	i = (int)f;
	i = (int)d;

	l = i;
	l = h;
	l = v;
	l = c;
	l = 0L;

	c = 'a';
	c = 32;
	c = (char)i;
	c = (char)f;
	c = (char)d;
	c = (char)v;
	c = (char)h;

	v = 32;
	v = (byte)i;
	v = (byte)c;
	v = (byte)f;
	v = (byte)d;

	d = i = 'a';

	s = null;

	// This is correct eventhough the rhs is wider than the lhs.
	i += d;
	i += l;
	i /= d;
	i <<= i;
	i >>>= i;
	i %= i;

	// The following assigments are not legal

	 i += null;
	 d %= null;
	 i -= s;
	 i += s;

	// (d = i) = 2;

	f = d; 
	f = 0.0;  // Should specify 0.0F

	c = i;
	c = l;
	c = b;
	c = s;
	c = v;

	b = i;
	s = i;
	
	i = l;
	i = b;
	i = f;
	i = d;
	i = 0L;
	
	v = c;
	v = i;

	// lhs not a variable
	++i = 2;


    }
}
