// Test the ternary ? operator typing rule.

public class testQuestion {

    static void main(String args[]){
	char c; byte b; short s; int i; double d; float f;
	boolean b2, b3; Integer I;

	i = 100;
	d = 2.0;
	f = (float)3.0;
	s = 100;
	c = 'a';
	b = -3;
	b3 = true;
	b2 = false;
	// These are all typeable assignments.

	i = b3 ? i : i;
	d = b2 ? d : f;
	
	d = b2 ? i : f;

	i = b2 ? 2 : 3;
	
	i = b2 ? b : s;
	i = b2 ? b : c;
	i = b2 ? c : s;
	i = b2 ? b : i;

	b = b2 ? b : 127;
	I = b2 ? new Integer(1) : new Integer(2);
	I = b2 ? new Integer(1) : null;
	I = b2 ? null : null;

	// The following are errors.

	I = b2 ? new Integer(1) : new Long(2);

	// int not assignable to short. 
	// (it looks ok but numeric promotion 5.6.2 makes 128 and b an int)
	s = b2 ? b : 128;	

	// boolean is not assignable to f or vice versa
	d = b2 ? b3 : f;

	// int not assignable to byte
	b = b2 ? b : i;	
	b = b2 ? b : 128;
	// short not assignable to byte
	b = b2 ? b : s;	

	// incompatible since ? is right-associative
	b = b2 ? b3 : b2 ? 2 : 3;
	b = b2 ? b2 ? 2 : 3 : b3;

    }
}
