// This not in the same package as testProtected/testProtected, so
// the protected accesses might be erroneous.

class testProtected3 {

    void g(){
	// Error, since constructor is protected, so not accessible.
	testProtected.testProtected x = new testProtected.testProtected();
	// Error, a not accessible.
	x.a++;
    }

}

class testProtected4 extends testProtected.testProtected {

    private int b;
    protected int c;

    public testProtected4(){
	// The protected constructor is accessible.
	super();
    }

    void k(){
	// Error, since constructor is protected, so not accessible. 6.6.2
	testProtected.testProtected x = new testProtected.testProtected();
	testProtected4 x2 = new testProtected4();
	testProtected5 x3 = new testProtected5();

	// Accessible
	b++;
	super.b++;

	// a accessible
	super.a++;
	// a accessible
	a++;
	// a not accessible through x even though we are in a subclass
	// of testProtected.
	x.a++;

	// a is accessible, since it is in this class.
	x2.a++;
	// a is accessible since it is in a subclass.
	x3.a++;

	// d exists in testProtected but it is shadowed by the private d
	// of testPortected5. So not accessible.
	x3.d++;

	// Now d is accessible.
	((testProtected4)x3).d++;
    }
}

interface protectedI {
    float b = 7;
    int c = 10;
}

class testProtected5 extends testProtected4 implements protectedI {

    private int d;

    // m does not override m of testProtected.testProtected since it is not
    // visible from here. (different package)
    int m(){
	// b not accessible since private.
	super.b++;

	System.out.println(c); // ambiguous reference

	// Error:
	// b of testProtected is not visible because of b from testProtected4,
	// which is private. But it is accessible in protectedI which is not
	// an int. (This is not an ambiguous reference).
	return(b);
    }

}
