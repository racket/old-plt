// This is to test the floating point operations and conversion.

public class runFloatDouble {

    static void main(String args[]){
	double d=2.0; float f; int i = 1; byte b = 127; short s = 32767;
	char c = 'a';

	// Some tests on precision
	System.out.println(0.0);
	System.out.println(1.1);
	System.out.println(1.2);
	System.out.println(1.3);
	System.out.println(1.4);
	System.out.println(1.5);
	System.out.println(1.6);
	System.out.println(1.7);
	System.out.println(1.8);
	System.out.println(1.9);
	System.out.println(2.0);
	System.out.println(2.1);
	System.out.println(2.2);
	System.out.println(2.3);
	System.out.println(2.4);
	System.out.println(2.5);
	System.out.println(2.6);
	System.out.println(2.7);
	System.out.println(2.8);
	System.out.println(2.9);

	System.out.println(2.4E-10);
	System.out.println(2.4E-12);
	System.out.println(2.4E-13);
	System.out.println(2.4E-14);
	System.out.println(2.4E-15);
	System.out.println(2.4E-16);
	System.out.println(2.4E-17);
	System.out.println(2.4E-18);
	System.out.println(2.4E-19);

	System.out.println(2.4E-31);
	System.out.println(2.4E-32);
	System.out.println(2.4E-33);

	System.out.println(2.4E-41);
	System.out.println(2.4E-42);
	System.out.println(2.4E-43);


	System.out.println(2.4E-61);
	System.out.println(2.4E-62);
	System.out.println(2.4E-63);

	System.out.println(2.4E-71);
	System.out.println(2.4E-72);
	System.out.println(2.4E-73);


	System.out.println(2.4E-100);
	System.out.println(2.4E-110);
	System.out.println(2.4E-120);
	System.out.println(2.4E-130);
	System.out.println(2.4E-200);
	System.out.println(2.4E-210);
	System.out.println(2.4E-220);
	System.out.println(2.4E-230);	


	System.out.println(2.4E-300);
	/* These are outside the admitted ranges. TBF
	System.out.println(2.4E-310);
	System.out.println(2.4E-320);
	System.out.println(2.4E-330);
	System.out.println(2.4E-340);
	*/

	d = 1E38;
	System.out.println(d);
	
	f = (float)-3.4028235E38;
	System.out.println(f);
	f = (float)3.4028235E38;
	System.out.println(f);
	
	d = -1.7976931348623157E308; 
	System.out.println(d);
	d = 1.7976931348623157E308;
	System.out.println(d);

	d = 2.0;
	System.out.println(d);
	d = i;
	System.out.println(d);

	int j = 12;
	d = b;
	System.out.println(d);
	d = s;
	System.out.println(d);

	d = 127.3333;
	s = (short)d;
	System.out.println(s);	   
	i = (int)d;
	System.out.println(i);
	
	b = (byte)d;
	System.out.println(b);
    }
}
