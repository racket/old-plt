import testBasicArithmetic;

public class testCallBasicArithmetic {

    static void main(String args[]){
	System.out.println(testBasicArithmetic.addInt(4,2));
	System.out.println(testBasicArithmetic.subInt(4,2));
	System.out.println(testBasicArithmetic.mulInt(4,2));
	System.out.println(testBasicArithmetic.divInt(4,2));
	System.out.println(testBasicArithmetic.remInt(4,2));

	System.out.println(testBasicArithmetic.addLong(4,2));
	System.out.println(testBasicArithmetic.subLong(4,2));
	System.out.println(testBasicArithmetic.mulLong(4,2));
	System.out.println(testBasicArithmetic.divLong(4,2));
	System.out.println(testBasicArithmetic.remLong(4,2));

	System.out.println(testBasicArithmetic.addDouble(4.0,2.0));
	System.out.println(testBasicArithmetic.subDouble(4.0,2.0));
	System.out.println(testBasicArithmetic.mulDouble(4.0,2.0));
	System.out.println(testBasicArithmetic.divDouble(4.0,2.0));
	System.out.println(testBasicArithmetic.remDouble(4.0,2.0));

    }
}
