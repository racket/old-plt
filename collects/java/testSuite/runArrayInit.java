public class runArrayInit {

    static public String[] getInfo1() {
	String[] info = {"abcd", "URL", "!!!"};
	return info;
    }

    // The array initializer code must convert from int to double.
    static public double[] f1 = {1,2,3};
    static public double[][] f2 = {{1,2,3.1f}, {1.3,4,5}};

    static public String[][] getInfo2() {
	String[][] info = {
	    {"abcd", "URL", "!!!"},
	    {"efgh",  "", "123"},
	};
	return info;
    }


    static public String[][][] getInfo3() {
	String[][][] info = {{
	    {"abcd", "URL", "!!!"},
	    {"efgh",  "", "123"},
	},
			     {
				 {"abcd", "URL", "!!!"},
				 {"efgh",  "", "123"},
			     }};
	return info;
    }

    public static void main(String args[]){
	printObject(f1);
	printObject(f2);
	printObject(getInfo1());
	printObject(getInfo2());
	printObject(getInfo3());
    }
}
