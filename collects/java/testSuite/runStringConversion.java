// Test the operators + and += on strings.

class runStringConversion {

    static public void main(String argv[]){
	String st="VIDE"; short s = 127; Number e = new Integer(99);
	int i = 77; boolean b = true; float f = 3.1212F; byte k=23;
	long l = 1234567890;
	char c='@'; double d=4.32;
	int[] a = new int[3];

	st = a+(2.0+(b+(i+(c+(l+(k+(e+(d+st))))))));
	st += "FIN";
	st += 12345;
	System.out.println(st);
    }

}
