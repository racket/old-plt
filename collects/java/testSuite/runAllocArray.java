public class runAllocArray {

    static void main(String args[]){
	
	int v1[], v2[][]; Integer v3[], v4[][];

	v1 = new int[10];
	v2 = new int[10][2];
	
	v2 = new int[3][];
	v3 = new Integer[4];

	v4 = new Integer[2][];
	
	System.out.println(v1.length);
	System.out.println(v2.length);
	System.out.println(v2[0].length);
	System.out.println(v3.length);
	System.out.println(v4.length);
	System.out.println(v4[1].length);
    }
}
