

public class runArrayAccess {

    static void main(String args[]){
	
	int v1[], v2[];

	v1 = new int[100];
	v2 = new int[100];

	v1[1] = 1;
	v2[1] = 2;

	v1[2] = v1[1] + v2[1];
	System.out.println(v1[2]);
	System.out.println(v1.length);

	int[] v3;
	v3 = (int [])v1.clone();
	System.out.println(v3[1]);

	int v4[][];

	v4 = new int[7][10];
	v4[1][1] = 5;
	System.out.println(v4[1][1]);
	System.out.println(v4.length);
	System.out.println(v4[1].length);
    }
}
