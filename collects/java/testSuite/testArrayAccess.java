import java.lang.*;

public class testArrayAccess {

    public static void main(String args[]) {

	int a[], i;
	float b[];
	int aa[][];
	int bb[][];

	a = new int[100];
	b = new float[2];
	aa = new int[2][2];

	// The followings are legal  

	for(i=0; i<100; i++) a[i] = i;

	a[1] = 2;
	a[1] = a[i];
	a[i] = a[i+1];
	aa[i][i] = 1;
	i = aa.length;
	i = aa[1].length;
	bb = (int[][])aa.clone();

	// The followings are illegal

	a[b[1]] = 'a'; // Wrong type for index
	b[1]    = 2.0; // Wrong type double to float
	aa[i]   = 2;   // The left hand side is the wrong type.

	bb = new int[2];
	i = aa.length(); // length is not a function of bb.
    }
}
