// This test the most specific method algorithm


public class runMostSpecific {

    static void main(String args[]){
	// Expect 101
	System.out.println("Expect 101, get "+runMostA.f(new Integer(99), 2));
    }
}


class runMostA {

    public static int f(Object a, int b){
	return b;
    }

    public static int f(Number a, int b){
	return a.intValue()+b;
    }
}
