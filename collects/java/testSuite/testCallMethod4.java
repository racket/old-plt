
import java.lang.*;

class S {
    static int f1(int i, int j) {return f1('a','b');}
    static int f1(int c1, char c2)    {return 2;}
    int f1(char c1, int c2) {return 3;}
}

class S2 extends S {
    static int f1(int i, char c) {return 3;}
}

public class testCallMethod4{

    public static void main(String args[]) {
	S s = new S2();
	
    }    
}

