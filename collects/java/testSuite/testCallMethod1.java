
class S {
    static int f1() {return 1;}

    private int f3(){return 3;}
    int f4(){return 10;}
}

class S2 extends S {
    int f1() {return 4;} // Error, it should be static.
    int f2() {return 5;}
    public int f3(){return 6;} // This is legal
    char f4(){return 11;} // This is illegal, char is not int
}


class T {
    int f(char c){ return 10;}
}

class T2 extends T {
    int f(int i){ return 11;} 
}

public class testCallMethod1{

    public static void main(String args[]) {
	S s = new S2();

	s.f2();       // Error, no f2 in S.
	((S2)s).f2(); // Correct.

	T t = new T2();
	t.f(2);       // Error, since T has no f(int).
        t.f((char)2); // Correct

	T2 t2 = new T2();
	t2.f(2);        // Correct and it calls f of T2
	t2.f((char)2);  // Ambiguous
    }    
}

