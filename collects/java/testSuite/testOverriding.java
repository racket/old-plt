
//

interface I {
    int m1(float a);
}


interface J extends I {
    int m1(int b);
}

interface H extends J {
    // Error in both cases.
    float m1(int c);
    float m1(float d);

}

public class testOverriding {

    static int f1(int x) {return x;}

    int f2(int x) {return x;}

    private int f3(int x) {return x;}

    int f4(int x) {return x;}

    public int f5(int x) {return x;}

    public int f6(int x) {return x;}

    protected int f7(int x) {return x; }
}

class OverB extends testOverriding  {

    // Error, this instance method overrides a static method,
    // and the accessibility is more restrictive.
    private int f1(int x){return x;}

    // Error, this static method overrides an instance method.
    static int f2(int x){return x;}

    // This is correct.
    public int f3(int x) {return x;}

    // This is correct.
    protected int f4(int x) {return x;}

    // This is incorrect: accessibility more restrictive.
    protected int f5(int x) {return x;}

    // This is incorrect: accessibility more restrictive.
    int f6(int x) {return x;}

    // This is incorrect: accessibility more restrictive.
    int f7(int x) {return x;}
}
