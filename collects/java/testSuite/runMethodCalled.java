/*  A running test */


interface I {
    int w=10000;
    int g(int a, int b);
}

public class runMethodCalled implements I {

    int w; 
    static final int p = 2;

    static void main (String args[]){
	System.out.println(f(1,2,3,4));
	System.out.println("lll".toUpperCase());
    }

    public int g(int a, int b) { return a*b;}
    
    static int f(int a, int b, int c, int d){
      return a+b+c+d;
    }

}
