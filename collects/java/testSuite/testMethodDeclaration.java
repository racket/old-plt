
import java.lang.*;

public class testMethodDeclaration {

    public int f1(int x){
	return x;
    }

    int f1(int x, int y){
	return x+y;
    }

    int f1(char c1, byte c2){
	return c1;
    }

    int f1(byte c1, char c2){
	return c1;
    }


    int f1(int c1, char c2){
	return c1;
    }

    Double f1(byte c1, byte c2){
	int Double;
	Double x;

	return Double.abs(-2.0);
    }

    int f2(int w){
	return f1(w)+f1(w,w);
    }

    // Illegal declarations

    int f1(int y){
	return y;
    }

    float f1(int y){
	return y;
    }

    public static void main(String args[]) {
	testMethodDeclaration a = new testMethodDeclaration();

	a.f1(2,2);
	a.f1('a', 'b');
    }    

}
