// Test the notion of variable and the modifier final.

public class testVariable {

    public int f(int i){
	return i;
    }

    static void main(String args[]){
	final double x = 220.3; int i;

	// Integer is undefined.
	i = Integer;

	i = f + 2;
	f = i;

	// Error, x is final, not a variable.
	x = -1.0;
    }
}
