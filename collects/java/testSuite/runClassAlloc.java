public class runClassAlloc {

    public void f(){ System.out.println("call f");}

    static void main(String args[]){
	// Test to see if value is removed from stack.
	new runClassAlloc();

	(new runClassAlloc()).f();
    }
}
