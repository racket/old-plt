

class A {
    private int x;
    protected int y;
    public int z;
    int h;
}

class B extends A {
    private float w;
    int i;

    int m(){
	if(w==0.0) return i; else return i*2;
    }

    int m2(){
	i = y;
	// Error
        i = x;
	i = z;
        i = h;
    }

}

public class testFieldModifiers {
    void main(String [] args){
	float r; int j;
	B a = new B();

	// Error
	r = a.w;

	// Error
	j = a.x;

	// Error
	j = a.y;

	j = a.z;

	j = a.i;
    }
}
