interface I1 {
    int f3(int i, int j);
}

class BaseClass {

    public int f1(int i){
	return 1;
    }

    public  double f2(int i){
	return f1(i);
    }

    public int f3(int i){
	return i;
    }

}

class C1 extends BaseClass {

    public void f4(int i){
	int a; double x; I1 b;

	a = 2;
	a = f1(a);
	x = f2(a);
	f3(a);
	b.f3(a,a);
	f4(i);
    }
}


class C2 {
    int a; double x;
    BaseClass b = new BaseClass();

    void f(){
	a = b.f1(2);
	x = b.f2(2);
    }
}

public class testMethodReference {
    void main(String argv[]){
	String s = argv[0];
	System.out.println(s.charAt(0));
	return;
    }

}
