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
	int a; double x; BaseClass b= new BaseClass();

	a = 2;
	a = f1(a);
	x = f2(a);
	f3(a);
	b.f3(a);
	f4(i);
    }
}


class C2 {
    int a; double x;
    BaseClass b = new BaseClass();

    double f(){
	a = b.f1(2);
	x = b.f2(2);
	return a;
    }
}

public class runMethodReference {
    public static void main(String argv[]){
	C2 c2 = new C2();
	System.out.println(c2.f());
	System.out.println("abc".length());
    }

}
