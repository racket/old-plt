// Various tests on super and this.

class T1 {
    public int x; 
}

class T2 extends T1 {

    public T2(){
	f(this);

    }

    static int f(T2 a){
	System.out.println(a.x);
	return 2;
    }
}

public class runSuperThis {

   static public void main(String args[]){
       T2 t2;
       t2 = new T2();
    }

}
