// Mario Latendresse, 19 May 2000
// Test anonymous class syntax.

public class test7 {

    public static F f(int i){
	return new F(){
		int x;
		public int g(int i){ return i; }
	    };}
}
