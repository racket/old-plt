// 

class testCond {

    static boolean testPP(int x, long y){
	if(x < y) return true;
	else return false;
    }

    static boolean testOROR(int x, long y){
	if(x >= y || x == 0) return true;
	else return false;
    }

    static boolean testAndAnd(int x, long y){
	if(x >= y && x == 0) return true;
	else return false;
    }

}
