class testPoint extends points.Point {

    public void moveBack(int dx, int dy){
	x -= dx; y -= dy; useCount++; totalUseCount++;
    }

    public static void main(String args[]){
	testPoint a = new testPoint();
	a.moveBack(1,1);
	System.out.println("UseCount "+a.useCount+" totalUseCount "+a.totalUseCount);
    }
}
