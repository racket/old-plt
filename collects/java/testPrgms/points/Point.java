package points;

public class Point {

    public           int x, y;
    protected        int useCount = 0;
    static protected int totalUseCount = 0;

    public void move(int dx, int dy) {
	x += dx; y += dy; useCount++; totalUseCount++;
    }

}
