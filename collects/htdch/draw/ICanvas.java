package draw;

public interface ICanvas {
     // initial and show a width x height canvas
     public boolean start(int width, int height);

     // tear down the canvas and shut down the program
     public boolean stop();

     // draw a circle at p, paint circle c
     public boolean drawCircle(Posn p, int r, Color c);

     // draw a solid disk at p, fill with color c
     public boolean drawDisk(Posn p, int r, Color c);

     // draw a width x height rectangle at p, fill with color c
     public boolean drawRect(Posn p, int width, int height, Color c);

     // draw a line from p0 to p1, use color c
     public boolean drawLine(Posn p0, Posn p1, Color c);

     // draw a string at position p
     public boolean drawString(Posn p, String s);

     // clear a circle at p, paint circle c
     public boolean clearCircle(Posn p, int r, Color c);


     // clear a solid disk at p, fill with color c
     public boolean clearDisk(Posn p, int r, Color c);

     // clear a width x height rectangle at p, fill with color c
     public boolean clearRect(Posn p, int width, int height, Color c);

     // clear a line from p0 to p1, use color c
     public boolean clearLine(Posn p0, Posn p1, Color c);

     // wait for s seconds (roughly)
     public boolean sleepForAWhile(int s);
}
