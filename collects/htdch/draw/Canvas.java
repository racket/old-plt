package draw;
        
public class Canvas implements ICanvas {

     public native boolean start(int width, int height);

     // tear down the canvas and shut down the program
     public native boolean stop();

     // draw a circle at p, paint circle c
     public native boolean drawCircle(Posn p, int r, Color c);

     // draw a solid disk at p, fill with color c
     public native boolean drawDisk(Posn p, int r, Color c);

     // draw a width x height rectangle at p, fill with color c
     public native boolean drawRect(Posn p, int width, int height, Color c);

     // draw a line from p0 to p1, use color c
     public native boolean drawLine(Posn p0, Posn p1, Color c);

     // draw a string at position p
     public native boolean drawString(Posn p, String s);

     // clear a circle at p, paint circle c
     public native boolean clearCircle(Posn p, int r, Color c);

     // clear a solid disk at p, fill with color c
     public native boolean clearDisk(Posn p, int r, Color c);

     // clear a width x height rectangle at p, fill with color c
     public native boolean clearRect(Posn p, int width, int height, Color c);

     // clear a line from p0 to p1, use color c
     public native boolean clearLine(Posn p0, Posn p1, Color c);

     // wait for s seconds (roughly)
     public native boolean sleepForAWhile(int s);

     // start the clock and set the world to this canvas 
     public native boolean bigBang(double s); 

     // call on every tick
     public native Canvas onTick();

     // call on keystroke event 
     public native Canvas onKeyEvent(String ke); 

     // end time 
     public native boolean endOfTime();

     // last world created 
     public native Canvas lastWorld();
}    
