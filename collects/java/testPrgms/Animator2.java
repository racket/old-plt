// This example is from the book _Java in a Nutshell_ by David Flanagan.
// Written by David Flanagan.  Copyright (c) 1996 O'Reilly & Associates.
// You may study, use, modify, and distribute this example for any purpose.
// This example is provided WITHOUT WARRANTY either expressed or implied.

// This applet displays an animation.  It uses the MediaTracker class to 
// load the images and verify that there are no errors.
import java.applet.*;
import java.awt.*;
import java.net.*;
import java.util.*;

public class Animator2 extends Applet implements Runnable {
    protected Image[] images;
    protected int num_images;
     protected int current_image;
    protected MediaTracker tracker;

    // Read the basename and num_images parameters.
    // Then read in the images, using the specified base name.
    // For example, if basename is images/anim, read images/anim0, 
    // images/anim1, etc.  These are relative to the current document URL.
    public void init() {
        String basename = this.getParameter("basename");
        try { num_images = Integer.parseInt(this.getParameter("num_images")); }
        catch (NumberFormatException e) { num_images = 0; }
        
        // getImage() creates an Image object from a URL specification,
        // but it doesn't actually load the images; that is done 
        // asynchronously.  Store all the images in a MediaTracker
        // so we can wait until they have all loaded (in run()).
        tracker = new MediaTracker(this);
        images = new Image[num_images];
        for(int i = 0; i < num_images; i++) {
            images[i] = this.getImage(this.getDocumentBase(), basename + i);
            tracker.addImage(images[i], i);
        }
    }

    // This is the thread that runs the animation, and the methods
    // that start it and stop it.
    private Thread animator_thread = null;
    public void start() {
        if (animator_thread == null) {
            animator_thread = new Thread(this);
            animator_thread.start();
        }
    }
    public void stop() {
        if ((animator_thread != null) && animator_thread.isAlive()) 
            animator_thread.stop();
        animator_thread = null;
    }
    
    // This is the body of the thread--the method that does the animation.
    public void run() {
        // First, force all the images to be loaded, and wait until
        // they have all loaded completely.
        for (int i = 0; i < num_images; i++) {
            this.showStatus("Loading image: " + i);
            // The argument is the same one we passed to addImage()
            try { tracker.waitForID(i); } catch (InterruptedException e) { ; }
            // Check for errors loading it.
            if (tracker.isErrorID(i)) {
                this.showStatus("Error loading image " + i + "; quitting.");
                return;
            }
        }
        this.showStatus("Loading images: done.");
        
        // Now do the animation
        while(true) {
            if (++current_image >= images.length) current_image = 0;
            this.getGraphics().drawImage(images[current_image], 0, 0, this);
            this.getToolkit().sync();  // Force it to be drawn *now*.
            try { Thread.sleep(200); } catch (InterruptedException e) { ; }
        }
    }
}
