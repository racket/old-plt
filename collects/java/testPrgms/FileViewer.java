// This example is from the book _Java in a Nutshell_ by David Flanagan.
// Written by David Flanagan.  Copyright (c) 1996 O'Reilly & Associates.
// You may study, use, modify, and distribute this example for any purpose.
// This example is provided WITHOUT WARRANTY either expressed or implied.

import java.awt.*;
import java.io.*;

public class FileViewer extends Frame {
    Button close;
    // Query the size of the specified file, create an array of bytes big
    // enough, and read it in.  Then create a TextArea to display the text
    // and a "Close" button to pop the window down.
    public FileViewer(String filename) throws IOException {
        super("FileViewer: " + filename);
        File f = new File(filename);
        int size = (int) f.length();
        int bytes_read = 0;
        FileInputStream in = new FileInputStream(f);
        byte[] data = new byte[size];
        while(bytes_read < size)
            bytes_read += in.read(data, bytes_read, size-bytes_read);
        
        TextArea textarea = new TextArea(new String(data, 0), 24, 80);
        textarea.setFont(new Font("Helvetica", Font.PLAIN, 12));
        textarea.setEditable(false);
        this.add("Center", textarea);
        
        close = new Button("Close");
        this.add("South", close);
        this.pack();
        this.show();
    }
    
    // Handle the close button by popping this window down 
    public boolean action(Event e, Object what) {
        if (e.target == close) {
            this.hide();
            this.dispose();
            return true;
        }
        return false;
    }
    
    // The FileViewer can be used by other classes, or it can be
    // used standalone with this main() method.
    static public void main(String[] args) throws IOException {
        if (args.length != 1) {
            System.out.println("Usage: java FileViewer <filename>");
            System.exit(0);
        }
        try { Frame f = new FileViewer(args[0]); }
        catch (IOException e) { System.out.println(e); }
    }
}

