// This example is from the book _Java in a Nutshell_ by David Flanagan.
// Written by David Flanagan.  Copyright (c) 1996 O'Reilly & Associates.
// You may study, use, modify, and distribute this example for any purpose.
// This example is provided WITHOUT WARRANTY either expressed or implied.

import java.io.*;

// This class demonstrates the use of the GrepInputStream class.
// It prints the lines of a file that contain a specified substring.
public class Grep {
    public static void main(String args[]) {
        if ((args.length == 0) || (args.length > 2)) {
            System.out.println("Usage: java Grep <substring> [<filename>]");
            System.exit(0);
        }
        
        try {
            DataInputStream d;
            if (args.length == 2) 
                d = new DataInputStream(new FileInputStream(args[1]));
            else
                d = new DataInputStream(System.in);
            
            GrepInputStream g = new GrepInputStream(d, args[0]);
            
            String line;
            for(;;) {
                line = g.readLine();
                if (line == null) break;
                System.out.println(line);
            }
            g.close();
        }
        catch (IOException e) { System.err.println(e); }
    }
}
