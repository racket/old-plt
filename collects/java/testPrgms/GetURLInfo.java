// This example is from the book _Java in a Nutshell_ by David Flanagan.
// Written by David Flanagan.  Copyright (c) 1996 O'Reilly & Associates.
// You may study, use, modify, and distribute this example for any purpose.
// This example is provided WITHOUT WARRANTY either expressed or implied.

import java.net.*;
import java.io.*;
import java.util.*;

public class GetURLInfo {
    public static void printinfo(URLConnection u) throws IOException {
        // Display the URL address, and information about it.
        System.out.println(u.getURL().toExternalForm() + ":");
        System.out.println("  Content Type: " + u.getContentType());
        System.out.println("  Content Length: " + u.getContentLength());
        System.out.println("  Last Modified: " + new Date(u.getLastModified()));
        System.out.println("  Expiration: " + u.getExpiration());
        System.out.println("  Content Encoding: " + u.getContentEncoding());
        
        // Read and print out the first five lines of the URL.
        System.out.println("First five lines:");
        DataInputStream in = new DataInputStream(u.getInputStream());
        for(int i = 0; i < 5; i++) {
            String line = in.readLine();
            if (line == null) break;
            System.out.println("  " + line);
        }
    }
    
    // Create a URL from the specified address, open a connection to it,
    // and then display information about the URL.
    public static void main(String[] args) 
        throws MalformedURLException, IOException
    {
	URL url = new URL(args[0]);
	URLConnection connection = url.openConnection();
	printinfo(connection);
    }
}
