// This example is from the book _Java in a Nutshell_ by David Flanagan.
// Written by David Flanagan.  Copyright (c) 1996 O'Reilly & Associates.
// You may study, use, modify, and distribute this example for any purpose.
// This example is provided WITHOUT WARRANTY either expressed or implied.

import java.awt.*;
import java.io.*;

public class FileLister extends Frame {
    private List list;
    private TextField infoarea;
    private Panel buttons;
    private Button parent, quit;
    private FilenameFilter filter;
    private File cwd;
    private String[] entries;
    
    // Create the graphical user interface, and list the initial directory.
    public FileLister(String directory, FilenameFilter filter) throws IOException
    { 
        super("File Lister"); 
        this.filter = filter;
        list = new List(12, false);
        infoarea = new TextField();
        infoarea.setEditable(false);
        buttons = new Panel();
        parent = new Button("Up a Directory");
        quit = new Button("Quit");
        buttons.add(parent);
        buttons.add(quit);
        this.add("Center", list);
        this.add("South", infoarea);
        this.add("North", buttons);
        this.resize(550, 350);
        this.show();

        // list the initial directory.
        list_directory(directory);
    }
    
    // This method uses the list() method to get all entries in a directory
    // and then displays them in the List component. 
    public void list_directory(String directory) throws IOException {
        File dir = new File(directory);
        
        if (!dir.isDirectory()) 
            throw new IllegalArgumentException("FileLister: no such directory");
        list.clear();
        cwd = dir;
        this.setTitle(directory);
        
        entries = cwd.list(filter);
        for(int i = 0; i < entries.length; i++) 
            list.addItem(entries[i]);
    }
    
    // This method uses various File methods to obtain information about
    // a file or directory.  Then it displays that info in a TextField.
    public void show_info(String filename) throws IOException {
        File f = new File(cwd, filename);
        String info;
        
        if (!f.exists()) 
            throw new IllegalArgumentException("FileLister.show_info(): " +
                               "no such file or directory");
        
        if (f.isDirectory()) info = "Directory: ";
        else info = "File: ";
        
        info += filename + "    ";
        
        info += (f.canRead()?"read   ":"       ") + 
            (f.canWrite()?"write   ":"        ") +
                f.length() + "   " +
                    new java.util.Date(f.lastModified());
        
        infoarea.setText(info);
    }
    
    // This method handles the buttons and list events.
    public boolean handleEvent(Event e) {
        if (e.target == quit) System.exit(0);
        else if (e.target == parent) {
            String parent = cwd.getParent();
            if (parent == null) parent = "/";  // Bug workaround
            try { list_directory(parent); }
            catch (IllegalArgumentException ex) {
                infoarea.setText("Already at top");
            }
            catch (IOException ex) { infoarea.setText("I/O Error"); }
            return true;
        }
        else if (e.target == list) {
            // when an item is selected, show its info.
            if (e.id == Event.LIST_SELECT) {
                try { show_info(entries[((Integer)e.arg).intValue()]); }
                catch (IOException ex) { infoarea.setText("I/O Error"); }
            }
            // When the user double-clicks, change to the selected directory
            // or display the selected file.
            else if (e.id == Event.ACTION_EVENT) {
                try {
                    String item = new File(cwd, (String)e.arg).getAbsolutePath();
                    try { list_directory(item); }
                    catch (IllegalArgumentException ex) {new FileViewer(item);}
                }
                catch (IOException ex) { infoarea.setText("I/O Error"); }
            }
            return true;
        }
        return super.handleEvent(e);
    }
    
    public static void usage() {
        System.out.println("Usage: java FileLister [directory_name] " + 
                   "[-e file_extension]");
        System.exit(0);
    }
    
    // Parse command line arguments and create the FileLister object.
    // If an extension is specified, create a FilenameFilter for it.
    // If no directory is specified, use the current directory.
    public static void main(String args[]) throws IOException {
        FileLister f;
        FilenameFilter filter = null;
        String directory = null;
        
        for(int i = 0; i < args.length; i++) {
            if (args[i].equals("-e")) {
                i++;
                if (i >= args.length) usage();
                filter = new EndsWithFilter(args[i]);
            }
            else {
                if (directory != null) usage();  // Already set
                else directory = args[i];
            }
        }
        
        // if no directory specified, use the current directoy
        if (directory == null) directory = System.getProperty("user.dir");
        // Create the FileLister object
        f = new FileLister(directory, filter);
    }
}

// This class is a simple FilenameFilter.  It defines the required accept()
// method to determine whether a specified file should be listed.  A file
// will be listed if its name ends with the specified extension, or if
// it is a directory.
class EndsWithFilter implements FilenameFilter {
    private String extension;  
    public EndsWithFilter(String extension) {
        this.extension = extension;
    }
    public boolean accept(File dir, String name) {
        if (name.endsWith(extension)) return true;
        else return (new File(dir, name)).isDirectory();
    }
}
