// This example is from the book _Java in a Nutshell_ by David Flanagan.
// Written by David Flanagan.  Copyright (c) 1996 O'Reilly & Associates.
// You may study, use, modify, and distribute this example for any purpose.
// This example is provided WITHOUT WARRANTY either expressed or implied.

import java.io.*;

public class ThreadLister {
    // Display info about a thread.
    private static void print_thread_info(PrintStream out, Thread t, 
                          String indent) {
        if (t == null) return;
        out.println(indent + "Thread: " + t.getName() +
                "  Priority: " + t.getPriority() +
                (t.isDaemon()?" Daemon":"") +
                (t.isAlive()?"":" Not Alive"));
    }
    
    // Display info about a thread group and its threads and groups
    private static void list_group(PrintStream out, ThreadGroup g, 
                       String indent) {
        if (g == null) return;
        int num_threads = g.activeCount();
        int num_groups = g.activeGroupCount();
        Thread[] threads = new Thread[num_threads];
        ThreadGroup[] groups = new ThreadGroup[num_groups];
        
        g.enumerate(threads, false);
        g.enumerate(groups, false);
        
        out.println(indent + "Thread Group: " + g.getName() + 
                "  Max Priority: " + g.getMaxPriority() +
                (g.isDaemon()?" Daemon":""));
        
        for(int i = 0; i < num_threads; i++)
            print_thread_info(out, threads[i], indent + "    ");
        for(int i = 0; i < num_groups; i++)
            list_group(out, groups[i], indent + "    ");
    }
    
    // Find the root thread group and list it recursively
    public static void listAllThreads(PrintStream out) {
        ThreadGroup current_thread_group;
        ThreadGroup root_thread_group;
        ThreadGroup parent;
        
        // Get the current thread group
        current_thread_group = Thread.currentThread().getThreadGroup();
        
        // Now go find the root thread group
        root_thread_group = current_thread_group;
        parent = root_thread_group.getParent();
        while(parent != null) {
            root_thread_group = parent;
            parent = parent.getParent();
        }
        
        // And list it, recursively
        list_group(out, root_thread_group, "");
    }
    
    
    public static void main(String[] args) {
        ThreadLister.listAllThreads(System.out);
    }
}
