// Testing synchronized bytecode.

public class runSynchronized {

    public static void main(String args[]) throws InterruptedException {
	int a=2; int b=1; Integer t,r; Thread thread1, thread2; 

	// Simple execution.
	synchronized(r=new Integer(1)) { System.out.println("once "+r.intValue());}
	synchronized(r) { System.out.println("twice "+r.intValue());}

	t=new Integer(2);
	// // Double lock, no problem.
	//synchronized(t) { 
	//    synchronized(t) { System.out.println(t.intValue());}
	//}
	//
	//thread1 = new Thread(new runSynchronizedWaitT(t,1));
	//
	//// Inside a try with an error. The lock must be relinquished.
	//try{
	//synchronized(t) {
	//    r = null;
	//    thread1.start();
	//    System.out.println("Ok");
	//    // Will raise exception.
	//    r.intValue();
	//}
	//}
	//catch(NullPointerException e){
	//    // t should not be locked.
	//    synchronized(t) { 
	//	  System.out.println("Must print this: t unlocked!");
	//    }
	//}
	/*
	thread2 = new Thread(new runSynchronizedWaitT(t,2));

	// A synchronized with a break. Check to see that the lock on t
	// has been released after the break.
	for(int i=0;i<10;i++){
	    synchronized(t){
		synchronized(t) {
		    if(i==0) thread2.start();
		    System.out.println("i=="+i);
		    if(i>3) break;
		}}
	}

	thread2 = new Thread(new runSynchronizedWaitT(t,3));

	// A synchronized with a continue. Check to see that the lock on t
	// has been released after the continue.
	for(int i=0;i<10;i++){
	    synchronized(t){
		synchronized(t) {
		    if(i==0) thread2.start();
		    System.out.println("i=="+i);
		    if(i>3) continue;
		}}
	}

	thread2 = new Thread(new runSynchronizedWaitT(t,4));
	// A synchronized with a return. Check to see that the lock on
	// t has been released after the return.
	for(int i=0;i<10;i++){
	    synchronized(t){
		synchronized(t) {
		    if(i==0) thread2.start();
		    System.out.println("i=="+i);
		    if(i>3) return;
		}}
	}
	*/
    }
}

class runSynchronizedWaitT implements Runnable {
    private Integer t;
    private int id;

    public runSynchronizedWaitT(Integer t,int id){
	this.t = t; this.id = id;
    }
    
    public void run(){
	synchronized(t) 
	{ System.out.println("Got through "+id); }
    }
    
}
