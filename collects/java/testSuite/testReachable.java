public class testReachable {


    public static void main(String args[]){
	int e =3; int k = 3; final boolean b = true; final int n = 5;

	while(e>0){
	    break;
	    // Not reached.
	    e = 3;
	    e = 5;
	}

	l : break l;

	// Reachable. But Sun's compiler is fooled on that one.
	try {
	    throw new RuntimeException();
	    // Not reached.
	    e = 4;
	}
	catch (RuntimeException f){};

	while(e>0){
	    break;
	    // Not reached.
	    l1 : { e = 5;
	    break l1; }
	}

	// e=8 is not reachable.
	// Sun's compiler do not report this error.
	for(int i=0; n>5; i++) 
	    e = 8;
	
	// e = 9 is considered reachable.
	if(!b) 
	    e = 9;
	// e = 10 is considered unreachable.
	// Sun's compiler do not report this error.
	while(!b) 
	    e = 10;

	switch(e){
	case 1:  break; 
	    // Unreachable.
	    k = 2;
	case 2:  k = 3;
	}

	while(e < 9){
	    while(true);
	    // unreachable.
	    e =11;
	    for(int i=0; true; i++);
	    // unreachable
	    e = 12;
	    for(int i=0;;);
	    // unreachable
	    e =13;
	    for(int i=0; true; i++) break;
	    // reachable
	    e = 14;
	    for(int i=0;;) break;
	    // reachable
	    e = 15;
	}

	while(true) { l2: break l2;}
	// In my compiler, an empty statement is not flagged as unreachable.
	;;;;
	// This is not reachable.
	e = 16;
	l : while(true) { l2: break l; };
	// This is reachable.
	e = 17;

	while(e<8){
	    l : while(true) { 
		 while(true); 
		 // Not reachable
	         l2: break l; 
	    }
	    // This is not reachable.
	    e = 18;

	    // The throw itself is not reachable.
	    while(false) { throw new RuntimeException(); }
	    // This is reachable.
	    e = 19;

	    while(true) { throw new RuntimeException(); }
	    // This is unreachable.
	    e = 20;
	}
	
	while(true) L:{ k = 5;  { break; }}
	// This is reachable
	e = 21;

	while(true) {
	    // Catch is unreachable.
	    try {  } catch(InterruptedException f){break;}
	}
	// Unreachable.
	while(e<9){
	    try{
		if(true) throw new RuntimeException(); 
		// This is reachable
		e = 23;
		if(true) break;
		// This is reachable
		e = 24;
	    }
	    catch(RuntimeException f){};
	}

	while(true) L:{ k = 5;  { break L; }}
	// This is unreachable
	e = 25;

	L:while(true) { k = 5;  { break L; }}
	// This is reachable
	e = 26;

	L:while(true) { k = 5;  { break; }}
	// This is reachable
	e = 27;
	
	while(e>0){
	    do { } while (true);
	    // Unreachable
	    e = 28;

	    do {break; } while (true);
	    // reachable
	    e = 29;
	}

        L: while(e>0) { 
	    while(true) break L; 
	    // Unreachable.
	    e = 30;
	}
	
        L: while(true) { 
	    while(false) 
		// Unreachable
		break L; 
	    // reachable.
	    e = 31;
	}

	// Unreachable
	e = 32;

        L: while(true) { 
	    while(true);
	    // Unreachable
	    k= 4;
	    while(true) break L; 
	    // unreachable.
	    e = 33;
	}

	// unreachable, but Sun's compiler treats it as reachable since
	// after k=4 (unreachable) the while is treated as reachable.
	e = 34;

	while(false | true);
	// Unreachable
	e = 35;
    }
}

