public class testSwitch {

    // Testing switch statement.

    public static void main(String args[]) {
	int  i=2; final char c=1; final int d = new Integer(8).intValue();
	short s=8;
	long l=2;

	// This is legal
	switch (i) {
	case c : i=2;    // even if c is type char
        case 'a' : i=2;
	case 2+2+2 : i=3; 
	default: i = 3;
	}


	// This is legal
        switch(i){}

	// This is legal
	switch(i){
	     case 2 : default : {l =3;}
	}

	// This is legal
	switch(i){
	    case 2 :  case 3 : l =3;
	}

	// This is legal
	switch(i){
	    case 2 : int x; x =3;
	}

	// This is legal
	switch(i){
	case 2 : int x; x =3;
	case 3 : x = 4;  // x is known
	}

	// This is legal
	switch(s){
	case 3 : s = 4;  
	}

	// This is legal
	switch(c){
	case 1 : }


        x = 5;  // illegal, x is unknown

	// This is illegal
	switch(i){
	case 2 :  int i; x =3; // repeating declaration i
	}


	// This is illegal, non assignable type, -1 too small
	switch(c){
	case -1 : }

	// This is illegal, non assignable type, 70000 too large.
	switch(c){
	case 70000 : }


	// This is illegal
	switch(i){
	case 2 : { int x; x =3; }
	case 3 : x = 4;  // x is unknown here
        case d : i = 3; // d is not a constant expression
	}


	// This is illegal
	switch(i){
	case 2 : int x; x =3;
        case 2 : x = 4; // repeating case
	case 3 :  ;
        case 1+1+1 : x = 4;  // repeating case 
	}


	// This is illegal
	switch(i){
	case 2 : int x; x =3;
	    //case 3 : { case 4 : x = 4; } // can't put case or default in internal block
	}


	// This is illegal
	switch(i){
	    l = 4;
	}

        l = 3;	

	// Several errors in a switch statement.

	// A Switch statement selector must be of type char, byte, short or int
	switch (l) { 
	    // There should not be a declarations in the block.
	    // Actually the only legal first statement is a case or default statement.
             int v; int k; 
             i = 4;
        default: ; // This is legal
	case 2 : i = 1; 
	    // It must be a constant expression on a case statement.
        case i : ;
	    // More than one default case is an error
        default : ; 
	};

    }
}
