public class test11 {

    // Testing switch statement.

    public static void main(String args[]) {
	int  i=2; final char c=1;
	long l=2;

	// Here is a legal switch
	switch (i) {
	case c : i=2;
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

        x = 5;  // illegal, x is unknown

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
        case default: ; // This is legal
	case 2 : i = 1; 
	    // It must be a constant expression on a case statement.
        case i : ;
	    // More than one default case is an error
        case default : ; 
	};


       

    }
    
}
