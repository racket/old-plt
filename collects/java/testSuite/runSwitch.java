
public class runSwitch {

    static void main(String args[]){
      byte b; char c; short s; int i;
	
      b = 3;
      switch(b) {
      case 2 : System.out.println(2);
	break;
      case 3 : System.out.println(3);
	break;
      case 1 : System.out.println(1);
	break;
      case 4 : System.out.println(4);
      default:
      }
      
 	c = 'a';
	switch(c) {
	case 'b' : System.out.println('b');
	    break;
	case 'z' : System.out.println('z');
            break;
	case 's' : System.out.println('s');
	    break;
	case 'a' : System.out.println('a');
	}

	// Nothing is printed by this one.
	c = 'o';
	switch(c) {
	case 'b' : System.out.println('b');
	    break;
	case 'z' : System.out.println('z');
            break;
	case 's' : System.out.println('s');
	    break;
	case 'a' : System.out.println('a');
	}

	s = -1;
	switch(s) {
	case -2 : System.out.println(s);
	    break;
	case -1 : System.out.println(s);
            break;
	case -3 : System.out.println(s);
	    break;
	case -4 : System.out.println(s);
	}
      

    }
}
