public class testLabel {

    static public void main(String[] args){
	int f, i;

	// Correct use of label
    lab1:  for(i=0; i<10; i++){
	break lab1;
    }

	// Correct use of label
    lab2:  for(i=0; i<10; i++){
	continue lab2;
    }

	// Correct use of label
    lab2: for(i=0; i<10; i++){
	while(i>0){
	    continue lab2;
	}
    }

	// No problem with the following.
    lab2: if(i==0) lab4: lab5: lab6: {System.out.println(i); break lab2; }
    else lab5: ;

	// This is acceptable, the target does not need to be a loop
	// statement but still the break is inside the compound statement.
    lab3:{
	int x=2;
	f =3;
    lab2: for(i=0; i<10; i++){
	while(i>0){
	    continue lab2;
	}
    }
	while (f<0) {
	    break lab3;
	}}

	// Useless, but correct.
    lab12: { i=3;  break lab12;}

	// Error, the continue target must be a while, for, or do
	// statement
    lab3: {
	    f =3;
	    while (f<0) {
		continue lab3;
	    }
	}

    lab6: {{// Error, nested repetition of lab6
	lab6: {;}
	}
    }

	// Correct.
	lab7: switch(f){
	case 1: f = 2; break lab7;
	    case2 : f = 3;
	}

	// The label can be followed by an empty instruction.
    lab10: ;
    }


}

