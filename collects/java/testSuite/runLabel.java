public class runLabel {

    static public void main(String[] args){
	int f, i;

	f = 0;
	// Correct use of label
    lab1:  for(i=1; i<10; i++){
	f += i;
	break lab1;
    }
	
	System.out.println(f);


    lab1:{

	f = 10;
	for(i=1; i<10; i++){
	f += i;
	break lab1;
	}}
	
	System.out.println(f);


    lab1:  for(i=1; i<10; i++){
	f += i;
	if(i<8) continue lab1;
	f += 1;
    }
	
	System.out.println(f);

	f=0; i = 10;
    lab2:do {
	i--;
	if(i<8) continue;
	f += i; 
    }
	while(i>0);

	System.out.println(f);
    }

    
}
