
class runContractPre {
    
    public int f(int n) {
	System.out.println("In f with "+n);
	return n+100;
    }
    
  public int g(int i) 
    /*@ @pre(i > 0 && x>0) @post(@result>=0 && x>=0)  @*/ 
  { 
    System.out.println("i= "+i+" x= "+x);
    x = x-1;
    return 1;
  }
  
  int x;
  
  public static void main(String args[]){
    runContractPre a = new runContractPre();
    a.x = 1;
    System.out.println(a.g(-1));
  }
}
