// Code of JLS page 224.

class Super {
    static { System.out.print("Super ");}
}

class One {
    static { System.out.print("One ");}
}

class Two extends Super {
    static {System.out.print("Two ");}
}

class test{
    public static void main(String[] args) {
	One o = null;
	Two t = new Two();
	System.out.println((Object)o == (Object)t);
    }
}
    
