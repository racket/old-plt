class runEscapeCharacters {


    public static void main(String args[]){
	String s="0123456789\b\t\n\f\r\"\'\\\000\022\2\10\12\13\14\377\111";

	for(int i=0; i<s.length(); i++)
	    System.out.println(String.valueOf(s.charAt(i)));
    }
}
