/** A class to play with boolean values */
class Cvt {
/** converts boolean arguments */
  public static void main (String args []) {
    if (args == null || args.length == 0) {
      System.err.println("usage: Cvt true FALSE ...");
      System.exit(1);
    }
    System.out.println("\tvalueOf\ttruth\tequals");
    boolean truth = Boolean.TRUE.booleanValue();
    for (int n = 0; n < args.length; ++ n) {
      Object arg = Boolean.valueOf(args[n]);
      if (arg != Boolean.TRUE)
        System.out.println(args[n] +"\t"+ arg +"\t"+ truth +"\t"+
                        (arg.equals(Boolean.TRUE) ? "ok" : "no"));
    }
  }
}
