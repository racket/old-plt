public class runConstExpr {

    public static void main(String args[]) {

      // The followings are legal  

      final char c1 = 'a';
      final char c2 = 'a' + 'b';
      final char c3 = 'a' + 3;
      final char c4 = 'a'*2;
      final char c5 = 'a' & 'b';
      final char c6 = 'z' ^ '1';
      final char c7 = c5 | c6;
      final char c8 = c5 & 2;
      ///final char c9 = 1<<6 & 'a';;
      //final char c10 = 'a'<<1;;
      //final char c11 = 'a'>>2;;
      //final char c12 = 'a'>>>3;;

      final byte by1 = -1;
      final byte by2 = -128;
      final byte by3 = 127;
      //final byte by4 = -1>>8;

      // Not an error, but a truncation of the integer values to 0.
      final short sh1 = (65536*65536*2);
      final short sh2 = (65536*65536*4);
      final short sh3 = (65536*65536*5);
      final short sh4 = 'a'+'b';
      final short sh5 = (byte)256+(byte)256;

      final double d = 'a'+2.3d;
      final double d2 = 'a'*2.1;
      final double d3 = 'a'+2.3;
      //final double d4 = 1.0/0.0;
      //final double d5 = 0.0/0.0;

      final String s1 = 'a'+"<== a";
      final String s2 = 2.3+"b";
      final String s3 = true+"b";
      final String s4 = c1+"b";
      final String s5 = ('a'+'a')+"<== 194";

      final int  i = c1;
      final int  j = i;
      
      final int e = 2+j*(3)/c1 % 3 + 'v';
      final int f = (e==3) ? 1 : 2;
      final long n1 = 2L ^ 1L;
      final int n2 = 5 & 1;
      final int n3 = 100 | 10000;
      final int n4 = -191283 & -33421;
      final long n5 = -1L | -1L;
      // The shift operations
      final int n6 = -1>>1;
      final int n7 =  100>>3;
      final int n8 =  100>>0;
      final int n9 =  -100>>-3;
      final int n10 = -123<<3;
      final int n11 =  123<<8;
      final int n12 =  8<<0;
      final int n13 =  8<<-3;
      final int n14 =  -8<<-3;
      
      //final int n6 = (1<<20) - (1<<20);

      final boolean b4 = (e > 0) || (e < 0) || (e == 0);
      final boolean b5 = e != 0 && e == 0;
      final boolean b6 = '\000' < '\001';
      final boolean b7 = '\000' < 1;
      final boolean b8 = (byte)257 > (short)65536;
      final int h = ~10;
      
      final char z = 1;
      final byte w = 127;
      final byte v = 1000-1000;
      
      // But pizza refuses final byte v2 = 21474836478L - 2147483648L;
      //                   final byte v2 = 1000L - 1000;

      System.out.println(c1);
      System.out.println(c2);
      System.out.println(c3);
      System.out.println(c4);
      System.out.println(c5);
      System.out.println(c6);
      System.out.println(c7);
      System.out.println(c8);
      //System.out.println(c9);
      //System.out.println(c10);
      //System.out.println(c11);
      //System.out.println(c12);

      System.out.println(by1);
      System.out.println(by2);
      System.out.println(by3);
      //System.out.println(by4);

      System.out.println("The shorts");
      System.out.println(sh1);
      System.out.println(sh2);
      System.out.println(sh3);
      System.out.println(sh4);
      System.out.println(sh5);

      System.out.println("The strings");
      System.out.println(s1);
      System.out.println(s2);
      System.out.println(s3);
      System.out.println(s4);
      System.out.println(s5);

      System.out.println(i);
      System.out.println(j);
      System.out.println(n1);
      System.out.println(n2);
      System.out.println(n3);
      System.out.println(n4);
      System.out.println(n5);

      System.out.println("The shift operations");
      System.out.println(n6);
      System.out.println(n7);
      System.out.println(n8);
      System.out.println(n9);
      System.out.println(n10);
      System.out.println(n11);
      System.out.println(n12);
      System.out.println(n13);
      System.out.println(n14);
      
      System.out.println(d);
      System.out.println(d2);
      System.out.println(d3);
      //System.out.println(d4);
      //System.out.println(d5);
      System.out.println(e);
      System.out.println(f);
      System.out.println(b4);
      System.out.println(b5);
      System.out.println(b6);
      System.out.println(b7);
      System.out.println(b8);
      System.out.println(h);
      System.out.println(z);      
      System.out.println(w);      
      System.out.println(v);
      
      switch (i) {
      case e*8 : ;
      case '1': ;
      case true ? 1 : 2 : ;
	  // pizza does not consider it to be a constant expression.
      case (1==1) ? 10 : 11+12+13 : ;
      }
      
    }

}
