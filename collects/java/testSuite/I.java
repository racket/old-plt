package a.b.c;

interface J {
    
    int g(float y);
}


public interface I extends J {
    int x=2; double y=x+1.0;
    int [] b = {1,2,3,4};
    int f(int x);
    int g(float y);
    int h(char a, byte b, short c, long d, double e);
    I [] h(I x, J Y[][]);
}

