/* Cycle through classes */

class A extends B {
    int x;
}

class B extends C {
    int y;
}


class C extends D {
    int w;
}


class D extends A {
    int z;
}

/* Cycle in three interfaces */

interface I extends J, K {
    int a = 2;
}


interface J extends K {
    int a = 2;
}

interface K extends I, J {
    int a = 2;
}


/* Not a tree, this is not an error */

interface I2 extends J2, K2, L2 {
    int a = 2;
}


interface J2 extends K2 {
    int a = 4;
}

interface K2 {
    int a = 2;
}

interface L2 {
    int a = 3;
}

/* No cycle */

interface I3 { }

interface J3 extends I3 {
    int w = 33;
}

class B3 implements J3 {
    int t = w;
}

class C3 extends B3 implements I3 {
    int b = 2;
}

/* Cycle through extends and implements */

interface I4 extends J4 {}

interface J4 extends I4 {
    int w = 44;
}

class B4 implements J4 {
    int t = w;
}

class C4 extends B4 implements I4 {
    int b = 2;
}
