/**
* Class InternalFPF
*
* This is the Java equivalent of a typedef from the ealier C version
* of the BYTEmark. This defines a storage class for internal floating
* point numbers.
*/

class InternalFPF
{

// The following items correspond to fields within the
// floating-point number.

byte type;          // Indicated NORMAL, SUBNORMAL, etc.
byte sign;          // Mantissa sign.
short exp;          // Signed exponent...no bias.
int[] mantissa;     // 64-bit mantissa.

/**
* constructor
*
* This routine really just initializes the number to a zero.
*/

InternalFPF()
{
    type = 0;                 // **PROBABLY NEED TO CHANGE THIS**
    sign = 0;
    exp = 0;
    mantissa = new int[2];    // Allocate space for mantissa.
    mantissa[0] = 0;          // Clear the mantissa.
    mantissa[1] = 0;
}

/**
* InternalFPF
*
* Convert a signed integer into an internal FPF number.
*/

InternalFPF(int myint)
{

// Allocate the new InternalFPF and prepare it.

type = EmFloatPnt.IFPF_IS_NORMAL;
mantissa = new int [2];
mantissa[0] = 0;
mantissa[1] = 0;

// Capture the sign.

if (myint < 0)
{   sign = 1;
    myint = 0 - myint;
}
else
    sign = 0;

// See if you've got a zero. If so, make the resultant FP
// number a true zero and go home.

if (myint == 0)
{
    type = EmFloatPnt.IFPF_IS_ZERO;
    exp = 0;
    return;
}

// Not a true zero. Set the exponent to 32 (internal FPFs' exponents
// have no bias) and load the low and high words into their proper
// locations in the mantissa. Then normalize. The action of
// normalizing slides the mantissa bits into place and sets
// up the exponent properly.

exp = 32;
mantissa[0] = myint;
EmFloatPnt.normalize(this);

}

/**
* copy
*
* Copy the contents of the source InternalFPF into the
* destination. We presume that space has been allocated for
* the destination.
*/

void copy(InternalFPF src)
{
    this.type = src.type;
    this.sign = src.sign;
    this.exp = src.exp;
    this.mantissa[0] = src.mantissa[0];
    this.mantissa[1] = src.mantissa[1];
}

}
