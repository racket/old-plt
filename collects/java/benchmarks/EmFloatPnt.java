/**
* Class EmFloatPnt
*
* This class carries the methods for performing emulated
* floating-point on InternalFPF numbers.
*/

final class EmFloatPnt
{

// We'll declare these constants here so we can get at them
// as necessary.

static final short MAX_EXP = 32767;
static final short MIN_EXP = -32767;

static final byte IFPF_IS_ZERO = 0;
static final byte IFPF_IS_SUBNORMAL = 1;
static final byte IFPF_IS_NORMAL = 2;
static final byte IFPF_IS_INFINITY = 3;
static final byte IFPF_IS_NAN = 4;
static final byte IFPF_TYPE_COUNT = 5;

// Following are defined for two-operand operations so
// the routine can figure out what to do.

private static final int ZERO_ZERO = 0;
private static final int ZERO_SUBNORMAL = 1;
private static final int ZERO_NORMAL = 2;
private static final int ZERO_INFINITY = 3;
private static final int ZERO_NAN = 4;
private static final int SUBNORMAL_ZERO = 5;
private static final int SUBNORMAL_SUBNORMAL = 6;
private static final int SUBNORMAL_NORMAL = 7;
private static final int SUBNORMAL_INFINITY = 8;
private static final int SUBNORMAL_NAN = 9;
private static final int NORMAL_ZERO = 10;
private static final int NORMAL_SUBNORMAL = 11;
private static final int NORMAL_NORMAL = 12;
private static final int NORMAL_INFINITY = 13;
private static final int NORMAL_NAN = 14;
private static final int INFINITY_ZERO = 15;
private static final int INFINITY_SUBNORMAL = 16;
private static final int INFINITY_NORMAL = 17;
private static final int INFINITY_INFINITY = 18;
private static final int INFINITY_NAN = 19;
private static final int NAN_ZERO = 20;
private static final int NAN_SUBNORMAL = 21;
private static final int NAN_NORMAL = 22;
private static final int NAN_INFINITY = 23;
private static final int NAN_NAN = 24;

// These instance variables are useful as masks.

private static long lo32mask;      // Lo 32 bits set.
private static long hi32mask;      // Hi 32 bits set.
private static long bit32set;      // 1<<32 -- finds carry out of 32 bit add.
private static long bit63set;      // Highmost bit set.
private static long lo3clear;      // Lowmost 3 bits clear..other set (rounding).

// Static initializer.

static {            // We'll set up all our masks and stuff.

    bit32set = 1;
    bit32set = bit32set << 32;
    lo32mask = bit32set - 1;
    hi32mask = lo32mask << 32;
    bit63set = 1;
    bit63set = bit63set << 63;
    lo3clear = -1 ^ 3;
}

/**
* SetInternalFPFZero
*
*
* Set an InternalFPF number to Zero.
*/

private static void SetInternalFPFZero(InternalFPF dest, byte sign)
{
    dest.type = IFPF_IS_ZERO;
    dest.sign = sign;
    dest.mantissa[0] = 0;
    dest.mantissa[1] = 0;
}

/**
* SetInternalFPFInfinity
*
* Set an InternalFPF number to infinity.
* This can happen if the exponent exceeds MAX_EXP.
* As above, sign picks the sign of infinity.
*/

private static void SetInternalFPFInfinity(InternalFPF dest,
        byte sign)
{
    dest.type = IFPF_IS_INFINITY;
    dest.sign = sign;
    dest.exp = MIN_EXP;   // MIN_EXP = -32767.
    dest.mantissa[0] = 0;
    dest.mantissa[1] = 0;
}

/**
* SetInternalFPFNaN
*
* Set an InternalFPF number to NaN (not a number).
* Note that we emulate an 80x87 as far as the mantissa
* bits go.
*/

private static void SetInternalFPFNaN(InternalFPF dest)
{
    dest.type = IFPF_IS_NAN;
    dest.exp = MAX_EXP;
    dest.sign = 1;
    dest.mantissa[0] = 0x40000000;
    dest.mantissa[1] = 0;
}

/**
* IsMantissaZero
*
* Returns true if InternalFPF's mantissa is all zeros, false otherwise.
*/

private static boolean IsMantissaZero(InternalFPF dest)
{
    if ((dest.mantissa[0] == 0) && (dest.mantissa[1] == 0))
        return(true);
    return(false);
}

/**
* CompUnsigned64Bits
*
* Compare two 64-bit numbers in an "unsigned" way.
* Returns -1 0 +1 if a < = > b
*/

private static long CompUnsigned64Bits(long a, long b)
{

long ap, bp;        // Substitutes for a and b.

// First, shift them both right by 1 (making them unsigned)
// and compare that way.

ap = a >>> 1;
bp = b >>> 1;

if (ap < bp)   return (-1L);
if (ap > bp)   return (1L);

// If we get here, all the upper bits are equal.
// Compare the lowmost bit.

ap = a & 1;
bp = b & 1;
return (ap - bp);
}

/**
* Add32Bits
*
* Add two 32-bit numbers with carry. Yields new 32-bit number
* plus carry. Note that the numbers "coming in" are cast into
* longs so we can catch the carry. The result is that the
* math is carried out as though the numbers were unsigned.
* Since Java doesn't allow us to pass primitive data types by
* reference, this routine accepts an array of int[] as its
* sole argument. It's members are as follows:
*   inarray[0] = carry -- in and out
*   inarray[1] = a -- in and out
*   inarray[2] = b -- in only
*   inarray[3]= c -- in only
* It works like this a = b + c + carry & new carry returned.
*/

private static void Add32Bits(int[] inarray)
{
    long a;
    long b;
    long c;

    // Coerce stuff into locals.

    b = ((long) (inarray[2])) & lo32mask;
    c = ((long) (inarray[3])) & lo32mask;

    // Do addition.

    a = b + c + (long) inarray[0];

    // Get carry to return.

    if ((a & bit32set) != 0)
        inarray[0] = 1;
    else
        inarray[0] = 0;

    // Set up return value correctly.

    inarray[1] = (int) (a & lo32mask);
}

/**
* Sub32Bits
*
* Additive inverse of above.
*/

private static void Sub32Bits(int[] inarray)
{
    long a;
    long b;
    long c;

    // Coerce stuff into locals.

    b = (long)(inarray[2]) & lo32mask;
    c = (long)(inarray[3]) & lo32mask;

    // Do the subtraction.

    a = b;
    a -= c;
    a -= (long) inarray[0];

    // Return the carry.

    if ((a & bit32set) != 0)
        inarray[0] = 1;
    else
        inarray[0] = 0;

    // Return the result.

    inarray[1] = (int) (a & lo32mask);
}

/**
* ShiftMantLeft1
*
* Shift a mantissa (64 bits) to the left 1 bit. Also
* provides a carry bit, which is shifted in at the beginning
* and out at the end.
* Again, since Java doesn't allow us to pass primitive types
* by reference, this will require an array.
* However, so that we don't have to move stuff in and out of
* arrays so much, we'll pull a "trick". The carry will
* be a 1-element array.
*/

private static void ShiftMantLeft1(int[] carry,
        int [] locmantissa)
{
    int loccarry;      // Local to hold carry.

    // Do low int first.
    // Get the carry that's about to "come out."

    if ((locmantissa[1] & 0x80000000) != 0)
        loccarry = 1;
    else
        loccarry = 0;
    locmantissa[1] = (locmantissa[1] << 1) + carry[0];

    // Do high int now.
    // Get carry about to come out.

    if ((locmantissa[0] & 0x80000000) != 0)
        carry[0] = 1;
    else
        carry[0] = 0;

    locmantissa[0] = (locmantissa[0] << 1) + loccarry;
}

/**
* ShiftMantRight1
*
* Shift a mantissa right by 1 bit. Provides carry in and out,
*  as with previous call.
* Again, since Java doesn't allow us to pass primitive types
*  by reference, this will require an array for carry.
*/

private static void ShiftMantRight1(int[] carry,
    int [] locmantissa)
{
    int loccarry;  // Local to hold carry "coming out."

    // Get the "intermediate" carry first.

    loccarry = locmantissa[0] & 1;

    // Shift the mantissa (no sign propagation).

    locmantissa[0] >>>= 1;

    // Put in the previous carry.

    if (carry[0] != 0)
        locmantissa[0] |= 0x80000000;

    // Get the returned carry.

    carry[0] = locmantissa[1] & 1;

    // Shift lower mantissa.

    locmantissa[1] >>>= 1;

    // Put in the intermediate carry.

    if (loccarry != 0)
        locmantissa[1] |= 0x80000000;
}

/**
* StickyShiftMantRight
*
* This is a shift right of the mantissa with a "sticky bit."
* i.e., if a carry of 1 is shifted out of the least
* significant bit, then least significant bit is forced to 1.
*/

private static void StickyShiftMantRight(InternalFPF locnum,
            long amount)
{
int [] carry = new int [1];    // Required to call shift operations.
long i;                        // Index.

if (locnum.type != IFPF_IS_ZERO)     // Don't bother shifting a zero.
{
    // If the amount of shifting will shift everything
    // out of existence, then just clear the whole
    // mantissa and set the lowmost bit to 1.

    if (amount >= 64)
    {
        locnum.mantissa[0] = 0;
        locnum.mantissa[1] = 1;
    }
    else
        for (i = 0; i < amount; i++)
        {
            carry[0] = 0;        // Carry.
            ShiftMantRight1(carry,locnum.mantissa);
            if(carry[0] != 0)
                locnum.mantissa[1] |= 1;
        }
}
}

/**
* normalize
*
* Normalize an internal-representation number. Normalization
* discards empty most-significant bits.
*/

static void normalize(InternalFPF locnum)
{
int[] carry = new int [1];   // Required for shift op.

// As long as there's a highmost 0 bit, shift the significand
// left 1 bit. Each time you do this, though, you've gotta
// decrement the exponent.

while ((locnum.mantissa[0] & 0x80000000) == 0)
{
    carry[0] = 0;    // Carry.
    ShiftMantLeft1(carry,locnum.mantissa);
    locnum.exp--;
}
}

/**
* denormalize
*
* Denormalize an internal floating point number. This means
*  shifting it right until its exponent is equivalent to
*  minimum_exponent. (You have to do this often in order
*  to perform additions and subtractions.)
*/

private static void denormalize(InternalFPF locnum,
            int minimum_exponent)
{
long exponent_difference;

// Check for denormalizing a zero mantissa.
// NOTE: WE SHOULD MAKE THIS AN EXCEPTION. --RG

if(IsMantissaZero(locnum))
{
    System.out.println("ERROR: Zero significand in denormalize\n");
    while(true) { }
}

exponent_difference = locnum.exp - minimum_exponent;
if (exponent_difference < 0)
{
    // The number is subnormal.

    exponent_difference = -exponent_difference;

    // Verify that the exponent difference will not cause
    //  everything to be shifted out of existence.

    if (exponent_difference >= 64)
    {   // Underflow
        SetInternalFPFZero(locnum,locnum.sign);
    }
    else
    {   locnum.exp += exponent_difference;
        StickyShiftMantRight(locnum,exponent_difference);
    }
}
}

/**
* RoundInternalFPF
*
* Round an internal floating point number.
* The kind of rounding we do here is simplest...referred to as
*  "chop". "Extraneous" rightmost bits are simply hacked off.
*/

private static void RoundInternalFPF(InternalFPF locnum)
{

if( (locnum.type == IFPF_IS_NORMAL) ||
    (locnum.type == IFPF_IS_SUBNORMAL))
{
    denormalize(locnum,MIN_EXP);
    if(locnum.type != IFPF_IS_ZERO)
    {   // Clear the extraneous bits (lowmost 3).

        locnum.mantissa[1] &= (int) lo3clear;

        // Check for underflow.

        if(locnum.exp > MAX_EXP)
            SetInternalFPFInfinity(locnum,locnum.sign);
    }
}
}

/**
* choose_nan
*
* Called by routines that are forced to perform math on
*  a pair of NaN's. This routine "selects" which NaN is to
*  be returned.
* The chosen NAN is returned in Z...whose space MUST be allocated
*/

private static void choose_nan(InternalFPF x,
    InternalFPF y,
    InternalFPF z,
    int intel_flag)
{
int i;              // Index.
long result;        // Used to compare numbers.
long lx, ly;        // Locals for conversion.

// Compare the two mantissas; return the larger. Note that we
//  will be emulating an 80387 in this operation.

lx = (x.mantissa[0] << 32) + x.mantissa[1];
ly = (y.mantissa[0] << 32) + y.mantissa[1];
result = CompUnsigned64Bits(lx,ly);

 if(((int) result) == -1){
     // x < y.
     z.copy(y);
 }
 else if(((int) result) == 1){       // x > y.
        z.copy(x);
 }
 else {
             // x == y.
     if (intel_flag == 1)
	 // Operation is addition.
	 z.copy(x);
     else
	 // Operation is multiplication.
	 z.copy(y);
 }
}

/**
* AddSubInternalFPF
*
* Adding or subtracting internal-representation numbers.
* Internal numbers x and y are added/subtracted and the
* result is returned in z...whose space MUST be allocated
*/

static void AddSubInternalFPF(byte operation,
    InternalFPF x,
    InternalFPF y,
    InternalFPF z)
{

// Following locals needed because they get altered.

InternalFPF locx = new InternalFPF();
InternalFPF locy = new InternalFPF();

// Following arrays used in the 32-bit math.

int [] carry = new int [1];
int [] intarray = new int [4];

// Holds the exponent difference prior to shifting mantissas into
// proper alignment.

int exponent_difference;

 int switch1[] = {0,3,3,3,2,1,4,4,3,2,1,4,4,3,2,1,1,1,5,2,1,1,1,1,6}; 

// This big switch statement handles the various
// combinations of operand types.

 int icase = switch1[((x.type * IFPF_TYPE_COUNT) + y.type)];

 if(icase == 0) { // ZERO_ZERO
     z.copy(x);
     if ((x.sign ^ y.sign ^ operation) != 0)
	 z.sign = 0;   // Positive
 }
 else if(icase == 1) {
     // NAN_ZERO NAN_SUBNORMAL NAN_NORMAL NAN_INFINITY
     // SUBNORMAL_ZERO NORMAL_ZERO INFINITY_ZERO INFINITY_SUBNORMAL
     // INFINITY_NORMAL
     z.copy(x);
 }
 else if(icase == 2) {
     // ZERO_NAN SUBNORMAL_NAN NORMAL_NAN
     // INFINITY_NAN
     z.copy(y);
 }
 else if(icase == 3) {
     // ZERO_SUBNORMAL ZERO_NORMAL ZERO_INFINITY
     // SUBNORMAL_INFINITY NORMAL_INFINITY
     z.copy(y);
     z.sign ^= operation;
 }
 else if(icase == 4) {
     //  SUBNORMAL_SUBNORMAL  SUBNORMAL_NORMAL NORMAL_SUBNORMAL NORMAL_NORMAL
     // Copy x and y to locals, since we may have to
     // alter them.
     
     locx.copy(x);
     locy.copy(y);
	
        // Computer sum/difference.

        exponent_difference = locx.exp - locy.exp;
        if (exponent_difference == 0)
        {   // If locx.exp == locy.exp, no shifting required.

            if ((locx.type == IFPF_IS_SUBNORMAL) ||
                (locy.type == IFPF_IS_SUBNORMAL))
                z.type = IFPF_IS_SUBNORMAL;
            else
                z.type = IFPF_IS_NORMAL;

            // Assume that locx.mantissa > locy.mantissa.

            z.sign = locx.sign;
            z.exp = locx.exp;
        }
        else
            if (exponent_difference > 0)
            {
                // locx.exp > locy.exp.

                StickyShiftMantRight(locy,(long) exponent_difference);
                z.type = locx.type;
                z.sign = locx.sign;
                z.exp = locx.exp;
            }
            else    // exponent_difference < 0.
                    // locx.exp < locy.exp.
            {
                StickyShiftMantRight(locx,(long) -exponent_difference);
                z.type = locy.type;
                z.sign = (byte) (locy.sign ^ operation);
                z.exp = locy.exp;
            }

            if ((locx.sign ^ locy.sign ^ operation) != 0)
            {
                // Signs are different - subtract mantissa.
                intarray[0] = 0;        // First borrow = 0.
                intarray[2] = locx.mantissa[1];
                intarray[3] = locy.mantissa[1];
                Sub32Bits(intarray);  // Subtract lo 32 bits.
                z.mantissa[1] = intarray[1];
                intarray[2] = locx.mantissa[0];
                intarray[3] = locy.mantissa[0];
                Sub32Bits(intarray);  // Subtract hi 32 bits.
                z.mantissa[0] = intarray[1];
                if(intarray[0] != 0)
                {
                    // The y.mantissa was larger than the
                    //  x.mantissa leaving a negative result.
                    //  Change the result back to an unsigned
                    //  number and flip the sign flag.

                    z.sign = (byte) (locy.sign ^ operation);
                    intarray[0] = 0;    // First borrow=0.
                    intarray[2] = 0;
                    intarray[3] = z.mantissa[1];
                    Sub32Bits(intarray);
                    z.mantissa[1] = intarray[1];
                    intarray[2] = 0;
                    intarray[3] = z.mantissa[0];
                    Sub32Bits(intarray);
                    z.mantissa[0] = intarray[1];
                }
                else
                {   // The assumption made above (i.e. x.mantissa >=
                    // y.mantissa) was correct. Therefore, do nothing.
                    // z.sign = x.sign;
                }

                if (IsMantissaZero(z))
                {   z.type = IFPF_IS_ZERO;
                    z.sign = 0;   // Positive.
                }
                else
                    if ((locx.type == IFPF_IS_NORMAL) ||
                       (locy.type == IFPF_IS_NORMAL))
                        normalize(z);
            }
            else
            {   // Signs are the same, add mantissas.

                intarray[0] = 0;    // Initial carry.
                intarray[2] = locx.mantissa[1];
                intarray[3] = locy.mantissa[1];
                Add32Bits(intarray);  // Add lo 32 bits.
                z.mantissa[1] = intarray[1];
                intarray[2] = locx.mantissa[0];
                intarray[3] = locy.mantissa[0];
                Add32Bits(intarray);  // Add hi 32 bits.
                z.mantissa[0] = intarray[1];

                if(intarray[0] != 0)   // There was a carry.
                {
                    z.exp++;
                    carry[0] = 1;     // Shift the carry back in.
                    ShiftMantRight1(carry, z.mantissa);
                    z.mantissa[0] |= 0x80000000;
                    z.type = IFPF_IS_NORMAL;
                }
                else
                    if ((z.mantissa[0] & 0x80000000)!= 0)
                        z.type = IFPF_IS_NORMAL;
    }
 }
 else if(icase == 5) { // INFINITY_INFINITY
     SetInternalFPFNaN(z);
 }
 else if(icase == 6) { // NAN_NAN
     choose_nan(x,y,z,1);
 }

// All the math is done. Round the result.

RoundInternalFPF(z);

}

/**
* MultiplyInternalFPF
*
* Two internal-representation numbers x and y are multiplied;
* the result is returned in z.
*/

static void MultiplyInternalFPF(InternalFPF x,
        InternalFPF y,
        InternalFPF z)
{

int i;          // Used as index.
int icase;
InternalFPF locy = new InternalFPF();  // Local so we can alter stuff.
int [] extra_bits = new int [2];    // An extra mantissa.
int [] carry = new int [1];         // Used in shifting.
int [] intarray = new int[4];       // Used in adding.

// As in the preceding function, this large switch statement
// selects among the many combinations of operands.

 int switch1[] = {0,0,0,2,4,1,5,5,1,4,1,5,5,1,4,2,0,0,0,4,3,3,3,3,6};

 icase = switch1[((x.type * IFPF_TYPE_COUNT) + y.type)];
 if(icase == 0) {
     z.copy(x);
     z.sign ^= y.sign;
 }
 else if (icase == 1) {
     z.copy(y);
     z.sign ^= x.sign;
 }
 else if(icase == 2){
     SetInternalFPFNaN(z);
 }
 else if(icase == 3) {
     z.copy(x);
 }
 else if(icase == 4) {
     z.copy(y);
 } 
 else if(icase == 5){

        // Make a local copy of y, since we will be altering
        //  it in the process of multiplying.

        locy.copy(y);

        // Check for unnormal zero arguments.
        // NOTE: SHOULD IT EXIT HERE??? RG

        if (IsMantissaZero(x) || IsMantissaZero(y))
        {
            SetInternalFPFInfinity(z,(byte)0);
            return;
        }

        // Initialize the result.

        if((x.type == IFPF_IS_SUBNORMAL) ||
           (y.type == IFPF_IS_SUBNORMAL))
            z.type = IFPF_IS_SUBNORMAL;
        else
            z.type = IFPF_IS_NORMAL;

        z.sign = (byte) (x.sign ^ y.sign);
        z.exp = (short) (x.exp+y.exp);
        z.mantissa[0] = 0;
        z.mantissa[1] = 0;
        extra_bits[0] = 0;
        extra_bits[1] = 0;

        for (i = 0; i < 64; i++)	// Loop thru all 64 bits.
        {
            // Get rightmost bit of multiplier.

            carry[0] = 0;     // 0 carry.
            ShiftMantRight1(carry,locy.mantissa);
            if(carry[0] != 0)
            {
                // If there was a carry out, add the multiplicand
                //  to the product.

                intarray[0] = 0;            // Next carry *is* zero.
                intarray[2] = z.mantissa[1];
                intarray[3] = x.mantissa[1];
                Add32Bits(intarray);        // Add lo 32 bits.
                z.mantissa[1] = intarray[1];
                intarray[2] = z.mantissa[0];
                intarray[3] = x.mantissa[0];
                Add32Bits(intarray);        // Add hi 32 bits.
                z.mantissa[0] = intarray[1];
                carry[0] = intarray[0];     // Capture the carry.
            }
            else
            {
                carry[0] = 0;      // carry = 0.
            }

            // Shift the product right. Overflow bits get shifted
            // into extra bits. We'll use the overflow later to help
            // with the "sticky" bit.

            ShiftMantRight1(carry,z.mantissa);
            ShiftMantRight1(carry,extra_bits);
        }

        // Normalize.
        // Note that we use a "special" normalization routine
        //  because we need to use the exra bits. (There are
        //  bits that may have been shifted off the bottom that
        //  we want to reclaim...if we can.

        while ((z.mantissa[0] & 0x80000000) == 0)
        {
            carry[0] = 0;
            ShiftMantLeft1(carry,extra_bits);
            ShiftMantLeft1(carry,z.mantissa);
            z.exp--;
        }

        // Set the sticky bit if any bits are set in
        // the extra bits.

        if ((extra_bits[0] | extra_bits[1])!= 0)
            z.mantissa[1]|= 1;

 }
 else if (icase == 6) {
     choose_nan(x,y,z,0);
 }

// All math done...do rounding.

RoundInternalFPF(z);
}

/**
* DivideInternalFPF *
*
* Divide internal FPF number x by y. Result returned in z...whose
*  space MUST be allocated.
*/

static void DivideInternalFPF(InternalFPF x,
        InternalFPF y,
        InternalFPF z)
{

int icase;
InternalFPF locx = new InternalFPF();       // Local for alteration.
int [] extra_bits = new int [2];            // Extra mantissa.
int [] carry = new int [1];                 // Carry (declared as an
                                            // array so we can pass it
                                            // by reference).
long lngytemp;      // Temp for holding long version of y mantissa.
long lngetemp;      // Temp for holding long version of extra bits.
long lngtemp;       // Temp for longs.

int [] intarray = new int [4];  // Array for calling shift functions.

boolean dosub;                  // Flag indicating subtract to do.

 int switch1[] ={0,1,1,2,6,3,7,7,2,6,3,7,7,2,6,4,4,4,0,6,5,5,5,5,8};

// As with the preceding function, the following switch
//  statement selects among the various possible operands.

 icase = switch1[((x.type * IFPF_TYPE_COUNT) + y.type)];
 if(icase == 0){ /*  ZERO_ZERO INFINITY_INFINITY */
     SetInternalFPFNaN(z);
 }
 else if(icase == 1) { /* ZERO_SUBNORMAL ZERO_NORMAL */
     if ((y.mantissa[0] | y.mantissa[1]) == 0)
	 {
	     SetInternalFPFNaN(z);
	 }
     else      
	 SetInternalFPFZero(z, (byte) (x.sign ^ y.sign));
 }
 else if(icase == 2) { /* ZERO_INFINITY SUBNORMAL_INFINITY NORMAL_INFINITY */
     SetInternalFPFZero(z, (byte) (x.sign ^ y.sign));
 }
 else if(icase == 3) {// SUBNORMAL_ZERO NORMAL_ZERO
     if (IsMantissaZero(x))
	 {
	     SetInternalFPFNaN(z);
	 }
     else {
        SetInternalFPFInfinity(z, (byte)0);
        z.sign = (byte)(x.sign ^ y.sign);
     }
 }
 else if(icase == 4) { // INFINITY_ZERO INFINITY_SUBNORMAL INFINITY_NORMAL
     SetInternalFPFInfinity(z, (byte)0);
     z.sign = (byte)(x.sign ^ y.sign);
 }
 else if(icase == 5) { // NAN_ZERO  NAN_SUBNORMAL NAN_NORMAL NAN_INFINITY
     z.copy(x);
 }
 else if(icase == 6) { // ZERO_NAN SUBNORMAL_NAN NORMAL_NAN INFINITY_NAN
     z.copy(y);
 }
 else if(icase == 7) { 
     // SUBNORMAL_SUBNORMAL NORMAL_SUBNORMAL SUBNORMAL_NORMAL
     // NORMAL_NORMAL

    // Make a local copy of x number, since we'll be altering
    // it in the process of doing division.

    locx.copy(x);

    // Check for unnormal zero arguments.

    if ((locx.mantissa[0] | locx.mantissa[1]) == 0)
    {   if ((y.mantissa[0] | y.mantissa[1]) == 0)
            SetInternalFPFNaN(z);
        else
            SetInternalFPFZero(z,(byte)0);
    }
    else if ((y.mantissa[0] | y.mantissa[1]) == 0) {
	SetInternalFPFInfinity(z,(byte)0);
	
    }
    else {
    // Initialize the result.

    z.type = x.type;
    z.sign = (byte) (x.sign ^ y.sign);
    z.exp = (short) (x.exp - y.exp + (64 * 2));
    z.mantissa[0] = 0;
    z.mantissa[1] = 0;
    extra_bits[0] = 0;
    extra_bits[1] = 0;

    while ((z.mantissa[0] & 0x80000000) == 0)
    {
        carry[0] = 0;
        ShiftMantLeft1(carry,locx.mantissa);
        ShiftMantLeft1(carry,extra_bits);

        // Time to subtract yet?
        lngytemp = (long) y.mantissa[0];
        lngytemp = lngytemp << 32;
        lngtemp = (long) y.mantissa[1];
        lngtemp = lngtemp & lo32mask;
        lngytemp = lngytemp | lngtemp;
        lngetemp = (long) extra_bits[0];
        lngetemp = lngetemp << 32;
        lngtemp = (long) extra_bits[1];
        lngtemp = lngtemp & lo32mask;
        lngetemp = lngetemp | lngtemp;

        dosub = true;                // Assume subtract.
        if (carry[0] == 0)
            if (CompUnsigned64Bits(lngytemp,lngetemp) == 1)
                dosub = false;        // No subtract necessary.

        if (dosub)
        {
            // Divisor (y) <= dividend (x), subtract.

            carry[0] = 0;
            intarray[0] = carry[0];
            intarray[2] = extra_bits[1];
            intarray[3] = y.mantissa[1];
            Sub32Bits(intarray);
            extra_bits[1] = intarray[1];
            intarray[2] = extra_bits[0];
            intarray[3] = y.mantissa[0];
            Sub32Bits(intarray);
            extra_bits[0] = intarray[1];
            carry[0] = 1;    // 1 shifted into quotient.
        }
        else    // No subtract.

            carry[0] = 0;

        ShiftMantLeft1(carry,z.mantissa);
        z.exp--;
    }
    }
 }
 else if(icase == 8) { // NAN_NAN
    choose_nan(x,y,z,0);
 }


// Math complete...do rounding.

RoundInternalFPF(z);
}

}
