/**
* Class BitOpsTest
*
* Performs the bitfield operations test for the Java BYTEmark
* suite. Reports results in number of bitfield operations per sec.
*
* The test does a series of single bit operations (sets,
* clears, and complements) inside a bitmap represented by an array
* of integers (signed in Java). The bitfield operations occur in
* runs of adjacent bits with offset and length of run randomly
* determined.
*
* During initialization, the test repeats with successively more
* loops until the test lasts long enough to be accurately measured
* by the system clock.
*/

final class BitOpsTest extends BmarkTest
{

private int [][] BitFieldOpsArray; // Dual array to hold bitops.

private int [] BitArray;           // Bitmap array to operate on.

int bitops_per_loop; // Number of bit operations performed per loop.
                     // The value is assigned when data set is built
                     // in buildTestData.

/**
* constructor
*
* This routine sets the adjustment flag to false (indicating
* that the initialize method hasn't yet adjusted the number of
* bitfield operations to the clock accuracy, and initializes
* the test name (for error context reasons).
* It also loads up important instance variables with their
* "starting" values. Variables in this method are all instance
* variables declared in the parent class.
*/

BitOpsTest()
{
    adjust_flag = true;  // Has test adjusted to clock?
                                        // Default is false at first,
                                        // so adjust.

    testname = "CPU: Bitfield Operations"; // Test name for context.

    array_rows = 3000; // Length of bitfield array.
                                          // Default is 32768.

    num_arrays = BMglobals.bitoparraysize; // Length of Bitops arrays.
                                           // Default is 30 operations.

    loops_per_iter = 1;     // Start with just one loop in each test.

    base_iters_per_sec = BMglobals.bftestbase; // Indexing value.
}

/*
* initialize
*
* Sets the number of times the test loops through the array of
* bitfield operations in preparation for the test. Repeatedly runs
* with increasingly more loops until the elapsed time falls
* within the accuracy of the system clock. Once complete, the
* test is the right size for the actual timed run.
*/

private void initialize()
{

    long duration;      // Elapsed time (ms) of test iteration.

    while (true)
    {
        // Increment test load until test runs for ten clock ticks.
        // Clock tick is minimum measurable interval in milliseconds
        // of the system clock. Global variable minTicks is 100
        // times accuracy of system clock in milliseconds or 100 ticks.

        duration = DoIteration();
        if (duration > BMglobals.minTicks / 10) // minTicks is 100 ticks.
            break;

        // Current setting does not meet clock requrements.
        // Increse # of bitfield operation loops and try again.

        loops_per_iter += 1;

    }

    // Scale up to whatever it takes to do minTicks (100 clock ticks).

    int factor = (int) (BMglobals.minTicks / duration);
    loops_per_iter += (factor * loops_per_iter);

    // Set flag so that other test iterations don't have to repeat this.

    adjust_flag = true;
}

/**
* dotest
*
*
* Perform the actual bitfield operations benchmark test.
* The steps involved are:
*  1. See if the test has already been "adjusted".
*     If it hasn't do an adjustment phase (initialize()).
*  2. If the test has been adjusted, go ahead and
*      run it.
*/

void dotest()
{
    long duration;          // Time in milliseconds for doing test.

    // Create bitmap that will be used (and possibly reused) for
    // bitfield operations.

    BitArray = new int [array_rows];

    // Create array of two paired arrays that determine position and
    // extent of each bitfield operation. These values are
    // pseudo-randomly determined.

    buildTestData();

    if(adjust_flag == false)  // Has it been initialized yet?
        initialize();         // Adjust number of bitops to clock.

    // All's well if we get here. Perform the test.

    duration = DoIteration();

    // Calculate the bitfield operations per second (stored in an
    // instance variable). Note that we assume here that duration
    // is in milliseconds.

    iters_per_sec = (double)loops_per_iter * (double)bitops_per_loop
                    / ((double)duration / (double)1000);

    // Debug code: Reports some result details.

    if (debug_on == true)
    {
        System.out.println("--- Bitfield Operations debug data ---");
        System.out.println("Number loops per iteration: "
            + loops_per_iter);
        System.out.println("Elapsed time: " + duration);
        System.out.println("Total bitop count: " + (bitops_per_loop
                            * loops_per_iter));
        System.out.println("Bitfield ops per sec: " + iters_per_sec);

    } //End debug code.

    // Clean up and exit

    cleanup();
}

/*
* DoIteration
*
* Perform an iteration of the benchmark.
* Returns the elapsed time in milliseconds.
*/

private long DoIteration()
{
    long testTime;      // Duration of the test (milliseconds).

    // Start the stopwatch.

    testTime=System.currentTimeMillis();

    // Loop through array of offset/run length pairs however
    // many times as are necessary. Which operation runs is
    // based on modulus of index.

    for (int i = 0; i < loops_per_iter; i++)
    {
        for(int j = 0; j < num_arrays; j++)
        {
            if(j % 3 == 0)
                ToggleBitRun(BitFieldOpsArray[0][j],// Address of bits.
                             BitFieldOpsArray[1][j],    // Number of bits.
                             true);                     // Set bits.
	    else if(j % 3 == 1) // Clear run of bits.
                ToggleBitRun(BitFieldOpsArray[0][j],// Address of bits.
                             BitFieldOpsArray[1][j],    // Number of bits.
                             false);                    // Clear bits.
	    else if(j % 3 == 2) // Complement run of bits.
                FlipBitRun(BitFieldOpsArray[0][j], // Address of bits.
			   BitFieldOpsArray[1][j]);   // Number of bits.
        }
    }

    // Stop the stopwatch.

    testTime = System.currentTimeMillis() - testTime;

    return(testTime);   // Returns elapsed time for test iteration.
}

/*
*     ToggleBitRun
*
* Set or clear a run of nbits bits in the bitmap at offset bit_addr.
* Parameter set_bit determines whether bits are set (true) or cleared
* (false).
*/

private void ToggleBitRun(int bit_addr, int nbits, boolean set_bit)
{
    int bindex;   // Index into array.
    int bitnumb;  // Bit number.

    while(nbits-- > 0)
    {
        bindex = bit_addr >> 5;   // Index is number /32.
        bitnumb = bit_addr % 32;  // bit number in word

        if(set_bit)                             // If set bit,
            BitArray[bindex] |= (1 << bitnumb); // Or bit with 1.
        else                                    // Else clear.
            BitArray[bindex] &= ~(1<<bitnumb);  // And bit with 0.
        bit_addr++;                             // Next bit.
    }
}

/*
*     ToggleBitRunFast
*
* Set or clear a run of nbits bits in the bitmap at offset bit_addr.
* Parameter set_bit determines whether bits are set (true) or cleared
* (false). Improved algorithm can work with a whole array element at
* a time.
*/

private void ToggleBitRunFast(int bit_addr, int nbits,
                                boolean set_bit)
{
    int bindex;   // Index into array.
    int bitnumb;  // Bit number.

    while(nbits > 0)
    {
        bindex = bit_addr >> 5;   // Array index is bit address /32.
        bitnumb = bit_addr % 32;  // Bit number in int array element.

        if (bitnumb == 0 && nbits >= 32)
        {
            if (set_bit)                    // If set bit,
                BitArray[bindex] |= (-1);   // Or all bits with 1.
            else                            // Else clear.
                BitArray[bindex] &= (0);    // And all bits with 0.
            bit_addr += 32;                 // Next 32 bits.
            nbits -= 32;
        }
        else
        {
            if (set_bit)                            // If set bit,
                BitArray[bindex] |= (1 << bitnumb); // Or bit with 1.
            else                                    // Else clear.
                BitArray[bindex] &= ~(1 << bitnumb);// And bit with 0.
            bit_addr++;                             // Next bit.
            nbits--;
        }
    }
}

/*
* FlipBitRun
*
* Complements a run of nbits bits in the bitmap at offset bit_addr.
*/

private void FlipBitRun(int bit_addr, int nbits)
{
    int bindex;   // Index into array.
    int bitnumb;  // Bit number.

    while(nbits-- > 0)
    {
        bindex=bit_addr >> 5;   // Index is number /32.
        bitnumb=bit_addr % 32;  // Bit number in word.

        BitArray[bindex] ^= (1L << bitnumb);    // XOR bit with 1.
        bit_addr++;                             // Next bit.
    }
}

/*
* FlipBitRunFast
*
* Complements a run of nbits bits in the bitmap at offset bit_addr.
*/

private void FlipBitRunFast(int bit_addr, int nbits)
{
    int bindex;   // Index into array.
    int bitnumb;  // Bit number.

    while(nbits > 0)
    {
        bindex = bit_addr >> 5;   // Array index is bit address /32.
        bitnumb = bit_addr % 32;  // Bit number in int array member.

        if (bitnumb == 0 && nbits >= 32)
        {
            BitArray[bindex] ^= (-1);       // XOR all bits with 1.
            bit_addr += 32;                 // Next 32 bits.
            nbits -= 32;
        }
        else
        {
            BitArray[bindex] ^= (1 << bitnumb); // XOR bit with 1.
            bit_addr++;                         // Next bit.
            nbits--;
        }
    }
}

/*
* buildTestData
*
* Instantiates an array of two array(s) and fills with parameters
* that determine offset and extent of each bitfield operation.
* Constructs a set of bitmap offsets in first array and run lengths
* in the second array. The offset can be any random number from 0 to
* the size of the bitmap (in bits). The run length can be any random
* number from 1 to the number of bits between the offset and the end
* of the bitmap. Note that the bitmap has 32,768 * 32 bits in it
* (1,048,576 bits).
*/

private void buildTestData()
{
    // Allocate double array of adjusted test size to hold bitfield
    // operation parameters.

    BitFieldOpsArray = new int [2][num_arrays];

    bitops_per_loop = 0;   // Zero bit operations count.

    // Create random number generator (same sequence every time).

    RandNum rndnum = new RandNum();

    // Fill bitops array with random operations parameters.

    for (int i = 0; i < num_arrays; i++)
    {
        int bit_offset;     // bit offset into bitmap

        // First item is offset.

        bit_offset = rndnum.abs_nextwc(array_rows * 32);
        BitFieldOpsArray[0][i] = bit_offset;

        // Next item is run length.

        BitFieldOpsArray[1][i] = rndnum.abs_nextwc(array_rows * 32
                                                    - bit_offset);

        // Keep count of bits for performance statistic.

        bitops_per_loop += BitFieldOpsArray[1][i];
    }
}

/*
* freeTestData
*
* Nulls array and forces garbage collection to free up memory.
*/

private void freeTestData()
{
    BitFieldOpsArray = null;    // Destroy the bitops arrays.
    System.gc();                // Force garbage collection.
}

/*
* cleanup
*
* Clean up after the benchmark. This nulls the bitmap array and
* calls freeTestData, which nulls the bitfield operations array
* and forces system garbage collection.
*/

private void cleanup()
{
    BitArray = null;           // Destroy the bitmap array.
    freeTestData();
}

}
