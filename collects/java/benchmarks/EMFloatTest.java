/**
* Class EMFloatTest
*
* Performs the emulated floating point test benchmark for the
* jBYTEmark benchmark suite.
*/

final class EMFloatTest extends BmarkTest
{

// Arrays: The emulated floating point test uses three arrays. The
// operations are tested in "vector-like" fashion (though we don't
// use a Java vector because the members of the array are alike).

InternalFPF [] aarray;          // A array.
InternalFPF [] barray;          // B array.
InternalFPF [] carray;          // C array.

/**
* constructor
*
* This routine sets the adjustment flag to false by default
* (indicating that the test hasn't adjusted to the system clock
* yet) and initializes the test name (for error context readons).
* It also loads up important instance variables with their "starting"
* values. The constructor for this particular test also loads up the
* arrays (explained in the comments for the initialize() routine).
*/

EMFloatTest()          // Initialize instance variables.
{
    adjust_flag = true; // Adjusted to clock?
                                           // Default is false at first,
                                           // so adjust.

    testname = "CPU: FP Emulation"; // Set test name.

    loops_per_iter =  5;   // Number of loops at start.
                                            // Default is 10.

    array_rows = 100;    // Size of arrays.
                                            // Default is 250.

    base_iters_per_sec = BMglobals.fpetestbase; // Score indexing base.
}

/*
* initialize
*
* Initialize the arrays in preparation for a test.
*/

private void initialize()
{

InternalFPF locFPF1 = new InternalFPF();
InternalFPF locFPF2 = new InternalFPF();

// Local random number.

RandNum rndnum = new RandNum();

// Allocate arrays.

aarray = new InternalFPF [array_rows];
barray = new InternalFPF [array_rows];
carray = new InternalFPF [array_rows];

// Instantiate objects.

for (int i = 0; i < array_rows; i++)
{
    aarray[i] = new InternalFPF();
    barray[i] = new InternalFPF();
    carray[i] = new InternalFPF();
}

for(int i = 0; i < array_rows; i++)
{
    locFPF1 = new InternalFPF(rndnum.nextwc(50000));
    locFPF2 = new InternalFPF(rndnum.nextwc(50000) + 1);
    EmFloatPnt.DivideInternalFPF(locFPF1,locFPF2,aarray[i]);
    locFPF2 = new InternalFPF(rndnum.nextwc(50000) + 1);
    EmFloatPnt.DivideInternalFPF(locFPF1,locFPF2,barray[i]);
}


System.gc();    // Do garbage collection.

}

/*
* DoIteration
*
* Perform an iteraton of the emulated floating point
* benchmark. This will perform and time the test.
* (The arrays need not be initialized).
* Returns the elapsed time in milliseconds.
*/

private long DoIteration()
{
long testTime;      // Duration of the test (milliseconds).
int i;              // Index.
int locloops;       // Local for loops.

// Following is a "jump table" that defines which operation
// will be performed per pass through the loop. Each pass
// through the arrays performs operations in the following
// ratios:
// 4 adds, 4 subtracts, 5 multiplies, 3 divides
// (adds and subtracts being nearly the same operation).

byte [] jtable = { 0,0,0,0,1,1,1,1,2,2,2,2,2,3,3,3 };

locloops = loops_per_iter;    // Set # of loops to perform.

// Start the stopwatch.

testTime = System.currentTimeMillis();

while ((locloops--) > 0)
{
    for (i = 0; i < array_rows; i++)
    {
        if((jtable[i % 16])==0)   {
	    // Add.
            EmFloatPnt.AddSubInternalFPF((byte)0,aarray[i],barray[i],carray[i]);
	}
	else if((jtable[i % 16]) == 1) {
	    // Subtract.
            EmFloatPnt.AddSubInternalFPF((byte)1,aarray[i],barray[i],carray[i]);
	}
	else if((jtable[i % 16]) == 2) {
            // Multiply.
            EmFloatPnt.MultiplyInternalFPF(aarray[i],barray[i],carray[i]);
	}
	else if((jtable[i % 16]) == 3) {
            // Divide.
            EmFloatPnt.DivideInternalFPF(aarray[i],barray[i],carray[i]);
        }
    }
}

return(System.currentTimeMillis() - testTime);

}

/**
* dotest
*
* Perform the actual emulated floating-point benchmark
* The steps performed are:
* 1. See if the test has already been "adjusted".
*    If it hasn't do an adjustment phase.
* 2. If the test has been adjusted, go ahead and run it.
*/

void dotest()
{

long duration;      // Time in milliseconds for doing test.

// Has it been adjusted yet?

if(adjust_flag == false)
{

    while(true)    // Do adjustment phase
    {
        initialize();

        // See if the current setting meets requirements.

        duration = DoIteration();
        if (duration > BMglobals.minTicks / 10) // minTicks = 100 ticks.
            break;                              // We'll take just 10.

        // Current setting does not meet requirements.
        //  Increase the # of loops and try again.

        loops_per_iter += 1;
    }

    // Scale up to whatever it takes to do 100 clock ticks.

    int factor = (int) (BMglobals.minTicks / duration);
    loops_per_iter += (factor * loops_per_iter);

    adjust_flag = true;                // Don't adjust next time.

}   // End adjust section.

// If we fall through the preceding IF statement, adjustment
// not necessary -- simply initialize

initialize();

// All's well if we get here... perform the test.

duration = DoIteration();

// Calculate the iterations per second. Note that we
// assume here that duration is in milliseconds.

iters_per_sec = (double)loops_per_iter / ((double)duration /
    (double)1000);

// Debug code.

if (debug_on == true)
{
    System.out.println("--- Emulated Floating Point Test debug data ---");
    System.out.println("Number of loops: "
                        + loops_per_iter);
    System.out.println("Elapsed time (ms): " + duration);
    System.out.println("Iterations per sec: " + iters_per_sec);

} //End debug code.


// Clean up and exit.

cleanup();

}

/*
* cleanup
*
* Clean up after the stringsort benchmark. This simply points the
* arrays at an empty array and performs system garbage collection.
*/

private void cleanup()
{
    // Zap the arrays.

    aarray = null;
    barray = null;
    carray = null;

    // Garbage collect.

    System.gc();

}

}
