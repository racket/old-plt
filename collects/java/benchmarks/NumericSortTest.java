/**
* Class NumericSortTest
*
* Performs the numeric sort test for the Java BYTEmark
* benchmark suite. This test implements a heapsort
* algorithm, performed on an int array. Results are
* reported in number of iterations per sec.
*/

final class NumericSortTest extends BmarkTest
{

private int [] [] TestArray;  // Array of arrays

// The test sorts an array of pseudo-random 32-bit
// integers. Actually, it sorts one or more copies of the
// same pseudorandom array, so the object data is an array
// of arrays. In the initialize method, a test sort is run
// repeatedly as the number of arrays is incremented (from one),
// until the test lasts long enough to be accurately measured
// by the system clock.

/**
* constructor
* This routine sets the adjustment flag to false (indicating
* that the initialize method hasn't yet adjusted the number of
* arrays to the clock accuracy, and initializes
* the test name (for error context reasons).
* It also loads up important instance variables with their
* "starting" values. Variables in this method are all instance
* variables.
*/

NumericSortTest()
{
    adjust_flag = true;   // Has test adjusted to clock?
                          // Default is true  at first,
                                        // so adjust.

    testname = "CPU: Numeric Sort"; // Set test name for error context.

    array_rows = 10000; // Length of test array.
                                          // Default is 10000.

    base_iters_per_sec = BMglobals.numsorttestbase; // Score indexing value.

    num_arrays = 1;  // Number of arrays to start.
                     // Default is 1.
}

/*
* initialize
*
* Sets the number of arrays in preparation for the test.
* Repeatedly runs a single iteration of the sort test with one
* more array each time until the elapsed time falls within the
* accuracy of the system clock. It's two-step process: Increment
* the data size linearly until elapsed time is ten times the
* duration of a measurable clock tick, and then calculate the
* data size to get 100 times the minimum clock accuracy. Once
* complete, the test array is ready to go for the actual test.
*/

private void initialize()
{

    long duration;      // Elapsed time (ms) to do doing test iteration.

    while (true)
    {
        // Increment data size until test runs for ten clock ticks
        // Clock tick is minimum measurable interval in milliseconds
        // of the system clock. Global variable minTicks is 100
        // times accuracy of system clock in milliseconds.

        duration = DoIteration();
        if (duration > BMglobals.minTicks/10) // minTicks is 100 ticks.
            break;

        // Current setting does not meet clock requrements.
        // Increase # of arrays and try again.

        num_arrays += 1;

        // Clean up memory used by array.

        freeTestData(); // Nulls array and forces garbage collection.

        // Build new arrays (with one more this time).

        buildTestData();
    }

    // Scale up to whatever it takes to do 100 clock ticks.

    int factor = (int) (BMglobals.minTicks / duration);
    num_arrays += (factor * num_arrays);

    // Clean up memory used by arrays.

    freeTestData();

    // Build new arrays using final adjusted size.

    buildTestData();

    // Flag so that other iterations of test don't have to repeat this.

    adjust_flag = true;
}

/*
* buildTestData
*
* Instantiates array(s) and fills first array with random
* 32-bit integers, then copies first array to other arrays,
* if any.
*/

private void buildTestData()
{
    // Allocate appropriate number of int arrays.

    TestArray = new int [num_arrays][array_rows];

    // We'll need random numbers. (rndnum is seeded as it is created.)

    RandNum rndnum = new RandNum();     // Same way every time.

    // Fill first array in TestArray with pseudo-random values.

    for(int i = 0; i < array_rows; i++)
        TestArray [0][i] = rndnum.next();

    // Copy first array to others, if there's more than one array.

    for(int i = 1; i < num_arrays; i++)
        System.arraycopy(TestArray[0],0,
            TestArray[i],0, array_rows);
}

/**
* dotest
*
* Perform the actual numeric sort benchmark test.
* The steps involved are:
*  1. See if the test has already been "adjusted".
*     If it hasn't do an adjustment phase (initialize()).
*  2. If the test has been adjusted, go ahead and
*      run it.
*/

void dotest()
{
    long duration;          // Time in milliseconds for doing test.

    // Create array(s) and fill with random 32-bit integers.

    buildTestData();

    if (adjust_flag == false)   // Has it been initialized yet?
        initialize();           // Adjust number of arrays to clock.

    duration = DoIteration();

    // Calculate the iterations per second (an instance variable).
    // Note that we assume here that duration is in milliseconds.

    iters_per_sec = (double) num_arrays / ((double) duration /
        (double) 1000);

    // Debug code: Checks that first array (TestArray[0]) is sorted.

    if (debug_on == true)
    {
        System.out.println("--- Numeric Sort debug data ---");
        System.out.println("Number of arrays: " + num_arrays);
        System.out.println("Elapsed time: " + duration);
        System.out.println("Iterations per sec: " + iters_per_sec);

        // Start with second array element and compare to previous.

        for (int i = 1; i < array_rows; i++)
        {
            if (TestArray[0][i] < TestArray[0][i-1])
            {
                // Array wasn't sorted.

                System.out.println("Numeric Sort Error.");
                break;  // Exit for loop.
            }

        }

        System.out.println("After sort, first array starts with:");
        for (int i = 0; i < 10; i++)
            System.out.print(TestArray[0][i] + " ");
        System.out.println("and ends with:");

        for (int i = (array_rows - 10); i < array_rows; i++)
            System.out.print(TestArray[0][i] + " ");
        System.out.print("\n");

    } //End debug code.

    // Clean up and exit

    cleanup();
}

/*
* DoIteration
*
* Perform an iteration of the numeric sort benchmark. Returns
* the elapsed time in milliseconds.
*/

private long DoIteration()
{
    long testTime;      // Duration of the test (milliseconds).

    // Start the stopwatch.

    testTime = System.currentTimeMillis();

    // Step through each array in the array of arrays. Sort each
    // as you go through.

    for (int i = 0; i < num_arrays; i++)
        NumHeapSort(i);

    testTime = System.currentTimeMillis() - testTime;

    return(testTime);   // Returns elapsed time for test iteration.
}

/*
* NumSift
*
* Performs the sift operation on a numeric array,
* constructing a heap in the array.
* Instructions from strsift:
* Pass this function:
*  1. The string array # being sorted
*  2. Offsets within which to sort
* This performs the core sort of the Heapsort algorithm
*/

private void NumSift(int array_number,      // Array number.
        int min, int max)                     // Sort range offsets.
{
    int k;      // Used for index arithmetic.
    int temp;   // Used for exchange.

    while((min + min) <= max)
    {
            k = min + min;
            if (k < max)
                if (TestArray[array_number][k]
                    < TestArray[array_number][k+1])
                    ++k;
            if (TestArray[array_number][min]
                < TestArray[array_number][k])
            {
                temp = TestArray[array_number][k];
                TestArray[array_number][k]
                    = TestArray[array_number][min];
                TestArray[array_number][min] = temp;
                min = k;
            }
            else
                min = max + 1;
    }
}


/*
* NumHeapSort
*
* Sorts one of the int arrays in the array of arrays.
* This routine performs a heap sort on that array.
*/

private void NumHeapSort(int array_number)
{
    int temp;                   // Used to exchange elements.
    int top = array_rows - 1;   // Last index in array. First is zero.

    // First, build a heap in the array. Sifting moves larger
    // values toward bottom of array.

    for (int i = top/2; i > 0; --i)
        NumSift(array_number, i,top);

    // Repeatedly extract maximum from heap and place it at the
    // end of the array. When we get done, we'll have a sorted
    // array.

    for (int i = top; i > 0; --i)
    {
        NumSift(array_number, 0,i);

        // Exchange bottom element with descending top.

        temp = TestArray[array_number][0];
        TestArray[array_number][0] = TestArray[array_number][i];
        TestArray[array_number][i] = temp;
    }
}

/*
* freeTestData
*
* Nulls array and forces garbage collection to free up memory.
*/

private void freeTestData()
{
    TestArray = null;    // Destroy the array.
    System.gc();         // Force garbage collection.
}


/*
* cleanup
*
* Clean up after the numeric sort benchmark. This simply calls
* freeTestData, which nulls the test arrays and forces system
* garbage collection.
* This is a required BmarkTest class method (abstract in super).
*/

private void cleanup()
{
    freeTestData();
}

}
