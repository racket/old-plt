/**
*
* Class BmarkTest
*
* This is the base class for all tests.
*
*/

abstract class BmarkTest
{

// Instance variables common to all tests follow

boolean adjust_flag; // Initially set false. Set to true
                                  // if the adjustment phase has
                                  // has already occurred on this
                                  // object.
double iters_per_sec;   // Iterations per second. This is
                        // the calculated result of the
                        // current test.

double base_iters_per_sec;  // Baseline iterations per sec. Used
                            // to calculate the index and based
                            // on some standard system.

int numtries;           // # of tries attempted.

double scores[] =       // Array of five scores for confidence
   {0,0,0,0,0};         //  interval testing.

double mean;            // Mean of the scores.

double stdev;           // Standard deviation of scores.

int num_arrays;         // # of arrays used in a test.

int array_rows;         // Size of array in rows. This is also
                        // the variable used to indicate array
                        // size for 1D array tests.

int array_cols;         // Size of array in columns.

int loops_per_iter;         // Some tests increase workload by
                        // adding loops. This holds the # of
                        // loops used in the test.

String testname;        // Name of current test.

// Flag to run test in debug mode

protected boolean debug_on = false;

// Flag to indicate successful test.

boolean testConfidence;

/**
* initialize
*
* Initialize the object. Note that this is NOT a constructor.
* This refers to the adjustment phase that an object goes
* through as the system verifies that the test runs for
* the minimum requires clock ticks (specified in the
* minTicks argument.
*/

private abstract void initialize();

/**
* dotest
*
* Actually performs the benchmark test. The instance
* variables are set appropriately for test results.
* No arguments and no return value.
*/

abstract void dotest();

/**
* dotest (debug version)
*
* Do the test with debug error checking. This is an overload of the
* dotest() method and is usually the same code from test to test so
* abstract isn't appropriate.
*/

void dotest (boolean debug_on)
{
    this.debug_on = debug_on;
    dotest();
    this.debug_on = false;
}

/**
* cleanup
*
* Cleans up after a test. Explicitly deallocates any arrays and
* forces a garbage collection. No arguments and no return value.
*/

private abstract void cleanup();

/**
* benchWithConfidence
*
* Given a reference to a BmarkTest object, this routine
* repeatedly executes its associated benchmark, seeking
* to collect enough scores to get 5 that meet the
* confidence criteria. Return true if OK, else return false.
*/

void benchWithConfidence(int n)
{

double halfinterval;    // Confidence half interval.

// Get first 5 scores. Then begin confidence testing.

for (int i = 0; i < n; i++)
{
    try
    {
        dotest();        // Run once.
    }
    catch (OutOfMemoryError e)  // Handle test errors/exceptions.
    {
        testConfidence = false; // Flag for bad test.
        throw new OutOfMemoryError(testname + ". Error allocating"
            + " memory for test data.");
    }
}

numtries = 5;                       // Show 5 tries already.

// The system allows a maximum of 10 tries before it
// gives up. Since we've done 5 already, we'll allow
// 5 more.

// Enter loop to test for confidence criteria

// ML
/*
while(true)
{
    // Calculate confidence

    halfinterval = calcConfidence();

    // Is half interval 5% or less of mean?
    // If so, we can go home. Otherwise, we
    // have to continue.

    if(halfinterval / mean <= (double).05)
        break;

    // Go get a new score and see if it improves
    // the existing scores.

    do
    {
        if (numtries == 10)
        {
            testConfidence = false;
            return;
        }

        // Run the test

        try
        {
            dotest();               // Run test.
        }
        catch (OutOfMemoryError e)  // Handle test errors/exceptions.
        {
            testConfidence = false; // Flag for bad test.
            throw new OutOfMemoryError(testname + ": Error allocating"
                + " memory for test data.");
        }

        numtries++;

    } while (!seekConfidence());
}
*/

testConfidence = true;
}

/**
* seekConfidence
*
* Given an array of 5 scores PLUS a new score, this routine tries
* the new score in place of each of the other 5 to determine if
* the new score, when replacing one of the others, improves the
* confidence half-interval.
* Return FALSE if failure...original 5 scores unchanged.
* Also calculates new half-interval, mean, and std. deviation.
*/

private boolean seekConfidence()
{

double halfinterval;    // Confidence halfinterval
double stdev_to_beat;   // Standard deviation to be improved
double temp;            // Holding place for score being swapped out
int isbeaten;           // Flag indicating progress; also used to
                        //  hold index of swapped score.

// Calculate original standard deviation.

halfinterval = calcConfidence();
stdev_to_beat = stdev;
isbeaten = -1;

// Try to beat original score by exchanging new score
// with each of the originals one at a time.

for (int i = 0; i < 5; i++)
{
    temp=scores[i];
    scores[i] = iters_per_sec;
    calcConfidence();
    if(stdev_to_beat > stdev)
    {   isbeaten = i;
        stdev_to_beat = stdev;
    }
}

if (isbeaten != -1)
{
    scores[isbeaten] = iters_per_sec;
    return(false);
}

return(true);

}

/**
* calcConfidence
*
* Given a set of 5 scores, calculate the confidence half-interval.
* We'll also calculate the sample mean and sample standard deviation.
* Returns the half-interval. NOTE: This routine presumes a confidence
* of 95% and a confidence coefficient of .95.
*/

private double calcConfidence()
{
double halfinterval;    // Returned half interval value.

// Calculate mean

mean = (scores[0] + scores[1] + scores[2] + scores[3] + scores[4])
       / (double)5;

// Calculate standard deviation -- first get variance.

stdev = (double)0;
for (int i = 0; i < 5; i++)
    stdev += (scores[i] - mean) * (scores[i] - mean);
stdev /= (double) 4;
stdev = Math.sqrt(stdev / (double) 5);

// Now calculate the confidence half-interval. For a confidence
// level of 95% our confidence coefficient gives us a mulitplying
// factor of 2.776. (The upper .025 quartile of a t distribution
// with 4 degrees of freedom.)

halfinterval = stdev * (double) 2.776;
return (halfinterval);
}

}
