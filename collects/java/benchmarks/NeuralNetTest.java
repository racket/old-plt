/**
* Class NeuralNetTest
*
* Aka Back Propagationn Neural Net. This code is a modified
* version of the code that was submitted to BYTE Magazine by
* Maureen Caudill. It accompanied her article "Expert Networks,"
* BYTE, Oct. '91, though that article doesn't discuss this type
* of neural net algorithm in particular. For that, see "Back
* Propagation," BYTE, Oct. '97 by William P. Jones and Josiah
* Hoskins.
*
* The author's original heading/comment was as follows:
*
*  Backpropagation Network
*  Written by Maureen Caudill
*  in Think C 4.0 on a Macintosh
*
*  (c) Maureen Caudill 1988-1991
*  This network will accept 5x7 input patterns and produce 8 bit
*  output patterns. The source code may be copied or modified
*  without restriction, but no fee may be charged for its use.
*
* RG has modified the code so that it will work on systems other
* than a Macintosh. DMR converted the test from Java to C.
*
* The test reports results as the number of loops (learning
* cycles) performed per second.
*
* During initialization, the test repeats with successively more
* neural net learning cycles loops until the test lasts long enough
* to be accurately measured by the system clock.
*/

// There are 26 input and output patterns in the arrays below (as in
// BYTEmark 2.0), but the test uses only the first five so that it
// doesn't last too long under some Java implementations. -- DMR

final class NeuralNetTest extends BmarkTest
{

// DEFINES (from C version), not all still in use.
// IN_X_SIZE = 5. Number of neurodes/row of input layer.
// IN_Y_SIZE = 7. Number of neurodes/col of input layer.
// MARGIN = 0.1.  How near to 1,0 do we have to come to stop?

private int IN_SIZE = 35;   // Equals IN_X_SIZE*IN_Y_SIZE.
private int MID_SIZE = 8;   // Number of neurodes in middle layer.
private int OUT_SIZE = 8;   // Number of neurodes in output layer.
private int MAXPATS = 5 ;   // Number input and output patterns.
private double BETA = 0.09;     // Beta learning constant.
private double ALPHA = 0.09;    // Momentum term constant.
private double STOP = 0.1;      // When worst_error less than STOP,
                                // then training is done.

// Various arrays and other variables used by neural net.

// Middle layer weights.
private double [][] mid_wts = new double [MID_SIZE][IN_SIZE];

// Output layer weights.
private double [][] out_wts = new double [OUT_SIZE][MID_SIZE];

// Middle layer output.
private double [] mid_out = new double [MID_SIZE];

// Output layer output.
private double [] out_out = new double [OUT_SIZE];

// Middle layer errors.
private double [] mid_error = new double [MID_SIZE];

// Output layer errors.
private double [] out_error = new double [OUT_SIZE];

// Last weight change.
private double [][] mid_wt_change = new double [MID_SIZE][IN_SIZE];

// Last weight change.
private double [][] out_wt_change = new double [OUT_SIZE][MID_SIZE];

// Measure of doneness.
private double [] tot_out_error = new double [MAXPATS];

// Cumulative middle layer changes.
private double [][] mid_wt_cum_change = new double [MID_SIZE][IN_SIZE];

// Cumulative output changes..
private double [][] out_wt_cum_change = new double [OUT_SIZE][MID_SIZE];

// Average error for each pattern.
private double [] avg_out_error = new double [MAXPATS];

private double  worst_error;   // Worst error for each pass through data.
private double  average_error; // Average error for each data pass.
private int iteration_count;    // Number of passes thru network so far.
private int numpats;            // Number of patterns in data file (26).
private int numpasses;          // Number of training passes.
private boolean learned;        // Flag: network has learned all patterns.

// Input pattern data, enough for 26 patterns, each 5 neurodes per row
// and 7 neurodes per column, or 26 35-member arrays. Zeros will
// be converted to 0.1 and ones to 0.9 before the neural net starts.
// Not all 26 patterns may be used depending on the value of MAXPATS.

private double [][] in_pats =
{
{0,0,1,0,0,0,1,0,1,0,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1,1,0,0,0,1,1,0,0,0,1},
{1,1,1,1,0,1,0,0,0,1,1,0,0,0,1,1,1,1,1,0,1,0,0,0,1,1,0,0,0,1,1,1,1,1,0},
{0,1,1,1,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,1,1,1,0},
{1,1,1,1,0,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,0},
{1,1,1,1,1,1,0,0,0,0,1,0,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,0,0,1,1,1,1,1},
{1,1,1,1,1,1,0,0,0,0,1,0,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0},
{0,1,1,1,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0,0,1,0,0,1,1,1,0,0,0,1,0,1,1,1,0},
{1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1},
{0,1,1,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,1,1,0},
{0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,1,0,0,0,1,1,0,0,0,1,0,1,1,1,0},
{1,0,0,0,1,1,0,0,1,0,1,0,1,0,0,1,1,0,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,0,1},
{1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,1,1,1,1},
{1,0,0,0,1,1,1,0,1,1,1,0,1,0,1,1,0,1,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1},
{1,0,0,0,1,1,1,0,0,1,1,0,1,0,1,1,0,1,0,1,1,0,1,0,1,1,0,0,1,1,1,0,0,0,1},
{0,1,1,1,0,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,0,1,1,1,0},
{1,1,1,1,0,1,0,0,0,1,1,0,0,0,1,1,1,1,1,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0},
{0,1,1,1,0,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,1,0,1,1,0,0,1,1,0,1,1,1,1},
{1,1,1,1,0,1,0,0,0,1,1,0,0,0,1,1,1,1,1,0,1,0,1,0,0,1,0,0,1,0,1,0,0,0,1},
{0,1,1,1,1,1,0,0,0,0,1,0,0,0,0,0,1,1,1,0,0,0,0,0,1,0,0,0,0,1,1,1,1,1,0},
{1,1,1,1,1,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0},
{1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,0,1,1,1,0},
{1,0,0,0,1,1,0,0,0,1,0,1,0,1,0,0,1,0,1,0,0,1,0,1,0,0,1,0,1,0,0,0,1,0,0},
{1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,1,0,1,1,0,1,0,1,1,0,1,0,1,0,1,0,1,0},
{1,0,0,0,1,0,1,0,1,0,0,1,0,1,0,0,0,1,0,0,0,1,0,1,0,0,1,0,1,0,1,0,0,0,1},
{1,0,0,0,1,0,1,0,1,0,0,1,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0},
{1,1,1,1,1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,1,1,1,1,1}
};

// Desired output patterns, 26 of them to match 26 input patterns, though
// all may not be used depending on the value of MAXPATS.

private double [][] out_pats =
{
{0,1,0,0,0,0,0,1},{0,1,0,0,0,0,1,0},{0,1,0,0,0,0,1,1},{0,1,0,0,0,1,0,0},
{0,1,0,0,0,1,0,1},{0,1,0,0,0,1,1,0},{0,1,0,0,0,1,1,1},{0,1,0,0,1,0,0,0},
{0,1,0,0,1,0,0,1},{0,1,0,0,1,0,1,0},{0,1,0,0,1,0,1,1},{0,1,0,0,1,1,0,0},
{0,1,0,0,1,1,0,1},{0,1,0,0,1,1,1,0},{0,1,0,0,1,1,1,1},{0,1,0,1,0,0,0,0},
{0,1,0,1,0,0,0,1},{0,1,0,1,0,0,1,0},{0,1,0,1,0,0,1,1},{0,1,0,1,0,1,0,0},
{0,1,0,1,0,1,0,1},{0,1,0,1,0,1,1,0},{0,1,0,1,0,1,1,1},{0,1,0,1,1,0,0,0},
{0,1,0,1,1,0,0,1},{0,1,0,1,1,0,1,0}
};

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

NeuralNetTest()
{
    adjust_flag = true; // Has test adjusted to clock?
                                        // Default is false at first,
                                        // so adjust.

    testname = "FPU: Neural Net"; // Test name for error context

    base_iters_per_sec = BMglobals.nnettestbase; // Score indexing value.

    loops_per_iter = 1;   // Number of loops at start.
                                            // Default is 1.

    numpats = MAXPATS;      // 26 input and output patterns.

    // Adjust input values of 0 and 1 to 0.1 and 0.9, respectively.

    for (int patt = 0; patt < numpats; patt++)
    {
        for (int i = 0; i < IN_SIZE; i++)
        {
            if (in_pats [patt][i] >= 0.9)
                in_pats [patt][i] = 0.9;
            if (in_pats [patt][i] <= 0.1)
                in_pats [patt][i] = 0.1;
        }
    }
}

/*
* initialize
*
* Sets the number of times the test loops through the neural net
* algorithm in preparation for the test. Repeatedly runs a single
* iteration of the test with increasingly more loops until the
* elapsed time falls within the accuracy of the system clock. Once
* complete, the test is the right size for the actual timed run.
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
        if (duration > BMglobals.minTicks/10) // minTicks is 100 ticks.
            break;

        // Current setting does not meet clock requrements.
        // Increse number of test loops and try again.

        loops_per_iter += 1;

    }

    // Scale up to whatever it takes to do minTicks (100 clock ticks).

    int factor = (int) (BMglobals.minTicks / duration); // int division
    loops_per_iter += (factor * loops_per_iter);

    // Set flag so that other iterations of test don't have to repeat this.

    adjust_flag = true;
}

/**
* dotest
*
*
* Perform the actual benchmark test.
* The steps involved are:
*  1. See if the test has already been "adjusted".
*     If it hasn't do an adjustment phase (initialize()).
*  2. If the test has been adjusted, go ahead and
*      run it.
*/

void dotest()
{
    long duration;          // Time in milliseconds for doing test.

    // Set up test data.

    buildTestData();

    if(adjust_flag==false)  // Has it been initialized yet?
        initialize();       // Adjust number of bitops to clock.

    // All's well if we get here. Perform the test.

    duration=DoIteration();

    // Calculate the bitfield operations per second (stored in an
    // instance variable). Note that we assume here that duration
    // is in milliseconds.

    iters_per_sec = (double) loops_per_iter / ((double) duration
                    / (double) 1000);

    // Debug code: Reports some result details.

    if (debug_on == true)
    {
        System.out.println("--- Neural Net debug data ---");
        System.out.println("Number loops per iteration: "
            + loops_per_iter);
        System.out.println("Elapsed time: " + duration);
        System.out.println("Neural net cycles per sec: "
                            + iters_per_sec);
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

    for (int i = 0; i < loops_per_iter; i++)
    {
        randomize_wts();     // Initialize wts. of mid and out layers.
        zero_changes();      // Zero out the weight change arrays.
        iteration_count = 1; // Strictly for output reporting (debug).
        learned = false;     // Instance property.
        numpasses = 0;       // Used here only, and for debug.

        while (! learned)
        {
            for (int patt = 0; patt < numpats; patt++)
            {
                worst_error = 0.0;  // Reset each pass, (intance variable).
                move_wt_changes();  // Move last passes wt. changes to
                                    // to momentum array mid_wt_change.
                do_forward_pass(patt);  // Forward pass through net.
                do_back_pass(patt);     // Backward propagation of error.
                iteration_count++;
            }
            numpasses++;            // Count passes to learn.
            learned = check_out_error();
        }
    }

    // Stop the stopwatch.

    testTime = System.currentTimeMillis() - testTime;

    return(testTime);   // Returns elapsed time for test iteration.
}

/*
* buildTestData
*
* Builds (or resets) the data used for the test each time the test is run.
*/

private void buildTestData()
{
    // Nothing to do here for this test.
}

/*
* do_forward_pass() *
*
* Control function for the forward pass through the network
*/

private void do_forward_pass(int patt)
{
    do_mid_forward(patt);   // Process forward pass, middle layer.
    do_out_forward();       // Process forward pass, output layer.
}

/*
* do_mid_forward()
*
* Process the middle layer's forward pass.The activation of middle
* layer's neurode is the weighted sum of the inputs from the input
* pattern, with sigmoid function applied to the inputs.
*/

private void do_mid_forward(int patt)
{
    double sum;

    for (int neurode = 0; neurode < MID_SIZE; neurode++)
    {
        sum = 0.0;
        for (int i = 0; i < IN_SIZE; i++)
        {
            // Compute weighted sum of input signals.

            sum += mid_wts[neurode][i] * in_pats[patt][i];
        }

        // Apply sigmoid function f(x) = 1/(1+exp(-x)) to weighted sum.

        sum = 1.0 / (1.0 + Math.exp(-sum));
        mid_out[neurode] = sum;
    }
}

/*
* do_out_forward()
*
* Process the forward pass through the output layer. The activation
* of the output layer is the weighted sum of the inputs (outputs
* from middle layer), modified by the sigmoid function.
*/

private void do_out_forward()
{
    double sum;

    for (int neurode = 0; neurode < OUT_SIZE; neurode++)
    {
        sum = 0.0;
        for (int i = 0; i < MID_SIZE; i++)
        {
            // Compute weighted sum of input signals from middle layer.

            sum += out_wts[neurode][i] * mid_out[i];
        }

        // Apply f(x) = 1/(1+exp(-x)) to weighted input.

        sum = 1.0 / (1.0 + Math.exp(-sum));
        out_out[neurode] = sum;
    }
}

/*
* do_back_pass()
*
* Process the backward propagation of error through network.
*/

private void do_back_pass(int patt)
{

    do_out_error(patt);     // Compute error for output layer.
    do_mid_error();         // Compute error for middle layer.
    adjust_out_wts();       // Adjust output layer weights.
    adjust_mid_wts(patt);   // Adjust middle layer weights.
}

/*
* do_out_error(patt)
*
* Compute the error for the output layer neurodes.
* This is simply Desired - Actual.
*/

private void do_out_error(int patt)
{
    double error, tot_error, sum;

    tot_error = 0.0;
    sum = 0.0;

    for (int neurode = 0; neurode < OUT_SIZE; neurode++)
    {
        out_error[neurode] = out_pats[patt][neurode] - out_out[neurode];

        // While we're here, also compute magnitude of total error and
        // worst error in this pass. We use these to decide if we are
        // done yet.

        error = out_error[neurode];
        if (error < 0.0)
        {
            sum += -error;
            if (-error > tot_error)
                tot_error = -error;     // Worst error this pattern.
        }
        else
        {
            sum += error;
            if (error > tot_error)
                tot_error = error;      // Worst error this pattern.
        }
    }

    avg_out_error[patt] = sum / OUT_SIZE;
    tot_out_error[patt] = tot_error;

}

/*
* do_mid_error()
*
* Compute the error for the middle layer neurodes. This is based on
* the output errors computed above. Note that the derivative of the
* sigmoid f(x) is:
*   f'(x) = f(x)(1 - f(x))
* Recall that f(x) is merely the output of the middle layer neurode
* on the forward pass.
*/

private void do_mid_error()
{
    double sum;

    for (int neurode = 0; neurode < MID_SIZE; neurode++)
    {
        sum = 0.0;
        for (int i = 0; i < OUT_SIZE; i++)
            sum += out_wts[i][neurode]*out_error[i];

        // Apply the derivative of the sigmoid here. Because of the
        // choice of sigmoid f(I), the derivative of the sigmoid is
        // f'(I) = f(I)(1 - f(I))

        mid_error[neurode] = mid_out[neurode]
                            * (1-mid_out[neurode]) * sum;
    }
}

/*
* adjust_out_wts()
*
* Adjust the weights of the output layer. The error for the output
* layer has been previously propagated back to the middle layer.
* Use the Delta Rule with momentum term to adjust the weights.
*/

private void adjust_out_wts()
{
    double learn, delta, alph;

    learn = BETA;   // 0.09 default
    alph  = ALPHA;  // 0.09 default

    for (int neurode = 0; neurode < OUT_SIZE; neurode++)
    {
        for (int weight = 0; weight < MID_SIZE; weight++)
        {
                // Standard delta rule.

                delta = learn * out_error[neurode] * mid_out[weight];

                // Now the momentum term.

                delta += alph * out_wt_change[neurode][weight];
                out_wts[neurode][weight] += delta;

                // Keep track of this pass's cum wt changes for next
                // pass's momentum.

                out_wt_cum_change[neurode][weight] += delta;
        }
    }
}

/*
* adjust_mid_wts(patt)
*
* Adjust the middle layer weights using the previously computed
* errors. We use the Generalized Delta Rule with momentum term.
*/

private void adjust_mid_wts(int patt)
{
    double learn, alph, delta;

    learn = BETA;   // 0.09 default
    alph  = ALPHA;  // 0.09 default

    for (int neurode = 0; neurode < MID_SIZE; neurode++)
    {
        for (int weight = 0; weight < IN_SIZE; weight++)
        {
                // First the basic delta rule.

                delta = learn * mid_error[neurode]
                        * in_pats[patt][weight];

                // With the momentum term.

                delta += alph * mid_wt_change[neurode][weight];
                mid_wts[neurode][weight] += delta;

                // Keep track of this pass's cum wt changes for
                // next pass's momentum.

                mid_wt_cum_change[neurode][weight] += delta;
        }
    }
}

/*
* move_wt_changes()
*
* Move the weight changes accumulated last pass into the wt-change
* array for use by the momentum term in this pass. Also zero out
* the accumulating arrays after the move.
*/

private void move_wt_changes()
{
    for (int i = 0; i < MID_SIZE; i++)
    {
        for (int j = 0; j < IN_SIZE; j++)
        {
            mid_wt_change[i][j] = mid_wt_cum_change[i][j];

            // Zero out for next pass accumulation.

            mid_wt_cum_change[i][j] = 0.0;
        }
    }

    for (int i = 0; i < OUT_SIZE; i++)
    {
        for (int j = 0; j < MID_SIZE; j++)
        {
            out_wt_change[i][j] = out_wt_cum_change[i][j];

            // Zero out for next pass accumulation.

            out_wt_cum_change[i][j] = 0.0;
        }
    }
}

/*
* check_out_error()
*
* Check to see if the error in the output layer is below
* MARGIN for all output patterns. If so, then assume the
* network has learned acceptably well. This is simply an
* arbitrary measure of how well the network has learned
* -- many other standards are possible.
*/

private boolean check_out_error()
{

    boolean result = true;
    boolean error = false;

    worst_pass_error();   // Identify the worst error in this pass.
    if (worst_error >= STOP) result = false;    // Default STOP = 0.1

    for (int i = 0; i < numpats; i++)
    {
        if (tot_out_error[i] >= 16.0)
        {
            error = true;
            System.out.println("Learning error: total error >= 16.");
        }
    }

    return(result);    // Or return true or false numbers.
}

/*
* worst_pass_error()
*
* Find the worst and average error in the pass and save it.
*/

private void worst_pass_error()
{
    double error,sum;

    error = 0.0;
    sum = 0.0;
    for (int i = 0; i < numpats; i++)
    {
        if (tot_out_error[i] > error) error = tot_out_error[i];
        sum += avg_out_error[i];
    }

    worst_error = error;
    average_error = sum / numpats;
}

/*
* zero_changes()
*
* Zero out the weight change arrays: mid_wt_change, out_wt_change,
* mid_wt_cum_change, and out_wt_cum_change.
*/

private void zero_changes()
{

    // Zero out weight change values for middle neuron layer.

    for (int i = 0; i < MID_SIZE; i++)
    {
        for (int j = 0; j < IN_SIZE; j++)
        {
                mid_wt_change[i][j] = 0.0;
                mid_wt_cum_change[i][j] = 0.0;
        }
    }

    // Zero out weight change values for output neuron layer.

    for (int i = 0; i < OUT_SIZE; i++)
    {
        for (int j = 0; j < MID_SIZE; j++)
        {
                out_wt_change[i][j] = 0.0;
                out_wt_cum_change[i][j] = 0.0;
        }
    }
}

/*
* randomize_wts()
*
* Intialize the weights in the middle and output layers
* (arrays mid_wts and out_wts) to random values between
* -0.25 and +0.25.
*/

private void randomize_wts()
{
    double value;

    // Reseed random number generator for each test iteration.
    // Having the same pseudo-random number sequence for each
    // iteration is important for consistent test results.

    RandNum rndnum = new RandNum();

    for (int neurode = 0; neurode < MID_SIZE; neurode++)
    {
        for(int i = 0; i < IN_SIZE; i++)
        {
                value = (double) rndnum.abs_nextwc(100000);
                value = value / 100000.0 - 0.5;
                mid_wts [neurode][i] = value / 2.0;
        }
    }

    for (int neurode = 0; neurode < OUT_SIZE; neurode++)
    {
        for(int i = 0; i < MID_SIZE; i++)
        {
                value = (double) rndnum.abs_nextwc(100000);
                value = value / 100000.0 - 0.5;
                out_wts [neurode][i] = value / 2.0;
        }
    }
}

/*
* freeTestData
*
* Nulls arrays created with each test run and forces garbage
* collection to free up memory. Does not affect arrays created
* upon instantiation as these must be there for every test run.
*/

private void freeTestData()
{
    // No major arrays that should be nulled between test iterations.

    System.gc();                // Force garbage collection.
}


/*
* cleanup
*
* Clean up after the benchmark. This calls freeTestData, which
* nulls data arrays of any size and forces system garbage collection.
* This is a required BmarkTest class method (abstract in super).
*/

private void cleanup()
{
    freeTestData();
}

}
