/**
* AssignmentTest
*
* Perform the assignment benchmark for the jBYTEmark
* benchmark. The algorithm presented here was adapted
* from the step-by-step guide found in "Quantitative
* Decision Making for Business" (Gordon, Pressman, and
* Cohn; Prentice-Hall)
*
* As in the original benchmark, this test preserves the
* "squareness" requirements of the arrays.
*
* Test result is in iterations per second.
*/

final class AssignmentTest extends BmarkTest
{

// The array of arrays.

private int [][][] aarray;

/**
* constructor
*
*/

AssignmentTest()
{
    adjust_flag = true; // Has test adjusted to clock?
                                          // Default is false at first,
                                          // so adjust.

    testname = "CPU: Assignment";          // Set test name.

    // Set instance variables to "starting" values.

    array_rows = 25;  // Number of row (default 51).

    array_cols = 25; // of cols (default 51).

    base_iters_per_sec = BMglobals.asgntestbase; // Score indexing value.

    num_arrays = 1;    // Number of arrays.
                                            // Default 1 at start.
}

/*
* initialize
*
*/

private void initialize()
{
    // We'll need new random numbers.

    RandNum rndnum = new RandNum();

    // Allocate the array or arrays.

    aarray = new int [num_arrays][array_rows][array_cols];

    // Garbage collect.

    System.gc();

    // Set up the first array. Then, if there are more arrays,
    // just copy the first to all the rest.

    for (int i = 0; i < array_rows; i++)
        for (int j = 0; j < array_cols; j++)
            aarray[0][i][j] = rndnum.abs_nextwc(50000);

    if (num_arrays > 1)
        for (int i = 1; i < num_arrays; i++)
            for(int j = 0; j < array_rows; j++)
                System.arraycopy(aarray[0][j],0,aarray[i][j],0,array_cols);
}

/*
* DoIteration
*
*/

private long DoIteration()
{
long testTime;      // Duration of the test (milliseconds).

// Start the stopwatch.

testTime = System.currentTimeMillis();

// Step through each of the assignment arrays. Do an assignment
// operation on each.

for (int i = 0; i < num_arrays; i++)
    assignment(i);

testTime = System.currentTimeMillis() - testTime;

return (testTime);

}

/**
* dotest
*
* Perform the actual assignment benchmark test.
* The steps involved are:
*  1. See if the test has already been "adjusted".
*     If it hasn't do an adjustment phase.
*  2. If the test has been adjusted, go ahead and
*     run it.
*/

void dotest()
{

long duration;      // Time in milliseconds for doing test.

// Has it been adjusted yet?

if (adjust_flag == false)
{                                   // Do adjustment phase.
    while (true)
    {
        initialize();

        // See if the current setting meets requirements.

        duration = DoIteration();
        if (duration > BMglobals.minTicks / 10) // minTicks = 100 ticks.
            break;                              // We'll take just 10.

        // Current setting does not meet requirements.
        // Increase # of arrays and try again.

        num_arrays += 1;
    }

    // Scale up to whatever it takes to do 100 clock ticks.

    int factor = (int) (BMglobals.minTicks / duration);
    num_arrays += (factor * num_arrays);

    adjust_flag = true;                // Don't adjust next time.

}   // End adjust section.

// If we fall through preceding IF statement, adjustment
// not necessary -- simply initialize.

initialize();

// Do the test.

duration = DoIteration();

// Calculate the iterations per second. Note that we
// assume here that duration is in milliseconds.

iters_per_sec = (double)num_arrays / ((double)duration
                / (double)1000);

// Debug code.

if (debug_on == true)
{
    System.out.println("--- Assignment test debug data ---");
    System.out.println("Number of arrays: " + num_arrays);
    System.out.println("Elapsed time: " + duration);
    System.out.println("Iterations per sec: " + iters_per_sec);

} //End debug code.

// Clean up and exit.

cleanup();

}

/*
* cleanup
*
*/

private void cleanup()
{
    aarray = new int [0][0][0];   // Empty the array.

    System.gc();                  // Garbage collect it.
}

/*
* assignment
*
*/

private void assignment(int anum)
{
short [][] assignedtableau;

// Set up assignedtableau.

assignedtableau = new short [array_rows][array_cols];

// Calculate minimum costs.

calc_minimum_costs(anum);


// Repeat the following until the number of rows selected
// equals the number of rows in the tableau.

while (first_assignments(anum,assignedtableau) != array_rows)
{
    second_assignments(anum,assignedtableau);
}

}

/*
* calc_minimum_costs
*
* Revise the tableau by calculating the minimum costs on a
* row and column bases. These minima are subtracted from
* their rows and columns, creating a new tableau.
*/

private void calc_minimum_costs(int anum)
{

int currentmin; // Current minimum.

// Determine minimum costs on row basis. This is done by
// subtracting -- on a row-per-row basis, the minimum
// value of that row.

for (int i = 0; i < array_rows; i++)
{
    currentmin = 5000001; // Initialize minimum.
    for (int j = 0; j < array_cols; j++)
        if (aarray[anum][i][j] < currentmin)
            currentmin = aarray[anum][i][j];

    for(int j = 0; j < array_cols; j++)
        aarray[anum][i][j] -= currentmin;
}

// Determine the minimum cost on a column basis. This works just
// as above, only now we step through the array column-wise.

for (int j = 0; j < array_cols; j++)
{
    currentmin = 5000001;       // Initialize minimum.
    for (int i = 0; i < array_rows; i++)
        if (aarray[anum][i][j] < currentmin)
            currentmin = aarray[anum][i][j];

    // Here we'll take the trouble to see if the current
    // minimum is zero. This is likely worth it, since the
    // preceding loop will have created at least one zero in
    // each row. We can save ourselves a few iterations.

    if (currentmin != 0)
        for (int i = 0; i < array_rows; i++)
            aarray[anum][i][j] -= currentmin;
}

}

/*
* first_assignments
*
* Do first assignments.
* The assignedtableau[] array holds a set of values that indicate
* the assignment of a value, or its elimination.
* The values are:
*  0 = Item is neither assigned nor eliminated
*  1 = Item is assigned
*  2 = Item is eliminated
* Retuns the number of selections made. If this equals
* the number of rows, then an optimum has been determined.
*/

private int first_assignments(int anum,short [][] assignedtableau)
{
int i,j,k;                  // Index variables.
int numassigns;             // # of assignments.
int totnumassigns;          // Total # of assignments.
int numzeros;               // # of zeros in row.
int selected;               // Flag used to indicate selection.

// Clear the assignedtableau, setting all members to show
// that no one is yet assigned, eliminated, or anything.

for (i = 0; i < array_rows; i++)
    for (j = 0; j < array_rows; j++)
        assignedtableau[i][j] = 0;
totnumassigns = 0;

selected = 0;     // Make java happy.

do {
    numassigns = 0;

    // Step through rows. For each one that is not currently
    // assigned, see if the row has only one zero in it. If so,
    // mark that as an assigned row/col. Eliminate other zeros
    // in the same column.

    for (i = 0; i < array_rows; i++)
    {   numzeros = 0;
        for (j = 0; j < array_cols; j++)
            if (aarray[anum][i][j] == 0L)
                if (assignedtableau[i][j] == 0)
                {
                    numzeros++;
                    selected = j;
                }

        if (numzeros == 1)
        {
            numassigns++;
            totnumassigns++;
            assignedtableau[i][selected] = 1;
            for (k = 0; k < array_rows; k++)
                if ((k != i) && (aarray[anum][k][selected] == 0))
                    assignedtableau[k][selected] = 2;
        }
    }

    // Step through columns, doing same as above. Now, be careful
    // of items in the other rows of a selected column.

    for (j = 0; j < array_cols; j++)
    {   numzeros = 0;
        for (i = 0; i < array_rows; i++)
            if (aarray[anum][i][j] == 0)
                if (assignedtableau[i][j] == 0)
                {
                    numzeros++;
                    selected = i;
                }

        if (numzeros == 1)
        {   numassigns++;
            totnumassigns++;
            assignedtableau[selected][j] = 1;
            for (k = 0; k < array_cols; k++)
                if((k != j) && (aarray[anum][selected][k] == 0))
                    assignedtableau[selected][k] = 2;
        }
    }

    // Repeat until no more assignments to be made.

} while (numassigns != 0);

// See if we can leave at this point.

if (totnumassigns == array_rows) return (totnumassigns);

// Now step through the array by row. If you find any
// unassigned zeros, pick the first in the row. Eliminate
// all zeros from that same row and column. This occurs if
// there are multiple optima...possibly.

for (i = 0; i < array_rows; i++)
{   selected = -1;
    for (j = 0; j < array_cols; j++)
        if ((aarray[anum][i][j] == 0) && (assignedtableau[i][j] == 0))
        {
            selected = j;
            break;
        }

    if (selected != -1)
    {
        assignedtableau[i][selected] = 1;
        totnumassigns++;
        for (k = 0; k < array_cols; k++)
            if ((k != selected) && (aarray[anum][i][k] == 0))
                assignedtableau[i][k] = 2;
        for (k = 0; k < array_rows; k++)
            if ((k != i) && (aarray[anum][k][selected] == 0))
                assignedtableau[k][selected] = 2;
    }
}
return (totnumassigns);
}

/*
* second_assignments
*
* This section of the algorithm is difficult to explain.
* It creates the revised tableau. I suggest you refer to
* the algorithm's source, mentioned in comments at the
* beginning of this file.
*/

private void second_assignments(int anum, short [][] assignedtableau)
{
int i,j;                    // Indexes.
short [] linesrow = new short[array_rows];
short [] linescol = new short[array_cols];

int smallest;               // Holds smallest value.
int numassigns;             // Number of assignments.
int newrows;                // New rows to be considered.

// Scan rows, flag each row that has no assignment in it.

for (i = 0; i < array_rows; i++)
{   numassigns = 0;
    for (j = 0; j < array_cols; j++)
        if (assignedtableau[i][j] == 1)
        {
            numassigns++;
            break;
        }
    if (numassigns == 0) linesrow[i] = 1;
}

do {
    newrows = 0;

    // For each row checked above, scan for any zeros. If found,
    // check the associated column.

    for (i = 0; i < array_rows; i++)
    {
        if (linesrow[i] == 1)
            for (j = 0; j < array_cols; j++)
                if (aarray[anum][i][j] == 0)
                    linescol[j] = 1;
    }

    // Now scan checked columns. If any contain assigned zeros, check
    // the associated row.

    for (j = 0; j < array_cols; j++)
        if (linescol[j] == 1)
            for (i = 0; i < array_cols; i++)
                if ((assignedtableau[i][j] == 1) && (linesrow[i] != 1))
                {
                    linesrow[i] = 1;
                    newrows++;
                }

} while (newrows != 0);

// linesrow[n] == 0 indicate rows covered by imaginary line.
// linescol[n] == 1 indicate cols covered by imaginary line.
// For all cells not covered by imaginary lines, determine smallest
// value.

smallest = 5000001;
for (i = 0; i < array_rows; i++)
    if (linesrow[i] != 0)
        for (j = 0; j < array_cols; j++)
            if (linescol[j] != 1)
                if (aarray[anum][i][j] < smallest)
                    smallest = aarray[anum][i][j];

// Subtract the smallest from all cells in the above set.

for (i = 0; i < array_rows; i++)
    if (linesrow[i] != 0)
        for (j = 0; j < array_cols; j++)
            if (linescol[j] != 1)
                aarray[anum][i][j] -= smallest;

// Add smallest to all cells covered by two lines.

for (i = 0; i < array_rows; i++)
    if (linesrow[i] == 0)
        for (j = 0; j < array_cols; j++)
            if (linescol[j] == 1)
                aarray[anum][i][j] += smallest;
}

}
