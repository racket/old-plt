/**
* Class StringSortTest
*
* Performs the string sort benchmark for the jBYTEmark
* benchmark suite. Note that we handle strings differently
* than one might expect in Java. This test would probably
* more correctly described as a "byte array sort" test. The
* original test sorted "strings" of 8-byte characters. That's
* what we'll do here.
*/

final class StringSortTest extends BmarkTest
{

// Arrays. The string sort actually uses two arrays. (Well, it
// REALLY uses more than that, but...) One is the array of
// string (byte) arrays. The other is the offset pointer array.
// See, to keep track of where strings begin, the system uses
// an array of offset pointers. Each offset pointer points to
// the start of a "string" in the array. This allows the outer
// sorting routines to rapidly locate a string's beginning.

byte [] [] StrArray;    // The array of "strings."

int [] [] optrArray;    // Offset pointer array.

// Following instance variable holds the number of strings
//  currently in the array.

int nstrings;

/**
* constructor
*
* This routine sets the adjustment flag to false (indicating
* no adjustment done yet) and initialize the test name (for
* error context reasons). It also loads up important instance
* variables with their "starting" values.
*/

StringSortTest()
{
    array_rows = 5000;     // Array size (default
                                                // 4111).

    num_arrays = 1;     // # arrays starts at
                                                // default 1.

    base_iters_per_sec = BMglobals.strsorttestbase; // Score indexing base.

    testname = "CPU: String Sort";      // Set test name.

    adjust_flag = true;  // Has test adjusted to clock?
                                        // Default is false at first,
                                        // so adjust.
}

/*
* initialize
*
* Initialize the arrays in preparation for a test.This method not
* only allocates space for the two arrays,it also loads the string
* array with random strings.
*/

private void initialize()
{
    int curroffset;     // Current offset.
    boolean fullflag;   // Indicates whether array is full.
    byte stringlength;  // Holds length of current string.

    // We'll need random numbers.

    RandNum rndnum = new RandNum();

    // Initialize local variables.

    nstrings = 0;         // No strings yet.
    curroffset = 0;       // Offset into array is 0.
    fullflag = false;     // Array not yet full.

    // Allocate appropriate number of string (uh, byte) arrays.
    // Note that we ad an extra 100 bytes to take into account
    // growing that might take place as strings are adjusted.

    StrArray = new byte [num_arrays][array_rows + 100];

    // Garbage collect old stuff.

    System.gc();

    // Now load the array up with random bytes (strings).

    do {
        // Allocate a string with a random length no shorter
        // than 4 bytes and no longer than 80 bytes. Note that
        // we also make sure there's room in the array.

        stringlength = (byte) (4 + (rndnum.abs_nextwc(76) & 255));

        if (((int) stringlength + curroffset + 1) > array_rows)
        {
            stringlength = (byte) ((array_rows-curroffset - 1) & 255);
            fullflag = true;
        }

        // Store the length at the current offset and advance
        // the current offset.

        StrArray[0][curroffset] = stringlength;
        curroffset++;

        // Fill up the rest of this string with random bytes.

        for (int i = 0; i < stringlength; i++)
        {
            StrArray[0][curroffset] = (byte)(97
                                    + rndnum.abs_nextwc(26));
            curroffset++;
        }

        // Increment # of strings counter.

        nstrings++;

    } while (fullflag == false);

    // We now have a single full array initialized. If there
    // is more than one array, copy the original into the
    // others.

    for (int k = 1; k < num_arrays; k++)
        for (int i = 0; i < array_rows; i++)
            StrArray[k][i] = StrArray[0][i];

    // The string array is now taken care of. Set up the
    // offset pointer array.

    optrArray  =  new int [num_arrays][nstrings];

    // Garbage collect old stuff.

    System.gc();

    // Pass through the newly-built string array, building
    // offsets and putting them into the offset pointer array.

    curroffset = 0;
    for (int i = 0; i < nstrings; i++)
    {
        optrArray[0][i] = curroffset;
        curroffset += (int) (StrArray[0][curroffset]) + 1;
    }

    // As with the string array, we've made one copy of the
    // offset pointers, so duplicate this array in the
    // remaining ones.

    for (int k = 1; k < num_arrays; k++)
        for(int i = 0; i < nstrings; i++)
            optrArray[k][i] = optrArray[0][i];
}

/*
* stradjust
*
* Used by the heapsort algorithm. This routine is called to adjust
* the "string" at offset given by soffset to length given by slen.
* The members of the array are moved accordingly, and the length
* of the string at offset i is set to slen. Note that the particular
* array being modified is given by anum.
*/

private void stradjust(int anum,        // Array number.
            int soffset,                // Offset.
            byte slen)                  // New string length.

{
    int direction;      // Direction of adjustment.
    int nbytes;         // # of bytes that will have to move.

    // Determine direction of move. If new length is less than
    // old length, the direction is down. If new length is
    // greater than old length, the direction is up.
    // Note the direction also holds the adjustment amount.

    direction = (int) slen -
                (int) (StrArray[anum][optrArray[anum][soffset]]);

    // See if the adjustment is being made to the last
    // string in the array. If so, we don't have to do
    // anything more than adjust the length field.

    if (soffset == (nstrings - 1))
    {
        StrArray[anum][optrArray[anum][soffset]] = slen;
        return;
    }

    // Calculate the total # of bytes in string array from
    // location soffset + 1 to end of array. Whether we're moving
    // "up" or "down," this is how many bytes we'll have to move.

    nbytes = optrArray[anum][nstrings - 1] +
        (int) StrArray[anum][optrArray[anum][nstrings - 1]] + 1 -
        optrArray[anum][soffset + 1];

    // Calculate the source and destination. Source is position
    // soffset + 1. Destination is position soffset + slen.
    // We'll hand this to arraycopy() and hope it works.
    // ****THIS MAY NEED TO CHANGE IN THE FUTURE - RG ****

    System.arraycopy(StrArray[anum],optrArray[anum][soffset + 1],
            StrArray[anum],optrArray[anum][soffset] + slen + 1,
            nbytes);

    // We have to adjust the offset pointer array. This covers
    // string i + 1 to numstrings - 1.
    // NOTE: This is a departure from the original C version.
    // Since direction not only holds the adjustment amount,
    // but also the direction (that information is contained
    // in the sign of direction).

    for (int i = soffset + 1; i < nstrings; i++)
        optrArray[anum][i] += direction;

    // Store the new length and go home.

    StrArray[anum][optrArray[anum][soffset]] = slen;

}

/*
* strncmp
*
* "Equivalent" to the C strncmp() function. However, this
* routine presumes the strings being compared are the
* "strings" in the StrArray[][] array.
* s1 is offset indicating first string;
* s2 is offset indicating second string.
* Return is < > = 0 if s1 < > = s2
*/

private int strncmp(int anum,   // Array number.
        int s1,                 // String 1 offset.
        int s2,                 // String 2 offset.
        int slength)            // String length.
{

    int j = 0;       // Holds "difference" of characters.

    // Note. Initializing j to 0 was necessary to keep some
    // compilers happy. They noted that it might be possible
    // that string length is zero, in which case the for loop
    // would never be executed and j would never be initialized.

    for (int i = 0; i < slength; i++)
    {   j = (int) (StrArray[anum][s1 + i] - StrArray[anum][s2 + i]);
        if (j != 0)
        {
            return (j);
        }
    }
    return(j);
}

/*
* str_is_less
*
* Pass this method:
* 1) The array number
* 2) Offsets to two strings (a & b)
* It returns true if string a is < string b.
*/

private boolean str_is_less( int anum,     // Array number.
                int aoffset,               // Offset to string a.
                int boffset)               // Offset to string b.
{
    int slen;           // String length

    // Determine which string has the minimum length.
    // Use that to call strncmp(). If they match up to that
    // point, the string with the longer length wins.

    slen = (int) StrArray[anum][optrArray[anum][aoffset]];
    if (slen > (int) StrArray[anum][optrArray[anum][boffset]])
        slen = (int) StrArray[anum][optrArray[anum][boffset]];

    slen = strncmp(anum,optrArray[anum][aoffset] + 1,
        optrArray[anum][boffset] + 1,slen);

    if (slen == 0)
    {
        // The strings match. Return true if the length of
        // string a is greater than the length of string b.

        if (StrArray[anum][optrArray[anum][aoffset]]
            > StrArray[anum][optrArray[anum][boffset]])
            return (true);

        return (false);
    }

    if (slen < 0) return (true);   // a is strictly less than b.

    return(false);                 // Only other possibility.
}

/*
* strsift
*
* Pass this function:
* 1. The string array # being sorted
* 2. Offsets within which to sort
* This performs the core sort of the Heapsort algorithm.
*/

private void strsift(int anum,          // Array number.
        int i, int j)                   // Sort range offsets.
{
    int k;                              // Temporaries.
    byte [] temp = new byte[80];
    byte tlen;

    while ((i + i) <= j)
    {
        k = i + i;
        if (k < j)
            if (str_is_less(anum, k, k+1))
                ++k;

        if (str_is_less(anum, i, k))
        {
            // temp = string[k].

            tlen = StrArray[anum][optrArray[anum][k]];
            System.arraycopy(StrArray[anum], optrArray[anum][k],
                temp, 0, tlen + 1);

            // string [k] = string[i].

            tlen = StrArray[anum][optrArray[anum][i]];
            stradjust(anum, k, tlen);
            System.arraycopy(StrArray[anum],
                optrArray[anum][i],
                StrArray[anum],
                optrArray[anum][k],
                tlen + 1);

            // string[i] = temp.

            tlen = temp[0];
            stradjust(anum, i, tlen);
            System.arraycopy(temp,0,
                StrArray[anum],
                optrArray[anum][i], tlen + 1);
            i = k;
        }
        else
            i = j + 1;
    }
}

/*
* strheapsort
*
* Pass this function the "top" of the region in the array to
* sort. (The bottom of the region is assumed to be zero.)
* The routine performs a heapsort on the array.
*/

private void strheapsort(int anum,    // Array to sort.
            int top)
{

    byte temp[] = new byte[80];   // Temp for exchanging elements.
    byte tlen;                    // Temp to hold length.

    // Build a heap in the array.

    for (int i = (top / 2); i > 0; --i)
        strsift(anum, i, top);

    // Repeatedly extract maximum from heap and place it at
    // the end of the array. When we get done, we'll have a
    // sorted array.

    for (int i = top; i > 0; --i)
    {
        strsift(anum, 0, i);

        // temp = string[0].

        tlen = StrArray[anum][0];
        System.arraycopy(StrArray[anum],0,
            temp,0,
            tlen + 1);

        // string[0] = string[i].

        tlen = StrArray[anum][optrArray[anum][i]];
        stradjust(anum, 0, tlen);
        System.arraycopy(StrArray[anum],
            optrArray[anum][i],
            StrArray[anum],0,
            tlen + 1);

        // string[i] = temp.

        tlen = temp[0];
        stradjust(anum, i, tlen);
        System.arraycopy(temp,0,
            StrArray[anum],
            optrArray[anum][i],
            tlen + 1);
    }
}

/*
* DoIteration
*
* Perform an iteration of the string sort benchmark.
* This will initialize the arrays, perform and time the
* test. Returns the elapsed time in milliseconds.
*/

long DoIteration()
{
    long testTime;      // Duration of test (milliseconds).

    // Start the stopwatch.

    testTime = System.currentTimeMillis();

    // Step through each of the string arrays. Sort each
    // as you go through.

    for (int i = 0; i < num_arrays; i++)
    {
        strheapsort(i, nstrings - 1);
    }

    testTime = System.currentTimeMillis() - testTime;

    return(testTime);
}

/**
* dotest
*
* Perform the actual string benchmark test.
* The steps involved are:
* 1. See if the test has already been "adjusted".
*    If it hasn't do an adjustment phase.
* 2. If the test has been adjusted, go ahead and
*    run it.
*/

void dotest()
{
    long duration;          // Time in milliseconds for doing test.

    // Has it been adjusted yet?

    if (adjust_flag == false)        // Do adjustment phase.
    {
        while (true)
        {
            initialize();

            // See if the current setting meets requirements.

            duration = DoIteration();
            if (duration > BMglobals.minTicks / 10) // minTicks = 100 ticks.
                break;                              // We'll take just 10.

            // Current setting does not meet requirements.
            // Increse # of arrays and try again.

            num_arrays += 1;
        }

        // Scale up to whatever it takes to do 100 clock ticks.

        int factor = (int) (BMglobals.minTicks / duration);
        num_arrays += (factor * num_arrays);

        adjust_flag = true;                // Don't adjust next time.

    }   // End adjust section.

    // (If we fall though preceding IF statement, adjustment
    //  not necessary -- simply initialize.

    initialize();

    // All's well if we get here. Perform the test.

    duration = DoIteration();

    // Calculate the iterations per second. Note that we
    // assume here that duration is in milliseconds.

    iters_per_sec = (double)num_arrays / ((double)duration
                    / (double)1000);

    // Debug code.

    if (debug_on == true)
    {
        System.out.println("--- String Sort test debug data ---");
        System.out.println("Number of arrays: " + num_arrays);
        System.out.println("Elapsed time: " + duration);
        System.out.println("Iterations per sec: " + iters_per_sec);

        // Print out starting chars of first five "words" and last
        // five "words" to see if they're sorted.

        int offset;         // Offset into string array.
        byte word_length;   // Length of currennt word.
        int oA_len;         // Length of optrArray.

        oA_len = optrArray[0].length;

        System.out.println("First five sorted words:");

        for (int i = 0; i < 5; i++)         // First five words.
        {
            offset = optrArray[0][i];
            word_length = StrArray[0][offset];
            for (int j = 1; j < word_length; j++)
            {
                System.out.print((char) StrArray[0][offset + j]);
            }

            System.out.println();
        }

        System.out.println("Last five sorted words:");

        for (int i = oA_len -5; i < oA_len; i++)    // Last five words.
        {
            offset = optrArray[0][i];
            word_length = StrArray[0][offset];
            for (int j = 1; j < word_length; j++)
            {
                System.out.print((char) StrArray[0][offset + j]);
            }

            System.out.println();
        }

    } //End debug code.

    // Clean up and exit.

    cleanup();
}

/*
* cleanup
*
* Clean up after the stringsort benchmark. This simply points the
* two arrays at empty array and performs system garbage collection.
*/

void cleanup()
{
    StrArray = null;            // null the arrays.
    optrArray = null;

    System.gc();                // Garbage collect it.
}

}
