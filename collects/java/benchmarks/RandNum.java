
/**
* Class RandNum
*
* Provides random numbers based on the second-order linear
* congruential generator that we've always used. Initialization
* constants are suggested by J. G. Skellam.
*/

final class RandNum
{

// The current value of the random number generator.

int [] currval;

/**
* constructor **
*/

// The constructor merely initializes the current value.

public RandNum()
{
    currval = new int[2];       // Allocate array.
    currval[0] = 13;
    currval[1] = 117;
}

/**
* next
*
* Returns the next random number in the sequence.
*/

public int next()
{
    int temp;       // Holds intermediate calculations.

    temp = (currval[0] * 254754 + currval[1] * 529562) % 999563;
    currval[1] = currval[0];
    currval[0] = temp;
    return (temp);
}

/**
* nextwc
*
* Returns next random number, but with a ceiling.
*/

public int nextwc(int ceiling)
{
    return(next() % ceiling);
}

/**
* abs_nextwc
*
* Returns next random number with ceiling; applies absolute value
* function on the way out.
*/

public int abs_nextwc(int ceiling)
{
    int temp;
    temp = nextwc(ceiling);
    return (Math.abs(temp));
}

}
