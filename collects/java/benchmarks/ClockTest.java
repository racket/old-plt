/**
* A system timer test class to determine the smallest interval in
* milliseconds that the Java timer can deliver on the test system.
* You don't have to instantiate ClockTest nor should you. The class
* tests the system timer once when it's first loaded into memory with
* a static initializer block.
*
*/

final class ClockTest
{

private static long ClockTick;      // Stores minimum clock tick.


// Test system clock with static initializer.

static {

    long this_time;         // Elapsed time for test loop.
    long last_time;         // Holds time from last run of loop.
    long sum_of_ticks = 0;  // Statistics: Sums ten consecutive ticks.

    // Get current time and set two comparators equal.

    this_time = last_time = System.currentTimeMillis();

    for (int i = 0; i < 10; i++)    // Get ten clock ticks.
    {
        while (this_time == last_time)          // Wait for tick.
            this_time = System.currentTimeMillis();

        sum_of_ticks += (this_time - last_time);  // Sum tick duration.
        last_time = this_time;                    // Reset comparison.
    }

    // Set static variable to average clock tick.

    ClockTick = sum_of_ticks / 10L;
}


/**
* ClockTest() constructor is a dummy.
*/

ClockTest() {}

/**
* getClockTick() method returns the smallest accurate system clock
* interval in milliseconds as type int.
*/

static long getClockTick()
{
    return ClockTick;
}

}
