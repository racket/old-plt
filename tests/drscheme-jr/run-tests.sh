#!/bin/bash

# test script for language levels
# Author:  Paul Steckler
# Revised: March 16 1998

LEVELS="Beginner Intermediate Advanced MzSchemeDebug"

for level in $LEVELS
do 
    echo -n "Running tests at $level level ... "
    echo "$level level tests of DrScheme Jr" > $level.out
    echo Date: `date` >> $level.out
    ~scheme/plt/bin/drscheme-jr -l $level < the-tests.ss >> $level.out 2>&1
    echo "done."
done

for level in $LEVELS
do
    echo Testing for differences at $level level ...
    diff $level.out $level.std | awk '(NR > 5) { print }'
    echo "Done."
done

