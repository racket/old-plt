#!/bin/sh

# This script should not be moved from the plt/bin directory
if [ "$PLTHOME" = '' ] ; then
  PLTHOME="`dirname \"$0\"`"
  PLTHOME="`cd \"$PLTHOME\"; cd ..; pwd`"
  export PLTHOME
fi

exec "${PLTHOME}/MrEd.app/Contents/MacOS/MrEd" ${1+"$@"}
