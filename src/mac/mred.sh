#!/bin/sh

if [ "$PLTHOME" = '' ] ; then
  PLTHOME="/home/mflatt/proj/plt"
  export PLTHOME
fi

exec "${PLTHOME}/MrEd.app/Contents/MacOS/MrEd" ${1+"$@"}
