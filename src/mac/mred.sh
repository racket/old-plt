#!/bin/sh

if [ "$PLTHOME" = '' ] ; then
  PLTHOME=/usr/local/lib/plt
  export PLTHOME
fi

exec "${PLTHOME}/MrEd.app/Contents/MacOS/MrEd" ${1+"$@"}
