#!/bin/sh

if [ "$PLTHOME" = '' ] ; then
  PLTHOME=/usr/local/lib/plt
  export PLTHOME
fi

exec "${PLTHOME}/MrEd3m.app/Contents/MacOS/MrEd3m" ${1+"$@"}
