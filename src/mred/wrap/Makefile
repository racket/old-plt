
MZSCHEME=../../mzscheme/${MZBUILDDIR}mzscheme

all: ../wxs/wrap.inc ../wxs/cwrap.inc

../wxs/wrap.inc: wrap.ss
	/usr/local/bin/perl ../../mzscheme/src/sstoinc < wrap.ss > ../wxs/wrap.inc

../wxs/cwrap.inc: ../wxs/wrap.inc
	$(MZSCHEME) -gqrna ../../mzscheme/src/sstoinc.ss < wrap.ss > ../wxs/cwrap.inc

wrap.ss: import.ss export.ss propgate.ss mred.ss mkwrap.ss
	$(MZSCHEME) -qmvqr mkwrap.ss
