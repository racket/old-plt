
all: ../wxs/wrap.inc ../wxs/cwrap.inc

../wxs/wrap.inc: wrap.ss
	../../mzscheme/src/sstoinc < wrap.ss > ../wxs/wrap.inc

../wxs/cwrap.inc: ../wxs/wrap.inc
	mzscheme -gqrna ../../mzscheme/src/sstoinc.ss < wrap.ss > ../wxs/cwrap.inc

wrap.ss: import.ss export.ss propgate.ss mred.ss mkwrap.ss
	mzscheme -qmvqr mkwrap.ss
