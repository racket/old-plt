
/matthew/proj/plt/mzscheme.exe -qmvqr mkwrap.ss
/usr/local/bin/perl ../../mzscheme/src/sstoinc < wrap.ss > ../wxs/wrap.inc
/matthew/proj/plt/mzscheme.exe -gqrna ../../mzscheme/src/sstoinc.ss < wrap.ss > ../wxs/cwrap.inc
