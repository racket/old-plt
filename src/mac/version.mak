
FWVERSION = $(shell grep ' MZSCHEME_VERSION ' $(srcdir)/../mzscheme/src/schvers.h | cut -d '"' -f 2)
