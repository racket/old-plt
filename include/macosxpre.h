// Used to generate a precompiled header; see ../Makefile.in

#ifndef CARBON_PRECOMP
#define CARBON_PRECOMP

// prevent Carbon.h's "char_p" from colliding with char.c's char_p
// (by omitting Open Transport altogether)
#define __OT__

#include <Carbon/Carbon.h>

#endif // CARBON_PRECOMP
