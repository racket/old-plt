// Used to generate a precompiled header; see ../Makefile.in

// prevent Carbon.h's "char_p" from colliding with char.c's char_p:
#define char_p ADFKJSDLKJSDLKJSDLKFJSDLKj
#include <Carbon/Carbon.h>
#undef char_p
