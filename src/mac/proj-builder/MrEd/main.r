/* main.r */

#include <Carbon/Carbon.r>
#include "main.h"

/* Unfortunately, we cannot yet process symbolic resource descriptions...*/
#define RAW 1

#if RAW
data 'MBAR' (rMenuBar, preload) {
	$"0003 0080 0081 0082"                           /* ...Ä.Å.Ç.É */
};
#else
resource 'MBAR' (rMenuBar, preload) {
	{mApple, mFile, mEdit};
};
#endif

#if RAW
data 'MENU' (mApple, preload) {
	$"0080 0000 0000 0000 0000 FFFF FFFB 0114"            /* .Ä........ˇˇˇ˚.. */
	$"0C41 626F 7574 2048 656C 6C6F C900 0000"            /* .About Hello…... */
	$"0001 2D00 0000 0000"                                /* ..-..... */
};
#else
resource 'MENU' (mApple, "Apple", preload) {
	mApple, textMenuProc,
	AllItems & ~MenuItem2,
	enabled, apple,
	{       
	"About Hello…",                      noicon, nokey, nomark, plain;
	"-",                            noicon, nokey, nomark, plain;   
	}
};
#endif

#if RAW
data 'MENU' (mFile, preload) {
	$"0081 0000 0000 0000 0000 0000 2801 0446"            /* .Å..........(..F */
	$"696C 6503 4E65 7700 4E00 0004 4F70 656E"            /* ile.New.N...Open */
	$"004F 0000 012D 0000 0000 0543 6C6F 7365"            /* .O...-.....Close */
	$"0057 0000 0453 6176 6500 5300 0008 5361"            /* .W...Save.S...Sa */
	$"7665 2041 73C9 0000 0000 012D 0000 0000"            /* ve As….....-.... */
	$"0B50 6167 6520 5365 7475 70C9 0000 0000"            /* .Page Setup….... */
	$"0650 7269 6E74 C900 0000 0001 2D00 0000"            /* .Print….....-... */
	$"0004 5175 6974 0051 0000 00"                        /* ..Quit.Q... */
};
#else
resource 'MENU' (mFile, "File", preload) {
	mFile, textMenuProc,
	AllItems & ~MenuItem3 & ~MenuItem7 & ~MenuItem10,
	enabled, "File",
	{
		"New",		noicon,	"N",	nomark,	plain;
		"Open",		noicon,	"O",	nomark,	plain;
		"-",		noicon,	nokey,	nomark,	plain;
		"Close",	noicon,	"W",	nomark,	plain;
		"Save",		noicon,	"S",	nomark,	plain;
		"Save As…",	noicon,	nokey,	nomark,	plain;
		"-",		noicon,	nokey,	nomark,	plain;
		"Page Setup…",	noicon,	nokey,	nomark,	plain;
		"Print…",	noicon,	"P",	nomark,	plain;
		"-",		noicon,	nokey,	nomark,	plain;
		"Quit",		noicon,	"Q",	nomark,	plain;
	}
};
#endif

#if RAW
data 'MENU' (mEdit, preload) {
	$"0082 0000 0000 0000 0000 0000 0001 0445"            /* .Ç.............E */
	$"6469 7404 556E 646F 005A 0000 012D 0000"            /* dit.Undo.Z...-.. */
	$"0000 0343 7574 0058 0000 0443 6F70 7900"            /* ...Cut.X...Copy. */
	$"4300 0005 5061 7374 6500 5600 0005 436C"            /* C...Paste.V...Cl */
	$"6561 7200 0000 0000"                                /* ear..... */
};
#else
resource 'MENU' (mEdit, "Edit", preload) {
	mEdit, textMenuProc,
	AllItems & ~MenuItem2 & ~MenuItem7 & ~MenuItem9 & ~MenuItem14 & ~MenuItem18,
	enabled, "Edit",
	{
		"Undo",		noicon,	"Z",	nomark,	plain;
		"-",		noicon,	nokey,	nomark,	plain;
		"Cut",		noicon,	"X",	nomark,	plain;
		"Copy",		noicon,	"C",	nomark,	plain;
		"Paste",	noicon,	"P",	nomark,	plain;
		"Clear",	noicon,	nokey,	nomark,	plain;
	}
};
#endif

#if RAW
data 'DITL' (kAboutBox) {
	$"0002 0000 0000 0010 0015 0026 00D0 8814"            /* ...........&.–à. */
	$"4865 6C6C 6F20 6865 6C6C 6F20 6865 6C6C"            /* Hello hello hell */
	$"6F2E 2E2E 0000 0000 0074 011F 0088 0159"            /* o........t...à.Y */
	$"0402 4F4B 0000 0000 0036 008B 004A 00C5"            /* ..OK.....6.ã.J.≈ */
	$"0402 4F4B"                                          /* ..OK */
};
#else
resource 'DITL' (kAboutBox) {
	{
		{16, 21, 38, 208}, StaticText {disabled, "Hello hello hello"},
		{116, 287, 136, 345}, Button {enabled, "OK"},
	}
};
#endif

data 'ALRT' (kAboutBox) {
	$"0028 0028 008B 0118 00C8 5555"                      /* .(.(.ã...»UU */
};