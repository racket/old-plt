
This is the PLT software package directory for Windows.

Executables
-----------

The following are the executables:

  DrScheme.exe -- the Scheme development environment

  DrScheme Jr.exe -- a text-only version of DrScheme
  MzScheme.exe -- a text-only Scheme interpreter
  MrEd.exe -- a graphica Scheme interpreter
  mzc.exe -- a Scheme compiler
  Help Desk.exe --- for Help (also built into DrScheme)

DrScheme relies on MrEd (i.e., the DrScheme executable launches
MrEd with special startup instructions), and DrScheme Jr and mzc rely
on MzScheme. You may not have some of these executables, depending
on which distribution you downloaded.

Compiling
---------

If you downloaded the source code distribution instead of the standard
distribution, you must compile MzScheme and MrEd before using the PLT
software (including DrScheme). 

For compilation instructions, see plt/src/worksp/README. 

If there is no plt\src\mzscheme directory, you did not download
the source code distribution. In that case, you should be ready to
run.

More Information
----------------

For further information, select `Help Desk' from DrScheme's `Help'
menu, or run Help Desk.exe.
