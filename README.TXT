
This is the PLT software package directory for Windows.


Executables
-----------

The following are the executables:

  DrScheme.exe -- the Scheme development environment

  DrScheme Jr.exe -- a text-only version of DrScheme
  MzScheme.exe -- a text-only Scheme interpreter
  MrEd.exe -- a graphical Scheme interpreter
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


License
-------

PLT Software
Copyright (c) 1995-99 PLT

PLT software is distributed under the GNU Library General Public
License (LGPL).  This means you can link PLT software (such as
MzScheme or MrEd) into proprietary applications, provided you follow
the specific rules stated in the LGPL.  You can also modify PLT
software; if you distribute a modified version, you must distribute it
under the terms of the LGPL, which in particular means that you must
release the source code for the modified software. See
notes/COPYING.LIB for more information.


More Information
----------------

For further information, select `Help Desk' from DrScheme's `Help'
menu, or run Help Desk.exe.
