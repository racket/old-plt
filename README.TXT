
This is the PLT software package directory. 

Executables
-----------

The following are the executables:

  plt\Help Desk.exe --- for Help
  plt\DrScheme.exe -- PLT's main Scheme development environment
  plt\DrScheme Jr.exe -- A simple text-based DrScheme
  plt\MrEd.exe -- a Scheme GUI engine
  plt\MzScheme.exe -- a text-based Scheme interpreter
  plt\mzc.exe -- the MzScheme compiler

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

Run plt\Help Desk.exe for further information.
