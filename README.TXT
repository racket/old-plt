
This is the PLT software package directory. 

The main executables are:

  plt\drscheme.exe - PLT's Scheme development environment
  plt\mred.exe - a Scheme GUI engine
  plt\mzscheme.exe - a text-based Scheme interpreter
  plt\mzc.exe -- the MzScheme compiler

Depending on which distribution you downloaded, you may have fewer
executables.

Compiling
---------

If you downloaded the source code distribution instead of the standard
distribution, you must compile MzScheme and MrEd before using the PLT
software (including DrScheme). For compilation instructions, see
plt/src/worksp/README. 

If there is no plt/src/worksp directory, you did not download the
source code distribution and you are ready to run.

Documentation
-------------

Some documentation may be included in plt\collects\doc, but some of
the PLT documentation must be downloaded separately from the
executables.

HTML documentation for PLT software should be installed into
plt\collects\doc. For example, the DrScheme documenation should be in
plt\collects\doc/drscheme, with button icons in
plt\collects\doc\icons.

DrScheme and MrEd will automatically add documentation installed in
plt\collects\doc to the "Help" menu.

More Information
----------------

For more information, see
  http://www.cs.rice.edu/CS/PLT/packages/
