
/**
* BMglobals
*
* This class holds globals that are used by all the member tests.
* When the system "comes up", it initializes these globals to
* their default values. In most cases, the globals can be overridden
* by parameters read from the parameter file. [In the command line
* application version, that is, not the Web applet version.]
*/

final class BMglobals
{
  // "Dummy" constructor; this is not a class to be instantiated.

  private BMglobals() {}

  // Define "constants".
  // ERROR CODES first.

  static final int ERROR_MEMORY = 1;
  static final int ERROR_MEMARRAY_FULL = 2;
  static final int ERROR_MEMARRAY_NFOUND = 3;

  // Parameter flags.

  static final int PF_GMTICKS = 0;     // GLOBALMINTICKS
  static final int PF_ALLSTATS = 1;    // ALLSTATS
  static final int PF_OUTFILE = 2;     // OUTFILE
  static final int PF_CUSTOMRUN = 3;   // CUSTOMRUN
  static final int PF_DONUM = 4;       // DONUMSORT
  static final int PF_NUMNUMA = 5;     // NUMNUMARRAYS
  static final int PF_NUMASIZE = 6;    // NUMARRAYSIZE
  static final int PF_DOSTR = 7;       // DOSTRINGSORT
  static final int PF_STRASIZE = 8;    // STRARRAYSIZE
  static final int PF_NUMSTRA = 9;     // NUMSTRARRAYS
  static final int PF_DOBITF = 10;     // DOBITFIELD
  static final int PF_NUMBITOPS = 11;  // NUMBITOPS
  static final int PF_BITFSIZE = 12;   // BITFIELDSIZE
  static final int PF_DOEMF = 13;      // DOEMF
  static final int PF_EMFASIZE = 14;   // EMFARRAYSIZE
  static final int PF_EMFLOOPS = 15;   // EMFLOOPS
  static final int PF_DOFOUR = 16;     // DOFOUR
  static final int PF_FOURASIZE = 17;  // FOURASIZE
  static final int PF_DOASSIGN = 18;   // DOASSIGN
  static final int PF_AARRAYS = 19;    // ASSIGNARRAYS
  static final int PF_DOIDEA = 20;     // DOIDEA
  static final int PF_IDEAASIZE = 21;  // IDEAARRAYSIZE
  static final int PF_IDEALOOPS = 22;  // IDEALOOPS
  static final int PF_DOHUFF = 23;     // DOHUFF
  static final int PF_HUFFASIZE = 24;  // HUFFARRAYSIZE
  static final int PF_HUFFLOOPS = 25;  // HUFFLOOPS
  static final int PF_DONNET = 26;     // DONNET
  static final int PF_NNETLOOPS = 27;  // NNETLOOPS
  static final int PF_DOLU = 28;       // DOLU
  static final int PF_LUNARRAYS = 29;  // LUNUMARRAYS
  static final int PF_LUASIZE = 30;    // LUARRAYSIZE

  static final int PF_MAXPARAM = 30;

  static final String paramnames[] = {
    "GLOBALMINTICKS",
    "ALLSTATS",
    "OUTFILE",
    "CUSTOMRUN",
    "DONUMSORT",
    "NUMNUMARRAYS",
    "NUMARRAYSIZE",
    "DOSTRINGSORT",
    "STRARRAYSIZE",
    "NUMSTRARRAYS",
    "DOBITFIELD",
    "NUMBITOPS",
    "BITFIELDSIZE",
    "DOEMF",
    "EMFARRAYSIZE",
    "EMFLOOPS",
    "DOFOUR",
    "FOURASIZE",
    "DOASSIGN",
    "ASSIGNARRAYS",
    "DOIDEA",
    "IDEARRAYSIZE",
    "IDEALOOPS",
    "DOHUFF",
    "HUFFARRAYSIZE",
    "HUFFLOOPS",
    "DONNET",
    "NNETLOOPS",
    "DOLU",
    "LUNUMARRAYS",
    "LUARRAYSIZE" };


  // Test-to-do flags...must coincide with tests.

  static final int TF_NUMSORT = 0;
  static final int TF_SSORT = 1;
  static final int TF_BITOP = 2;
  static final int TF_FPEMU = 3;
  static final int TF_FFPU = 4;
  static final int TF_ASSIGN = 5;
  static final int TF_IDEA = 6;
  static final int TF_HUFF = 7;
  static final int TF_NNET = 8;
  static final int TF_LU = 9;

  static final int NUMTESTS = 10;

  // Following array amounts to a collection of flags that
  // indicate which tests to perform. Initially we've
  // set them all to true.

  static boolean tests_to_do[] = {
    true, true, true, true, true, true, true,
    true, true, true };

  // Test names.

  static final String testnames[] = {
        "Numeric Sort",
        "String Sort",
        "Bitfield Operations",
        "FP Emulation",
        "Fourier",
        "Assignment",
        "IDEA Encryption",
        "Huffman Compression",
        "Neural Net",
        "LU Decomposition" };

  // Baselines (Dell Dimension XPS90 running C-based BYTEmark 2.0).

/*
  static final double numsorttestbase = 0.8955;
  static final double strsorttestbase = 2.5559;
  static final double bftestbase = 6499100;
  static final double fpetestbase = 2.1621;
  static final double fourtestbase = 344.66;
  static final double asgntestbase = 0.5563;
  static final double ideatestbase = 4.8736;
  static final double hufftestbase = 1.9672;
  static final double nnettestbase = 0.1197;
  static final double lutestbase = 0.6279;
*/
/* index werte mit CC -O */

  static final double numsorttestbase = 99.280000;
  static final double strsorttestbase = 9.065904;
  static final double bftestbase = 27733634.637117;
  static final double fpetestbase = 7.397046;  
  static final double fourtestbase = 2440.275089;
  static final double asgntestbase = 1.594898;
  static final double ideatestbase = 253.907025;
  static final double hufftestbase = 106.008257;
  static final double nnettestbase = 3.831429;
  static final double lutestbase = 158.320000;

/* index werte mit CC */

/*
  static final double numsorttestbase = 47.263313;
  static final double strsorttestbase = 6.648493;
  static final double bftestbase = 15292253.515724;
  static final double fpetestbase = 3.762406;  
  static final double fourtestbase = 2411.404746;
  static final double asgntestbase = 0.651042;
  static final double ideatestbase = 229.885395;
  static final double hufftestbase = 69.446952;
  static final double nnettestbase = 0.761037;
  static final double lutestbase = 23.374521;
*/

  // Define global variables. These are set to their "default"
  // values here. Possibly overridden later.

  // For numeric sort test.

  static boolean numadjust = false;
  static int numnumarrays = 100;             // # of arrays
  static int numarraysize = 8111;          // Size of numeric sort array

  // For string sort test.

  static boolean stradjust = false;
  static int numstringarrays = 1;         // # of arrays
  static int stringarraysize = 8111;      // Size of string array

  // For bitfield operations test.

  static boolean bitadjust = false;  // Hasn't been adjusted to clock.
  static int bitoparraysize = 30;    // Size of bit ops arrays.
  static int bitfarraysize = 32768;  // Size of bitfield array.

  // For emulated floating point test.

  static boolean emfloatadjust = false;
  static int emfarraysize = 3000;       // Size of array.
  static int emfloops = 2;             // Loops per iteration.
  static int cpuemfloatloopmax = 50000; // Maximum number of loops
                                       // that the system will
                                       // allow before flagging
                                       // an error.

  // For Fourier test.

  static boolean fouradjust = false;  // Hasn't been adjusted to clock.
  static int fourarraysize = 100;     // Size of array.

  // For Assignment test.

  static boolean assignadjust = false;
  static int assignarrays = 1;        // # of arrays.
  static int assignrows = 101;         // Rows of array.
  static int assigncols = 101;         // Cols of array (square matrix).

  // For IDEA encryption test.

  static boolean ideaadjust = false;
  static int ideaarraysize = 4000;     // Array size (multiple of 8).
  static int idealoops = 1;            // Starting loops per iteration.
  static int maxidealoops = 50000;     // Maximum number of loops that
                                       // the system will allow before
                                       // flagging an error.

  // For Huffman compression test.

  static boolean huffadjust = false;   // Hasn't been adjusted to clock.
  static int huffarraysize = 5000;     // Huffman array size.
  static int huffloops = 2;            // Starting number of test loops.
  static int maxhuffloops = 50000;     // Maximum # of Huffman comp.
                                       // loops the system will try
                                       // before flagging an error.

  // For Neural Net test.

  static boolean nnetadjust = false;  // Hasn't been adjusted to clock.
  static int nnetloops = 1;           // Number of loops at start of
  static int maxnnetloops = 50000;    // Maximum # of loops of the
                                      // neural net test the system
                                      // will allow before flagging
                                      // an error.
                                      // adjustment.

  // For LU Decomposition test.

  static boolean LUadjust = false;
  static int luarrayrows = 101;    // # of rows/cols
  static int lunumarrays = 1;      // # of arrays
  static int maxluarrays = 1000;   // Max # of LU arrays the system
                                        //  will try to build before
                                        //  flagging an error.

  // Global parameters.

  static boolean allstats;        // Statistics dump flag.
  static String ofile_name;       // Output file name.
  static java.io.FileOutputStream ofile;  // Output file reference.
  static java.io.DataOutputStream odata;  // Output stream reference.
  static boolean custrun;        // Custom run flag.
  static boolean write_to_file;  // Write output to file.
  static long minTicks;          // Minimum ticks to run test. Should
                                 // be 100 times the resolution of the
                                 // Java clock.

}