/**
*jBYTEMmark
*
* The class that serves as a shell for running jBYTEmark
* in a console window. This is a reasonably direct port of
* the original BYTEmark code in C.
* BYTEmark port to Java by Rick Grehan and Dave Rowell, BYTE
* Magazine.
*/

/**
* DISCLAIMER
* The source, executable, and documentation files that comprise the
* jBYTEmark benchmarks are made available on an "as is" basis. This means
* that we at BYTE Magazine have made every reasonable effort to verify
* that there are nor errors in the source and executable code. We cannot,
* however, guarantee tha the programs are error-free. Consequently, McGraw-Hill
* and BYTE Magazine make no claims in regard to the fitness of the source
* code, executable code, and documentation of the BYTEmark. Furthermore,
* BYTE Magazine, McGraw-Hill, and all employees of McGraw-Hill cannot be
* held responsible for any damages resulting from the use of this code
* or the results obtained from using this code.
*/


class jBYTEmark6
{
    static boolean showIndexes;    // True if we can show indexes.
    boolean testConfidence; // Flag for successful test.

    public static void main (String args[])
    {
    int i,j;                // Indexes for loops.
    BmarkTest test;         // The test object.
    double rsltIndex;       // Resulting index of test.
    double intIndex;        // Integer index.
    double FPIndex;         // Floating-point index.
    int testtype;           // 0=Integer; 1=FP.
    java.io.FileInputStream cfile;  // Command input file.
    String argstring;

    // Set initial globals.

    BMglobals.allstats=false;
    BMglobals.ofile_name = new String("");
    BMglobals.custrun = false;
    BMglobals.write_to_file = false;

    // Following code retrieves the minimum "safe" clock ticks.

    BMglobals.minTicks=ClockTest.getClockTick()*100;

    showIndexes=true;   // For now, we can show indexes.

    test = new LuTest();    // Initialize it to something to make Java happy.

    // Parse command line arguments.
    /*
    if(args.length > 0)
        for(i=0;i<args.length;i++)
            if(parse_arg(args[i])==-1)
            {   System.out.println("Usage: jBYTEmark [-?][-C<path>]");
                System.out.println("  -? Presents this help screen.");
                System.out.println("  -C<path> Specifies command file at <path>.");
                System.out.println(" PRESS CTRL-C to close");
                while(true) { System.out.print(""); }
            }
    */
    // Initialize the index values.

    intIndex=1.0;
    FPIndex=1.0;
    rsltIndex=0;
    testtype=0;     // Make JAVA happy.

    // Announce yourself
    System.out.println("jBYTEmark ver. 0.9");

    System.out.print("Running: ");
    i = 6;
    System.out.println(BMglobals.testnames[i]);
    test = new IDEATest();
    testtype=0;     // Integer
    
    try
	{
	    test.benchWithConfidence(5);     // Start test.
	}
    catch (OutOfMemoryError e)  // Handle test errors/exceptions.
	{
	    String msg = e.getMessage();
	    System.out.println(msg);
	}
    
    if (test.testConfidence)
	{
	    // Calculate results and accumulate indexes.
	    
	    rsltIndex = test.mean/test.base_iters_per_sec;
	    if(testtype == 0)
		intIndex *= rsltIndex;
	    else
		FPIndex *= rsltIndex;
	    
	    // Show scores.
	    
	    System.out.print("Avg: ");
	    System.out.print(test.mean);
	    System.out.print("  Index: ");
	    System.out.println(rsltIndex);
	}
    else
	{
	    System.out.println("Scores did not converge.");
	    showIndexes=false;
	}

    // Show indexes if allowed.

    if(showIndexes)
    {   
       
        try
        {
            intIndex=Math.pow(intIndex,.1428571);    // 7th root
            FPIndex=Math.pow(FPIndex,.3333333);
        }

        catch (ArithmeticException e)
        {   
            System.out.println("**Error in pow() **");
        }
       
        System.out.println("********************");
        System.out.print("Integer Index: ");
        System.out.println(intIndex);
        System.out.print("FP Index: ");
        System.out.println(FPIndex);
        System.out.println("********************");
    }


}

/**************
** parse_arg **
***************
*/
// Given a string, we assume that's an argument.
// Parse the argument and act accordingly.
// Return 0 if ok, else -1

static int parse_arg(String argstr)
{
int offst;      // Offset into string.
char cmdchar;   // Command character.

// First character has got to be a hypen.

try
{
    if (argstr.charAt(0) != '-') return (-1);
}
catch (StringIndexOutOfBoundsException e)
{
    return(-1);
}

// Hack off the hyphen & convert to upper case.

argstr = argstr.substring(1);
argstr = argstr.toUpperCase();
java.io.FileInputStream cfile;

// Next character picks the action.

try
{
  cmdchar = argstr.charAt(0);
}
catch (StringIndexOutOfBoundsException e)
{
    return (-1);
}

switch (cmdchar)
{
 case '?':       return(-1);     // Will display help.

 case 'A' : return (1);
 case 'B' : return (2);
 case 'C' : return (3);
 case 'D' : return (4);
 case 'E' : return (5);
 case 'F' : return (6);
 case 'G' : return (7);
 case 'H' : return (8);
 case 'I' : return (9);
 case 'J' : return (10);

    case 'Z':                       // Command file name.
        argstr=argstr.substring(1); // Lop off the 'C'
        try {
            cfile=new java.io.FileInputStream(argstr);
        } catch (java.io.FileNotFoundException e)
        {   System.out.print("**Error opening file ");
            System.out.println(argstr);
            return(-1);
        }
        read_comfile(cfile);        // Read commands

        try {
            cfile.close();
        } catch(java.io.IOException e)
        {   return(-1);
        }
        break;

    default:
        return(-1);
}
return(0);
}

/*****************
** read_comfile **
******************
** Read the command file.  Set global parameters as
** specified.  This routine assumes that the command file
** is already open.
*/

static void read_comfile(java.io.FileInputStream cfile)
{
int i;                  // Index.
String inbuf;           // Input buffer.
String paramstr;        // Parameter string.
String pvalstr;         // Parameter value string.
int equloc;             // Location of "=" sign.
java.io.DataInputStream instream = new java.io.DataInputStream(cfile);

/*
** Sit in a big loop, reading a line from the file at each
** pass.  Terminate on EOF.
*/

try {
    while((inbuf = instream.readLine())!=null)
    {
       // Locate the "=" sign. If not found, bail out with error.
       equloc = inbuf.indexOf("=");
       if(equloc==-1)
       {    System.out.println("**ERROR PARSING COMMAND FILE LINE:");
            System.out.println(inbuf);
            System.out.println("**REMAINDER OF COMMAND FILE IGNORED**");
            return;
       }

        // Pull out the parameter substring
        try {
            paramstr=inbuf.substring(0,equloc);
        }
        catch ( StringIndexOutOfBoundsException e)
        {   System.out.println("**ERROR PARSING COMMAND FILE LINE:");
            System.out.println(inbuf);
            System.out.println("**REMAINDER OF COMMAND FILE IGNORED**");
            return;
        }
        // Convert the parameter to uppercase
        paramstr=paramstr.toUpperCase();

        // Pull out the parameter's value & convert it to upper case
        pvalstr=inbuf.substring(equloc+1);
        pvalstr=pvalstr.toUpperCase();

        i=BMglobals.PF_MAXPARAM-1;
        do {
            if(paramstr.equals(BMglobals.paramnames[i]))
                        break;
        } while(--i>=0);


        /*
        ** Advance eptr to the next field...which should be
        ** the value assigned to the parameter.
        */

        switch(i)
        {
                case BMglobals.PF_GMTICKS:        // GLOBALMINTICKS
                        try {
                            BMglobals.minTicks=java.lang.Long.parseLong(pvalstr);
                        }
                        catch(NumberFormatException e) { break; }
                        break;

                case BMglobals.PF_ALLSTATS:       // ALLSTATS
                        if(pvalstr.equals("T"));
                            BMglobals.allstats=true;
                        break;

                case BMglobals.PF_OUTFILE:        // OUTFILE
                        BMglobals.ofile_name = new String(pvalstr);
                        try {
                            BMglobals.ofile = new java.io.FileOutputStream(BMglobals.ofile_name);
                        }
                        catch(java.io.IOException e)
                        {   System.out.print("**Error opening output file: ");
                            System.out.println(BMglobals.ofile_name);
                            return;
                        }
                        BMglobals.odata = new java.io.DataOutputStream(BMglobals.ofile);
                        BMglobals.write_to_file=true;
                        break;

                case BMglobals.PF_CUSTOMRUN:      // CUSTOMRUN
                        if(pvalstr.equals("T"))
                        {   BMglobals.custrun=true;
                            for(i=0;i<BMglobals.NUMTESTS;i++)
                                BMglobals.tests_to_do[i]=false;
                        }
                        showIndexes=false;
                        break;

                case BMglobals.PF_DONUM:          // DONUMSORT
                        BMglobals.tests_to_do[BMglobals.TF_NUMSORT]=pvalstr.equals("T");
                        break;

                case BMglobals.PF_NUMNUMA:        // NUMNUMARRAYS
                        try {
                            BMglobals.numnumarrays=java.lang.Integer.parseInt(pvalstr);
                        }
                        catch(NumberFormatException e) { break; }
                        BMglobals.numadjust=true;
                        break;

                case BMglobals.PF_NUMASIZE:       // NUMARRAYSIZE
                        try {
                            BMglobals.numarraysize=java.lang.Integer.parseInt(pvalstr);
                        }
                        catch(NumberFormatException e) { break; }
                        break;

                case BMglobals.PF_DOSTR:          // DOSTRINGSORT
                            BMglobals.tests_to_do[BMglobals.TF_SSORT]=pvalstr.equals("T");
                        break;

                case BMglobals.PF_STRASIZE:       // STRARRAYSIZE
                         try {
                            BMglobals.stringarraysize=java.lang.Integer.parseInt(pvalstr);
                        }
                        catch(NumberFormatException e) { break; }
                        break;

                case BMglobals.PF_NUMSTRA:        // NUMSTRARRAYS
                         try {
                            BMglobals.numstringarrays=java.lang.Integer.parseInt(pvalstr);
                        }
                        catch(NumberFormatException e) { break; }
                        BMglobals.stradjust=true;
                        break;

                case BMglobals.PF_DOBITF:         // DOBITFIELD
                        BMglobals.tests_to_do[BMglobals.TF_BITOP]=pvalstr.equals("T");
                        break;

                case BMglobals.PF_NUMBITOPS:      /* NUMBITOPS */
                         try {
                            BMglobals.bitoparraysize=java.lang.Integer.parseInt(pvalstr);
                        }
                        catch(NumberFormatException e) { break; }
                        BMglobals.bitadjust=true;
                        break;

                case BMglobals.PF_BITFSIZE:       /* BITFIELDSIZE */
                         try {
                            BMglobals.bitfarraysize=java.lang.Integer.parseInt(pvalstr);
                        }
                        catch(NumberFormatException e) { break; }
                        break;

                case BMglobals.PF_DOEMF:          /* DOEMF */
                        BMglobals.tests_to_do[BMglobals.TF_FPEMU]=pvalstr.equals("T");
                        break;

                case BMglobals.PF_EMFASIZE:       /* EMFARRAYSIZE */
                        try {
                            BMglobals.emfarraysize=java.lang.Integer.parseInt(pvalstr);
                        }
                        catch(NumberFormatException e) { break; }
                        break;

                case BMglobals.PF_EMFLOOPS:       /* EMFLOOPS */
                         try {
                            BMglobals.emfloops=java.lang.Integer.parseInt(pvalstr);
                        }
                        catch(NumberFormatException e) { break; }
                        BMglobals.emfloatadjust=true;
                        break;

                case BMglobals.PF_DOFOUR: /* DOFOUR */
                        BMglobals.tests_to_do[BMglobals.TF_FFPU]=pvalstr.equals("T");
                        break;

                case BMglobals.PF_FOURASIZE:      /* FOURASIZE */
                        try {
                            BMglobals.fourarraysize=java.lang.Integer.parseInt(pvalstr);
                        }
                        catch(NumberFormatException e) { break; }
                        BMglobals.fouradjust=true;
                        break;

                case BMglobals.PF_DOASSIGN:       /* DOASSIGN */
                        BMglobals.tests_to_do[BMglobals.TF_ASSIGN]=pvalstr.equals("T");
                        break;

                case BMglobals.PF_AARRAYS:        /* ASSIGNARRAYS */
                        try {
                            BMglobals.assignarrays=java.lang.Integer.parseInt(pvalstr);
                        }
                        catch(NumberFormatException e) { break; }
                        BMglobals.assignadjust=true;
                        break;

                case BMglobals.PF_DOIDEA: /* DOIDEA */
                        BMglobals.tests_to_do[BMglobals.TF_IDEA]=pvalstr.equals("T");
                        break;

                case BMglobals.PF_IDEAASIZE:      /* IDEAARRAYSIZE */
                        try {
                            BMglobals.ideaarraysize=java.lang.Integer.parseInt(pvalstr);
                        }
                        catch(NumberFormatException e) { break; }
                        break;

                case BMglobals.PF_IDEALOOPS:      /* IDEALOOPS */
                        try {
                            BMglobals.idealoops=java.lang.Integer.parseInt(pvalstr);
                        }
                        catch(NumberFormatException e) { break; }
                        BMglobals.ideaadjust=true;
                        break;

                case BMglobals.PF_DOHUFF: /* DOHUFF */
                        BMglobals.tests_to_do[BMglobals.TF_HUFF]=pvalstr.equals("T");
                        break;

                case BMglobals.PF_HUFFASIZE:      /* HUFFARRAYSIZE */
                        try {
                            BMglobals.huffarraysize=java.lang.Integer.parseInt(pvalstr);
                        }
                        catch(NumberFormatException e) { break; }
                       break;

                case BMglobals.PF_HUFFLOOPS:      /* HUFFLOOPS */
                       try {
                            BMglobals.huffloops=java.lang.Integer.parseInt(pvalstr);
                       }
                       catch(NumberFormatException e) { break; }
                        BMglobals.huffadjust=true;
                        break;

                case BMglobals.PF_DONNET: /* DONNET */
                        BMglobals.tests_to_do[BMglobals.TF_NNET]=pvalstr.equals("T");
                        break;

                case BMglobals.PF_NNETLOOPS:      /* NNETLOOPS */
                       try {
                            BMglobals.nnetloops=java.lang.Integer.parseInt(pvalstr);
                       }
                       catch(NumberFormatException e) { break; }
                        BMglobals.nnetadjust=true;
                        break;

                case BMglobals.PF_DOLU:           /* DOLU */
                        BMglobals.tests_to_do[BMglobals.TF_LU]=pvalstr.equals("T");
                        break;

                case BMglobals.PF_LUNARRAYS:      /* LUNUMARRAYS */
                       try {
                            BMglobals.lunumarrays=java.lang.Integer.parseInt(pvalstr);
                       }
                       catch(NumberFormatException e) { break; }
                        BMglobals.LUadjust=true;
                        break;

                default:            // Fall through here if nothing found
                        break;      // We'll just ignore it for now.
        }
}       // End while.
}       // End try.

catch (java.io.IOException e)
{   System.out.println("**ERROR READING COMMAND FILE**");
}

return;
}

}

