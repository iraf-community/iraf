/*
 *  GENERIC -- Template file for VOApps task
 *
 *    Usage:
 *		generic [<otps>] <votable>
 *
 *  @file       generic.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/12
 *
 *  @brief      Template file for VOApps task.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "votParse.h"			/* keep these in order!		*/
#include "voApps.h"


/*  All tasks should support a "--return" flag if they can return a pointer
 *  to a result as part of the programmatic invocation.  As a cmdline task
 *  'reslen' and 'result' are ignored (actually, they're thrown away), but
 *  when called from an API the 'result' is a pointer to an arbitrary memory
 *  location that is passed back to the caller.  
 *
 *  The result object is defined by the task and can be anything.  The 
 *  '--return' flag can be defined to take an optional argument to specify
 *  which of multiple possible objects are returned (e.g. "--return=fits"
 *  "--return=votable") but the task is responsible for creating the object. 
 */
static int  do_return   = 0;		/* return result?		*/

/*  Global task declarations.  These should all be defined as 'static' to
 *  avoid namespace collisions.
 */
static int foo		= 0;


/*  A result buffer should be defined to point to the result object if it is
 *  created dynamically, e.g. a list of votable columns.  The task is
 *  responsible for initially allocating this pointer and then resizing as
 *  needed.
 */
#define	SZ_RESBUF	8192

static char *resbuf;


/*  Task specific option declarations.  Task options are declared using the
 *  getopt_long(3) syntax.
 */
static Task  self       = {  "generic",  generic,  0,  0,  0  };

static char  *opts 	= "%hno:r";
static struct option long_opts[] = {
        { "test",         2, 0,   '%'},		/* --test is std	*/
        { "help",         2, 0,   'h'},		/* --help is std	*/
        { "number",       2, 0,   'n'},		/* opt w/ no arg	*/
        { "output",       1, 0,   'o'},		/* opt w/ required arg	*/
        { "return",       2, 0,   'r'},		/* --return is std	*/
        { NULL,           0, 0,    0 }
};


/*  All tasks should declare a static Usage() method to print the help 
 *  text in response to a '-h' or '--help' flag.  The help text should 
 *  include a usage summary, a description of options, and some examples.
 */
static void Usage (void);
static void Tests (char *input);


/**
 *  Application entry point.  All VOApps tasks MUST contain this 
 *  method signature.
 */
int
generic (int argc, char **argv, size_t *reslen, void **result)
{
    /*  These declarations are required for the VOApps param interface.
     */
    char **pargv, optval[SZ_FNAME];

    /*  These declarations are specific to the task.
     */
    char  *iname, *oname;
    int    ch = 0, status = OK, number = 0, pos = 0;
    FILE  *fd = (FILE *) NULL;


    /* Initialize result object	whether we return an object or not.
     */
    *reslen = 0;	
    *result = NULL;

    /*  Initialize local task values.
     */
    iname  = NULL;
    oname  = NULL;


    /*  Parse the argument list.  The use of vo_paramInit() is required to
     *  rewrite the argv[] strings in a way vo_paramNext() can be used to
     *  parse them.  The programmatic interface allows "param=value" to
     *  be passed in, but the getopt_long() interface requires these to
     *  be written as "--param=value" so they are not confused with 
     *  positional parameters (i.e. any param w/out a leading '-').
     */
    pargv = vo_paramInit (argc, argv, opts, long_opts);
    while ((ch = vo_paramNext(opts,long_opts,argc,pargv,optval,&pos)) != 0) {
        if (ch > 0) {
	    /*  If the 'ch' value is > 0 we are parsing a single letter
	     *  flag as defined in the 'opts string.
	     */
	    switch (ch) {
	    case '%':  Tests ();			return (self.nfail);
	    case 'h':  Usage ();			return (OK);
	    case 'n':  number++;			break;
	    case 'o':  oname = strdup (optval);		break;
	    case 'r':  do_return=1;	    	    	break;
	    default:
		fprintf (stderr, "Invalid option '%s'\n", optval);
		return (1);
	    }

	} else if (ch == PARG_ERR) {
	    return (ERR);

	} else {
	    /*  This code processes the positional arguments.  The 'optval'
	     *  string contains the value but since this string is
	     *  overwritten w/ each arch we need to make a copy (and must
	     *  remember to free it later.
	     */
	    iname = strdup (optval);
	    break;
	}
    }


    /*  Sanity checks.  Tasks should validate input and accept stdin/stdout
     *  where it makes sense.
     */
    if (iname == NULL) iname = strdup ("stdin");
    if (oname == NULL) oname = strdup ("stdout");
    if (strcmp (iname, "-") == 0) { free (iname), iname = strdup ("stdin");  }
    if (strcmp (oname, "-") == 0) { free (oname), oname = strdup ("stdout"); }



   /********
    ********
    ********
    ********
    ********		Main body of task
    ********
    ********
    ********
    *******/




    /*  Clean up.  Rememebr to free whatever pointers were created when
     *  parsing arguments.
     */
    if (iname)
	free (iname);
    if (oname)
	free (oname);

    vo_paramFree (argc, pargv);

    return (status);	/* status must be OK or ERR (i.e. 0 or 1)     	*/
}


/**
 *  USAGE -- Print task help summary.
 */
static void
Usage (void)
{
    fprintf (stderr, "\n  Usage:\n\t"
        "generic [<opts>] votable.xml\n\n"
        "  where\n"
        "       -%%,--test		run unit tests\n"
        "       -h,--help		this message\n"
        "       -n,--number		number output\n"
        "       -o,--output=<file>	output file\n"
        "       -r,--return		return result from method\n"
	"\n"
 	"  Examples:\n\n"
	"    1)  First example\n\n"
	"	    %% generic test.xml\n"
	"	    %% generic -n test.xml\n"
	"	    %% cat test.xml | generic\n"
	"\n"
	"    2)  Second example\n\n"
	"	    %% generic -o pos.txt test.xml\n"
	"\n"
    );
}


/**
 *  Tests -- Task unit tests.
 */
static void
Tests (char *input)
{
   /*  First argument must always be the 'self' variable, the last must 
    *  always be a NULL to terminate the cmd args.
    */
   vo_taskTest (self, "--help", NULL);
}

