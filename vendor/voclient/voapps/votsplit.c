/*
 *  VOTSPLIT -- Split a Multi-RESOURCE VOTable into single tables.
 *
 *    Usage:
 *		votsplit [<otps>] <votable>
 *
 *  @file       votsplit.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/12
 *
 *  @brief      Split a Multi-RESOURCE VOTable into single tables.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "votParse.h"			/* keep these in order!		*/
#include "voApps.h"



/*  Global task declarations.  These should all be defined as 'static' to
 *  avoid namespace collisions.
 */
static int  vot         = 0;            /* VOTable handle               */
static int  do_return   = 0;		/* return result?		*/


/*  Result Buffer.
 */
#ifdef USE_RESBUF
#define	SZ_RESBUF	8192

static char *resbuf;
#endif


/*  Task specific option declarations.  Task options are declared using the
 *  getopt_long(3) syntax.
 */
int  votsplit (int argc, char **argv, size_t *len, void **result);

static Task  self       = {  "votsplit",  votsplit,  0,  0,  0  };
static char  *opts 	= "hr";
static struct option long_opts[] = {
        { "help",         2, 0,   'h'},		/* --help is std	*/
        { "return",       2, 0,   'r'},		/* --return is std	*/
        { "test",         2, 0,   '%'},		/* --return is std	*/
        { NULL,           0, 0,    0 }
};


/*  All tasks should declare a static Usage() method to print the help 
 *  text in response to a '-h' or '--help' flag.  The help text should 
 *  include a usage summary, a description of options, and some examples.
 */
static void Usage (void);
static void Tests (char *input);


/**
 *  Application entry point.
 */
int
votsplit (int argc, char **argv, size_t *reslen, void **result)
{
    char **pargv, optval[SZ_FNAME];
    char  *iname, *oname;
    int    ch = 0, status = OK, number = 0, pos = 0;


    /* Initialize result object	whether we return an object or not.
     */
    *reslen = 0;	
    *result = NULL;

    /*  Initialize local task values.
     */
    iname  = NULL;
    oname  = NULL;
    vot    = -1;


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
	    case '%':  Tests (optval);			return (self.nfail);
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
        "votsplit [<opts>] votable.xml\n\n"
        "  where\n"
        "       -%%,--test		run unit tests\n"
        "       -h,--help		this message\n"
        "       -n,--number		number output\n"
        "       -o,--output=<file>	output file\n"
        "       -r,--return		return result from method\n"
	"\n"
 	"  Examples:\n\n"
	"    1)  First example\n\n"
	"	    %% votsplit test.xml\n"
	"\n"
	"    2)  Second example\n\n"
	"	    %% votsplit -o pos.txt test.xml\n"
	"\n"
    );
}


/**
 *  Tests -- Task unit tests.
 */
static void
Tests (char *input)
{
   Task *task = &self;

   /*  First argument must always be the 'self' variable, the last must
    *  always be a NULL to terminate the cmd args.
    */
   vo_taskTest (task, "--help", NULL);
}
