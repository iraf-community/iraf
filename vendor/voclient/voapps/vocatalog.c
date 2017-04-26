/**
 *  VOCATALOG -- Query all VO Catalog services
 *
 *    Usage:
 *		vocatalog [<opts>] [ <object> | <ra> <dec> ] [ <size> ]
 *
 *  @file       vocatalog.c
 *  @author     Mike Fitzpatrick
 *  @date       2/03/13
 *
 *  @brief      Query all VO Image services.
 */ 

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "votParse.h"			/* keep these in order!		*/
#include "voApps.h"


/*  Task specific option declarations.  Task options are declared using the
 *  getopt_long(3) syntax.
 */
int  vodata (int argc, char **argv, size_t *len, void **result);
int  vocatalog (int argc, char **argv, size_t *len, void **result);

static Task  self       = {  "vocatalog",  vocatalog,  0,  0,  0  };

extern void  vot_setArg (char **argv, int *argc, char *value);

static void Usage (void);
static void Tests (char *input);


/**
 *  Application entry point.  All VOApps tasks MUST contain this 
 *  method signature.
 */
int
vocatalog (int argc, char **argv, size_t *reslen, void **result)
{
    char  *pargv[argc+2];
    int    i, narg = 0, status = OK;


    /*  Initialize result object whether we return an object or not.
     */
    *reslen = 0;	
    *result = NULL;

    /*  Do a quick check of the args so we can provide a task-local
     *  help and test option.  Otherwise, we simply pass thru all the
     *  args to VODATA for processing.
     */
    if (strncmp (argv[1],"-h",2) == 0 || strncmp (argv[1],"--help",6) == 0) {
	Usage (); return (OK);
    }
    if (strncmp (argv[1],"-%",2) == 0 || strncmp (argv[1],"--test",6) == 0) {
	Tests (NULL); return (self.nfail);
    }

    /*  Initialize the new argument vector.
     */
    vot_setArg (pargv, &narg, argv[0]);
    vot_setArg (pargv, &narg, "--type=catalog");
    for (i=1; i < argc; i++)
        vot_setArg (pargv, &narg, argv[i]);


   /**
    *  The VODATA task does all the real work, we effectively just set the
    *  "-t catalog" option to force the service type as a logical naming 
    *  convenience for the user.  Note that return parameters are handled
    *  by vodata as well so there is no processing required here.
    */
    status = vodata (narg, pargv, reslen, result);


    /*  Clean up.  Rememebr to free whatever pointers were created when
     *  parsing arguments.
     */
    for (i=0; i < (argc + 2); i++)
	free ((void *) pargv[i]);

    return (status);
}


/**
 *  USAGE -- Print task help summary.
 */
static void
Usage (void)
{
    fprintf (stderr, "\n  Usage:\n\t"
        "vocatalog [<opts>] votable.xml\n\n"
        "  where\n"
        "       -%%,--test		run unit tests\n"
        "       -h,--help		this message\n"
	"\n"
	"	<opts> includes all valid VODATA options\n"
	"\n"
 	"  Examples:\n\n"
	"    1) Query the GSC 2.3 catalog for stars a) within the 0.1 \n"
	"	degree default search size around NGC 1234:  b) around all\n"
	"	positions contained in file 'pos.txt':  c) for the list \n"
	"	of objects given on the command line:  d) query a list of\n"
	"	services for a list of positions: e) print a count of \n"
	"	results that would be returned from 3 services for each\n"
	"	position in a file:\n\n"
  	"	    %% vocatalog gsc2.3 ngc1234                 (a)\n"
  	"	    %% vocatalog gsc2.3 pos.txt                 (b)\n"
  	"	    %% vocatalog gsc2.3 m31,m51,m93             (c)\n"
  	"	    %% vocatalog svcs.txt pos.txt               (d)\n"
  	"	    %% vocatalog hst,chandra,gsc2.3 pos.txt     (e)\n"
	"\n"
	"    2) Print a count of X-ray catalog data around Abell2712:\n\n"
  	"	    %% vocatalog -c -b x-ray any abell2712\n"
  	"	    %% vocatalog --count -b x-ray any abell2712\n"
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

    vo_taskTest (task, "--help", NULL);

    vo_taskTestFile ("m31\nm51\nm93\n", "pos.txt");
    vo_taskTestFile ("hst\nchandra\ngsc2.3\n", "svcs.txt");

    vo_taskTest (task, "gsc2.3", "ngc1234", NULL);			// Ex 1
    vo_taskTest (task, "gsc2.3", "pos.txt", NULL);
    vo_taskTest (task, "gsc2.3", "m31,m51,m93", NULL);
    vo_taskTest (task, "svcs.txt", "pos.txt", NULL);
    vo_taskTest (task, "hst,chandra,gsc2.3", "pos.txt", NULL);

    vo_taskTest (task, "-c", "-b", "x-ray", "any", "abell2712", NULL);	// Ex 2
    vo_taskTest (task, "--count", "-b", "x-ray", "any", "abell2712", NULL);


    if (access ("pos.txt", F_OK)  == 0)    unlink ("pos.txt");
    if (access ("svcs.txt", F_OK) == 0)    unlink ("svcs.txt");

    vo_taskTestReport (self);
}
