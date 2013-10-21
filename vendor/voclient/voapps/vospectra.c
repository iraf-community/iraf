/**
 *  VOSPECTRA -- Query all VO Spectra services
 *
 *    Usage:
 *		vospectra [<opts>] [ <object> | <ra> <dec> ] [ <size> ]
 *
 *  @file       vospectra.c
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
int  vospectra (int argc, char **argv, size_t *len, void **result);

static Task  self       = {  "vospectra",  vospectra,  0,  0,  0  };

extern void  vot_setArg (char **argv, int *argc, char *value);

static void Usage (void);
static void Tests (char *input);


/**
 *  Application entry point.  All VOApps tasks MUST contain this 
 *  method signature.
 */
int
vospectra (int argc, char **argv, size_t *reslen, void **result)
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
    vot_setArg (pargv, &narg, "-t");
    vot_setArg (pargv, &narg, "spectra");
    for (i=1; i < argc; i++)
        vot_setArg (pargv, &narg, argv[i]);


   /**
    *  The VODATA task does all the real work, we effectively just set the
    *  "-t spectra" option to force the service type as a logical naming 
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
        "vospectra [<opts>] votable.xml\n\n"
        "  where\n"
        "       -%%,--test		run unit tests\n"
        "       -h,--help		this message\n"
        "\n"
        "       <opts> includes all valid VODATA options\n"
	"\n"
 	"  Examples:\n\n"
	"    1) Query all services for spectra data of 3c273\n\n"
	"	    %% vospectra any 3c273\n"
	"\n"
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

   vo_taskTest (task, "any", "3c273", NULL);

   vo_taskTestReport (self);
}
