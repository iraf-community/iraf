/**
 *  VOTOPIC -- Query VO services by keyword topic
 *
 *    Usage:
 *		votopic [<opts>] [ <object> | <ra> <dec> ] [ <size> ]
 *
 *  @file       votopic.c
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

#define	MAX_ARGS	1024


/*  Task specific option declarations.  Task options are declared using the
 *  getopt_long(3) syntax.
 */
int  voregistry (int argc, char **argv, size_t *len, void **result);
int  vodata (int argc, char **argv, size_t *len, void **result);
int  votopic (int argc, char **argv, size_t *len, void **result);

static Task  self       = {  "votopic",  votopic,  0,  0,  0  };
#ifdef USE_OPTS
static char  *opts = "%hrs";
static struct option long_opts[] = {
    { "test",     2, 0, '%'},           /* --test is std                */
    { "help",     2, 0, 'h'},           /* --help is std                */
    { "subject",  2, 0, 's'},           /* subject keywords             */
    { "return",   2, 0, 'r'},           /* return result to API         */
    { NULL,       0, 0,  0 }
};
#endif


static int  debug	= 0;		/* debug flag			*/
static int  by_subj	= 0;		/* subject search		*/

extern void  vot_setArg (char **argv, int *argc, char *value);
extern char *vot_mktemp (char *root);
extern int   isDecimal (char *val);
extern int   isSexagesimal (char *val);

static void Usage (void);
static void Tests (char *input);


/**
 *  Application entry point.  All VOApps tasks MUST contain this 
 *  method signature.
 */
int
votopic (int argc, char **argv, size_t *reslen, void **result)
{
    int    i, nres_arg = 0, ndat_arg = 0, status = OK, term = 0;
    char  *res_argv[MAX_ARGS], *dat_argv[MAX_ARGS], *topic = NULL;
    char  *resname;


    memset (res_argv, 0, argc+2);	/* initialize	*/
    memset (dat_argv, 0, argc+2);
    resname = vot_mktemp ("votopic");


    /*  Parse the argument list.
     */
    if ((isDecimal (argv[argc-1]) || isSexagesimal (argv[argc-1])) &&
        (isDecimal (argv[argc-2]) || isSexagesimal (argv[argc-2])) &&
        (isDecimal (argv[argc-3]) || isSexagesimal (argv[argc-3])) )
	    term = argc - 4;
    if ((isDecimal (argv[argc-1]) || isSexagesimal (argv[argc-1])) &&
        !(isDecimal (argv[argc-2]) || isSexagesimal (argv[argc-2])) )
	    term = argc - 3;
    if (!(isDecimal (argv[argc-1]) || isSexagesimal (argv[argc-1])) )
	    term = argc - 2;

    /*  Initialize the VOREGISTRY arguments.
     */
    vot_setArg (res_argv, &nres_arg, "voregistry");

    /*  Initialize the VODATA arguments.
     */
    vot_setArg (dat_argv, &ndat_arg, "vodata");
#ifdef OLD_METHOD
    if (term <= 0) {
	fprintf (stderr, "Error: missing <topic> and/or <object> argument\n");
	return (ERR);
    }

    for (i=1; i < argc; i++) {
	if (i == term) {
            vot_setArg (dat_argv, &ndat_arg, resname);
	    topic = argv[i];
	} else {
	    if (strcmp(argv[i],"-h")==0 || strcmp(argv[i],"--help")==0) {
		Usage (); return (OK);
	    } else if (strcmp(argv[i],"-%")==0 || strcmp(argv[i],"--test")==0) {
		Tests (NULL); return (self.nfail);
	    } else if (strcmp(argv[i],"-d")==0 || strcmp(argv[i],"--dbg")==0) {
		debug++;

	    } else if (strcmp(argv[i],"-s")==0 || 
		       strcmp(argv[i],"--subject")==0) {
		           by_subj++;
	    } else if ((strcmp(argv[i],"-b")==0 || 
		        strcmp(argv[i],"--bpass")==0) ||
	               (strcmp(argv[i],"-t")==0 || 
		        strcmp(argv[i],"--type")==0)) {
    		vot_setArg (res_argv, &nres_arg, argv[  i]);	/* -b | -t  */
    		vot_setArg (res_argv, &nres_arg, argv[++i]);	/* val      */
	    } else
                vot_setArg (dat_argv, &ndat_arg, argv[i]);
	}
    }

#else
    for (i=1; i < argc; i++) {
	if (strcmp(argv[i],"-h")==0 || strcmp(argv[i],"--help")==0) {
	    Usage (); return (0);
	} else if (strcmp(argv[i],"-%")==0 || strcmp(argv[i],"--test")==0) {
	    Tests (NULL); return (0);
	} else if (strcmp(argv[i],"-d")==0 || strcmp(argv[i],"--dbg")==0) {
	    debug++;

	} else if (strcmp(argv[i],"-s")==0 || 
	           strcmp(argv[i],"--subject")==0) {
	               by_subj++;
	} else if ((strcmp(argv[i],"-b")==0 || 
	            strcmp(argv[i],"--bpass")==0) ||
	           (strcmp(argv[i],"-t")==0 || 
	            strcmp(argv[i],"--type")==0)) {
    		vot_setArg (res_argv, &nres_arg, argv[  i]);	/* -b | -t  */
    		vot_setArg (res_argv, &nres_arg, argv[++i]);	/* val      */
	} else {
	    if (!topic) {
                vot_setArg (dat_argv, &ndat_arg, resname);
	        topic = argv[i];
	    } else {
                vot_setArg (dat_argv, &ndat_arg, argv[i]);
	    }
	}
    }

    if (topic == NULL) {
	fprintf (stderr, "Error: missing <topic> argument\n");
	return (ERR);
    } else if (ndat_arg < 3) {
	fprintf (stderr, "Error: missing <object> (or <pos>) argument\n");
	return (ERR);
    }

#endif


    /*  Create the list of VOREGISTRY arguments.
     */
    vot_setArg (res_argv, &nres_arg, "-d");		/* dalOnly 	*/
    vot_setArg (res_argv, &nres_arg, "-o");		/* output name	*/
    vot_setArg (res_argv, &nres_arg, resname);
    if (by_subj)
        vot_setArg (res_argv, &nres_arg, "-s");
    vot_setArg (res_argv, &nres_arg, topic);

    if (debug) {
	for (i=0; i < nres_arg; i++) 
	    printf ("'%s' ", res_argv[i]);
	printf ("\n");
	for (i=0; i < ndat_arg; i++) 
	    printf ("'%s' ", dat_argv[i]);
	printf ("\n");
    }


    /*  Run the Registry query to get the resource list of topics.
     */
    status = voregistry (nres_arg, res_argv, reslen, result);


    /*  Initialize result object whether we return an object or not.
     */
    *reslen = 0;	
    *result = NULL;

   /*  The VODATA task does all the real work, execute it on the list of
    *  resources we just obtained.
    */
    status = vodata (ndat_arg, dat_argv, reslen, result);


    /*  Clean up.  Rememebr to free whatever pointers were created when
     *  parsing arguments.
     */
    for (i=0; i < nres_arg; i++)
	if (res_argv[i]);
	    free ((void *) res_argv[i]);
    for (i=0; i < ndat_arg; i++)
	if (dat_argv[i]);
	    free ((void *) dat_argv[i]);
    unlink (resname);

    return (status);
}


/**
 *  USAGE -- Print task help summary.
 */
static void
Usage (void)
{
    fprintf (stderr, "\n  Usage:\n\t"
        "votopic [<opts>] <topic>  { <object> | <ra> <dec> } [ <size> ]\n\n"
        "  where\n"
        "       -%%,--test		run unit tests\n"
        "       -h,--help		this message\n"
	"\n"
	"\n"
 	"  Examples:\n\n"
	"    1) Query for catalog data of A2712 from Resources related to\n"
        "       gravitational lensing:\n"
	"\n"
	"	    %% votopic -t catalog lens A2712\n"
	"\n"
        "       This query is against only 142 services (data found for 128),\n"
        "       a similar query against ALL catalog services would require\n"
        "       more than 8000 services to be queried. This is equivalent to\n"
        "       the commands:\n"
	"\n"
	"           %% voregistry -t catalog -d -o lens.xml lens\n"
        "           %% vodata lens.xml A2712\n"
        "\n"
        "       Here the 'lens.xml' output file contains the resources to be\n"
        "       queried, the '-d' flag says to return only DAL services, the\n"
        "       '-t' flag resticts by service type.  In the call to VODATA,\n"
        "       the 'lens.xml' file defines the 142 resources to be queried,\n"
        "       the contraint to service type and DAL-only was done in the\n"
        "       registry query that produced the file.\n"
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

   vo_taskTest (task, "-t", "catalog", "lens", "A2712", NULL);

   vo_taskTestReport (self);
}
