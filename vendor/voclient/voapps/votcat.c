/**
 *  VOTCAT -- Concatenate multiple VOTables.
 *
 *    Usage:
 *              votcat [-n] <votable> <votable> ....
 *
 *  @file       votcat.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/12
 *
 *  @brief      Concatenate multiple VOTables.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "votParse.h"
#include "voApps.h"



#define	MAX_FILES	1024


static int nfiles  	= 0;		/* Number of input files	*/
static int out		= 0;		/* Output VOTable handle	*/

static int vot[MAX_FILES];		/* First VOTable handle		*/
static int res[MAX_FILES];		/* <RESOURCE> handle 		*/

static int do_return	= 0;		/* return object?		*/

/*  Task specific option declarations.
 */
int  votcat (int argc, char **argv, size_t *len, void **result);

static Task  self       = {  "votcat",  votcat,  0,  0,  0  };
static char  *opts      = "hivo:r%:";
static struct option long_opts[] = {
        { "help",         2, 0,   'h'},         /* required             */
        { "test",         1, 0,   '%'},         /* required             */
        { "indent",       2, 0,   'i'},         /* task option          */
        { "output",       1, 0,   'o'},         /* task option          */
        { "verbose",      2, 0,   'v'},         /* task option          */
        { NULL,           0, 0,    0 }
};

static void Usage (void);
static void Tests (char *input);

extern int  vot_atoi (char *v);


/**
 *  Application entry point.
 */
int
votcat (int argc, char **argv, size_t *reslen, void **result)
{
    char **pargv, optval[SZ_FNAME];
    char  *oname = (char *) NULL, ch;
    char  *infile[MAX_FILES];
    int   i, verbose = 0, indent = 1, pos = 0;


    if (argc < 3) {
	fprintf (stderr, "Usage:  votconcat [-o <out>] <vot1> <vot2> ....\n");
	return (1);
    }

    /*  Parse the argument list.
     */
    pargv = vo_paramInit (argc, argv, opts, long_opts);
    while ((ch = vo_paramNext (opts,long_opts,argc,pargv,optval,&pos)) != 0) {
        if (ch > 0) {
            switch (ch) {
            case '%':  Tests (optval);                  return (self.nfail);
            case 'h':  Usage ();                        return (OK);
	    case 'i':  indent = vot_atoi (optval);	break;
	    case 'o':  oname = strdup (optval);		break;
	    case 'v':  verbose++; 			break;
            case 'r':  do_return = 1;                   break;
            default:
                fprintf (stderr, "Invalid option '%s'\n", optval);
                return (1);
            }

        } else if (ch == PARG_ERR) {
            return (ERR);

        } else {
	    infile[nfiles++] = strdup (optval);
        }
    }

    /* Sanity checks
     */
    if (oname == NULL) oname = strdup ("stdout");
    if (strcmp (oname, "-") == 0) { free (oname), oname = strdup ("stdout"); }
        

    /*  Open output table.
     */
    out = vot_openVOTABLE (NULL); 	

    /*  Loop over the input tables on the cmdline.
     */
    for (i=0; i < nfiles; i++) {
        vot[i] = vot_openVOTABLE (infile[i]);	/*  Parse the table	*/

	/*  Concatenate the tables.
 	 */
	if ((res[i] = vot_getRESOURCE (vot[i])))
            vot_attachNode (out, res[i]); 		
    }

    /*  Write it out.
     */
    vot_writeVOTable (out, oname, indent);

    for (i=0; i < nfiles; i++)
        vot_closeVOTABLE (vot[i]);	/* close the input tables	*/
    vot_closeVOTABLE (vot[i]);             /* close the output table	*/


    /*  Close (and free) the output table and allocated pointers.
     */
    if (oname) 
	free (oname);
    for (i=0; i < nfiles; i++) {
        if (infile[i]) 
	    free (infile[i]);
    }

    vo_paramFree (argc, pargv);
    return (OK);
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
        "       -%%,--test               run unit tests\n"
        "       -h,--help               this message\n"
        "       -n,--number             number output\n"
        "       -o,--output=<file>      output file\n"
        "       -r,--return             return result from method\n"
        "\n"
        "  Examples:\n\n"
        "    1)  First example\n\n"
        "           %% votcat test.xml\n"
        "\n"
        "    2)  Second example\n\n"
        "           %% votcat -o pos.txt test.xml\n"
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
