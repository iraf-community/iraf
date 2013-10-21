/*
 *  VOTSTAT -- Compute statistics for numeric columns of a VOTable.
 *
 *    Usage:
 *		votstat [<otps>] <votable>
 *
 *  @file       votstat.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/12
 *
 *  @brief      Compute statistics for numeric columns of a VOTable.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "votParse.h"			/* keep these in order!		*/
#include "voApps.h"



/*  Global task declarations.
 */
static int vot		= 0;		/* VOTable handle		*/

static int  do_all	= 0;		/* all columns?			*/
static int  do_return   = 0;		/* return result?		*/


/*  A result buffer should be defined to point to the result object if it is
 *  created dynamically, e.g. a list of votable columns.  The task is
 *  responsible for initially allocating this pointer and then resizing as
 *  needed.
 */
#ifdef USE_RESBUF
#define	SZ_RESBUF	8192

static char *resbuf;
#endif


/*  Task specific option declarations.  Task options are declared using the
 *  getopt_long(3) syntax.
 */
int  votstat (int argc, char **argv, size_t *len, void **result);

static Task  self       = {  "votstat",  votstat,  0,  0,  0  };
static char  *opts 	= "%:hao:r";
static struct option long_opts[] = {
        { "test",         1, 0,   '%'},
        { "help",         2, 0,   'h'},
        { "all",          2, 0,   'a'},
        { "output",       1, 0,   'o'},
        { "return",       2, 0,   'r'},
        { NULL,           0, 0,    0 }
};


/*  Standard usage method.
 */
static void Usage (void);
static void Tests (char *input);

void vot_colStat (int tdata, int col, int nrows, double *min, double *max, 
	double *mean, double *stddev); 

extern int    vot_isNumericField (handle_t field);
extern double vot_atof (char *v);


/**
 *  Application entry point.
 */
int
votstat (int argc, char **argv, size_t *reslen, void **result)
{
    /*  These declarations are required for the VOApps param interface.
     */
    char **pargv, optval[SZ_FNAME];

    /*  These declarations are specific to the task.
     */
    char  *iname, *oname, *name, *id, *fstr;
    int    ch = 0, status = OK, numeric = 0;
    int    res, tab, data, tdata, field;
    int    i, ncols, nrows, pos = 0;
    FILE  *fd = (FILE *) NULL;


    /* Initialize result object	whether we return an object or not.
     */
    *reslen = 0;	
    *result = NULL;

    iname  = NULL; 		/* initialize local task values  	*/
    oname  = NULL;


    /*  Parse the argument list.  
     */
    pargv = vo_paramInit (argc, argv, opts, long_opts);
    while ((ch = vo_paramNext(opts,long_opts,argc,pargv,optval,&pos)) != 0) {
        if (ch > 0) {
	    switch (ch) {
	    case '%':  Tests (optval);			return (self.nfail);
	    case 'h':  Usage ();			return (OK);
	    case 'a':  do_all++;			break;
	    case 'o':  oname = strdup (optval);		break;
	    case 'r':  do_return=1;	    	    	break;
	    default:
		fprintf (stderr, "Invalid option '%s'\n", optval);
		return (1);
	    }

        } else if (ch == PARG_ERR) {
            return (ERR);

	} else {
	    /*  Process the positional arguments.
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
    if (strcmp(iname, "-") == 0) { free (iname), iname = strdup ("stdin");  }
    if (strcmp(oname, "-") == 0) { free (oname), oname = strdup ("stdout"); }

    if (strcmp ("stdout", oname) == 0) 
	fd = stdout;
    else {
	if ((fd = fopen (oname, "w+")) == (FILE *) NULL) {
	    fprintf (stderr, "Cannot open output file '%s'\n", oname);
	    return (ERR);
	}
    }

    /* Open the table.  This also parses it.
    */
    if ( (vot = vot_openVOTABLE (iname) ) <= 0) {
        fprintf (stderr, "Error opening VOTable '%s'\n", iname);
        return (1);
    }

    res   = vot_getRESOURCE (vot);      /* get handles          */
    if (vot_getLength (res) > 1) {
        fprintf (stderr, "Error: multiple RESOURCE elements not supported\n");
        goto clean_up_;
    }
    tab   = vot_getTABLE (res);
    if ((data = vot_getDATA (tab)) <= 0)
	goto clean_up_;
    tdata = vot_getTABLEDATA (data);
    nrows = vot_getNRows (tdata);
    ncols = vot_getNCols (tdata);


    fprintf (fd, "# %3s  %-20.20s  %9.9s  %9.9s  %9.9s  %9.9s\n#\n",
	"Col", "Name", "Min", "Max", "Mean", "StdDev");

    for (i=0,field=vot_getFIELD(tab); field; field=vot_getNext (field), i++) {
        name  = vot_getAttr (field, "name");
        id    = vot_getAttr (field, "id");

	numeric = vot_isNumericField (field);
	fstr = (name ? name : (id ? id : "(none)"));

	if (do_all && !numeric)		/* non-numeric column		*/
            fprintf (fd, "  %3d  %-20.20s\n", i, fstr);

	else if (do_all || numeric) {	/* numeric column		*/
	    double  min, max, mean, stddev;

	    vot_colStat (tdata, i, nrows, &min, &max, &mean, &stddev);

	    if (mean > 1.0e6 || mean < 1.0e-3)
                fprintf (fd, "  %3d  %-20.20s  %9.4g  %9.4g  %9.4g  %9.4g\n",
                    i, fstr, min, max, mean, stddev);
	    else
                fprintf (fd, "  %3d  %-20.20s  %9.2f  %9.2f  %9.2f  %9.2f\n",
                    i, fstr, min, max, mean, stddev);
	}
    }


    /*  Clean up.  Rememebr to free whatever pointers were created when
     *  parsing arguments.
     */
clean_up_:
    if (iname) free (iname);
    if (oname) free (oname);

    vo_paramFree (argc, pargv);
    vot_closeVOTABLE (vot);

    if (fd != stdout)
	fclose (fd);

    return (status);	/* status must be OK or ERR (i.e. 0 or 1)     	*/
}


/**
 *  VOT_COLSTAT -- Determine the statistics of a table column.
 */
void
vot_colStat (int tdata, int col, int nrows, double *min, double *max, 
		double *mean, double *stddev)
{
    register int i = 0;
    double  sum = 0.0, sum2 = 0.0, val = 0.0;


    *min    =  0.99e306;
    *max    = -0.99e306;
    *mean   = 0.0;
    *stddev = 0.0;

    for (i=0; i < nrows; i++) {
	val   = vot_atof (vot_getTableCell (tdata, i, col));
	sum  += val;
	sum2 += (val * val);
	if (val < (*min))  *min = val;
	if (val > (*max))  *max = val;
    }

    *mean = (double) (sum / (double) nrows);
    *stddev = sqrt ( ( sum2 / (double) nrows) - 
	( (sum / (double) nrows) * (sum / (double) nrows) ));
}


/**
 *  USAGE -- Print task help summary.
 */
static void
Usage (void)
{
    fprintf (stderr, "\n  Usage:\n\t"
        "votstat [<opts>] votable.xml\n\n"
        "  where\n"
        "       -h,--help		this message\n"
        "       -%%,--test		run unit tests\n"
        "       -r,--return		return result from method\n"
	"\n"
        "       -a,--all		print all columns\n"
        "       -o,--output=<file>	output file\n"
	"\n"
 	"  Examples:\n\n"
	"    1) Print statistics for a VOTable\n\n"
	"	  %% votstat test.xml\n"
	"\n"
	"    2) Print statistics for all column in a VOTable and save \n"
        "       results to a file.\n\n"
	"	    %% votstat -a -o stats test.xml\n"
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
}
