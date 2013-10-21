/*
 *  VOTPOS -- Extract the main positional columns from a VOTable.
 *
 *    Usage:
 *	votpos [<opts>] votable.xml
 *
 *    where
 *       -%%,--test              run unit tests
 *       -h,--help               this message
 *       -n,--number             number output
 *       -o,--output=<file>      output file
 *       -r,--return             return result from method
 *
 *  @file       votpos.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/12
 *
 *  @brief       Extract the main positional columns from a VOTable.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

#include "votParse.h"
#include "voApps.h"


#define	SZ_RESBUF	8192


static int  vot		= 0;		/* VOTable handle		*/
static int  number	= 0;		/* number values?		*/
static int  do_return   = 0;		/* return result?		*/

#ifdef USE_RESBUF
static char *resbuf;			/* result buffer		*/
#endif


/*  Task specific option declarations.
 */
int  votpos (int argc, char **argv, size_t *len, void **result);

static Task  self	= {  "votpos",  votpos,  0,  0,  0  };
static char  *opts 	= "hNno:r%:";
static struct option long_opts[] = {
        { "Number",       2, 0,   'N'},		/* task option		*/
        { "number",       2, 0,   'n'},		/* task option		*/
        { "output",       1, 0,   'o'},		/* task option		*/
        { "return",       2, 0,   'r'},		/* task option		*/
        { "help",         2, 0,   'h'},		/* required		*/
        { "test",         1, 0,   '%'},		/* required		*/
        { NULL,           0, 0,    0 }
};


static void Usage (void);
static void Tests (char *input);



/**
 *  Application entry point.
 */
int
votpos (int argc, char **argv, size_t *reslen, void **result)
{
    char **pargv, optval[SZ_FNAME], *iname = NULL, *oname = NULL, *ucd = NULL;
    int    res, tab, data, tdata, field, status = OK;
    int    i, ncols, nrows, pos = 0, ch, ra_col, dec_col;
    int    got_ra_col = 0, got_dec_col = 0;
    FILE  *fd = (FILE *) NULL;


    /*  Initialize. 
     */
    oname   = NULL;
    iname   = NULL;
    *reslen = 0;
    *result = NULL;

    /*  Parse the argument list.
     */
    pargv = vo_paramInit (argc, argv, opts, long_opts);
    while ((ch = vo_paramNext (opts,long_opts,argc,pargv,optval,&pos)) != 0) {
        if (ch > 0) {
	    switch (ch) {
	    case '%':  Tests (optval);			return (self.nfail);
	    case 'h':  Usage ();			return (OK);
	    case 'N':  number=-1;			break;
	    case 'n':  number=+1;			break;
	    case 'o':  oname = strdup (optval);		break;
	    case 'r':  do_return=1;	    	    	break;
	    default:
		fprintf (stderr, "Invalid option '%s'\n", optval);
		return (1);
	    }

        } else if (ch == PARG_ERR) {
            return (ERR);

	} else {
	    iname = strdup (optval);
	    break;
	}
    }


    /* Sanity checks
     */
    if (iname == NULL) iname = strdup ("stdin");
    if (oname == NULL) oname = strdup ("stdout");
    if (strcmp (iname, "-") == 0) { free (iname), iname = strdup ("stdin"); }
    if (strcmp (oname, "-") == 0) { free (oname), oname = strdup ("stdout"); }
	

    /* Open the table.  This also parses it.
    */
    if ( (vot = vot_openVOTABLE (iname) ) <= 0) {
	fprintf (stderr, "Error opening VOTable '%s'\n", iname);
	return (1);
    }

    res   = vot_getRESOURCE (vot);	/* get handles		*/
    if (vot_getLength (res) > 1) {
	fprintf (stderr, "Error: multiple RESOURCE elements not supported\n");
        goto clean_up_;
    }
    tab   = vot_getTABLE (res);
    if ((data  = vot_getDATA (tab))) {
        tdata = vot_getTABLEDATA (data);
        nrows = vot_getNRows (tdata);
        ncols = vot_getNCols (tdata);
    } else
        goto clean_up_;


    /*  Find the columns.
    */
    for (i=0, field=vot_getFIELD(tab); field; field=vot_getNext (field),i++) {
	if ((ucd  = vot_getAttr (field, "ucd"))) {
	  if ((strcmp (ucd, "POS_EQ_RA_MAIN") == 0)  ||		/* UCD 1  */
	      (strcmp (ucd, "pos.eq.ra;meta.main") == 0)) {	/* UCD 1+ */
		ra_col = i;
		got_ra_col = 1;
	  }
	  if ((strcmp (ucd, "POS_EQ_DEC_MAIN") == 0) ||		/* UCD 1  */
	      (strcmp (ucd, "pos.eq.dec;meta.main") == 0)) {	/* UCD 1+ */
		dec_col = i;
		got_dec_col = 1;
	  }
	}
    }
    if (!got_ra_col || !got_dec_col) {
	fprintf (stderr, "Error: Cannot find position columns in table.\n");
	status = ERR;
        goto clean_up_;
    }
		
    /*  Print the position cells.
     */
    fd = stdout;
    if (strncasecmp ("stdout", oname, 6)) {
	if ((fd = fopen (oname, "w+")) == NULL) {
	    fprintf (stderr, "Error: Cannot open output file '%s'\n", oname);
	    return (ERR);
	}
    }

    for (i=0; i < nrows; i++) {
	char  *ra, *dec, *s;

	s   = vot_getTableCell (tdata, i, ra_col);   ra  = (s ? s : "INDEF");
	s   = vot_getTableCell (tdata, i, dec_col);  dec = (s ? s : "INDEF");
	if (number > 0)
	    fprintf (fd, "%d  ", i);
	fprintf (fd, "%s %s", ra, dec);
	if (number < 0)
	    fprintf (fd, "  %d", i);
	fprintf (fd, "\n");
    }


    /* Clean up.
     */
clean_up_:
    if (fd != stdout)
	fclose (fd);
    if (iname) free (iname);
    if (oname) free (oname);

    vo_paramFree (argc, pargv);
    vot_closeVOTABLE (vot);		/* close the table  	*/

    return (status);
}


/**
 *  USAGE -- Print task help summary.
 */
static void
Usage (void)
{
    fprintf (stderr, "\n  Usage:\n\t"
        "votpos [<opts>] votable.xml\n\n"
        "  where\n"
        "       -%%,--test		run unit tests\n"
        "       -h,--help		this message\n"
        "       -n,--number		number output\n"
        "       -o,--output=<file>	output file\n"
        "       -r,--return		return result from method\n"
	"\n"
 	"  Examples:\n\n"
	"    1)  Print the primary (RA,Dec) columns from a table:\n\n"
	"	    %% votpos test.xml\t\t\t# un-numbered\n"
	"	    %% votpos -n test.xml\t\t# numbered\n"
	"	    %% cat test.xml | votpos\t\t# un-numbered\n"
	"\n"
	"    2)  Print the primary (RA,Dec) columns to a file:\n\n"
	"	    %% votpos -o pos.txt test.xml\n"
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

    vo_taskTest (task, input, NULL);
    vo_taskTest (task, "-n", input, NULL);
    vo_taskTest (task, "-o", "-", input, NULL);
    vo_taskTest (task, "-o", "pos.txt", input, NULL);

    if (access ("pos.txt", F_OK) == 0)   unlink ("pos.txt");

    vo_taskTestReport (self);
}
