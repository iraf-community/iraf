/**
 *  VOTCNV -- Convert a VOTable to a different format. 
 *
 *    Usage:
 *	    votcnv [<opts>] <votable>
 *
 *    Where
 *	    -f,--fmt <fmt>	  Output format (XML, CSV, TSV, HTML, etc)
 *	    -h,--help 	    	  Print help summary
 *	    -i,--indent <N>    	  Indention at each level for VOTable output
 *	    -o,--output <fname>	  Name of output file
 *
 *	    -n,--noheader	  Don't write a header
 *	    -r,--return		  Return result to API
 *
 *	    <votable>	          Name of input file to compress
 *
 *  @file       votcnv.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/12
 *
 *  @brief      Convert a VOTable to a different format. 
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "votParse.h"
#include "voApps.h"


static char  *fmt	= NULL;		/* format string		*/
static int    vot	= 0;		/* VOTable root handle		*/
static int    indent	= 0;		/* indention at each level	*/
static int    hdr	= 1;		/* print header ??		*/
static int    do_return	= 0;		/* return result from method?	*/

/*  Task specific option declarations.
 */
int  votcnv (int argc, char **argv, size_t *len, void **result);

static Task  self       = {  "votcnv",  votcnv,  0,  0,  0  };
static char  *opts = "%:f:hi:no:r";
static struct option long_opts[] = {
    { "test",     1, 0, '%'},		/* --test is std		*/
    { "help",     2, 0, 'h'},		/* --help is std		*/
    { "fmt",      1, 0, 'f'},		/* output format		*/
    { "indent",   1, 0, 'i'},		/* XML indention level		*/
    { "noheader", 2, 0, 'n'},		/* suppress header		*/
    { "output",   1, 0, 'o'},		/* output name			*/
    { "return",   2, 0, 'r'},		/* return result to API		*/
    { NULL,       0, 0,  0 }
};


static void Usage (void);
static void Tests (char *input);

extern int strdic (char *in_str, char *out_str, int maxchars, char *dict);
extern int vot_isValidFormat (char *fmt);
extern int vot_atoi (char *v);



/**
 *  VOApps task entry point.
 */
int
votcnv (int argc, char **argv, size_t *reslen, void **result)
{
    int     status = OK, pos = 0;
    char   *iname = NULL, *name = NULL, *oname = NULL, format[SZ_FORMAT];
    char  **pargv, ch, optval[SZ_FNAME];


    /*  Parse the argument list.
     */
    pargv = vo_paramInit (argc, argv, opts, long_opts);
    while ((ch = vo_paramNext (opts,long_opts,argc,pargv,optval,&pos)) != 0)  {
	if (ch > 0) {
	    switch (ch) {
	    case '%':   Tests (optval);			return (self.nfail);
	    case 'h':   Usage();			return (OK);
	    case 'f':   if (!vot_isValidFormat ((fmt = strdup (optval)))) {
			    fprintf (stderr, "Error: invalid format '%s'\n",
				fmt);
			    return (ERR);
    			}
    			break;
	    case 'i':   indent = vot_atoi (optval);	break;
	    case 'n':   hdr--;				break;
	    case 'o':   oname  = strdup (optval);   	break;
	    case 'r':   do_return++;			break;
	    default:
		fprintf (stderr, "Invalid argument '%s'\n", optval);
		return (ERR);
	    }

	} else if (ch == PARG_ERR) {
	    return (ERR);

	} else {
	    iname = strdup (optval);
	    break;
	}
    }


    /* Sanity checks.
     */
    if (iname == NULL) iname = strdup ("stdin");                           
    if (oname == NULL) oname = strdup ("stdout");                          
    if (strcmp (iname, "-") == 0) { free (iname), iname = strdup ("stdin");  }
    if (strcmp (oname, "-") == 0) { free (oname), oname = strdup ("stdout"); }

    fmt = (fmt ? fmt : strdup ("xml"));

    if (do_return) {			/* return result		*/
	if (name) free (name);
	oname = calloc (1, SZ_FNAME);
	sprintf (oname, "/tmp/votcnv.%d", (int) getpid());
    } else 
        oname = (oname ? oname : strdup ("stdout"));


    /* Open and parse the input table.
    */
    if ( (vot = vot_openVOTABLE (iname) ) <= 0) {
	fprintf (stderr, "Error opening VOTable '%s'\n", iname);
	return (ERR);
    }

    /*  Output the new format.
     */
    switch (strdic (fmt, format, SZ_FORMAT, FORMATS)) {
    case   VOT:   vot_writeVOTable (vot, oname, indent);      break;
    case   ASV:   vot_writeASV (vot, oname, hdr);      	      break;
    case   BSV:   vot_writeBSV (vot, oname, hdr);      	      break;
    case   CSV:   vot_writeCSV (vot, oname, hdr);      	      break;
    case   TSV:   vot_writeTSV (vot, oname, hdr);      	      break;
    case  HTML:   vot_writeHTML (vot, iname, oname);          break;
    case SHTML:   vot_writeSHTML (vot, iname, oname);         break;
    case  FITS:   vot_writeFITS (vot, oname);    	      break;
    case   XML:   vot_writeVOTable (vot, oname, indent);      break;
    case ASCII:   vot_writeASV (vot, oname, hdr);      	      break;
    case   RAW:   vot_writeVOTable (vot, oname, indent);      break;
    default:
	fprintf (stderr, "Unknown output format '%s'\n", fmt);
	status = ERR;
    }
    vot_closeVOTABLE (vot);		/* close the table  	*/


    /*  If we requested a return object, get it from the output file.
     */
    if (do_return) {
	vo_setResultFromFile (oname, reslen, result);
	unlink (oname);
    }

    /*  Clean up.
     */
    if (fmt)   free (fmt);
    if (name)  free (name);
    if (iname) free (iname);
    if (oname) free (oname);
    vo_paramFree (argc, pargv);

    return (status);
}


/**
 *  USAGE -- Print a task help summary.
 */
static void
Usage (void)
{
    fprintf (stderr, "\n  Usage:\n\t"
	"votcnv [-f <fmt>] [-h] [-i <N>] [-o <outname>] [-n] votable.xml\n\n"
	"  where\n"
	"	-f,--fmt <fmt>		output format\n"
	"	-h,--help		this message\n"
	"	-i,--indent <N>		indented xml output?\n"
	"	-n,--noheader		disable output header\n"
	"	-o,--output <name>	output filename\n"
	"	-r,--return		return result from method\n"
	"\n"
	"  <fmt> is one of\n"
	"	    vot                 A new VOTable\n"
	"	    asv                 ascii separated values\n"
	"	    bsv                 bar separated values\n"
	"	    csv                 comma separated values\n"
	"	    tsv                 tab separated values\n"
	"	    html                standalone HTML document\n"
	"	    shtml               single HTML <table>\n"
	"	    fits                FITS binary table\n"
	"	    ascii               ASV alias\n"
	"	    xml                 VOTable alias\n"
	"	    raw                 VOTable alias\n"
	"\n"
	"  Examples:\n\n"
	"  1)  Convert a VOTable to a CSV file:\n\n"
	"	%% votcnv --fmt=csv test.xml\n"
	"\n"
	"  2)  Rewrite a VOTable with readable indention:\n\n"
	"	%% votcnv -f vot -i 2 test.xml\n"
	"\n"
	"  3)  Remove indention from a VOTable:\n\n"
	"	%% votcnv -f vot -i 0 test.xml\n"
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

    if (access (input, F_OK) != 0) {
	fprintf (stderr, "Warning:  cannot open file '%s'\n", input);
	return;
    }

    /* Test each of the conversion formats.
     */
    vo_taskTest (task, "--fmt=vot", input, NULL);			// Ex 1
    vo_taskTest (task, "--fmt=asv", input, NULL);
    vo_taskTest (task, "--fmt=bsv", input, NULL);
    vo_taskTest (task, "--fmt=csv", input, NULL);
    vo_taskTest (task, "--fmt=tsv", input, NULL);
    vo_taskTest (task, "--fmt=html", input, NULL);
    vo_taskTest (task, "--fmt=shtml", input, NULL);
    vo_taskTest (task, "--fmt=fits", "-o", "test.fits", input, NULL);
    vo_taskTest (task, "--fmt=ascii", input, NULL);
    vo_taskTest (task, "--fmt=xml", input, NULL);

    vo_taskTest (task, "-f", "vot", "-i", "2", input, NULL);		// Ex 2
    vo_taskTest (task, "-f", "vot", "-i", "0", input, NULL);		// Ex 3
    vo_taskTest (task, "-f", "csv", "-n", input, NULL);			// Ex 4


    if (access ("test.fits", F_OK) == 0)  unlink ("test.fits");

    vo_taskTestReport (self);
}

