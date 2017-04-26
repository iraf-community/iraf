/*
 *  VOTSORT -- Sort a VOTable based on a column value.
 *
 *  Usage:
 *	votsort [<otps>] <votable.xml>
 *
 *  Where
 *	-c,--col <N>		Sort column num
 *	-d,--desc		Sort in descending order
 *	-f,--fmt <format>	Output format
 *	-o,--output <name>	Output name
 *	-s,--string		String sort
 *	-t,--top <N>		Print top <N> rows
 *	-i,--indent <N>		XML indent level
 *	-n,--noheader		Suppress header
 *	-N,--name <name>	Find <name> column
 *	-I,--id <id>		Find <id> column
 *	-U,--ucd <ucd>		Find <ucd> column
 *
 *	-h,--help		This message
 *	-r,--return		Return result
 *	-%,--test 		Run unit tests
 *
 *  @file       votsort.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/12
 *
 *  @brief      Sort a VOTable based on a column.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "votParse.h"			/* keep these in order!		*/
#include "voApps.h"


static int  do_return   =  0;		/* return result?		*/
static int  sort_order  =  1;		/* ascending order		*/
static int  top         =  0;		/* top results (0 for all)      */



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
int  votsort (int argc, char **argv, size_t *len, void **result);

static Task  self       = {  "votsort",  votsort,  0,  0,  0  };
static char  *opts 	= "%:c:df:hi:LnN:I:U:o:rst:";
static struct option long_opts[] = {
        { "col",          1, 0,   'c'},		/* sort column num	    */
        { "desc",         2, 0,   'd'},		/* sort in descending order */
        { "fmt",          1, 0,   'f'},		/* output format	    */
        { "output",       1, 0,   'o'},		/* output name 		    */
        { "string",       2, 0,   's'},		/* string sort		    */
        { "top",          1, 0,   't'},		/* string sort		    */
        { "indent",       1, 0,   'i'},		/* xml indent level	    */
        { "noheader",     2, 0,   'n'},		/* suppress header	    */
        { "name",         1, 0,   'N'},		/* find <name> column	    */
        { "id",           1, 0,   'I'},		/* find <id> column	    */
        { "ucd",          1, 0,   'U'},		/* find <ucd> column	    */

        { "help",         2, 0,   'h'},		/* --help is std	    */
        { "return",       2, 0,   'r'},		/* --return is std	    */
        { "test",         1, 0,   '%'},		/* --test is std	    */
        { NULL,           0, 0,    0 }
};


/*  All tasks should declare a static Usage() method to print the help 
 *  text in response to a '-h' or '--help' flag.  The help text should 
 *  include a usage summary, a description of options, and some examples.
 */
static void Usage (void);
static void Tests (char *input);

extern int  vot_isNumericField (handle_t field);
extern int  vot_isValidFormat (char *fmt);
extern int  vot_atoi (char *val);
extern int strdic (char *in_str, char *out_str, int maxchars, char *dict);



/**
 *  Application entry point.  All VOApps tasks MUST contain this 
 *  method signature.
 */
int
votsort (int argc, char **argv, size_t *reslen, void **result)
{
    /*  These declarations are required for the VOApps param interface.
     */
    char **pargv, optval[SZ_FNAME], format[SZ_FORMAT];
    char  *iname, *oname, *fmt = NULL;
    char  *byName = NULL, *byID = NULL, *byUCD = NULL;
    int    i = 0, ch = 0, status = OK, pos = 0, col = -1, do_string = 0;
    int    vot, res, tab, data, tdata, field, tr;
    int    indent = 0, scalar = 0, hdr = 1;


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
	    case '%':   Tests (optval);			return (self.nfail);
	    case 'h':   Usage ();			return (OK);
	    case 'c':   col = vot_atoi (optval);	break;
	    case 'd':   sort_order = -1;		break;
            case 'f':   if (!vot_isValidFormat ((fmt = strdup (optval)))) {
                            fprintf (stderr, "Error: invalid format '%s'\n",
                                fmt);
                            return (ERR);
                        }
                        break;
	    case 'o':   oname = strdup (optval);	break;
	    case 'i':   indent = vot_atoi (optval);	break;
	    case 'n':   hdr=0;				break;
	    case 'N':   byName = strdup (optval);	break;
	    case 'I':   byID = strdup (optval);		break;
	    case 'U':   byUCD = strdup (optval);	break;
	    case 'r':   do_return = 1;	    	    	break;
	    case 's':   do_string = 1;	    	    	break;
	    case 't':   top = vot_atoi (optval);    	break;
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
	     *  remember to free it later).
	     */
	    iname = strdup (optval);
	    break;			/* only allow one file		*/
	}
    }


    /*  Sanity checks.  Tasks should validate input and accept stdin/stdout
     *  where it makes sense.
     */
    if (iname == NULL) iname = strdup ("stdin");
    if (oname == NULL) oname = strdup ("stdout");
    if (strcmp (iname, "-") == 0) { free (iname), iname = strdup ("stdin");  }
    if (strcmp (oname, "-") == 0) { free (oname), oname = strdup ("stdout"); }

    fmt = (fmt ? fmt : strdup ("xml"));


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
    if ((tab = vot_getTABLE (res)) <= 0)
	goto clean_up_;
    if ((data  = vot_getDATA (tab)))
        tdata = vot_getTABLEDATA (data);
    else
	goto clean_up_;


    /*  Find the requested sort column.  If the column isn't set explicitly
     *  check each field for the name/id/ucd.
     */
    if (col < 0) {
	char  *name, *id, *ucd;
	handle_t  field;

	for (field=vot_getFIELD(tab); field; field=vot_getNext(field),i++) {
            id    = vot_getAttr (field, "id");
            name  = vot_getAttr (field, "name");
            ucd   = vot_getAttr (field, "ucd");

	    /*  See whether this is a column we can sort numerically.
	     */
	    if (! do_string)
                scalar = vot_isNumericField (field);

	    if ((byName && name && strcasecmp (name, byName) == 0) ||
	        (byID && id && strcasecmp (id, byID) == 0) ||
	        (byUCD && ucd && strcasecmp (ucd, byUCD) == 0)) {
		    col = i, do_string = (do_string ? 1 : ! scalar);
		    break;
	    }
	}

    } else {
	register int i = 0;

	for (field = vot_getFIELD(tab); field && i < col; i++)
	    field = vot_getNext(field);
	if (! do_string)
            scalar = vot_isNumericField (field);
	do_string = (do_string ? 1 : ! scalar);
    }


    /*  Sort the table.
     */
    (void) vot_sortTable (tdata, (col < 0 ? 0 : col), do_string, sort_order);


    /*  Now trim the data rows if we've set a TOP condition.
    */
    if (top) {
	int row = 0, ntr = 0;

	/*  Skip over the rows we'll keep
	 */
        for (tr=vot_getTR (tdata); tr && row < top; tr=vot_getNext(tr)) 
	    row++;

	/*  Free the remaining rows.
	 */
	for ( ; tr; tr = ntr) {
	    ntr=vot_getNext(tr);
	    vot_deleteNode (tr);
	}
    }


    /*  Output the new format.
     */
    memset (format, 0, SZ_FORMAT);
    switch (strdic (fmt, format, SZ_FORMAT, FORMATS)) {
    case   VOT:   vot_writeVOTable (vot, oname, indent);     break;
    case   ASV:   vot_writeASV (vot, oname, hdr);            break;
    case   BSV:   vot_writeBSV (vot, oname, hdr);            break;
    case   CSV:   vot_writeCSV (vot, oname, hdr);            break;
    case   TSV:   vot_writeTSV (vot, oname, hdr);            break;
    case  HTML:   vot_writeHTML (vot, iname, oname);         break;
    case SHTML:   vot_writeSHTML (vot, iname, oname);        break;
    case  FITS:   vot_writeFITS (vot, oname);                break;
    case ASCII:   vot_writeASV (vot, oname, hdr);            break;
    case   XML:   vot_writeVOTable (vot, oname, indent);     break;
    case   RAW:   vot_writeVOTable (vot, oname, indent);     break;
    default:
        fprintf (stderr, "Unknown output format '%s'\n", fmt);
        status = ERR;
    }


    /*  Clean up.  Rememebr to free whatever pointers were created when
     *  parsing arguments.
     */
clean_up_:
    if (iname)  free (iname);
    if (oname)  free (oname);
    if (fmt)    free (fmt);
    if (byID)   free (byID);
    if (byUCD)  free (byUCD);
    if (byName) free (byName);

    vo_paramFree (argc, pargv);
    vot_closeVOTABLE (vot);

    return (status);	/* status must be OK or ERR (i.e. 0 or 1)     	*/
}


/**
 *  USAGE -- Print task help summary.
 */
static void
Usage (void)
{
    fprintf (stderr, "\n  Usage:\n\t"
        "votsort [<opts>] votable.xml\n\n"
	"  Where\n"
	"	-c,--col <N>		Sort column num\n"
	"	-d,--desc		Sort in descending order\n"
	"	-f,--fmt <format>	Output format\n"
	"	-o,--output <name>	Output name\n"
	"	-s,--string		String sort\n"
	"	-t,--top <N>		Print top <N> rows\n"
	"	-i,--indent <N>		XML indent level\n"
	"	-n,--noheader		Suppress header\n"
	"	-N,--name <name>	Find <name> column\n"
	"	-I,--id <id>		Find <id> column\n"
	"	-U,--ucd <ucd>		Find <ucd> column\n"
	"\n"
	"	-h,--help		This message\n"
	"	-r,--return		Return result\n"
	"	-%%,--test 		Run unit tests\n"
	"\n"
	"  <format> is one of\n"
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
	"\n"
 	"  Examples:\n\n"
	"    1)  Sort a VOTable based on first column\n\n"
	"	     %% votsort test.xml\n"
	"	     %% votsort http://generic.edu/test.xml\n"
	"	     %% cat test.xml | votsort -o sort_test.xml\n"
	"\n"
	"	 A string sort will be done automatically if this is a\n"
	"	 string-valued column, otherwise a numeric sort is done.\n"
	"\n"
	"    2)  Sort a VOTable based on the magnitude column\n\n"
	"	     %% votsort --name=id test.xml\n"
	"\n"
	"    3)  Same as above, select 10 faintest stars\n\n"
	"	     %% votsort --name=id --desc --top=10 test.xml\n"
	"\n"
	"    4)  String sort based on object name, output as CSV\n\n"
	"	     %% votsort -s -f csv test.xml\n"
	"	     %% votsort --string --fmt=csv test.xml\n"
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

   vo_taskTest (task, input, NULL);					// Ex 1
   vo_taskTest (task, "http://iraf.noao.edu/votest/sort.xml", NULL); 	// Ex 2
   vo_taskTest (task, "--name=id", input, NULL); 			// Ex 3
   vo_taskTest (task, "--name=id", "--desc", "--top=10", input, NULL); 	// Ex 4
   vo_taskTest (task, "-s", "-f", "csv", input, NULL); 			// Ex 5
   vo_taskTest (task, "--string", "--fmt=csv", input, NULL); 		// Ex 6

   vo_taskTest (task, "--name=id", "-s", "--desc", "--fmt=csv", input, NULL);

   vo_taskTestReport (self);
}

