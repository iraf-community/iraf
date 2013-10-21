/*
 *  VOTINFO -- Print information about the structure of a VOTable.
 *
 *    Usage:
 *		votinfo [-v] [-w] <votable>
 *
 *  @file       votinfo.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/11
 *
 *  @brief      Print information about the structure of a VOTable.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "voApps.h"
#include "votParse.h"

#define N_ITEMS	    	"|param|info|rows|cols|resources|"

#define	N_PARAMS	0		/* numberOf values		*/
#define	N_INFO		1
#define	N_ROWS		2
#define	N_COLS		3
#define	N_RESOURCES	4

#define	SZ_RESBUF	8192


static int  vot		= 0;		/* VOTable handle		*/
static int  size	= 0;		/* print size only		*/
static int  warn	= 0;		/* warning options		*/
static int  numberOf	= 0;		/* element values		*/
static int  verbose	= 0;		/* verbose output		*/
static int  getCols	= 0;		/* get column names		*/
static int  getDesc	= 0;		/* get <DESCRIPTION>		*/
static int  getInfo	= 0;		/* get <INFO>			*/
static int  getParam	= 0;		/* get <PARAM>			*/
static int  getQuery	= 0;		/* get <INFO> QUERY_STATUS val  */
static int  do_return   = 0;		/* return result?		*/

static char resbuf[SZ_RESBUF];		/* result buffer		*/


/*  Task specific option declarations.
 */
int  votinfo (int argc, char **argv, size_t *len, void **result);

static Task  self       = {  "votinfo",  votinfo,  0,  0,  0  };

static char  *opts = "%:bc::dhin:pqrsvw";
static struct option long_opts[] = {
        { "test",         1, 0,   '%'},
        { "help",         2, 0,   'h'},
        { "brief",        2, 0,   'b'},
        { "columns",      2, 0,   'c'},
        { "description",  2, 0,   'd'},
        { "info",         2, 0,   'i'},
        { "numberOf",     1, 0,   'n'},
        { "param",        2, 0,   'p'},
        { "query_status", 2, 0,   'q'},
        { "return",       2, 0,   'r'},
        { "size",         2, 0,   's'},
        { "verbose",      2, 0,   'v'},
        { "warn",         2, 0,   'w'},
        { NULL,           0, 0,    0 }
};


static void Usage (void);
static void Tests (char *input);

extern void ppMultiLine (char *result, int poffset, int pwidth, int maxchars);
extern int strdic (char *in_str, char *out_str, int maxchars, char *dict);



/**
 *  Application entry point.
 */
int
votinfo (int argc, char **argv, size_t *reslen, void **result)
{
    char  *iname, *id, *name, *ucd, *desc, *value, *numpar = NULL, *clist=NULL;
    char **pargv, optval[SZ_FNAME], param[SZ_FNAME];
    int   res, tab, data, tdata, field, handle, status = OK;
    int   i, nlen, ncols, nrows, pos, ch;


    /*  Initialize. 
     */
    size     = 0;
    warn     = 0;
    verbose  = 0;
    iname    = NULL;
    memset (param, 0, SZ_FNAME);
    memset (optval, 0, SZ_FNAME);
    memset (resbuf, 0, SZ_RESBUF);

    *reslen = 0;
    *result = NULL;

    
    /*  Parse the argument list.
     */
    pargv = vo_paramInit (argc, argv, opts, long_opts);
    while ((ch = vo_paramNext (opts,long_opts,argc,pargv,optval,&pos)) != 0)  {
        if (ch > 0) {
	    switch (ch) {
	    case '%':  Tests (optval);			return (self.nfail);
	    case 'h':  Usage ();			return (OK);

	    case 'b':  verbose=0;		    	     break;
	    case 'c':  
		if (optval[0]) {
		    if (optval[0] == 'i') {	    	/* ID */
			clist = "id";
		    } else if (optval[0] == 'n') {	/* NAME */
			clist = "name";
		    } else if (optval[0] == 'u') {	/* UCD */
			clist = "ucd";
		    }
		}
		getCols=1;	    	    	     break;
	    case 'd':  getDesc=1;	    	    	     break;
	    case 'i':  getInfo=1;	    	    	     break;
	    case 'n':  numberOf++, numpar = strdup (optval); break;
	    case 'p':  getParam=1;	    	    	     break;
	    case 'q':  getQuery=1;	    	    	     break;
	    case 'r':  do_return=1;	    	    	     break;
	    case 's':  size++;      			     break;
	    case 'v':  verbose++;		    	     break;
	    case 'w':  warn++;				     break;
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
    if (iname == NULL) 
	iname = strdup ("stdin");
    vot_setWarnings (warn);

    /* Open the table.  This also parses it.
    */
    if ( (vot = vot_openVOTABLE (iname) ) <= 0) {
	fprintf (stderr, "Error opening VOTable '%s'\n", iname);
	return (1);
    }

    res   = vot_getRESOURCE (vot);	/* get handles		*/
    tab   = vot_getTABLE (res);
    if ((data  = vot_getDATA (tab))) {
        tdata = vot_getTABLEDATA (data);
        nrows = vot_getNRows (tdata);
        ncols = vot_getNCols (tdata);
    } else
        tdata = nrows = ncols = 0;

    if (numberOf) {
	switch (strdic (numpar, param, SZ_FNAME, N_ITEMS)) {
	case N_PARAMS:
    	    if ((handle = vot_getPARAM (res)) > 0) {
		if (do_return)
	            vo_setResultFromInt (vot_getLength(handle), reslen, result);
		else
		    printf ("%d\n", vot_getLength(handle));
	    } else
		printf ("0\n");
	    break;
	case N_INFO:
    	    if ((handle = vot_getINFO (res)) > 0) {
		if (do_return)
	            vo_setResultFromInt (vot_getLength(handle), reslen, result);
		else
		    printf ("%d\n", vot_getLength(handle));
	    } else
                printf ("0\n");
	    break;
	case N_ROWS:
	    if (do_return)
	        vo_setResultFromInt (nrows, reslen, result);
	    else
		printf ("%d\n", nrows);
	    break;
	case N_COLS:
	    if (do_return)
	        vo_setResultFromInt (ncols, reslen, result);
	    else
		printf ("%d\n", ncols);
	    break;
	case N_RESOURCES:
	    if (do_return)
	        vo_setResultFromInt (vot_getLength (res), reslen, result);
	    else
		printf ("%d\n", vot_getLength (res));
	    break;
	default:
	    fprintf (stderr, "Unknown number request '%s'\n", param);
	    status = ERR;
	}


    } else if (size) {
	if (do_return) {
	    sprintf (resbuf, "%d %d", nrows, ncols);
	    vo_setResultFromString (resbuf, reslen, result);
	} else
	    printf ("%d %d\n", nrows, ncols);


    } else if (getCols) {

	printf ("%3s  %20.20s\t%20.20s\t%20.20s\n", "Col",
	    "ID", "Name", "UCD");
	printf ("%3s  %20.20s\t%20.20s\t%20.20s\n", "===", "==", "====", "===");

        for (i=1, field = vot_getFIELD(tab); field; field=vot_getNext (field)) {
	    id   = vot_getAttr (field, "id");
	    name = vot_getAttr (field, "name");
	    ucd  = vot_getAttr (field, "ucd");

	    printf ("%3d  %20.20s\t%20.20s\t%20.20s\n", i,
		(id ? id : "(none)"), (name ? name : "(none)"),
		(ucd ? ucd : "(none)"));
	    if (verbose) {
    	        if ( (desc = vot_getValue (vot_getDESCRIPTION (field))) )
		        printf ("\t  Desc:  %-s\n", desc);
	    }
	    i++;
        }


    } else if (getDesc) {
	char *desc = NULL;

        if ((handle = vot_getDESCRIPTION (vot)))
	    desc = vot_getValue (handle);
	else if ((handle = vot_getDESCRIPTION (res)))
	    desc = vot_getValue (handle);
	else 
	    desc = "none";
	if (do_return) 
	    vo_setResultFromString ((desc ? desc : "none"), reslen, result);
	else
	    printf ("%s\n", (desc ? desc : "none"));


    } else if (getParam || getInfo) {
        handle = (getParam ? vot_getPARAM (res) : vot_getINFO (res));
        if (handle > 0) {
	    char  buf[SZ_FNAME];

            nlen = vot_getLength (handle);
	    for (nlen-- ; handle; handle = vot_getNext (handle), nlen--) {
	        name  = vot_getAttr (handle, "name");
	        value = vot_getAttr (handle, "value");
		memset (buf, 0, SZ_FNAME);
    	        sprintf (buf, "%s = %s\n", name, value);
		strcat (resbuf, buf);
	    }

	    if (do_return)
	        vo_setResultFromString (resbuf, reslen, result);
	    else
	        printf ("%s", resbuf);
	}


    } else if (getQuery) {
	int  found = 0, resval = 0;

        if ((handle = vot_getINFO (res)) > 0) {
            nlen = vot_getLength (handle);
	    for (nlen-- ; handle; handle = vot_getNext (handle), nlen--) {
	        name = vot_getAttr (handle, "name");
		if (name && strncasecmp (name, "QUERY_STATUS", 12) == 0) {
	            value = vot_getAttr (handle, "value");
		    found++;
		    break;
		}
	    }
	}

	resval = (found ? (strncasecmp (value, "OK", 2) != 0) : -1);
	if (do_return)
	    vo_setResultFromInt (resval, reslen, result);
	else
	    printf ("%d\n", resval);


    } else {
        /* Print a table summary.
        */
        printf ("%s\n\n", (iname[0] == '-' ? "stdin" : iname));
        printf ("    Resources:  %d\tType: %-12s\t"
	    "    Table Size:  %d x %d\n", 
	    vot_getLength (res), vot_getDATATypeString (data), ncols, nrows);

        if ((handle = vot_getINFO (res)) > 0)
            printf ("         INFO:  %d\n", vot_getLength (handle));

        if ((handle = vot_getPARAM (res)) > 0)
            printf ("        PARAM:  %d\t", 
		(nlen=vot_getLength (handle)));
	for (nlen-- ; handle; handle=vot_getNext(handle),nlen--) {
	    if ((name  = vot_getAttr (handle, "id")) == NULL)
	        if ((name  = vot_getAttr (handle, "name")) == NULL)
		    name = "<noname>";
	    value = vot_getAttr (handle, "value");
    	    printf ("%s = %s  ", name, value);
	    if (nlen)
    	        printf ("\n\t\t\t");
        }
        printf ("\n");

#ifdef OLD_VOTABLE
        if ((handle = vot_getCOOSYS (res)))
            printf ("       COOSYS:  %d\n", vot_getLength (handle));
#endif

        if ((handle = vot_getDESCRIPTION (res))) {
            desc = vot_getValue (handle);
            printf ("  Description:  ");
            ppMultiLine (desc, 16, 60, 4096);
            printf ("\n");
        }


        /*  Print the column info in verbose mode.
        */
        if (verbose) {
            printf ("\n\t\t\tName\t\t\tUCD\n");
	    i = 0;
            for (field=vot_getFIELD(tab); field; field=vot_getNext (field)) {
	        name = vot_getAttr (field, "name");
	        ucd  = vot_getAttr (field, "ucd");

	        printf ("      Field %2d:  %-20s\t%-30s\n",
	            ++i, (name ? name : ""), (ucd ? ucd : ""));
	        if (verbose > 1) {
    	            if ( (desc = vot_getValue (vot_getDESCRIPTION (field))) )
	    	        printf ("\t  Desc:  %-s\n", desc);
	        }
            }
        }
    }
	    
    if (iname) free (iname);

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
        "votinfo [<opts>] votable.xml\n\n"
        "  where\n"
        "       -b,--brief		brief output\n"
        "       -d,--description	print <DESCRIPTION> elements\n"
        "       -h,--help		this message\n"
        "       -i,--info		print <INFO> elements\n"
        "       -n,--numberOf=<what>	get number of specified elements\n"
        "       -p,--param		print <PARAM> elements\n"
        "       -q,--query_status	print QUERY_STATUS\n"
        "       -r,--return		return result from method\n"
        "       -s,--size		print table size\n"
        "       -v,--verbose		verbose otuput\n"
        "       -w,--warn		print parser warnings\n"
	"\n"
	"  <what> is one of\n"
	"	    param		<PARAM> elements\n"
	"	    info		<INFO> elements\n"
	"	    rows		table rows\n"
	"	    cols		table cols\n"
	"	    resources		<RESOURCE> elements\n"
	"\n"
 	"  Examples:\n\n"
	"    1)  Print summary information about a VOTable\n\n"
	"	    %% votinfo -v test.xml\n"
	"\n"
	"    2)  Print the <PARAM> elements in a table, then get a count\n\n"
	"	    %% votinfo -p test.xml\n"
	"	    %% votinfo --numberOf=param test.xml\n"
	"\n"
	"    3)  Determine whether a VOTable contains a successful result\n\n"
	"	    %% votinfo -q test.xml\n\n"
	"        A zero indicates 'OK', a one is an 'ERR', and -1 means that\n"
	"        an <INFO> with a 'QUERY_STATUS' was not found\n"
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

    if (access (input, F_OK) != 0) {
	fprintf (stderr, "Warning:  cannot open file '%s'\n", input);
	return;
    }

    vo_taskTest (task, "-v", input, NULL);			// Ex 1
    vo_taskTest (task, "-p", input, NULL);			// Ex 2
    vo_taskTest (task, "--numberOf=param", input, NULL);	// Ex 3
    vo_taskTest (task, "--numberOf=info", input, NULL);
    vo_taskTest (task, "--numberOf=rows", input, NULL);
    vo_taskTest (task, "--numberOf=cols", input, NULL);
    vo_taskTest (task, "--numberOf=resource", input, NULL);

    vo_taskTest (task, "-n", "param", input, NULL);
    vo_taskTest (task, "-n", "info", input, NULL);
    vo_taskTest (task, "-n", "rows", input, NULL);
    vo_taskTest (task, "-n", "cols", input, NULL);
    vo_taskTest (task, "-n", "resource", input, NULL);

    vo_taskTest (task, "-q", input, NULL);			// Ex 4

    vo_taskTest (task, "-b", input, NULL);
    vo_taskTest (task, "-i", input, NULL);
    vo_taskTest (task, "-d", input, NULL);
    vo_taskTest (task, "-s", input, NULL);

    vo_taskTestReport (self);
}
