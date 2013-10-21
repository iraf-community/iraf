/*
 *  VOTINFO  
 *
 *  Print information about the structure of a VOTable.
 *
 *    Usage:
 *		votinfo [-v] <votable>
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "votParse.h"


int  vot	= 0;			/* VOTable handle		*/

int  verbose	= 0;			/* options			*/
int  warn	= 0;			/* warning options		*/

static void ppMultiLine (char *result, int poffset, int pwidth, int maxchars);

int
main (int argc, char **argv)
{
    char  *fname, *name, *ucd, *desc, *value;
    int   res, tab, data, tdata, field, handle;
    int   i, len, ncols, nrows;


    if (argc < 2) {
	fprintf (stderr, "Usage:  votinfo <votable>\n");
	return (1);

    } else if (argc >= 2) {
	for (i=1; i < argc; i++) {
	    if (argv[i][0] == '-') {
		switch (argv[i][1]) {
		case 'v':  verbose++;	break;
		case 'w':  warn++;	break;
		default:
		    fprintf (stderr, "Invalid option '%s'\n", argv[i]);
		    return (1);
		}
	    } else {
		fname = argv[i];
		break;
	    }
	}
    }


    /* Open the table.  This also parses it.
    */
    vot_setWarnings (warn);
    if ( (vot = vot_openVOTABLE (fname) ) <= 0) {
	fprintf (stderr, "Error opening VOTable '%s'\n", argv[1]);
	return (1);
    }


    res   = vot_getRESOURCE (vot);	/* get handles		*/
    tab   = vot_getTABLE (res);
    data  = vot_getDATA (tab);
    tdata = vot_getTABLEDATA (data);
    nrows = vot_getNRows (tdata);
    ncols = vot_getNCols (tdata);


    /* Print a table summary.
    */
    printf ("%s\n\n", (fname[0] == '-' ? "Stdin" : fname));

    printf ("    Resources:  %d\tType: %-12s\t    Table Size:  %d x %d\n", 
	vot_getLength (res), vot_getDATATypeString (data), ncols, nrows);

    handle = vot_getINFO (res);
    printf ("         INFO:  %d\n", (handle ? vot_getLength (handle) : 0));

    handle = vot_getPARAM (res);
    printf ("        PARAM:  %d\t", (len=(handle ? vot_getLength(handle):0)));
    if (verbose) {
	for (len-- ; handle; handle=vot_getNext(handle),len--) {
	    name  = vot_getAttr (handle, "id");
	    value = vot_getAttr (handle, "value");
    	    printf ("%s = %s  ", name, value);
	    if (len)
    	        printf ("\n\t\t\t");
	}
    }
    printf ("\n");


    /*
    handle = vot_getCOOSYS (res);
    printf ("       COOSYS:  %d\n", vot_getLength (handle));

    handle = vot_getDESCRIPTION (res);
    desc = vot_getValue (handle);
    printf ("  Description:  ");
        ppMultiLine (desc, 16, 63, 4096);
    printf ("\n\n");
    */



    /*  Print the column info in verbose mode.
    */
    if (verbose) {
        printf ("\n\t\t\tName\t\t\tUCD\n\n");
	i = 0;
        for (field=vot_getFIELD(tab); field; field=vot_getNext (field)) {
	    name = vot_getAttr (field, "name");
	    ucd  = vot_getAttr (field, "ucd");

	    printf ("      Field %2d:  %-20s\t%-30s\n",
	        ++i, (name ? name : ""), (ucd ? ucd : ""));

	    /* If very-verbose, print a FIELD description.
	    */
	    if (verbose > 1) {
    	        if ( (desc = vot_getValue (vot_getDESCRIPTION (field))) )
	    	    printf ("\t  Desc:  %-s\n\n", desc);
	    }
        }
    }
		
	    
    vot_closeVOTABLE (vot);		/* close the table  	*/

    return (0);
}


/**
 *  ppMultiLine 
 *
 *  Print a lengthy string on multiple lines.  Used to print the last column 
 *  of a table where we indent the carried-over lines to the specified offset.
 *  No effort is made to break lines at a 'nice' spot since long URLs and 
 *  such won't fit anyway, so we just cut the line and continue.
 */

static void
ppMultiLine (char *result, int poffset, int pwidth, int maxchars)
{
    register int i, j, ellipses = 0;
    int len = strlen((result ? result : ""));
    char *ip;


    if (result)
        len = strlen (result);
    else
        return;

    ip = &result[len-1];                        /* strip trailing w/s   */
    while ((isspace(*ip) || *ip == '\n') && ip > result)
        *ip-- = '\0';

    if (len > maxchars) {
        result[maxchars] = '\0';
        len = maxchars;
        ellipses++;
    }

    if (len < pwidth) {
        for (ip=result; *ip && isspace(*ip); )
            ip++;
        printf ("%s", ip);
    } else {
        j = pwidth;
        for (i=0; i < len; ) {
            while (isspace (result[i])) i++;
            printf ("%-*.*s\n", pwidth, pwidth, &result[i]);
            i = j + 1;
            j += pwidth;
            printf ("%*s", poffset, " ");
            if (j > len) {
                while (isspace (result[i])) i++;
                printf ("%s", &result[i]);
                if (ellipses)
                    printf (" (read more)....");
                break;
            }
        }
    }
}

