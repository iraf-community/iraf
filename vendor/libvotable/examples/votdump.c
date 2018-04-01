/**
 *  VOTDUMP 
 *
 *  Example program to convert a VOTable to a delimited file, ...the long way.
 *
 *  Usage:
 *	    votdump [-b|-c|-s|-t] <votable>
 *
 *  Where
 *	    -b		use '|' as the delimiter (Bar)
 *	    -c		use ',' as the delimiter (CSV)
 *	    -s		use ' ' as the delimiter (space-delimited)
 *	    -t		use '\t' as the delimiter (TSV)
 *
 *	    <votable>	Name of file to dump, or '-' for stding
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "votParse.h"


int	vot	= 0;			/* VOTable handle		*/

/**
 *  Program entry point.
 */
int
main (int argc, char **argv)
{
    char  *fname, *name, *id, *ucd, *s, delim=',';
    int   res, tab, data, tdata, field, tr, td;		/* handles	*/
    int   i, ncols;


    if (argc < 2) {
	fprintf (stderr, "Usage:  votdump [-b|-c|-s|-t]  <votable>\n");
	return (1);

    } else if (argc >= 2) {
	for (i=1; i < argc; i++) {
	    if (argv[i][0] == '-' && strlen (argv[i]) > 1) {
		switch (argv[i][1]) {
		case 'b':    delim = '|'; 	break;	    /* bar	*/
		case 'c':    delim = ','; 	break;	    /* comma	*/
		case 't':    delim = '\t'; 	break;	    /* tab	*/
		case 's':    delim = ' '; 	break;	    /* space    */
		default:
		    fprintf (stderr, "Invalid delimiter '%c'\n", argv[i][1]);
		    return (1);
		}
	    } else
		fname = argv[i];
	}
    }


    /* Open the table.  This also parses it.
    */
    if ( (vot = vot_openVOTABLE (fname) ) <= 0) {
	fprintf (stderr, "Error opening VOTable '%s'\n", argv[1]);
	return (1);
    }

    res = vot_getRESOURCE (vot);	/* get RESOURCES		*/
    if (vot_getLength (res) > 1) {
	fprintf (stderr, "Error: multiple <RESOURCES> not supported\n");
	return (1);
    }

    tab = vot_getTABLE (res);
    data = vot_getDATA (tab);
    tdata = vot_getTABLEDATA (data);
    ncols = vot_getNCols (tdata);


    /* Print the Column header names.
    */
    printf ("# ");
    for (field=vot_getFIELD (tab),i=0; field; field = vot_getNext (field),i++) {
	name = vot_getAttr (field, "name");	/* find reasonable value */
	id   = vot_getAttr (field, "id");
	ucd  = vot_getAttr (field, "ucd");
	if (name || id || ucd)
	    printf ("%s", (name ? name : (id ? id : ucd)) );
	else
	    printf ("col%d", i);
	if (i < (ncols-1))
	    printf ("%c", delim);
    }
    printf ("\n");
		
	    
    /* Now dump the data.
    */
    for (tr=vot_getTR (tdata); tr; tr=vot_getNext(tr)) {
        for (td=vot_getTD(tr),i=0; td; td=vot_getNext(td),i++) {
	    printf ("%s", ((s = vot_getValue (td)) ? s : "") );
	    if (i < (ncols-1))
	        printf ("%c", delim);
	}
	printf ("\n");
    }


    /* Close the table.
    */
    vot_closeVOTABLE (vot);		

    return (0);
}
