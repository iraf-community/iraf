/*
 *  VOTPOS  
 *
 *  Extract the main RA/Dec columns from a VOTable.
 *
 *    Usage:
 *		votpos -o <outname> <votable>
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "votParse.h"


int  vot	= 0;			/* VOTable handle		*/
char *oname	= NULL;			/* Output file name		*/

int  verbose	= 0;			/* options			*/
int  warn	= 0;			/* warning options		*/

int
main (int argc, char **argv)
{
    char  *fname, *ucd;
    int   res, tab, data, tdata, field;
    int   i, ncols, nrows;
    int   ra_col=-1, dec_col=-1, number=1;
    FILE *fd = stdout;


    if (argc < 2) {
	fprintf (stderr, "Usage:  votinfo <votable>\n");
	return (1);

    } else if (argc >= 2) {
	for (i=1; i < argc; i++) {
	    if (argv[i][0] == '-') {
		switch (argv[i][1]) {
		case 'o':  oname = argv[++i];		break;
		case 'v':  verbose++;			break;
		case 'w':  warn++;			break;
		case 'n':  number--;			break;
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

    if (oname) {
	if ((fd = fopen (oname, "w+")) == (FILE *) NULL)
	    return (1);
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


    /*  Find the columns.
    */
    for (i=0, field=vot_getFIELD(tab); field; field=vot_getNext (field),i++) {
	if ((ucd  = vot_getAttr (field, "ucd"))) {
	  if ((strcmp (ucd, "POS_EQ_RA_MAIN") == 0)  ||		/* UCD 1     */
	      (strcmp (ucd, "pos.eq.ra;meta.main") == 0))	/* UCD 1+    */
		ra_col = i;
	  if ((strcmp (ucd, "POS_EQ_DEC_MAIN") == 0) ||		/* UCD 1     */
	      (strcmp (ucd, "pos.eq.dec;meta.main") == 0))	/* UCD 1+    */
		dec_col = i;
	}
    }
		
    /*  Print the position cells.
     */
    for (i=0; i < nrows; i++) {
	if (number)
	    fprintf (fd, "%d %s %s\n", i,
	        vot_getTableCell (tdata, i, ra_col),
	        vot_getTableCell (tdata, i, dec_col));
	else
	    fprintf (fd, "%s %s\n", 
	        vot_getTableCell (tdata, i, ra_col),
	        vot_getTableCell (tdata, i, dec_col));
    }

    if (fd != stdout)
	fclose (fd);
	    
    vot_closeVOTABLE (vot);		/* close the table  	*/
    return (0);
}
