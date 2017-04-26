/*
 *  Test program to convert to a FITS file
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
#include "votParseP.h"
#include "fitsio.h"


int   vot	= 0;			/* VOTable handle		*/
int   verbose	= 0;			/* options			*/
int   warn	= 0;			/* options			*/

void zvot_writeFITS (handle_t vot, char *oname);

int vot_addFITSMeta (int handle, fitsfile *fp, char *meta, int index);
int vot_addFieldMeta (int handle, fitsfile *fp, int index);
int vot_writeFITSData (fitsfile *fp, char **data, char *fmt[], 
				int nrows, int ncols);
void printerror (int status);


#define	MAX_FIELDS		256

int
main (int argc, char **argv)
{
    char  *fname, *oname;
    int   i;


    if (argc < 2) {
	fprintf (stderr, "Usage:  votinfo <votable>\n");
	return (1);

    } else if (argc >= 2) {
	for (i=1; i < argc; i++) {
	    if (argv[i][0] == '-') {
		switch (argv[i][1]) {
		case 'o':  oname = argv[++i];	break;
		case 'v':  verbose++;		break;
		case 'w':  warn++;		break;
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


    /* Open the input VOTable, this also parses it.
    */
    vot_setWarnings (warn);
    if ( (vot = vot_openVOTABLE (fname) ) <= 0) {
	fprintf (stderr, "Error opening VOTable '%s'\n", fname);
	return (1);
    }

    if (oname == NULL) {
	fprintf (stderr, "Error: No output name specified.\n");
	return (1);
    } else {
	zvot_writeFITS (vot, oname);
    }


    return (0);
}


void
zvot_writeFITS (handle_t vot, char *oname)
{
    char  *name, *unit, *dtype, *width, **cells, *cell, *asize;
    char  *ttype[MAX_FIELDS], *tform[MAX_FIELDS], *tunit[MAX_FIELDS], *ch;
    int    res, tab, data, tdata, field, handle, hdutype, *widths, *spaces;
    int    i, j, len, ncols, nrows, status = 0, resnum = 1, bitpix = 8;
    long   naxis = 0,  naxes[2] = { 0, 0 };
    fitsfile  *fp;		/* CFITSIO descriptor		*/



    if (fits_create_file (&fp, oname, &status)) /* create new FITS file */
        printerror (status);

    if ( fits_create_img (fp,  bitpix, naxis, naxes, &status) )
        printerror (status);


    /*  Loop over all <RESOURCE> elements in the file, creating an new
     *  extension for each one.
     */
    for (res=vot_getRESOURCE (vot); res; res=vot_getNext(res) ) {

	/*  Get handles for the current resource.
	 */
      	tab   = vot_getTABLE (res);
      	data  = vot_getDATA (tab);
      	tdata = vot_getTABLEDATA (data);
      	nrows = vot_getNRows (tdata);
      	ncols = vot_getNCols (tdata);

	/*  Allocate space for the data cells.  Read in the cells so we can
 	 *  convert it for output.  Also collect the widths so we can
 	 *  properly size the table.
	 */
	cells  = (char **) calloc (1, (nrows * ncols) * sizeof (char *));
	widths = (int *) calloc (1, ncols * sizeof (int));
	spaces = (int *) calloc (1, ncols * sizeof (int));
        for (i = 0; i < nrows; i++) {
            for (j = 0; j < ncols; j++) {
                cell = cells[i*ncols+j] = vot_getTableCell(tdata, i, j);

		if ((len = strlen (cell)) > widths[j])
		    widths[j] = len;

		if (cell[0] && strchr (cell, (int)' ') && len > 1 && i < 1) {
		    for (ch=cell; *ch; ch++) {
			if (*ch == ' ')
		    	    spaces[j]++;
		    }
		}
            }
        }


        memset (&ttype[0], 0, MAX_FIELDS);	/* initialize		*/
	memset (&tform[0], 0, MAX_FIELDS);
	memset (&tunit[0], 0, MAX_FIELDS);

	/*  Move to proper extension HDU.
	 */
        if (fits_movabs_hdu (fp, resnum++, &hdutype, &status)) 
            printerror (status);

        /*  Get the column attributes and set them in the header.
         */
	i = 0;
        for (field=vot_getFIELD(tab); field; field=vot_getNext (field)) {
	    dtype = vot_getAttr (field, "datatype");
	    width = vot_getAttr (field, "width");
	    asize = vot_getAttr (field, "arraysize");

	    if ((name = vot_getAttr (field, "name")))
	    	ttype[i] =  (name ? name : strdup ("X"));
	    if ((unit = vot_getAttr (field, "unit")))
	    	tunit[i] =  (unit ? unit : strdup ("Y"));

	    tform[i] = calloc (1, 16);
	    if (strncasecmp (dtype, "char", 4) == 0) {
		if (asize[0]) {
		    sprintf (tform[i], "%dA", 
			(asize[0] == '*' ? widths[i] : atoi (asize)));
		} else
		    strcpy (tform[i], "A");

	    } else if (strncasecmp (dtype, "float", 4) == 0) {
		if (spaces[i])
		    sprintf (tform[i], "%dE", spaces[i]+1);
		else
		    strcpy (tform[i], "E");

	    } else if (strncasecmp (dtype, "double", 4) == 0) {
		if (spaces[i])
		    sprintf (tform[i], "%dD", spaces[i]+1);
		else
		    strcpy (tform[i], "D");

	    } else if (strncasecmp (dtype, "int", 3) == 0) {
		if (spaces[i])
		    sprintf (tform[i], "%dJ", spaces[i]+1);
		else
		    strcpy (tform[i], "J");
	    }

	    if (dtype)	free ( (void *) dtype);
	    if (width)	free ( (void *) width);
	    if (asize)	free ( (void *) asize);
	    i++;
        }

        /*  Append a new empty binary table onto the FITS file
	 */
        if (fits_create_tbl (fp, BINARY_TBL, nrows, ncols, ttype, tform,
            tunit, "extname", &status))
        	printerror (status);

	/*  Add UCD and UTYPE keywords for the FIELDs if defined.
	 */
        for (i=1,field=vot_getFIELD(tab); field; field=vot_getNext (field))
	    vot_addFieldMeta (field, fp, i++);

	/* Add keywords for all the <INFO> and <PARAM> elements.
	 */
      	handle = vot_getINFO (res);
	for (i=1, len=vot_getLength (handle); i < len; i++) {
	    vot_addFITSMeta (handle, fp, "INFO", i);
	    handle = vot_getNext (handle);
	}

      	handle = vot_getPARAM (res);
	for (i=1, len=vot_getLength (handle); i < len; i++) {
	    vot_addFITSMeta (handle, fp, "PARAM", i);
	    handle = vot_getNext (handle);
	}


	/*  Write the data to the file.
	 */
	vot_writeFITSData (fp, cells, tform, nrows, ncols);

	/*  Free the allocated pointers.
	 */
	for (i=0; i < ncols; i++) {
	    if (ttype[i])  free ((void *) ttype[i]);
	    if (tunit[i])  free ((void *) tunit[i]);
	    if (tform[i])  free ((void *) tform[i]);
	}
	if (cells)	free ((void *) cells);
	if (widths)	free ((void *) widths);
	if (spaces)	free ((void *) spaces);
    }


    vot_closeVOTABLE (vot);			/* close the VOTable  	*/
    if (fits_close_file (fp, &status))       	/* close the FITS file 	*/
         printerror (status);
}


int 
vot_addFITSMeta (int handle, fitsfile *fp, char *meta, int index)
{
    char  *id, *nam, *val, *unit, keyw[SZ_FNAME], comment[SZ_FNAME];
    int   status = 0;


    if ( (id = vot_getAttr (handle, "id")) ) {		/* ID attribute	      */
        memset (keyw, 0, SZ_FNAME);
        memset (comment, 0, SZ_FNAME);
        sprintf (keyw, "%3.3sID%d", meta, index);
        sprintf (comment, "%s id attribute", meta);
        if (fits_update_key (fp, TSTRING, keyw, id, comment, &status))
	    printerror ( status );
    }

    if ( (nam = vot_getAttr (handle, "name")) ) {	/* NAME attribute    */
        memset (keyw, 0, SZ_FNAME);
        memset (comment, 0, SZ_FNAME);
        sprintf (keyw, "%3.3sNAM%d", meta, index);
        sprintf (comment, "%s name attribute", meta);

        if (fits_update_key (fp, TSTRING, keyw, nam, comment, &status))
	    printerror ( status );
    }

    if ( (val = vot_getAttr (handle, "value")) ) {	/* VALUE attribute   */
        memset (keyw, 0, SZ_FNAME);
        memset (comment, 0, SZ_FNAME);
        sprintf (keyw, "%3.3sVAL%d", meta, index);
        sprintf (comment, "%s val attribute", meta);

        if (fits_update_key (fp, TSTRING, keyw, val, comment, &status))
	    printerror ( status );
    }

    if ( (unit = vot_getAttr (handle, "unit")) ) {	/* UNIT attribute   */
        memset (keyw, 0, SZ_FNAME);
        memset (comment, 0, SZ_FNAME);
        sprintf (keyw, "%3.3sUNI%d", meta, index);
        sprintf (comment, "%s unit attribute", meta);

        if (fits_update_key (fp, TSTRING, keyw, unit, comment, &status))
	    printerror ( status );
    }

    return (0);
}


int 
vot_addFieldMeta (int handle, fitsfile *fp, int index)
{
    char  *ucd, *utype, *id, keyw[SZ_FNAME];
    int   status = 0;


    if ( (ucd = vot_getAttr (handle, "ucd")) ) {	/* UCD attribute     */
        memset (keyw, 0, SZ_FNAME);
        sprintf (keyw, "TUCD%d", index);
        if (fits_update_key (fp, TSTRING, keyw, ucd, "UCD attribute", &status))
	    printerror ( status );
    }

    if ( (utype = vot_getAttr (handle, "utype")) ) {	/* UTYPE attribute   */
        memset (keyw, 0, SZ_FNAME);
        sprintf (keyw, "TUTYPE%d", index);
        if (fits_update_key (fp, TSTRING, keyw, utype, "UTYPE attribute",
	    &status))
	        printerror ( status );
    }

    if ( (id = vot_getAttr (handle, "id")) ) {		/* ID attribute     */
        memset (keyw, 0, SZ_FNAME);
        sprintf (keyw, "TID%d", index);
        if (fits_update_key (fp, TSTRING, keyw, id, "ID attribute", &status))
	    printerror ( status );
    }

    return (0);
}

int
vot_writeFITSData (fitsfile *fp, char **data, char *fmt[], int nrows, int ncols)
{
    int     i, j, n, type, width, status = 0;
    char    **ccol, *tform, cell[1024], *tok, *sep = " ", *brkt = NULL;
    float  *fcol;
    double *dcol;
    long   *icol;
    long    frow = 1, felem = 1;
    

    for (j = 0; j < ncols; j++) {

	tform = fmt[j];
	width = atoi (tform);
	type = strlen (tform) - 1;

	switch (tform[type]) {
	case 'A':						/* CHAR	    */
    	    ccol = (char **) calloc (1, (nrows * (width+1) * sizeof (char *)));

    	    for (i = 0; i < nrows; i++)
        	ccol[i] = (char *) data[i * ncols + j];

	    fits_write_col (fp, TSTRING, j+1, frow,felem, nrows, ccol, &status);
	    free ((void *) ccol);
	    break;

	case 'D':						/* DOUBLE   */
    	    dcol = (double *) calloc (1, (nrows * (width+1) * sizeof (double)));

	    if (width == 0) {
    	        for (i = 0; i < nrows; i++)
        	    ((double *) dcol)[i] = (double) atof (data[i * ncols + j]);
	        fits_write_col (fp, TDOUBLE, j+1, frow,felem, nrows, dcol, 
			&status);
	    } else {
		double *dp = dcol, *dpr = dp;

    	        for (i = 0; i < nrows; i++) {
		    brkt = NULL;
		    memset (cell, 0, 1024);
		    strcpy (cell, data[i * ncols + j]);

		    dpr = dp;
		    for (n=1, tok=strtok_r (cell, sep, &brkt); tok;
		         tok=strtok_r (NULL, sep, &brkt), n++)
			    *dp++ = (double) atof (tok);
		    for ( ; n < width; n++)		/* missing values  */
			*dp++ = (double) 0.0;

	            fits_write_col (fp, TDOUBLE, j+1, i+1,felem, width, dpr, 
			&status);
		}
	    }

	    free ((void *) dcol);
	    break;

	case 'E':						/* FLOAT    */
    	    fcol = (float *) calloc (1, (nrows * (width+1) * sizeof (float)));

	    if (width == 0) {
    	        for (i = 0; i < nrows; i++)
        	    ((float *) fcol)[i] = (float) atof (data[i * ncols + j]);
	        fits_write_col (fp, TFLOAT, j+1, frow,felem, nrows, fcol, 
			&status);
	    } else {
		float *rp = fcol, *rpr = rp;

    	        for (i = 0; i < nrows; i++) {
		    brkt = NULL;
		    memset (cell, 0, 1024);
		    strcpy (cell, data[i * ncols + j]);

		    rpr = rp;
		    for (n=1, tok=strtok_r (cell, sep, &brkt); tok;
		         tok=strtok_r (NULL, sep, &brkt), n++)
			    *rp++ = (float) atof (tok);
		    for ( ; n < width; n++)		/* missing values  */
			*rp++ = (float) 0.0;

	            fits_write_col (fp, TFLOAT, j+1, i+1,felem, width, rpr, 
			&status);
		}
	    }

	    free ((void *) fcol);
	    break;

	case 'J':						/* INT      */
    	    icol = (long *) calloc (1, (nrows * (width+1) * sizeof (long)));

	    if (width == 0) {
    	        for (i = 0; i < nrows; i++)
        	    ((long *) icol)[i] = (long) atoi (data[i * ncols + j]);
	        fits_write_col (fp, TLONG, j+1, frow,felem, nrows, icol, 
			&status);
	    } else {
		long *ip = icol, *ipr = ip;

    	        for (i = 0; i < nrows; i++) {
		    brkt = NULL;
		    memset (cell, 0, 1024);
		    strcpy (cell, data[i * ncols + j]);

		    ipr = ip;
		    for (n=1, tok=strtok_r (cell, sep, &brkt); tok;
		         tok=strtok_r (NULL, sep, &brkt), n++)
			    *ip++ = (long) atoi (tok);
		    for ( ; n < width; n++)		/* missing values  */
			*ip++ = (long) 0;

	            fits_write_col (fp, TLONG, j+1, i+1,felem, width, ipr, 
			&status);
		}
	    }

	    free ((void *) icol);
	    break;

	default:
	    fprintf (stderr, "Invalid column type '%c'\n", tform[type]);
	    continue;
	}
    }

    return (0);
}


void printerror (int status) 
{
    if (status) {
       fits_report_error (stderr, status); 	/* print error report 	*/
       exit (status);
    }
    return;
}

