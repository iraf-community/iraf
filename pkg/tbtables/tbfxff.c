# include <stdlib.h>
# include "cfitsio/fitsio.h"	/* CFITSIO include file */
# include "fitsio_spp.h"	/* sizes of SPP strings and Fortran FITSIO */
# include "underscore.h"	/* appends underscore, if needed */

/* These are buffers for character string values.  The sizes are defined
   in fitsio.h.
*/
static char c_filename[FLEN_FILENAME+1];
static char c_keyword[FLEN_KEYWORD+1];
static char c_card[FLEN_CARD+1];
static char c_value[FLEN_VALUE+1];
static char c_comment[FLEN_COMMENT+1];
static char c_message[FLEN_ERRMSG+1];

static void strpak (short *, char *, int);
static void strupk (char *, short *, int);

/* This file tbfxff.c contains the interface between the SPP FITSIO calls
   and the CFITSIO functions.

   Most subroutines begin with fs, but two of them (ftcmsg and ftdrec)
   begin with ft.

   These function names, in upper case and ending in "_U", will be
   converted to similar lower case names by underscore.h.  The
   resulting names will either end in "_" or not, depending on whether
   NO_UNDERSCORE has been defined (see tables$lib/mkpkg.inc).

   Phil Hodge, 22-Mar-1999  File created.
   Phil Hodge,  8-Apr-1999  Change FLEN_KEYWORD to FLEN_VALUE in fsukys.
   Phil Hodge,  7-Sep-1999  Add fsukyj.
   Phil Hodge, 25-May-2000  Add fsgrsz (fits_get_rowsize).
   Phil Hodge, 23-Jun-2000  Add fsukyd.
   Phil Hodge, 12-Sep-2000  Add fsgtbb and fsptbb.
*/

void FTDREC_U (fitsfile **fptr, int *keypos, int *status) {

	ffdrec (*fptr, *keypos, status);
}

void 
FTCMSG_U (void) {

	ffcmsg();
}

void FSGIOU_U (fitsfile **fptr, int *status) {
	;
}

void FSFIOU_U (fitsfile **fptr, int *status) {
	;
}

void FSCLOS_U (fitsfile **fptr, int *status) {

	ffclos (*fptr, status);
}

void FSCOPY_U (fitsfile **infptr, fitsfile **outfptr, int *morekeys,
		int *status) {

	ffcopy (*infptr, *outfptr, *morekeys, status);
}

void FSCRHD_U (fitsfile **fptr, int *status) {

	ffcrhd (*fptr, status);
}

void FSDHDU_U (fitsfile **fptr, int *hdutyp, int *status) {

	ffdhdu (*fptr, hdutyp, status);
}

void FSDROW_U (fitsfile **fptr, int *frow, int *nrows, int *status) {

	ffdrow (*fptr, (long)*frow, (long)*nrows, status);
}

/* read bytes */
void FSGTBB_U (fitsfile **fptr, int *frow, int *felem, int *nbytes,
		short array[], int *status) {

	ffgtbb (*fptr, (long)*frow, (long)*felem, (long)*nbytes,
		(unsigned char *)array, status);
}

/* write bytes */
void FSPTBB_U (fitsfile **fptr, int *frow, int *felem, int *nbytes,
		short array[], int *status) {

	ffptbb (*fptr, (long)*frow, (long)*felem, (long)*nbytes,
		(unsigned char *)array, status);
}

/* NOTE:  This is deprecated; use fsgcfl instead.  See next function.  ### */

void FSGCL_U (fitsfile **fptr, int *colnum,
		int *frow, int *felem, int *nelem,
		int lray[], int *status) {

	char nulval = 0;
	int anynul;
	int i;
	char *larray;	/* really an array of logical values, not a string */

	larray = calloc (*nelem, sizeof(char));

	ffgcvl (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		nulval, larray, &anynul, status);

	for (i = 0;  i < *nelem;  i++)
	    lray[i] = larray[i];

	free (larray);
}

void FSGCFL_U (fitsfile **fptr, int *colnum,
		int *frow, int *felem, int *nelem,
		int lray[], int flgval[], int *anynul, int *status) {

	int i;
	/* These two are really arrays of logical values, not strings. */
	char *larray;
	char *nularray;

	larray = calloc (*nelem, sizeof(char));
	nularray = calloc (*nelem, sizeof(char));

	ffgcfl (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		larray, nularray, anynul, status);

	for (i = 0;  i < *nelem;  i++) {
	    lray[i] = larray[i];
	    flgval[i] = nularray[i];
	}

	free (larray);
	free (nularray);
}

void FSGCVD_U (fitsfile **fptr, int *colnum,
		int *frow, int *felem, int *nelem,
		double *nulval, double array[], int *anynul, int *status) {

	ffgcvd (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		*nulval, array, anynul, status);
}

void FSGCVE_U (fitsfile **fptr, int *colnum,
		int *frow, int *felem, int *nelem,
		float *nulval, float array[], int *anynul, int *status) {

	ffgcve (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		*nulval, array, anynul, status);
}

void FSGCVI_U (fitsfile **fptr, int *colnum,
		int *frow, int *felem, int *nelem,
		short *nulval, short array[], int *anynul, int *status) {

	ffgcvi (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		*nulval, array, anynul, status);
}

void FSGCVJ_U (fitsfile **fptr, int *colnum,
		int *frow, int *felem, int *nelem,
		int *nulval, int array[], int *anynul, int *status) {

	long *larray;
	int i;

	larray = calloc (*nelem, sizeof(long));

	ffgcvj (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		(long)*nulval, larray, anynul, status);

	for (i = 0;  i < *nelem;  i++)
	    array[i] = larray[i];

	free (larray);
}

void FSGCVS_U (fitsfile **fptr, int *colnum,
		int *frow, int *felem, int *nelem,
		short nulval[], short array[], int *dim1,
		int *anynul, int *status) {

	char **larray;
	char *lnulval;
	int i, j;		/* j is the index for array */

	/* Note that the local variable for nulval has length dim1. */
	lnulval = calloc (*dim1+1, sizeof(char));
	larray = calloc (*nelem, sizeof(char*));

	for (i = 0;  i < *nelem;  i++)
	    larray[i] = calloc (*dim1+1, sizeof(char));

	strpak (nulval, lnulval, *dim1);

	ffgcvs (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		lnulval, larray, anynul, status);

	j = 0;
	for (i = 0;  i < *nelem;  i++) {
	    strupk (larray[i], &array[j], *dim1);
	    free (larray[i]);
	    j += (*dim1 + 1);		/* array is 2-D */
	}

	free (lnulval);
	free (larray);
}

void FSGHSP_U (fitsfile **fptr, int *nexist, int *nmore, int *status) {

	ffghsp (*fptr, nexist, nmore, status);
}

void FSGKEY_U (fitsfile **fptr, short sppkey[],
		short sppvalue[], short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);

	ffgkey (*fptr, c_keyword, c_value, c_comment, status);

	if (*status == 0) {
	    strupk (c_value, sppvalue, FLEN_VALUE);
	    strupk (c_comment, sppcomm, FLEN_COMMENT);
	}
}

void FSGKYD_U (fitsfile **fptr, short sppkey[], double *value,
                short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);

	ffgkyd (*fptr, c_keyword, value, c_comment, status);

	if (status == 0)
	    strupk (c_comment, sppcomm, FLEN_COMMENT);
}

void FSGKYJ_U (fitsfile **fptr, short sppkey[], int *value,
		short sppcomm[], int *status) {

	long lvalue;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);

	ffgkyj (*fptr, c_keyword, &lvalue, c_comment, status);
	*value = (int)lvalue;

	if (status == 0)
	    strupk (c_comment, sppcomm, FLEN_COMMENT);
}

void FSGKYS_U (fitsfile **fptr, short sppkey[], short sppvalue[],
		short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);

	ffgkys (*fptr, c_keyword, c_value, c_comment, status);

	if (*status == 0) {
	    strupk (c_value, sppvalue, FLEN_VALUE);
	    strupk (c_comment, sppcomm, FLEN_COMMENT);
	}
}

void FSGMSG_U (short sppmsg[]) {

	int i;

	i = ffgmsg (c_message);
	if (i > 0)
	    strupk (c_message, sppmsg, FLEN_ERRMSG);
	else
	    sppmsg[0] = 0;
}

void FSGREC_U (fitsfile **fptr, int *nrec, short spprecord[], int *status) {

	ffgrec (*fptr, *nrec, c_card, status);

	if (*status == 0)
	    strupk (c_card, spprecord, FLEN_CARD);
}

void FSGRSZ_U (fitsfile **fptr, int *maxrows, int *status) {

	long ndata;

	ffgrsz (*fptr, &ndata, status);
	*maxrows = (int)ndata;
}

void FSGTDM_U (fitsfile **fptr, int *colnum, int *maxdim,
		int *naxis, int naxes[], int *status) {

	long *axlen;
	int i;

	axlen = calloc (*maxdim, sizeof(long));

	ffgtdm (*fptr, *colnum, *maxdim, naxis, axlen, status);

	if (*status == 0) {
	    for (i = 0;  i < *naxis;  i++)
		naxes[i] = axlen[i];
	}

	free (axlen);
}

void FSIBIN_U (fitsfile **fptr, int *nrows, int *nfields,
		short sppttype[], short spptform[], short spptunit[],
		short sppextnam[], int *pcount, int *status) {

	char **ttype, **tform, **tunit;
	char *extnam;
	int i;
	int j1 = 0, j2 = 0, j3 = 0;

	ttype = calloc (*nfields, sizeof(char*));
	tform = calloc (*nfields, sizeof(char*));
	tunit = calloc (*nfields, sizeof(char*));

	extnam = calloc (FLEN_VALUE+1, sizeof(char));
	strpak (sppextnam, extnam, FLEN_VALUE);

	for (i = 0;  i < *nfields;  i++) {

	    ttype[i] = calloc (FLEN_VALUE+1, sizeof(char));
	    tform[i] = calloc (FLEN_VALUE+1, sizeof(char));
	    tunit[i] = calloc (FLEN_VALUE+1, sizeof(char));

	    strpak (&sppttype[j1], ttype[i], SZ_FTTYPE);
	    strpak (&spptform[j2], tform[i], SZ_FTFORM);
	    strpak (&spptunit[j3], tunit[i], SZ_FTUNIT);

	    j1 += SZ_FTTYPE+1;
	    j2 += SZ_FTFORM+1;
	    j3 += SZ_FTUNIT+1;
	}

	ffibin (*fptr, (long)*nrows, *nfields,
		ttype, tform, tunit,
		extnam, (long)*pcount, status);

	free (extnam);
	for (i = 0;  i < *nfields;  i++) {
	    free (ttype[i]);
	    free (tform[i]);
	    free (tunit[i]);
	}
	free (ttype);
	free (tform);
	free (tunit);
}

void FSICOL_U (fitsfile **fptr, int *colnum,
		short sppttype[], short spptform[], int *status) {

	char *ttype, *tform;

	ttype = calloc (SZ_FTTYPE+1, sizeof(char*));
	tform = calloc (SZ_FTFORM+1, sizeof(char*));
	strpak (sppttype, ttype, SZ_FTTYPE);
	strpak (spptform, tform, SZ_FTFORM);

	fficol (*fptr, *colnum, ttype, tform, status);

	free (ttype);
	free (tform);
}

void FSINIT_U (fitsfile **fptr, short sppname[],
		int *blocksize, int *status) {

	strpak (sppname, c_filename, FLEN_FILENAME);
	ffinit (fptr, c_filename, status);
}

void FSIROW_U (fitsfile **fptr, int *frow, int *nrows, int *status) {

	ffirow (*fptr, (long)*frow, (long)*nrows, status);
}

void FSMAHD_U (fitsfile **fptr, int *hdunum, int *exttype, int *status) {

	ffmahd (*fptr, *hdunum, exttype, status);
}

void FSMCOM_U (fitsfile **fptr, short sppkey[], short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffmcom (*fptr, c_keyword, c_comment, status);
}

void FSMKYD_U (fitsfile **fptr, short sppkey[], double *dval,
		int *decim, short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffmkyd (*fptr, c_keyword, *dval, *decim, c_comment, status);
}

void FSMKYE_U (fitsfile **fptr, short sppkey[], float *rval,
		int *decim, short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffmkye (*fptr, c_keyword, *rval, *decim, c_comment, status);
}

void FSMKYJ_U (fitsfile **fptr, short sppkey[], int *intval,
		short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffmkyj (*fptr, c_keyword, (long)*intval, c_comment, status);
}

void FSMKYL_U (fitsfile **fptr, short sppkey[], int *logval,
		short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffmkyl (*fptr, c_keyword, *logval, c_comment, status);
}

void FSMKYS_U (fitsfile **fptr, short sppkey[], short sppvalue[],
		short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppvalue, c_value, FLEN_VALUE);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffmkys (*fptr, c_keyword, c_value, c_comment, status);
}

void FSMREC_U (fitsfile **fptr, int *nkey, short sppcard[], int *status) {

	strpak (sppcard, c_card, FLEN_CARD);

	ffmrec (*fptr, *nkey, c_card, status);
}

void FSOPEN_U (fitsfile **fptr, short sppname[], int *iomode,
		int *blocksize, int *status) {

	strpak (sppname, c_filename, FLEN_FILENAME);

	ffopen (fptr, c_filename, *iomode, status);
}

void FSPCLD_U (fitsfile **fptr, int *colnum,
		int *frow, int *felem, int *nelem,
		double array[], int *status) {

	ffpcld (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		array, status);
}

void FSPCLE_U (fitsfile **fptr, int *colnum,
		int *frow, int *felem, int *nelem,
		float array[], int *status) {

	ffpcle (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		array, status);
}

void FSPCLI_U (fitsfile **fptr, int *colnum,
		int *frow, int *felem, int *nelem,
		short array[], int *status) {

	ffpcli (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		array, status);
}

void FSPCLJ_U (fitsfile **fptr, int *colnum,
		int *frow, int *felem, int *nelem,
		int array[], int *status) {

	long *larray;
	int i;

	larray = calloc (*nelem, sizeof(long));

	for (i = 0;  i < *nelem;  i++)
	    larray[i] = array[i];

	ffpclj (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		larray, status);

	free (larray);
}

void FSPCLL_U (fitsfile **fptr, int *colnum,
		int *frow, int *felem, int *nelem,
		int array[], int *status) {

	char *larray;
	int i;

	larray = calloc (*nelem, sizeof(char));

	for (i = 0;  i < *nelem;  i++)
	    larray[i] = array[i];

	ffpcll (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		larray, status);

	free (larray);
}

void FSPCLS_U (fitsfile **fptr, int *colnum,
		int *frow, int *felem, int *nelem,
		short array[], int *dim1, int *status) {

	char **larray;
	int i, j;		/* j is the index for array */

	larray = calloc (*nelem, sizeof(char*));

	j = 0;
	for (i = 0;  i < *nelem;  i++) {
	    larray[i] = calloc (*dim1+1, sizeof(char));
	    strpak (&array[j], larray[i], *dim1);
	    j += (*dim1 + 1);		/* array is 2-D */
	}

	ffpcls (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		larray, status);

	for (i = 0;  i < *nelem;  i++)
	    free (larray[i]);

	free (larray);
}

void FSPCLU_U (fitsfile **fptr, int *colnum,
		int *frow, int *felem, int *nelem,
		int *status) {

	ffpclu (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem, status);
}

void FSPCOM_U (fitsfile **fptr, short sppcomm[], int *status) {

	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffpcom (*fptr, c_comment, status);
}

void FSPHBN_U (fitsfile **fptr, int *nrows, int *nfields,
		short sppttype[], short spptform[], short spptunit[],
		short sppextnam[], int *pcount, int *status) {

	char **ttype, **tform, **tunit;
	char *extnam;
	int i;
	int j1 = 0, j2 = 0, j3 = 0;

	ttype = calloc (*nfields, sizeof(char*));
	tform = calloc (*nfields, sizeof(char*));
	tunit = calloc (*nfields, sizeof(char*));

	extnam = calloc (FLEN_VALUE+1, sizeof(char));
	strpak (sppextnam, extnam, FLEN_VALUE);

	for (i = 0;  i < *nfields;  i++) {

	    ttype[i] = calloc (FLEN_VALUE+1, sizeof(char));
	    tform[i] = calloc (FLEN_VALUE+1, sizeof(char));
	    tunit[i] = calloc (FLEN_VALUE+1, sizeof(char));

	    strpak (&sppttype[j1], ttype[i], SZ_FTTYPE);
	    strpak (&spptform[j2], tform[i], SZ_FTFORM);
	    strpak (&spptunit[j3], tunit[i], SZ_FTUNIT);

	    j1 += SZ_FTTYPE+1;
	    j2 += SZ_FTFORM+1;
	    j3 += SZ_FTUNIT+1;
	}

	ffphbn (*fptr, (long)*nrows, *nfields,
		ttype, tform, tunit,
		extnam, (long)*pcount, status);

	free (extnam);
	for (i = 0;  i < *nfields;  i++) {
	    free (ttype[i]);
	    free (tform[i]);
	    free (tunit[i]);
	}
	free (ttype);
	free (tform);
	free (tunit);
}

void FSPHIS_U (fitsfile **fptr, short sppcomm[], int *status) {

	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffphis (*fptr, c_comment, status);
}

void FSPHPR_U (fitsfile **fptr, int *simple, int *bitpix,
		int *naxis, long naxes[], long *pcount, long *gcount,
		int *extend, int *status) {

	long *axlen;
	int i;

	axlen = calloc (*naxis, sizeof(long));

	for (i = 0;  i < *naxis;  i++)
	    axlen[i] = naxes[i];

	ffphpr (*fptr, *simple, *bitpix, *naxis, axlen,
		(long)*pcount, (long)*gcount, *extend, status);

	free (axlen);
}

void FSPKYD_U (fitsfile **fptr, short sppkey[],
		double *dval, int *decim, short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffpkyd (*fptr, c_keyword, *dval, *decim, c_comment, status);
}

void FSPKYE_U (fitsfile **fptr, short sppkey[],
		float *rval, int *decim, short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffpkye (*fptr, c_keyword, *rval, *decim, c_comment, status);
}

void FSPKYJ_U (fitsfile **fptr, short sppkey[], int *value,
		short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffpkyj (*fptr, c_keyword, (long)*value, c_comment, status);
}

void FSPKYL_U (fitsfile **fptr, short sppkey[],
		int *logval, short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffpkyl (*fptr, c_keyword, *logval, c_comment, status);
}

void FSPKYS_U (fitsfile **fptr, short sppkey[], short sppvalue[],
		short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppvalue, c_value, FLEN_VALUE);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffpkys (*fptr, c_keyword, c_value, c_comment, status);
}

void FSPREC_U (fitsfile **fptr, short sppcard[], int *status) {

	strpak (sppcard, c_card, FLEN_CARD);

	ffprec (*fptr, c_card, status);
}

void FSPSVC_U (short sppcard[],
		short sppvalue[], short sppcomm[], int *status) {

	strpak (sppcard, c_card, FLEN_CARD);

	ffpsvc (c_card, c_value, c_comment, status);

	if (*status == 0) {
	    strupk (c_value, sppvalue, FLEN_VALUE);
	    strupk (c_comment, sppcomm, FLEN_COMMENT);
	}
}

void FSPTDM_U (fitsfile **fptr, int *colnum,
		int *naxis, int naxes[], int *status) {

	long *axlen;
	int i;

	axlen = calloc (*naxis, sizeof(long));

	for (i = 0;  i < *naxis;  i++)
	    axlen[i] = naxes[i];

	ffptdm (*fptr, *colnum, *naxis, axlen, status);

	free (axlen);
}

void FSRDEF_U (fitsfile **fptr, int *status) {

	ffrdef (*fptr, status);
}

void FSUKYD_U (fitsfile **fptr, short sppkey[],
		double *dval, int *decim, short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffukyd (*fptr, c_keyword, *dval, *decim, c_comment, status);
}

void FSUKYJ_U (fitsfile **fptr, short sppkey[], int *value,
		short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffukyj (*fptr, c_keyword, (long)*value, c_comment, status);
}

void FSUKYL_U (fitsfile **fptr, short sppkey[], int *logval,
		short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffukyl (*fptr, c_keyword, *logval, c_comment, status);
}

void FSUKYS_U (fitsfile **fptr, short sppkey[],
		short sppvalue[], short sppcomm[], int *status) {

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppvalue, c_value, FLEN_VALUE);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffukys (*fptr, c_keyword, c_value, c_comment, status);
}

static void strpak (short *in, char *out, int maxch) {

	int i = 0;

	while (in[i] != 0 && i < maxch) {
	    out[i] = in[i];
	    i++;
	}
	out[i] = '\0';
}

static void strupk (char *in, short *out, int maxch) {

	int i = 0;

	while (in[i] != '\0' && i < maxch) {
	    out[i] = in[i];
	    i++;
	}
	out[i] = 0;
}
