# include <stdlib.h>
# include <fitsio.h>		/* CFITSIO include file */
# include "fitsio_spp.h"	/* sizes of SPP strings and Fortran FITSIO */
# include "underscore.h"	/* appends underscore, if needed */

#define import_spp
#include <iraf.h>

/* These are buffers for character string values.  The sizes are defined
   in fitsio.h.
*/
static char c_filename[FLEN_FILENAME+1];
static char c_keyword[FLEN_KEYWORD+1];
static char c_card[FLEN_CARD+1];
static char c_value[FLEN_VALUE+1];
static char c_comment[FLEN_COMMENT+1];
static char c_message[FLEN_ERRMSG+1];

static void strpak (const XCHAR *, char *, long);
static void strupk (const char *, XCHAR *, long);

/* This file tbfxff.c contains the interface between the SPP FITSIO calls
   and the CFITSIO functions.

   Most subroutines begin with fs, but two of them (ftcmsg and ftdrec)
   begin with ft.

   These function names, in upper case and ending in "_U", will be
   converted to similar lower case names by underscore.h.  The
   resulting names will either end in "_" or not, depending on whether
   NO_UNDERSCORE has been defined (see tables$config/mkpkg.inc).

   Phil Hodge, 22-Mar-1999  File created.
   Phil Hodge,  8-Apr-1999  Change FLEN_KEYWORD to FLEN_VALUE in fsukys.
   Phil Hodge,  7-Sep-1999  Add fsukyj.
   Phil Hodge, 25-May-2000  Add fsgrsz (fits_get_rowsize).
   Phil Hodge, 23-Jun-2000  Add fsukyd.
   Phil Hodge, 12-Sep-2000  Add fsgtbb and fsptbb.
*/

void FTDREC_U (XPOINTER *x_fptr, XINT *keypos, XINT *x_status)
{

	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	ffdrec (*fptr, *keypos, &status);
	*x_status = status;
}

void FTCMSG_U()
{

	ffcmsg();
}

void FSGIOU_U (XPOINTER *x_fptr, XINT *x_status)
{
	;
}

void FSFIOU_U (XPOINTER *x_fptr, XINT *x_status)
{
	;
}

void FSCLOS_U (XPOINTER *x_fptr, XINT *x_status)
{

	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	ffclos (*fptr, &status);
	*x_status = status;
}

void FSCOPY_U (XPOINTER *x_infptr, XPOINTER *x_outfptr, XINT *morekeys,
	       XINT *x_status)
{
	fitsfile **infptr = (fitsfile **)x_infptr;
	fitsfile **outfptr = (fitsfile **)x_outfptr;
	int status = *x_status;
	ffcopy (*infptr, *outfptr, *morekeys, &status);
	*x_status = status;
}

void FSCRHD_U (XPOINTER *x_fptr, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	ffcrhd (*fptr, &status);
	*x_status = status;
}

void FSDHDU_U (XPOINTER *x_fptr, XINT *x_hdutyp, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	int hdutyp = *x_hdutyp;
	ffdhdu (*fptr, &hdutyp, &status);
	*x_hdutyp = hdutyp;
	*x_status = status;
}

void FSDROW_U (XPOINTER *x_fptr, XINT *frow, XINT *nrows, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	ffdrow (*fptr, (long)*frow, (long)*nrows, &status);
	*x_status = status;
}

/* read bytes */
void FSGTBB_U (XPOINTER *x_fptr, XINT *frow, XINT *felem, XINT *nbytes,
	       XCHAR array[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	ffgtbb (*fptr, (long)*frow, (long)*felem, (long)*nbytes,
		(unsigned char *)array, &status);
	*x_status = status;
}

/* write bytes */
void FSPTBB_U (XPOINTER *x_fptr, XINT *frow, XINT *felem, XINT *nbytes,
	       XCHAR array[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	ffptbb (*fptr, (long)*frow, (long)*felem, (long)*nbytes,
		(unsigned char *)array, &status);
	*x_status = status;
}

/* NOTE:  This is deprecated; use fsgcfl instead.  See next function.  ### */

void FSGCL_U (XPOINTER *x_fptr, XINT *colnum,
	      XINT *frow, XINT *felem, XINT *nelem,
	      XINT lray[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	char nulval = 0;
	int anynul;
	long i;
	char *larray;	/* really an array of logical values, not a string */

	larray = calloc (*nelem, sizeof(char));

	ffgcvl (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		nulval, larray, &anynul, &status);

	for (i = 0;  i < *nelem;  i++)
	    lray[i] = larray[i];

	free (larray);
	*x_status = status;
}

void FSGCFL_U (XPOINTER *x_fptr, XINT *colnum,
	       XINT *frow, XINT *felem, XINT *nelem,
	       XINT lray[], XINT flgval[], XINT *x_anynul, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	int anynul = *x_anynul;
	long i;
	/* These two are really arrays of logical values, not strings. */
	char *larray;
	char *nularray;

	larray = calloc (*nelem, sizeof(char));
	nularray = calloc (*nelem, sizeof(char));

	ffgcfl (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		larray, nularray, &anynul, &status);

	for (i = 0;  i < *nelem;  i++) {
	    lray[i] = larray[i];
	    flgval[i] = nularray[i];
	}

	free (larray);
	free (nularray);
	*x_anynul = anynul;
	*x_status = status;
}

void FSGCVD_U (XPOINTER *x_fptr, XINT *colnum,
	       XINT *frow, XINT *felem, XINT *nelem,
	       XDOUBLE *nulval, XDOUBLE array[], XINT *x_anynul, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	int anynul = *x_anynul;
	ffgcvd (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		*nulval, array, &anynul, &status);
	*x_anynul = anynul;
	*x_status = status;
}

void FSGCVE_U (XPOINTER *x_fptr, XINT *colnum,
	       XINT *frow, XINT *felem, XINT *nelem,
	       XREAL *nulval, XREAL array[], XINT *x_anynul, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	int anynul = *x_anynul;
	ffgcve (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		*nulval, array, &anynul, &status);
	*x_anynul = anynul;
	*x_status = status;
}

void FSGCVI_U (XPOINTER *x_fptr, XINT *colnum,
	       XINT *frow, XINT *felem, XINT *nelem,
	       XSHORT *nulval, XSHORT array[], XINT *x_anynul, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	int anynul = *x_anynul;
	ffgcvi (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		*nulval, array, &anynul, &status);
	*x_anynul = anynul;
	*x_status = status;
}

void FSGCVJ_U (XPOINTER *x_fptr, XINT *colnum,
	       XINT *frow, XINT *felem, XINT *nelem,
	       XINT *nulval, XINT array[], XINT *x_anynul, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	int anynul = *x_anynul;
	long *larray;
	long i;

	larray = calloc (*nelem, sizeof(long));

	ffgcvj (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		(long)*nulval, larray, &anynul, &status);

	for (i = 0;  i < *nelem;  i++)
	    array[i] = larray[i];

	free (larray);
	*x_anynul = anynul;
	*x_status = status;
}

void FSGCVS_U (XPOINTER *x_fptr, XINT *colnum,
	       XINT *frow, XINT *felem, XINT *nelem,
	       XCHAR nulval[], XCHAR array[], XINT *dim1,
	       XINT *x_anynul, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	int anynul = *x_anynul;
	char **larray;
	char *lnulval;
	long i, j;		/* j is the index for array */

	/* Note that the local variable for nulval has length dim1. */
	lnulval = calloc (*dim1+1, sizeof(char));
	larray = calloc (*nelem, sizeof(char*));

	for (i = 0;  i < *nelem;  i++)
	    larray[i] = calloc (*dim1+1, sizeof(char));

	strpak (nulval, lnulval, *dim1);

	ffgcvs (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		lnulval, larray, &anynul, &status);

	j = 0;
	for (i = 0;  i < *nelem;  i++) {
	    strupk (larray[i], &array[j], *dim1);
	    free (larray[i]);
	    j += (*dim1 + 1);		/* array is 2-D */
	}

	free (lnulval);
	free (larray);
	*x_anynul = anynul;
	*x_status = status;
}

void FSGHSP_U (XPOINTER *x_fptr, XINT *x_nexist, XINT *x_nmore, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	int nexist = *x_nexist;
	int nmore = *x_nmore;
	ffghsp (*fptr, &nexist, &nmore, &status);
	*x_nmore = nmore;
	*x_nexist = nexist;
	*x_status = status;
}

void FSGKEY_U (XPOINTER *x_fptr, XCHAR sppkey[],
	       XCHAR sppvalue[], XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);

	ffgkey (*fptr, c_keyword, c_value, c_comment, &status);

	if (status == 0) {
	    strupk (c_value, sppvalue, FLEN_VALUE);
	    strupk (c_comment, sppcomm, FLEN_COMMENT);
	}
	*x_status = status;
}

void FSGKYD_U (XPOINTER *x_fptr, XCHAR sppkey[], XDOUBLE *value,
	       XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);

	ffgkyd (*fptr, c_keyword, value, c_comment, &status);

	if (status == 0)
	    strupk (c_comment, sppcomm, FLEN_COMMENT);
	*x_status = status;
}

void FSGKYJ_U (XPOINTER *x_fptr, XCHAR sppkey[], XINT *value,
	       XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	long lvalue;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);

	ffgkyj (*fptr, c_keyword, &lvalue, c_comment, &status);
	*value = lvalue;

	if (status == 0)
	    strupk (c_comment, sppcomm, FLEN_COMMENT);
	*x_status = status;
}

void FSGKYS_U (XPOINTER *x_fptr, XCHAR sppkey[], XCHAR sppvalue[],
	       XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);

	ffgkys (*fptr, c_keyword, c_value, c_comment, &status);

	if (status == 0) {
	    strupk (c_value, sppvalue, FLEN_VALUE);
	    strupk (c_comment, sppcomm, FLEN_COMMENT);
	}
	*x_status = status;
}

void FSGMSG_U (XCHAR sppmsg[])
{
	int i;

	i = ffgmsg (c_message);
	if (i > 0)
	    strupk (c_message, sppmsg, FLEN_ERRMSG);
	else
	    sppmsg[0] = 0;
}

void FSGREC_U (XPOINTER *x_fptr, XINT *nrec, XCHAR spprecord[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	ffgrec (*fptr, *nrec, c_card, &status);

	if (status == 0)
	    strupk (c_card, spprecord, FLEN_CARD);
	*x_status = status;
}

void FSGRSZ_U (XPOINTER *x_fptr, XINT *maxrows, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	long ndata;

	ffgrsz (*fptr, &ndata, &status);
	*maxrows = ndata;
	*x_status = status;
}

void FSGTDM_U (XPOINTER *x_fptr, XINT *colnum, XINT *maxdim,
	       XINT *x_naxis, XINT naxes[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	int naxis = *x_naxis;
	long *axlen;
	long i;

	axlen = calloc (*maxdim, sizeof(long));

	ffgtdm (*fptr, *colnum, *maxdim, &naxis, axlen, &status);

	if (status == 0) {
	    for (i = 0;  i < naxis;  i++)
		naxes[i] = axlen[i];
	}

	free (axlen);
	*x_naxis = naxis;
	*x_status = status;
}

void FSIBIN_U (XPOINTER *x_fptr, XINT *nrows, XINT *nfields,
	       XCHAR sppttype[], XCHAR spptform[], XCHAR spptunit[],
	       XCHAR sppextnam[], XINT *pcount, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	char **ttype, **tform, **tunit;
	char *extnam;
	long i;
	long j1 = 0, j2 = 0, j3 = 0;

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
		extnam, (long)*pcount, &status);

	free (extnam);
	for (i = 0;  i < *nfields;  i++) {
	    free (ttype[i]);
	    free (tform[i]);
	    free (tunit[i]);
	}
	free (ttype);
	free (tform);
	free (tunit);
	*x_status = status;
}

void FSICOL_U (XPOINTER *x_fptr, XINT *colnum,
	       XCHAR sppttype[], XCHAR spptform[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	char *ttype, *tform;

	ttype = calloc (SZ_FTTYPE+1, sizeof(char*));
	tform = calloc (SZ_FTFORM+1, sizeof(char*));
	strpak (sppttype, ttype, SZ_FTTYPE);
	strpak (spptform, tform, SZ_FTFORM);

	fficol (*fptr, *colnum, ttype, tform, &status);

	free (ttype);
	free (tform);
	*x_status = status;
}

void FSINIT_U (XPOINTER *x_fptr, XCHAR sppname[],
	       XINT *blocksize, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	strpak (sppname, c_filename, FLEN_FILENAME);
	ffinit (fptr, c_filename, &status);
	*x_status = status;
}

void FSIROW_U (XPOINTER *x_fptr, XINT *frow, XINT *nrows, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	ffirow (*fptr, (long)*frow, (long)*nrows, &status);
	*x_status = status;
}

void FSMAHD_U (XPOINTER *x_fptr, XINT *hdunum, XINT *x_exttype, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	int exttype = *x_exttype;
	ffmahd (*fptr, *hdunum, &exttype, &status);
	*x_exttype = exttype;
	*x_status = status;
}

void FSMCOM_U (XPOINTER *x_fptr, XCHAR sppkey[], XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffmcom (*fptr, c_keyword, c_comment, &status);
	*x_status = status;
}

void FSMKYD_U (XPOINTER *x_fptr, XCHAR sppkey[], XDOUBLE *dval,
	       XINT *decim, XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffmkyd (*fptr, c_keyword, *dval, *decim, c_comment, &status);
	*x_status = status;
}

void FSMKYE_U (XPOINTER *x_fptr, XCHAR sppkey[], XREAL *rval,
	       XINT *decim, XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffmkye (*fptr, c_keyword, *rval, *decim, c_comment, &status);
	*x_status = status;
}

void FSMKYJ_U (XPOINTER *x_fptr, XCHAR sppkey[], XINT *intval,
	       XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffmkyj (*fptr, c_keyword, (long)*intval, c_comment, &status);
	*x_status = status;
}

void FSMKYL_U (XPOINTER *x_fptr, XCHAR sppkey[], XINT *logval,
	       XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffmkyl (*fptr, c_keyword, *logval, c_comment, &status);
	*x_status = status;
}

void FSMKYS_U (XPOINTER *x_fptr, XCHAR sppkey[], XCHAR sppvalue[],
	       XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppvalue, c_value, FLEN_VALUE);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffmkys (*fptr, c_keyword, c_value, c_comment, &status);
	*x_status = status;
}

void FSMREC_U (XPOINTER *x_fptr, XINT *nkey, XCHAR sppcard[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppcard, c_card, FLEN_CARD);

	ffmrec (*fptr, *nkey, c_card, &status);
	*x_status = status;
}

void FSOPEN_U (XPOINTER *x_fptr, XCHAR sppname[], XINT *iomode,
	       XINT *blocksize, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppname, c_filename, FLEN_FILENAME);

	ffopen (fptr, c_filename, *iomode, &status);
	*x_status = status;
}

void FSPCLD_U (XPOINTER *x_fptr, XINT *colnum,
	       XINT *frow, XINT *felem, XINT *nelem,
	       XDOUBLE array[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	ffpcld (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		array, &status);
	*x_status = status;
}

void FSPCLE_U (XPOINTER *x_fptr, XINT *colnum,
	       XINT *frow, XINT *felem, XINT *nelem,
	       XREAL array[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	ffpcle (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		array, &status);
	*x_status = status;
}

void FSPCLI_U (XPOINTER *x_fptr, XINT *colnum,
	       XINT *frow, XINT *felem, XINT *nelem,
	       XSHORT array[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	ffpcli (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		array, &status);
	*x_status = status;
}

void FSPCLJ_U (XPOINTER *x_fptr, XINT *colnum,
	       XINT *frow, XINT *felem, XINT *nelem,
	       XINT array[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	long *larray;
	long i;

	larray = calloc (*nelem, sizeof(long));

	for (i = 0;  i < *nelem;  i++)
	    larray[i] = array[i];

	ffpclj (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		larray, &status);

	free (larray);
	*x_status = status;
}

void FSPCLL_U (XPOINTER *x_fptr, XINT *colnum,
	       XINT *frow, XINT *felem, XINT *nelem,
	       XINT array[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	char *larray;
	long i;

	larray = calloc (*nelem, sizeof(char));

	for (i = 0;  i < *nelem;  i++)
	    larray[i] = array[i];

	ffpcll (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		larray, &status);

	free (larray);
	*x_status = status;
}

void FSPCLS_U (XPOINTER *x_fptr, XINT *colnum,
	       XINT *frow, XINT *felem, XINT *nelem,
	       XCHAR array[], XINT *dim1, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	char **larray;
	long i, j;		/* j is the index for array */

	larray = calloc (*nelem, sizeof(char*));

	j = 0;
	for (i = 0;  i < *nelem;  i++) {
	    larray[i] = calloc (*dim1+1, sizeof(char));
	    strpak (&array[j], larray[i], *dim1);
	    j += (*dim1 + 1);		/* array is 2-D */
	}

	ffpcls (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem,
		larray, &status);

	for (i = 0;  i < *nelem;  i++)
	    free (larray[i]);

	free (larray);
	*x_status = status;
}

void FSPCLU_U (XPOINTER *x_fptr, XINT *colnum,
	       XINT *frow, XINT *felem, XINT *nelem,
	       XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	ffpclu (*fptr, *colnum,
		(long)*frow, (long)*felem, (long)*nelem, &status);
	*x_status = status;
}

void FSPCOM_U (XPOINTER *x_fptr, XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffpcom (*fptr, c_comment, &status);
	*x_status = status;
}

void FSPHBN_U (XPOINTER *x_fptr, XINT *nrows, XINT *nfields,
	       XCHAR sppttype[], XCHAR spptform[], XCHAR spptunit[],
	       XCHAR sppextnam[], XINT *pcount, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	char **ttype, **tform, **tunit;
	char *extnam;
	long i;
	long j1 = 0, j2 = 0, j3 = 0;

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
		extnam, (long)*pcount, &status);

	free (extnam);
	for (i = 0;  i < *nfields;  i++) {
	    free (ttype[i]);
	    free (tform[i]);
	    free (tunit[i]);
	}
	free (ttype);
	free (tform);
	free (tunit);
	*x_status = status;
}

void FSPHIS_U (XPOINTER *x_fptr, XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffphis (*fptr, c_comment, &status);
	*x_status = status;
}

void FSPHPR_U (XPOINTER *x_fptr, XINT *simple, XINT *bitpix,
	       XINT *naxis, XLONG naxes[], XLONG *pcount, XLONG *gcount,
	       XINT *extend, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	long *axlen;
	long i;

	axlen = calloc (*naxis, sizeof(long));

	for (i = 0;  i < *naxis;  i++)
	    axlen[i] = naxes[i];

	ffphpr (*fptr, *simple, *bitpix, *naxis, axlen,
		(long)*pcount, (long)*gcount, *extend, &status);

	free (axlen);
	*x_status = status;
}

void FSPKYD_U (XPOINTER *x_fptr, XCHAR sppkey[],
	       XDOUBLE *dval, XINT *decim, XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffpkyd (*fptr, c_keyword, *dval, *decim, c_comment, &status);
	*x_status = status;
}

void FSPKYE_U (XPOINTER *x_fptr, XCHAR sppkey[],
	       XREAL *rval, XINT *decim, XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffpkye (*fptr, c_keyword, *rval, *decim, c_comment, &status);
	*x_status = status;
}

void FSPKYJ_U (XPOINTER *x_fptr, XCHAR sppkey[], XINT *value,
	       XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffpkyj (*fptr, c_keyword, (long)*value, c_comment, &status);
	*x_status = status;
}

void FSPKYL_U (XPOINTER *x_fptr, XCHAR sppkey[],
	       XINT *logval, XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffpkyl (*fptr, c_keyword, *logval, c_comment, &status);
	*x_status = status;
}

void FSPKYS_U (XPOINTER *x_fptr, XCHAR sppkey[], XCHAR sppvalue[],
	       XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppvalue, c_value, FLEN_VALUE);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffpkys (*fptr, c_keyword, c_value, c_comment, &status);
	*x_status = status;
}

void FSPREC_U (XPOINTER *x_fptr, XCHAR sppcard[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppcard, c_card, FLEN_CARD);

	ffprec (*fptr, c_card, &status);
	*x_status = status;
}

void FSPSVC_U (XCHAR sppcard[],
	       XCHAR sppvalue[], XCHAR sppcomm[], XINT *x_status)
{
	int status = *x_status;

	strpak (sppcard, c_card, FLEN_CARD);

	ffpsvc (c_card, c_value, c_comment, &status);

	if (status == 0) {
	    strupk (c_value, sppvalue, FLEN_VALUE);
	    strupk (c_comment, sppcomm, FLEN_COMMENT);
	}
	*x_status = status;
}

void FSPTDM_U (XPOINTER *x_fptr, XINT *colnum,
	       XINT *naxis, XINT naxes[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	long *axlen;
	long i;

	axlen = calloc (*naxis, sizeof(long));

	for (i = 0;  i < *naxis;  i++)
	    axlen[i] = naxes[i];

	ffptdm (*fptr, *colnum, *naxis, axlen, &status);

	free (axlen);
	*x_status = status;
}

void FSRDEF_U (XPOINTER *x_fptr, XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;
	ffrdef (*fptr, &status);
	*x_status = status;
}

void FSUKYD_U (XPOINTER *x_fptr, XCHAR sppkey[],
	       XDOUBLE *dval, XINT *decim, XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffukyd (*fptr, c_keyword, *dval, *decim, c_comment, &status);
	*x_status = status;
}

void FSUKYJ_U (XPOINTER *x_fptr, XCHAR sppkey[], XINT *value,
	       XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffukyj (*fptr, c_keyword, (long)*value, c_comment, &status);
	*x_status = status;
}

void FSUKYL_U (XPOINTER *x_fptr, XCHAR sppkey[], XINT *x_logval,
	       XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffukyl (*fptr, c_keyword, *x_logval, c_comment, &status);
	*x_status = status;
}

void FSUKYS_U (XPOINTER *x_fptr, XCHAR sppkey[],
	       XCHAR sppvalue[], XCHAR sppcomm[], XINT *x_status)
{
	fitsfile **fptr = (fitsfile **)x_fptr;
	int status = *x_status;

	strpak (sppkey, c_keyword, FLEN_KEYWORD);
	strpak (sppvalue, c_value, FLEN_VALUE);
	strpak (sppcomm, c_comment, FLEN_COMMENT);

	ffukys (*fptr, c_keyword, c_value, c_comment, &status);
	*x_status = status;
}

static void strpak (const XCHAR *in, char *out, long maxch)
{
	long i = 0;

	while (in[i] != 0 && i < maxch) {
	    out[i] = in[i];
	    i++;
	}
	out[i] = '\0';
}

static void strupk (const char *in, XCHAR *out, long maxch)
{
	long i = 0;

	while (in[i] != '\0' && i < maxch) {
	    out[i] = in[i];
	    i++;
	}
	out[i] = 0;
}
