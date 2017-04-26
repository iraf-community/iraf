#include <stdio.h>
#include <math.h>
#include <ctype.h>
#define  CDL_LIBRARY_SOURCE
#include "cdl.h"


/*
 * CDLFITS.C -- Routines to load simple FITS files.
 *
 *          cdl_displayFITS  (cdl, fname, frame, fbconfig, zscale)
 *               cdl_isFITS  (fname)
 *             cdl_readFITS  (fname, &pix, &nx, &ny, &bitpix, title);
 */


#define NCARDS 		36
#define BLOCKSIZE 	2880
#define SZ_CARD 	80

/* data types */
enum datatype { T_INT, T_LOG, T_REAL, T_STR, T_NOVAL };

typedef struct {
	FILE      *fp;              /* file pointer                           */
	int	  bitpix, size;     /* number of bits per pixel, sizeof(unit) */
	int	  naxis;            /* number of axes                         */
	long int  axes[2];     	    /* size of each axis                      */
	long int  ndata;       	    /* number of elements in data             */
	long int  cpos;        	    /* current position in data file          */
	float     bscale, bzero;    /* scaling parameters                     */
	char	  object[SZ_CARD];  /* object keyword name                    */
} FITS;

/* Function prototypes */
#ifdef __STDC__
#include <stddef.h>
#include <stdlib.h>
#endif

#undef max
#define f_max(a,b) ((a) > (b) ? (a) : (b))
#undef min
#define f_min(a,b) ((a) < (b) ? (a) : (b)) 

extern	int	cdl_debug;


#ifdef ANSI_FUNC

static int cdl_readFITSHdr(char *fname, float *bscale, float *bzero, char *obj);
static char *cdl_getFITSPixels(char *fname, uchar **pix, int *nx, int *ny, int *bitpix);
static char *cdl_openFITS(FITS *fs, char *file, int *nx, int *ny, int *bitpix);
static char *cdl_readHeader(FITS *fs);
static char *cdl_rdCard(char *card, char *name, enum datatype dtype, long *kvalue, float *rvalue, char *svalue);
static char *cdl_getData(FITS *fs, uchar *buffer, int nelem);
static char *cdl_fixData(FITS *fs, void *buffer, int nelem);

#else

static char *cdl_getFITSPixels();
static char *cdl_openFITS(), *cdl_readHeader ();
static char *cdl_rdCard(), *cdl_getData (), *cdl_fixData();
static int  cdl_readFITSHdr();

#endif


/*  CDL_DISPLAYFITS -- Display a simple FITS format image to the given frame, 
 *  optionally doing a Zscale transform of the image pixels.
 */

#ifdef ANSI_FUNC

int 
cdl_displayFITS (
    CDLPtr cdl,                         /* package ptr          */
    char *fname,			/* image name		*/
    int frame,				/* display frame	*/
    int fbconfig,			/* frame buffer config	*/
    int zscale				/* do zscale of image?	*/
)
#else

int
cdl_displayFITS (cdl, fname, frame, fbconfig, zscale)
CDLPtr  cdl;                            /* package ptr          */
char	*fname;				/* image name		*/
int	frame;				/* display frame	*/
int	fbconfig;			/* frame buffer config	*/
int	zscale;				/* do zscale of image?	*/
#endif
{
	char	obj[SZ_CARD];
	int	status, nx, ny, bitpix;
	uchar	*pix;
	float	z1 = 0.0, z2 = 0.0, bscale, bzero;

	/* See if this is a valid FITS file. */
	if (!cdl_isFITS (fname)) {
	    fprintf (stderr, "%s: not a simple FITS file or doesn't exist.\n",
		fname);
	    return (ERR);
	}

	/* Get the raw FITS image pixels. */
        if (cdl_readFITS (fname, &pix, &nx, &ny, &bitpix, obj))
	    return (ERR);

	if (cdl_debug) {
	    printf("[cdl_displayFITS] '%s' frame=%d zscale=%d\n",
		fname, frame, zscale);
	    printf("[cdl_displayFITS] %dx%d bitpix=%d pixels z1=%g z2=%g\n",
		nx, ny, bitpix, z1, z2);
	}

	/* Read the header. */
	if (cdl_readFITSHdr (fname, &bscale, &bzero, obj))
	    return (ERR);
	(void) cdl_setTitle (cdl, obj);

	(void) cdl_setName (cdl, fname);
	status =  cdl_displayPix (cdl, pix, nx, ny, bitpix, frame, fbconfig,
	    zscale);
	free ((char *) pix);
	return (status);
}


/*  CDL_ISFITS -- Test a file to see if it is a simple FITS file.
 */

#ifdef ANSI_FUNC

int 
cdl_isFITS (
    char *fname				/* input filename */
)
#else

int 
cdl_isFITS (fname)
char	*fname;				/* input filename */
#endif
{
	register FILE *fp;
	int value = 0;
	char keyw[8], val;

	if ((fp = fopen (fname, "r"))) {
	    fscanf (fp, "%6s = %c", keyw, &val);
	    if (strcmp ("SIMPLE", keyw) == 0 && val == 'T')
		value = 1;
	    fclose (fp);
	}
	return value;
}


/*  CDL_READFITS -- Read the pixels from a simple FITS format image, ret-
 *  urning the pixel array, dimensions, and image type.
 */

#ifdef ANSI_FUNC

int 
cdl_readFITS (
    char *fname,			/* image name		*/
    uchar **pix,			/* pixel array (output) */
    int *nx,
    int *ny,				/* dimensions (output)	*/
    int *bitpix,			/* pixel size (output)	*/
    char *title				/* image title (output) */
)
#else

int
cdl_readFITS (fname, pix, nx, ny, bitpix, title)
char	*fname;				/* image name		*/
uchar	**pix;				/* pixel array (output) */
int	*nx, *ny;			/* dimensions (output)	*/
int	*bitpix;			/* pixel size (output)	*/
char	*title;				/* image title (output) */
#endif
{
	char	*errstr, *cdl_getFITSPixels();
	float	bscale=1.0, bzero=0.0;

	/* See if this is a valid FITS file. */
	if (!cdl_isFITS (fname)) {
	    fprintf (stderr, "%s: not a simple FITS file or doesn't exist.\n",
		fname);
	    return (ERR);
	}

	if ((errstr = cdl_getFITSPixels (fname, pix, nx, ny, bitpix))) {
	    fprintf (stderr, "%s\n", errstr);
	    return (ERR);
	}
	if (cdl_readFITSHdr (fname, &bscale, &bzero, title))
	    return (ERR);

	if (cdl_debug)
	    printf ("[cdl_readFITS] '%s' nx=%d ny=%d bitpix=%d title='%s'\n",
		fname, *nx, *ny, *bitpix, title);

	return (OK);
}


/* ------------------
 * Private Procedures
 * ------------------*/


/*  CDL_READFITSHDR -- Read several header parameters from the image.
 */

#ifdef ANSI_FUNC

static int 
cdl_readFITSHdr (
    char *fname,			/* image name		*/
    float *bscale,
    float *bzero,			/* scaling (output)	*/
    char *obj				/* image title (output)	*/
)
#else

static int
cdl_readFITSHdr (fname, bscale, bzero, obj)
char	*fname;				/* image name		*/
float	*bscale, *bzero;		/* scaling (output)	*/
char	*obj;				/* image title (output)	*/
#endif
{
	char	*errstr;
	register FITS	*fs;
	register FILE	*fp;

	/* See if this is a valid FITS file. */
	if (!cdl_isFITS (fname)) {
	    fprintf (stderr, "%s: not a simple FITS file or doesn't exist.\n",
		fname);
	    return (ERR);
	}

	if ((fp = fopen(fname, "rb")) == NULL) 
	    return (ERR);

	fs = (FITS *) malloc (sizeof(FITS));
	fs->fp = fp;
	fs->bitpix = fs->naxis = fs->cpos = 0;

	/* Read the header. */
	if ((errstr = cdl_readHeader (fs))) {
	    fclose(fs->fp);
	    free ((char *) fs);
	    return (ERR);
	}

	*bscale = fs->bscale;
	*bzero = fs->bzero;
	if (obj == (char *)NULL)
	    obj = (char *) malloc (SZ_CARD+1);
	sprintf (obj, "%s", fs->object);

	if (cdl_debug)
	    printf ("[cdl_readFITSHdr] '%s' bscale=%g bzero=%g obj='%s'\n",
		fname, *bscale, *bzero, obj);

	fclose(fs->fp);
	free ((char *) fs);
	return (OK);
}


/*  CDL_GETFITSPIXELS -- Given an FITS filename return a pointer to the pixel
 *  array and the image dimensions.
 */

#ifdef ANSI_FUNC

static char *
cdl_getFITSPixels (
    char *fname,               		/* input filename 	*/
    uchar **pix,               		/* output pixels 	*/
    int *nx,
    int *ny,                       	/* dimensions 		*/
    int *bitpix				/* pixel size		*/
)
#else

static char *
cdl_getFITSPixels (fname, pix, nx, ny, bitpix)
char 	*fname;                		/* input filename 	*/
uchar   **pix;                 		/* output pixels 	*/
int 	*nx, *ny;                       /* dimensions 		*/
int	*bitpix;			/* pixel size		*/
#endif
{
	FITS 	fs;
	register int	i, nelem;
	char	*error = NULL;


	if ((error = cdl_openFITS (&fs, fname, nx, ny, bitpix)))
	    return error;

        /* Allocate a buffer to store the image. */
	nelem = (*nx) * (*ny);
        if (fs.bscale != 1.0 || fs.bzero != 0.0)
            *pix = (uchar *) malloc (nelem * f_max(fs.size,4));
        else
            *pix = (uchar *) malloc (nelem * fs.size);
        if (*pix == NULL) 
            return "Insufficient memory for workspace";

        /* If the data is uchar, then read it directly */
        if (fs.bitpix == 8 && (fs.bscale == 1.0 || fs.bzero == 0.0))
            return cdl_getData (&fs, *pix, nelem);
	else if ((error = cdl_getData (&fs, *pix, nelem)))
            return error;

        /* If we've got BSCALE/BZERO values compute the original pixel values
         * and convert the buffer to floating point before processing it.
         * The pix buffer was allocated above with this in mind so it should
         * be large enough that we can fix the pixels in place.
         */
        if (fs.bscale != 1.0 || fs.bzero != 0.0) {
            register float *buf;

            buf = (float *)*pix;

            if (fs.bitpix ==  8) {
                for (i=(nelem-1); i >= 0; i--)
                    buf[i] = (float) (*pix)[i] * fs.bscale + fs.bzero;
            } else if (fs.bitpix ==  16) {
                register short *old = (short *) *pix;
                for (i=(nelem-1); i >= 0; i--)
                    buf[i] = (float) old[i] * fs.bscale + fs.bzero;
            } else if (fs.bitpix ==  32) {
                register int *old = (int *) *pix;
                for (i=(nelem-1); i >= 0; i--)
                    buf[i] = (float) old[i] * fs.bscale + fs.bzero;
            } else if (fs.bitpix ==  -32) {
                register float *old = (float *) *pix;
                for (i=(nelem-1); i >= 0; i--)
                    buf[i] = (float) old[i] * fs.bscale + fs.bzero;
            } else if (fs.bitpix ==  -64) {
                register double *old = (double *) *pix, *dbuf;
                register float *fpix;

                dbuf = (double *) malloc (nelem * sizeof(double));
                for (i=(nelem-1); i >= 0; i--)
                    dbuf[i] = (float) old[i] * fs.bscale + fs.bzero;
                fpix = (float *) *pix;
                for (i=0; i<nelem; i++)
                    fpix[i] = (float) dbuf[i];
                free ((double *) dbuf);
            }
        
            fs.size   = 4;
            fs.bitpix = -32;
            *bitpix = -32;
        }

	/* sucess ! */
	fclose(fs.fp);
	return NULL;
}


/*  CDL_OPENFITS -- Open a 2-dimensional FITS file.  Stores the dimensions of
 *  the file in nx and ny, and updates the FITS structure passed in fs.
 *  If successful, returns NULL otherwise returns an error message.
 */

#ifdef ANSI_FUNC

static char *
cdl_openFITS (FITS *fs, char *file, int *nx, int *ny, int *bitpix)
#else

static char *
cdl_openFITS(fs, file, nx, ny, bitpix)
FITS 	*fs;
char	*file;
int	*nx, *ny, *bitpix;
#endif
{
	register int	i;
	FILE 	*fp;
	char	*error = NULL;

	if ((fp = fopen(file, "rb")) == NULL) 
	    return "Unable to open FITS file";

	fs->fp = fp;
	fs->bitpix = 0;
	fs->naxis = 0;
	fs->cpos = 0;

	/* Read the header. */
	if ((error = cdl_readHeader (fs))) {
	    fclose(fs->fp);
	    return error;
	}

	if (fs->naxis > 2) {
	    fclose(fs->fp);
	    return "Not a 2-D FITS image.";
	}

	/* Get number of pixel. */
	fs->ndata = 1;
	for (i = 0; i < fs->naxis; i++) 
	    fs->ndata = fs->ndata * fs->axes[i];

	*nx = fs->axes[0];
	*ny = fs->axes[1];
	*bitpix = fs->bitpix;

	return NULL;
}


/*  CDL_READHEADER -- Reads the fits header, and updates the FITS structure fs.
 *  Returns NULL on success, or an error message otherwise.
 */

#ifdef ANSI_FUNC

static char *
cdl_readHeader (FITS *fs)
#else

static char *
cdl_readHeader (fs)
FITS *fs;
#endif
{
	register int	 i, j, res;
	char	 name[9];
	char	 *block;
	char	 *error = NULL;
	long int val;         	     	/* the value 		*/
	float	 rval;		     	/* floating point value */
	char	 sval[SZ_FNAME];	/* string value		*/


	if ((block = (char *) malloc(BLOCKSIZE)) == NULL) 
	    return "Insufficient memory for workspace";

	if ((res = fread (block, sizeof(char), BLOCKSIZE, fs->fp)) != BLOCKSIZE)
	    return "Error reading FITS file";
	i = 0;

	/* Read SIMPLE keyword. */
	if ((error = cdl_rdCard (block, "SIMPLE", T_LOG, &val, &rval, sval)))
	    return error;
	if (val == 0) 
	    return "Not a SIMPLE FITS file";
	i++;

	/* Read BITPIX keyword. */
	if ((error = cdl_rdCard(&block[SZ_CARD], "BITPIX", T_INT, &val, &rval,
	    sval)))
	        return error;
	if (val != 8 && val != 16 && val != 32 && val != 64 && val != -32 && 
	    val != -64)
	      return "Bad BITPIX value in FITS file";
	fs->bitpix = val;
	j = fs->bitpix;
	if (j < 0) 
	    j = -j;
	fs->size = j / 8;
	i++;

	/* Read NAXIS keyword. */
	if ((error=cdl_rdCard(&block[2*SZ_CARD], "NAXIS", T_INT, &val, &rval,
            sval)))
	        return error;
	if (val < 0 || val > 999)
	    return "Bad NAXIS value in FITS file";
	if (val < 2) 
	    return "FITS file is not a two-dimensional image";
	fs->naxis = val;
	i++;

	/* Read NAXISnnn keys.  We allow NAXIS to be > 2 iff the dimensions
 	 * of the extra axes are 1.
 	 */
	for (j = 0; j < fs->naxis; j++) {
	    if (i == NCARDS) {
	    	res = fread(block, sizeof(char), BLOCKSIZE, fs->fp);
	    	if (res != BLOCKSIZE) 
	    	    return "Error reading FITS file";
	    	i = 0;
	    }

	    sprintf (name, "NAXIS%d", j + 1);
	    if ((error=cdl_rdCard (&block[i*SZ_CARD], name, T_INT, &val, &rval,
		sval)))
	    	    return error;
	    if (val < 0) 
	    	return "Bad NAXISn value in FITS file";
	    if (j < 2) 
	    	fs->axes[j] = val;
	    else if (val != 1) 
	    	return "FITS file is not a two-dimensional image";
	    i++;
	}
	fs->naxis = 2;

	/* Do the remainder. */
	fs->bscale  = 1.0;
	fs->bzero   = 0.0;
	fs->object[0] = '\0';
	while (1) {

	    /* Try reading a BSCALE or BZERO keyword from this card. */
	    if (block[i*SZ_CARD] == 'B') {
	        error = cdl_rdCard(&block[i*SZ_CARD], "BSCALE", T_REAL, &val,
		    &rval, sval);
	        if (error == NULL)
	            fs->bscale = rval;
	        error = cdl_rdCard (&block[i*SZ_CARD], "BZERO", T_REAL, &val,
		    &rval, sval);
	        if (error == NULL)
	            fs->bzero = rval;
	    } else if (strncmp("OBJECT", &block[i*SZ_CARD], 6) == 0) {
	        /* Read OBJECT keyword.  */
	        if ((error = cdl_rdCard (&block[i*SZ_CARD], "OBJECT", T_STR,
		    &val, &rval, sval))) {
	                strcpy (fs->object, "no title");
			break;
		}
	        strncpy (fs->object, sval, SZ_CARD);
	        i++;
	    }

	    if (i == NCARDS) {
	    	res = fread (block, sizeof(char), BLOCKSIZE, fs->fp);
	    	if (res != BLOCKSIZE) 
	    	    return "Unexpected EOF in FITS file";
	    	i = 0;
	    }
	    if (strncmp (&block[i*SZ_CARD], "END     ", 8) == 0) 
	    	break;
	    i++;
	}

	free (block);
	return NULL;
}


/*  CDL_RDCARD -- Read a header record, from the 80 byte buffer card.
 *  The keyword name must match 'name'; and parse its value according to
 *  dtype. This can have the following values:
 *      dtype = T_LOG      # value is logical, either 'T' or 'F' in column 30
 *      dtype = T_INT      # value is an int, right justified in columns 11-30
 *      dtype = T_REAL     # value is a real
 *      dtype = T_STR      # value is a string
 *
 *  The value is stored in kvalue.  Returns NULL on success, or an error
 *  message otherwise.
 */

#ifdef ANSI_FUNC

static char *
cdl_rdCard (
    char *card,				/* FITS keyword card 	*/
    char *name,				/* keyword name		*/
    enum datatype dtype,		/* type of value 	*/
    long *kvalue,			/* integer value 	*/
    float *rvalue,			/* real value 		*/
    char *svalue			/* string value 	*/
)
#else

static char *
cdl_rdCard (card, name, dtype, kvalue, rvalue, svalue)
char	*card;				/* FITS keyword card 	*/
char	*name;				/* keyword name		*/
enum datatype dtype;   			/* type of value 	*/
long 	*kvalue;			/* integer value 	*/
float 	*rvalue;			/* real value 		*/
char  	*svalue;			/* string value 	*/
#endif
{
	register int	i, ptr;
	char	namestr[9], *ip;

	/* Get the keyword from the card.  */
	bcopy (card, namestr, 8);
        for (i=7; i >= 0 && namestr[i] == ' '; i--)
	    ;
	namestr[i+1] = '\0';
	if (strcmp(namestr, name) != 0)
	    return "Keyword not found in FITS file.";

	/* Get start of value. */
	ptr = 10;
	while (ptr < SZ_CARD && card[ptr] == ' ') 
	    ptr++;
	if (ptr == SZ_CARD) 
	    return "FITS file has missing keyword value"; /* no value */

	if (dtype == T_LOG) {
	    *kvalue = (card[ptr] == 'T');

	} else if (dtype == T_STR) {
	    for (ip=&card[9]; *ip != '\''; ip++)
		;
	    for (i=0, ip=&card[ptr+1]; *ip != '\'' && i < SZ_FNAME; i++)
		svalue[i] = *ip++;
	    svalue[i] = '\0';
	} else {
	    /* An integer or real value. */
	    long int	ival;
	    float	fval;
	    char	num[21];

	    if (ptr > 29) 
	    	return "Keyword has bad integer value in FITS file";
	    memcpy (num, &card[ptr], 30 - ptr);
	    num[30-ptr] = '\0';
	    if (dtype == T_INT) {
	       i = sscanf (num, "%ld", &ival);
	       if (i != 1) 
	    	   return "Keyword has bad integer value in FITS file";
	       *kvalue = ival;
	       *rvalue = 0.0;
	    } else if (dtype == T_REAL) {
	       i = sscanf (num, "%g", &fval);
	       if (i != 1) 
	    	   return "Keyword has bad real value in FITS file";
	       *kvalue = 0;
	       *rvalue = fval;
	    }
	}

	return NULL;
}


/*  CDL_GETDATA -- Reads nelem values into the buffer.  Copes with the fact
 *  that the last 2880 byte record of the FITS file may be truncated, and
 *  should be padded out with zeros.  Returns NULL for success or an error
 *  message.
 */

#ifdef ANSI_FUNC

static char *
cdl_getData (FITS *fs, uchar *buffer, int nelem)
#else

static char *
cdl_getData (fs, buffer, nelem)
FITS 	*fs;
uchar 	*buffer;
int	nelem;
#endif
{
	int	res;

	if (nelem == 0) 
	    return NULL;

	if ((res = fread ((void *)buffer, fs->size, nelem, fs->fp)) != nelem) {
	    /* Nblock is the number of elements in a record. size is
	     * always a factor of BLOCKSIZE
	     */
	    int	loffs, nblock = BLOCKSIZE / fs->size;

	    if (!feof(fs->fp)) 
	    	return "I/O error reading FITS file";

	    /* The last record might be short; check this.
 	     * loffs is the offset of the start of the last record from
	     * the current position.
 	     */
	    loffs = ((fs->ndata + nblock - 1) / nblock - 1) * nblock - fs->cpos;

	    /* If we didn't read to the end of the penultimate record */
	    if (res < loffs) 
	    	return "Unexpected eof reading FITS file";

	    /* Pad with zeros */
	    memset ((char *)buffer+res*fs->size, '\0', (nelem-res)*fs->size);
	}

	fs->cpos += res;
	return cdl_fixData(fs, buffer, nelem);
}


/*  CDL_FIXDATA -- convert the raw data, as stored in the FITS file, to the
 *  format appropiate for the data representation of the host computer.
 *  Assumes that
 *       short int = 2 byte integer
 *       int       = 4 byte integer
 */

#ifdef ANSI_FUNC

static char *
cdl_fixData (FITS *fs, void *buffer, int nelem)
#else

static char *
cdl_fixData (fs, buffer, nelem)
FITS 	*fs;
void 	*buffer;
int	nelem;
#endif
{
	register int	i, n = nelem;
	register uchar	*ptr = buffer;

	/* Conversions. Although the data may be signed, reverse using unsigned 
         * variables.  Convert from big-endian two-byte signed integer to
	 * native form.
	 */
	if (fs->bitpix == 16)
	    for (i = 0; i < n; i++, ptr += 2)
	    	*(unsigned short int *)ptr = (((int)*ptr) << 8) | (int)(ptr[1]);

	/* Convert from big-endian four-byte signed integer to native form */
	else if (fs->bitpix == 32)
	    for (i = 0; i < n; i++, ptr += 4)
	    	*(unsigned int *)ptr = (((unsigned int)*ptr) << 24) | 
	    	    ((unsigned int)ptr[1] << 16) | 
	    	    ((unsigned int)ptr[2] << 8)  | 
	    	    (unsigned int)ptr[3];

	/* Convert from IEE 754 single precision to native form */
	else if (fs->bitpix == -32) {
	    register int	j, k, expo;
	    static float	*exps = NULL;

	    if (exps == NULL) {
	    	exps = (float *)calloc(256, sizeof(float));
	    	if (exps == NULL) 
	    	    return "Insufficient memory for workspace";
	    	exps[150] = 1.;
	    	for (i = 151; i < 256; i++) 
	    	    exps[i] = 2. * exps[i-1];
	    	for (i = 149; i >= 0; i--) 
	    	    exps[i] = 0.5 * exps[i+1];
	    }


	    for (i = 0; i < n; i++, ptr += 4) {
	    	k = (int)*ptr;
	    	j = ((int)ptr[1] << 16) | ((int)ptr[2] << 8) | (int)ptr[3];
	    	expo = ((k & 127) << 1) | (j >> 23);
	    	if ((expo | j) == 0) 
	    	    *(float *)ptr = 0.;
	    	else
	    	    *(float *)ptr = exps[expo] * (float)(j | 0x800000);
	    	if (k & 128) 
	    	    *(float *)ptr = -*(float *)ptr;
	    }

	/* Convert from IEE 754 double precision to native form */
	} else if (fs->bitpix == -64) {
	    register int	expo, k, l;
	    register unsigned int	j;
	    static double	*exps = NULL;

	    if (exps == NULL) {
	    	exps = (double *)calloc(2048, sizeof(double));
	    	if (exps == NULL) 
	    	    return "Insufficient memory for workspace";
	    	exps[1075] = 1.;
	    	for (i = 1076; i < 2048; i++) 
	    	    exps[i] = 2. * exps[i-1];
	    	for (i = 1074; i >= 0; i--) 
	    	    exps[i] = 0.5 * exps[i+1];
	    }

	    for (i = 0; i < n; i++, ptr += 8) {
	    	k = (int)*ptr;
	    	j = ((unsigned int)ptr[1] << 24) | ((unsigned int)ptr[2] << 16)|
	    	    ((unsigned int)ptr[3] << 8) | (unsigned int)ptr[4];
	    	l = ((int)ptr[5] << 16) | ((int)ptr[6] << 8) | (int)ptr[7];
	    	expo = ((k & 127) << 4) | (j >> 28);
	    	if ((expo | j | l) == 0) 
	    	    *(double *)ptr = 0.;
	    	else
	    	    *(double *)ptr = exps[expo] * (16777216. * 
	    	        (double)((j & 0x0FFFFFFF) | 0x10000000) + (double)l);
	    	if (k & 128) 
	    	    *(double *)ptr = -*(double *)ptr;
	    }
	}

	return NULL;
}
