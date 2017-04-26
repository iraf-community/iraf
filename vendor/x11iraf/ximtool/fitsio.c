/*
 * FITSIO.C -- Routines to load and save simple FITS files.
 *
 *       ival = isFITS  (fname)
 *            loadFITS  (fname, pixels, w, h, r, g, b, ncolors, 
 *		  	    zsc, zr, &z1, &z2, nsample)
 *           writeFITS  (fp, pixels, pixtype, w,h, r,g,b, ncolors)
 *    str = getFITSHdr  (fname)

 * LoadFITS(fname, numcols)  -  loads a FITS file
 * WriteFITS(fp, pic, w, h, rmap, gmap, bmap, numcols)
 *
 * isFITS    -- returns nonzero if the named file is a FITS file.
 * loadFITS  -- reads a FITS file and returns the decoded pixel array and gray-
 * 	        scale 8 bit colormap.  The caller is responsible for freeing
 * 	        the pixels buffer.  
 * writeFITS -- performs the converse operation, writing the given pixel array
 * 		and colormap to the output Sun rasterfile.
 * Based on contributed FITS I/O software for XV by David Robinson.
 */


#include <stdio.h>
#include <math.h>
#include <ctype.h>

#define NCARDS 		36
#define BLOCKSIZE 	2880
#define EPSILON 	1.192e-7


#define CONTRAST        0.25            /* zscaling parameters */
#define NSAMPLE         1000


/* MONO returns total intensity of r,g,b components */
#define MONO(rd,gn,bl) (((rd)*11 + (gn)*16 + (bl)*5) >> 5)  /*.33R+ .5G+ .17B*/

/* data types */
enum datatype { T_INT, T_LOG, T_REAL, T_NOVAL, T_STRING };

typedef unsigned char byte;
#ifndef AIXV3
#ifndef OSF1
typedef unsigned char uchar;
#endif
#endif

typedef struct {
	FILE      *fp;              /* file pointer 			*/
	int	  bitpix, size;     /* bits per pixel, sizeof(unit) 	*/
	int	  naxis;            /* number of axes 			*/
	long int  axes[2];     	    /* size of each axis 		*/
	long int  ndata;       	    /* number of elements in data       */
	long int  cpos;        	    /* current position in data file    */
	char      title[80];        /* image title 			*/
	int	  extend;           /* image has extensions?		*/
	int	  nextend;          /* number of extensions		*/
	float     bscale, bzero;    /* scaling parameters 		*/
} FITS;

/* Function prototypes */
#ifdef __STDC__

#include <stddef.h>
#include <stdlib.h>

static char *ftopen2d (FITS *fs, char *file, int *nx, int *ny, int *bitpix);
static void ftclose (FITS *fs);
static char *ftgbyte (FITS *fs, uchar *buffer, int nelem, int zsc, int zr, float *z1, float *z2, int nsample);
static char *rdheader (FITS *fs);
static char *wrheader (FILE *fp, int nx, int ny);
static char *rdcard (char *card, char *name, enum datatype dtype, long int *kvalue, float *rvalue);
static void wrcard (char *card, char *name, enum datatype dtype, int kvalue);
static char *ftgdata (FITS *fs, void *buffer, int nelem);
static char *ftfixdata (FITS *fs, void *buffer, int nelem);
#else
static char *ftopen2d ();
static void ftclose ();
static char *ftgbyte ();
static char *rdheader ();
static char *wrheader ();
static char *rdcard ();
static void wrcard ();
static char *ftgdata ();
static char *ftfixdata ();
#endif


/* ----------------
 * Public routines.
 * ----------------*/


/* loadFits - Load a simple FITS file.
 */
char *
loadFITS (fname, pix, nx, ny, r,g,b, ncolors, zsc, zr, z1, z2, nsample)
char 	*fname;                		/* input filename 	*/
uchar   **pix;                 		/* output pixels 	*/
int 	*nx, *ny;                       /* dimensions 		*/
uchar 	*r, *g, *b;               	/* colormap 		*/
int 	*ncolors;                       /* number of colors 	*/
int	zsc, zr;			/* z-scaling flags	*/
float	*z1, *z2;			/* zscale values	*/
int	nsample;			/* number of sample pts */

{
	FITS 	fs;
	int	i, w = 0, h = 0, bitpix, np;
	byte 	*image;
	char	*error;

	error = ftopen2d (&fs, fname, &w, &h, &bitpix);
	if (error)
	    return error;
	if (fs.extend)
	    return "Load support for MEF files\nis not currently implemented";

	/* allocate memory for image and read it in */
	np = w * h;
	image = (byte *) malloc (np);
	if (image == NULL) 
	    return "Insufficient memory";

	error = ftgbyte (&fs, image, np, zsc, zr, z1, z2, nsample);
	ftclose (&fs);
	if (error) {
	    free (image);
	    return error;
	}

	/* There seems to be a convention that fits files be displayed using
         * a cartesian coordinate system. Thus the first pixel is in the lower
	 * left corner. Fix this by reflecting in the line y=h/2.
 	*/
	flip (image, w, h);

	/* sucess ! */
	*pix = (unsigned char *) image;
	*nx = w;
	*ny = h;
	*ncolors = 256;
	for (i = 0; i < 256; i++) 
	    r[i] = g[i] = b[i] = i;

	return NULL;
}


/* writeFITS --  Write the current frame buffer out as a FITS image.
 */

char *
writeFITS (fp, image, w, h, rmap, gmap, bmap, numcols)
FILE 	*fp;
byte 	*image;
int	w, h;
byte 	*rmap, *gmap, *bmap;
int	numcols;
{
	register int	i, j, np, nend;
	register byte *ptr;
	char	*error;
	byte rgb[256];

	error = wrheader(fp, w, h);
	if (error != NULL)
	    return error;

	for (i = 0; i < numcols; i++)
	    rgb[i] = MONO(rmap[i], gmap[i], bmap[i]);

	/* flip line ordering when writing out */
	for (i = h - 1; i >= 0; i--) {
	    ptr = &image[i*w];
	    for (j = 0; j < w; j++, ptr++)
	    	putc(rgb[*ptr], fp);
	}

	np = w * h;
	/* nend is the number of padding characters at the end of the last
         * block.
	 */
	nend = ((np + BLOCKSIZE - 1) / BLOCKSIZE) * BLOCKSIZE - np;

	if (nend) 
	    for (i = 0; i < nend; i++) 
	    	putc('\0', fp);

	return NULL;
}


/* IsFITS -- Test a file to see if it is a FITS file.
 */
int 
isFITS (fname)
char	*fname;				/* input filename */
{
	register FILE *fp;
	int value = 0;
	char keyw[8], val;

	if (fp = fopen (fname, "r")) {
	    fscanf (fp, "%6s = %c", keyw, &val);
	    if (strcmp ("SIMPLE", keyw) == 0 && val == 'T')
		value = 1;
	    fclose (fp);
	}
	return value;
}


/* getFITSHdr -- Get some set of header information for the GUI.
 */

char *
getFITSHdr (fname)
char	*fname;
{
	FITS 	fs;
	char 	*error, *title, *line;
	int  	w, h, bitpix;

	line = (char *) malloc (80);

	error = ftopen2d (&fs, fname, &w, &h, &bitpix);
	if (error) {
	    strcpy (line, error);
	    return (line);
	}

	if (fs.extend) {
	    sprintf (line, "%-16.16s  %3d  (%2d extns)   %s", 
	    	fname, bitpix, fs.nextend, fs.title);
	} else {
	    sprintf (line, "%-16.16s  %3d  %5dx%-5d  %s", 
	    	fname, bitpix, w,h, fs.title);
	}

	ftclose (&fs);

	return (line);
}



/*
 * Private Procedures
 * ------------------
 */

/* Writes a minimalist FITS file header */
static char
*wrheader (fp, nx, ny)
FILE 	*fp;
int	nx, ny;
{
	char	*block;
	int	i;

	block = (char *) malloc (BLOCKSIZE);
	if (block == NULL) 
	    return "Insufficient memory for workspace";
	memset(block, ' ', BLOCKSIZE);

	i = 0;
	wrcard(&block[80*i++], "SIMPLE", T_LOG, 1);  	/* SIMPLE keyword */
	wrcard(&block[80*i++], "BITPIX", T_INT, 8);  	/* BITPIX keyword */
	wrcard(&block[80*i++], "NAXIS", T_INT, 2);   	/* NAXIS keyword  */
	wrcard(&block[80*i++], "NAXIS1", T_INT, nx); 	/* NAXIS1 keyword */
	wrcard(&block[80*i++], "NAXIS2", T_INT, ny); 	/* NAXIS2 keyword */
	wrcard(&block[80*i++], "END", T_NOVAL, 0);   	/* END keyword    */
	i = fwrite(block, sizeof(char), BLOCKSIZE, fp);
	if (i != BLOCKSIZE) 
	    return "Error writing FITS file";
	return NULL;
}


/* open a 2-dimensional fits file.
 * Stores the dimensions of the file in nx and ny, and updates the FITS
 * structure passed in fs.
 * If successful, returns NULL otherwise returns an error message.
 * Will return an error message if the primary data unit is not a 2-dimensional
 * array.
 */
static char	
*ftopen2d(fs, file, nx, ny, bitpix)
FITS 	*fs;
char	*file;
int	*nx, *ny, *bitpix;
{
	FILE 	*fp;
	int	naxis, i;
	char	*error;

	fp = fopen(file, "rb");
	if (fp == NULL) 
	    return "Unable to open FITS file";

	fs->fp = fp;
	fs->bitpix = 0;
	fs->naxis = 0;
	fs->cpos = 0;

	/* read header */
	error = rdheader(fs);

	if (error != NULL) {
	    ftclose(fs);
	    return error;
	}

	/* get number of data */
	fs->ndata = 1;
	for (i = 0; i < fs->naxis; i++) 
	    fs->ndata = fs->ndata * fs->axes[i];

	naxis = fs->naxis;

	*nx = fs->axes[0];
	*ny = fs->axes[1];
	*bitpix = fs->bitpix;

	return error;
}


/* closes a fits file */
static void
ftclose (fs)
FITS 	*fs;
{
	if (fs == NULL) 
	    return;
	if (fs->fp != NULL) 
	    fclose(fs->fp);
}


/* reads the fits header, and updates the FITS structure fs.
 * Returns NULL on success, or an error message otherwise.
 */
static char *
rdheader (fs)
FITS 	*fs;
{
	int	i, j, res;
	char	name[9];
	char	*block;
	char	*error;
	long int	val;         /* the value */
	float	rval;		     /* floating point value */

	block = (char *) malloc(BLOCKSIZE);
	if (block == NULL) 
	    return "Insufficient memory for workspace";

	res = fread(block, sizeof(char), BLOCKSIZE, fs->fp);
	if (res != BLOCKSIZE) 
	    return "Error reading FITS file";
	i = 0;

	/* read SIMPLE key */
	error = rdcard(block, "SIMPLE", T_LOG, &val, &rval);
	if (error != NULL) 
	    return error;
	/*
	if (val == 0) 
	    return "Not a SIMPLE FITS file";
	*/
	i++;

	/* read BITPIX key */
	error = rdcard(&block[80], "BITPIX", T_INT, &val, &rval);
	if (error != NULL) 
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

	/* read NAXIS key */
	error = rdcard(&block[2*80], "NAXIS", T_INT, &val, &rval);
	if (error != NULL) 
	    return error;
	if (val < 0 || val > 999)
	    return "Bad NAXIS value in FITS file";
	if (val == 1 || val > 2)
	    return "FITS file is not a two-dimensional image";
	fs->naxis = val;
	i++;

	/* Check for an EXTEND/NEXTEND keyword pair. */
	error = rdcard(&block[3*80], "EXTEND", T_LOG, &val, &rval);
	if (error == NULL) {
	    fs->extend = val; i++;
	} else 
	    fs->extend = 0;
	error = rdcard(&block[4*80], "NEXTEND", T_INT, &val, &rval);
	if (error == NULL) {
	    fs->nextend = val; i++;
	} else 
	    fs->nextend = 0;


	/* read NAXISnnn keys.
 	 * We allow NAXIS to be > 2 iff the dimensions of the extra axes are 1
 	 */
	for (j = 0; j < fs->naxis; j++) {
	    if (i == NCARDS) {
	    	res = fread(block, sizeof(char), BLOCKSIZE, fs->fp);
	    	if (res != BLOCKSIZE) 
	    	    return "Error reading FITS file";
	    	i = 0;
	    }

	    sprintf(name, "NAXIS%d", j + 1);
	    error = rdcard(&block[i*80], name, T_INT, &val, &rval);
	    if (error != NULL) 
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

	/* do remainder */
	fs->bscale  = 1.0;
	fs->bzero   = 0.0;
	memset(fs->title, '\0', 80);
	strcpy (fs->title, "No Title");
	for (; ; ) {

	    if (block[i*80] == 'B') {
		/* Try reading a BSCALE or BZERO keyword from this card. */
	        error = rdcard(&block[i*80], "BSCALE", T_REAL, &val, &rval);
	        if (error == NULL)
	            fs->bscale = rval;
	        error = rdcard(&block[i*80], "BZERO", T_REAL, &val, &rval);
	        if (error == NULL)
	            fs->bzero = rval;
	    }

	    if (i == NCARDS) {
	    	res = fread(block, sizeof(char), BLOCKSIZE, fs->fp);
	    	if (res != BLOCKSIZE) 
	    	    return "Unexpected eof in FITS file";
	    	i = 0;
	    }

	    if (strncmp(&block[i*80], "OBJECT  ", 8) == 0) {
		char *ip = &block[i*80];
		int i = 0, j = 0;

		/* Skip ahead to opening quote. */
		while (*ip != '\'' && i < 80) ip++, i++; 
		ip++, i++;
		for (j=0; j < 80 && *ip != '\''; j++) {
		    fs->title[j] = *ip;
		    ip++;
		}
		fs->title[j] = '\0';
	    }

	    if (strncmp(&block[i*80], "END     ", 8) == 0) 
	    	break;
	    i++;
	}
	free(block);
	return NULL;
}


/* write a header record into the 80 byte buffer card.
 * The keyword name is passed in name. The value type is in dtype; this
 * can have the following values:
 *    dtype = T_NOVAL
 *         no keyword value is written
 *    dtype = T_LOG
 *         a logical value, either 'T' or 'F' in column 30 is written
 *    dtype = T_INT
 *         an integer is written, right justified in columns 11-30
 */
static void
wrcard (card, name, dtype, kvalue)
char	*card, *name;
enum 	datatype dtype;   /* type of value */
int	kvalue;
{
	int	l;
	memset(card, ' ', 80);
	l = strlen(name);
	if (l) 
	    memcpy(card, name, l);   /* copy name */

	if (dtype == T_NOVAL) 
	    return;

	card[8] = '=';

	if (dtype == T_LOG)
	    card[29] = kvalue ? 'T' : 'F';
	else /* an integer */    {
	    sprintf(&card[10], "%20d", kvalue);
	    card[30] = ' ';
	}

}


/* Read a header record, from the 80 byte buffer card.
 * the keyword name must match 'name'; and parse its value according to
 * dtype. This can have the following values:
 *    dtype = T_LOG
 *        value is logical, either 'T' or 'F' in column 30.
 *    dtype = T_INT
 *        value is an integer, right justified in columns 11-30.
 *    dtype = T_REAL
 *        value is a real
 *
 * The value is stored in kvalue.
 * It returns NULL on success, or an error message otherwise.
 */
static char *
rdcard (card, name, dtype, kvalue, rvalue)
char	*card, *name;
enum 	datatype dtype;   			/* type of value */
long 	int *kvalue;
float 	*rvalue;
{
	int	i, ptr;
	char	namestr[9];
	static char	error[45];

	memcpy(namestr, card, 8);
	i = 8;
	do 
	    i--; 
	while (i >= 0 && namestr[i] == ' ');
	namestr[i+1] = '\0';

	if (strcmp(namestr, name) != 0) {
	    sprintf(error, "Keyword %s not found in FITS file", name);
	    return error;
	}

	/* get start of value */
	ptr = 10;
	while (ptr < 80 && card[ptr] == ' ') 
	    ptr++;
	if (ptr == 80 || card[ptr] == '/') 
	    return "FITS file has missing keyword value"; /* no value */

	if (dtype == T_LOG) {
	    if ((card[ptr] != 'T' && card[ptr] != 'F'))
	    	return "Keyword has bad logical value in FITS file";
	    *kvalue = (card[ptr] == 'T');

	} else /* an integer or real */    {
	    int		j, end;
	    long int	ival;
	    float	fval;
	    char	num[21];

	    /*
	    if (ptr > 29) 
	    	return "Keyword has bad integer value in FITS file";
	    */
	    end = ptr;
	    while (end < 80 && (card[end] != ' ' && card[end] != '/'))
	        end++;
	    memcpy(num, &card[ptr], end - ptr);
	    num[end-ptr] = '\0';
	    if (dtype == T_INT) {
	       j = sscanf(num, "%ld", &ival);
	       if (j != 1) 
	    	   return "Keyword has bad integer value in FITS file";
	       *kvalue = ival;
	       *rvalue = 0.0;
	    } else if (dtype == T_REAL) {
	       j = sscanf(num, "%g", &fval);
	       if (j != 1) 
	    	   return "Keyword has bad real value in FITS file";
	       *kvalue = 0;
	       *rvalue = fval;
	    }
	}

	return NULL;
}


/* reads nelem values into the buffer.
 * returns NULL for success or an error message.
 * Copes with the fact that the last 2880 byte record of the FITS file
 * may be truncated, and should be padded out with zeros.
 */
static char *
ftgdata (fs, buffer, nelem)
FITS 	*fs;
void 	*buffer;
int	nelem;
{
	int	res;

	if (nelem == 0) 
	    return NULL;

	res = fread(buffer, fs->size, nelem, fs->fp);
	/* if failed to read all the data */
	if (res != nelem) {
	    /* nblock is the number of elements in a record. size is
	     * always a factor of BLOCKSIZE */
	    int	loffs, nblock = BLOCKSIZE / fs->size;

	    if (!feof(fs->fp)) 
	    	return "I/O error reading FITS file";

	    /* The last record might be short; check this.
 	     * loffs is the offset of the start of the last record from
	     * the current position.
 	     */
	    loffs = ((fs->ndata + nblock - 1) / nblock - 1) * nblock - fs->cpos;

	    /* if we didn't read to the end of the penultimate record */
	    if (res < loffs) 
	    	return "Unexpected EOF reading FITS file";

	    /* pad with zeros */
	    memset((char *)buffer + res * fs->size, '\0',
		(nelem - res) * fs->size);
	}
	fs->cpos += res;
	return ftfixdata(fs, buffer, nelem);
}


/* convert the raw data, as stored in the FITS file, to the format
 * appropiate for the data representation of the host computer.
 * Assumes that
 *  short int = 2 byte integer
 *  int       = 4 byte integer
 */
static char *
ftfixdata (fs, buffer, nelem)
FITS 	*fs;
void 	*buffer;
int	nelem;
{
	register int	i, n = nelem;
	register uchar	*ptr = buffer;

	/* conversions. Although the data may be signed, reverse using unsigned 
         * variables.  Convert from big-endian two-byte signed integer to
	 * native form 
	 */
	if (fs->bitpix == 16)
	    for (i = 0; i < n; i++, ptr += 2)
	    	*(unsigned short int *)ptr = (((int)*ptr) << 8) | (int)(ptr[1]);

	/* convert from big-endian four-byte signed integer to native form */
	else if (fs->bitpix == 32)
	    for (i = 0; i < n; i++, ptr += 4)
	    	*(unsigned int *)ptr = (((unsigned int)*ptr) << 24) | 
	    	    ((unsigned int)ptr[1] << 16) | 
	    	    ((unsigned int)ptr[2] << 8)  | 
	    	    (unsigned int)ptr[3];

	/* convert from IEE 754 single precision to native form */
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

	/* convert from IEE 754 double precision to native form */
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
	    	j = ((unsigned int)ptr[1] << 24) | ((unsigned int)ptr[2] << 16) | 
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


#undef max
#define f_max(a,b) ((a) > (b) ? (a) : (b))
#undef min
#define f_min(a,b) ((a) < (b) ? (a) : (b)) 


/* Reads a byte image from the FITS file fs. The image contains nelem pixels.
 * If bitpix = 8, then the image is loaded as stored in the file if not scaled.
 * Otherwise, it is rescaled so that the minimum value is stored as 0, and
 * the maximum is stored as 255
 */
static char *
ftgbyte(fs, cbuff, nelem, zsc, zr, z1, z2, nsample)
FITS 	*fs;
uchar	*cbuff;
int	nelem;
int	zsc, zr;
float 	*z1, *z2;
int	nsample;
{
	char * voidbuff;
	register int	i, n = nelem;
	char	*error;
	int	pmin = 0, pmax = 255;
	int	npts, stdline;
	extern	void zscale();

	/* if the data is uchar, then read it directly */
	if (fs->bitpix == 8 && (fs->bscale == 1.0 && fs->bzero == 0.0)) {
	    *z1 = 0.0;
	    *z2 = 255.0;
	    return ftgdata(fs, cbuff, nelem);
	}

	/* allocate a buffer to store the image */
	if (fabs((double)fs->bscale-1.0) > EPSILON || 
	    fabs((double)fs->bzero) > EPSILON)
	        voidbuff = (char * )malloc(nelem * f_max(fs->size,4));
	else
	    voidbuff = (char * )malloc(nelem * fs->size);
	if (voidbuff == NULL) 
	    return "Insufficient memory for workspace";
	error = ftgdata(fs, voidbuff, nelem);
	if (error != NULL) {
	    printf ("ftgbyte: %s\n", error);
	    return error;
	}

	/* If we've got BSCALE/BZERO values compute the original pixel values
	 * and convert the buffer to floating point before processing it.
	 * The voidbuff was allocated above with this in mind so it should be
	 * large enough that we can fix the pixels in place.
	 */
	            
	npts = fs->axes[0] * fs->axes[1];
	stdline = (int)((float)fs->axes[1] / sqrt((float)npts/(float)nsample));
	if (fs->bscale != 1.0 || fs->bzero != 0.0) {
	    register float *buf;

	    buf = (float *)voidbuff;

	    if (fs->bitpix ==  8) {
	        for (i=(nelem-1); i >= 0; i--)
		    buf[i] = (float) voidbuff[i] * fs->bscale + fs->bzero;
	    } else if (fs->bitpix ==  16) {
	        register short *old;
	        for (i=(nelem-1); i >= 0; i--) {
		    old = (short *) &voidbuff[i * 2];
		    buf[i] = (float) *old * fs->bscale + fs->bzero;
	        }
	    } else if (fs->bitpix ==  32) {
	        register int *old;
	        for (i=(nelem-1); i >= 0; i--) {
		    old = (int *) &voidbuff[i * 4];
		    buf[i] = (float) *old * fs->bscale + fs->bzero;
	        }
	    } else if (fs->bitpix ==  -32) {
	        register float *old;
	        for (i=(nelem-1); i >= 0; i--) {
		    old = (float *) &voidbuff[i * 4];
		    buf[i] = (float) *old * fs->bscale + fs->bzero;
	        }
	    } else if (fs->bitpix ==  -64) {
	        register double *old, *dbuf;
		register float *fpix;

	 	dbuf = (double *) malloc (nelem * sizeof(double));
	        for (i=(nelem-1); i >= 0; i--) {
		    old = (double *) &voidbuff[i * 8];
		    dbuf[i] = (float) *old * fs->bscale + fs->bzero;
	        }
		fpix = (float *) voidbuff;
		for (i=0; i<nelem; i++)
		    buf[i] = fpix[i] = (float) dbuf[i];
		free ((double *) dbuf);
	    }
	
	    fs->size   = 4;
	    fs->bitpix = -32;

	    /* compute the optimal zscale values */
	    if (zsc)
	        zscale ((char *)buf, fs->axes[0], fs->axes[1], fs->bitpix,
	            z1, z2, CONTRAST, nsample, stdline); 
	    else if (zr)
		min_max ((float *)buf, nelem, fs->bitpix, z1, z2);

	} else {
	    /* compute the optimal zscale values */
	    if (zsc)
	        zscale (voidbuff, fs->axes[0], fs->axes[1], fs->bitpix,
	            z1, z2, CONTRAST, nsample, stdline); 
	    else if (zr)
		min_max ((float *)voidbuff, nelem, fs->bitpix, z1, z2);
	}

	/* convert short int to uchar */
	if (fs->bitpix == 16) {
	    register short int	*buffer = (short *)voidbuff;
	    register int	max, min;
	    register float	scale;

	    min = (int) *z1;
	    max = (int) *z2;
	    scale = (max == min) ? 0. : 255. / (*z2 -*z1);

	    /* rescale and convert */
	    for (i = 0, buffer = (short *)voidbuff; i < n; i++)
	    	cbuff[i] = f_max (pmin, f_min (pmax, 
		    (int)(scale * (float)((int)buffer[i] - min)) ));

	/* convert long int to uchar */
	} else if (fs->bitpix == 32) {
	    register int	*buffer = (int *)voidbuff;
	    register int	max, min;
	    register float	scale;

	    min = (int) *z1;
	    max = (int) *z2;
	    scale = (max == min) ? 0. : 255. / (*z2 - *z1);

	    /* rescale and convert */
	    for (i = 0, buffer = (int *)voidbuff; i < n; i++)
	    	cbuff[i] = f_max (pmin, f_min (pmax, 
		    (int)(scale * (float)((int)buffer[i] - min)) ));

	/* convert float to uchar */
	} else if (fs->bitpix == -32) {
	    register float	*buffer = (float *)voidbuff;
	    register float	max, min, scale;

	    min = *z1;
	    max = *z2;
	    scale = (max == min) ? 0. : 255. / (*z2 - *z1);

	    /* rescale and convert */
	    for (i = 0, buffer = (float *)voidbuff; i < n; i++)
	    	cbuff[i] = f_max (pmin, f_min (pmax, 
		    (int)(scale * ((float)buffer[i] - min)) ));

	/* convert double to uchar */
	} else if (fs->bitpix == -64) {
	    register double	*buffer = (double *)voidbuff;
	    register double	max, min, scale;

	    min = (double) *z1;
	    max = (double) *z2;
	    scale = (max == min) ? 0. : 255. / (*z2 - *z1);

	    /* rescale and convert */
	    for (i = 0, buffer = (double *)voidbuff; i < n; i++)
	    	cbuff[i] = f_max (pmin, f_min (pmax, 
		    (int)(scale * ((double)buffer[i] - min)) ));
	}

	free( (char *)voidbuff);
	return NULL;
}
