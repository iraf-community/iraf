#include "imfort.h"

/*
 * NOTE <|> This interface has been obsoleted by the IMFORT interface; see
 * sys/imio/imfort.
 */

/* IMFORT -- Fortran interface to IRAF images.  This interface permits a
 * Fortran program to read or write an existing IRAF image.  There is currently
 * no provision for creating new images or deleting old images from Fortran.
 * The interface routines are as follows:
 *
 *	im =	   imopen (image, mode, ndim, len_axes)
 *		   imclos (im)
 *
 *		imget[sr] (im, buf, x1, x2, linenum)
 *		imput[sr] (im, buf, x1, x2, linenum)
 *
 * Legal access modes are "r", "w", and "rw".  All coordinates are 1-indexed.
 * The integer array "len_axes" should be dimensioned at least 7.  The "im"
 * variable is an integer.  The get and put routines will perform datatype
 * conversion if necessary.  The imget and imput routines will abort program
 * execution if there is an error.
 *
 * THIS IS NOT A PRODUCTION INTERFACE AND WILL CHANGE.
 * THIS CODE IS NOT PORTABLE.
 */

#define	ERR		(-1)
#define	NULL		0
#define	XEOS		0
#define	SZ_PAKSTR	512
#define	SZ_ERRMSG	160


/* IMOPEN -- Open an existing image for read or write access from a Fortran
 * program.  Multiple images may be open at a time.
 */
imopen_ (image, mode, ndim, len_axes)
char	*image;
char	*mode;
int	*ndim;
int	*len_axes;
{
	register struct	imhdr *im;
	register int fd;
	char	*cp;
	int	i, unix_mode;
	char	*malloc(), *impakstr();
	char	*stridx();

	/* Allocate buffer for image header.
	 */
	im = (struct imhdr *)malloc (sizeof *im);
	if (im == NULL)
	    imfatal ("cannot allocate buffer space for image header");

	/* Map user access mode into UNIX access mode code.
	 */
	if (strcmp (mode, "r") == 0)
	    unix_mode = 0;
	else if (strcmp (mode, "w") == 0)
	    unix_mode = 1;
	else if (strcmp (mode, "rw") == 0)
	    unix_mode = 2;
	else
	    imfatals ("illegal image access mode '%s'", mode);

	/* Open the image header file.  Not strictly necessary to open image
	 * header with write perm, but may as well use same permission as for
	 * pixel file to simplify any contention problems.  If pixel file is
	 * to be opened write-only (unix_mode == 1), the header file is opened
	 * read-write, since we must always have read perm on the header.
	 */
	fd = open (image, (unix_mode == 1) ? 2 : unix_mode);
	if (fd == ERR)
	    imfatals ("cannot open image '%s'", image);

	/* Read in the image header, a binary structure written by the IRAF
	 * image i/o interface.  If read is successful check the magic string
	 * to verify that the named file is indeed an IRAF image header file.
	 */
	if (read (fd, im, sizeof(*im)) != sizeof (*im))
	    imfatals ("cannot read image header for '%s'", image);
	if (strcmp (impakstr (im->im_magic), "imhdr") != 0)
	    imfatals ("file '%s' is not an image header file", image);

	/* Open pixel storage file.  Strip the node name prefix, if any.
	 * The file must reside on the local node.
	 */
	cp = stridx (impakstr (im->im_pixfile), '!');
	if (cp != NULL)
	    cp++;
	else
	    cp = impakstr (im->im_pixfile);

	im->im_pfnum = open (cp, unix_mode);
	if (im->im_pfnum == ERR)
	    imfatals ("cannot open pixel storage file for image '%s'", image);

	/* Ready to do i/o to image.  Return dimensions of image to the user
	 * and exit.
	 */
	*ndim = im->im_ndim;
	for (i=0;  i < IM_MAXDIM;  i++)
	    len_axes[i] = im->im_len[i];

	return ((unsigned int)im);
}


/* IMCLOS -- Close an image opened from a Fortran program.
 */
imclos_ (u_im)
unsigned int *u_im;
{
	struct	imhdr *im = (struct imhdr *)*u_im;
	char	*impakstr();

	if (im == NULL || strcmp (impakstr (im->im_magic), "imhdr") != 0)
	    imfatal ("imclos: bad image descriptor");

	close (im->im_pfnum);
	free ((char *)im);
	*u_im = NULL;
}


/* IMGETS -- Get type "short" (16 bit signed integer) pixels from an image.
 * Pixels will be converted to type short if necessary.
 */
imgets_ (u_im, buf, x1, x2, linenum)
unsigned int *u_im;
short	*buf;
int	*x1, *x2;
int	*linenum;
{
	static	int shortpix = TY_SHORT;
	register struct imhdr *im = (struct imhdr *)*u_im;
	short	*pixbuf;
	int	npix, nbytes;
	char	*impakstr();

	/* Verify image descriptor is ok and section is in bounds.
	 */
	imbounds (im, *x1, *x2, *linenum, *linenum);

	/* Determine size of buffer needed to read line.  May be larger than
	 * user buffer if type conversion is required.
	 */
	npix = *x2 - *x1 + 1;
	nbytes = im_pixbufsize (npix, im->im_pixtype);

	if (nbytes > npix * sizeof (short)) {
	    pixbuf = (short *) malloc (nbytes);
	    if (pixbuf == NULL)
		imfatal ("imgets: cannot allocate buffer space");
	} else
	    pixbuf = buf;

	/* Read pixels into buffer.  Will abort if cannot seek or read.
	 */
	imseek (im, *x1, *linenum);
	if (read (im->im_pfnum, (char *)pixbuf, nbytes) != nbytes)
	    imfatal ("imgets: read error");

	/* Do type conversion, if necessary, writing converted pixels to
	 * user buffer.  Otherwise pixels are already in user buffer.
	 */
	if (im->im_pixtype != TY_SHORT)
	    acht_ (pixbuf, buf, &npix, &im->im_pixtype, &shortpix);

	if (pixbuf != buf)
	    free ((char *)pixbuf);
}


/* IMGETR -- Get type "real" (single precision floating) pixels from an image.
 * Pixels will be converted to type real if necessary.
 */
imgetr_ (u_im, buf, x1, x2, linenum)
unsigned int *u_im;
float	*buf;
int	*x1, *x2;
int	*linenum;
{
	static	int realpix = TY_REAL;
	register struct imhdr *im = (struct imhdr *)*u_im;
	float	*pixbuf;
	int	npix, nbytes;
	char	*impakstr();

	/* Verify image descriptor is ok and section is in bounds.
	 */
	imbounds (im, *x1, *x2, *linenum, *linenum);

	/* Determine size of buffer needed to read line.  May be larger than
	 * user buffer if type conversion is required.
	 */
	npix = *x2 - *x1 + 1;
	nbytes = im_pixbufsize (npix, im->im_pixtype);

	if (nbytes > npix * sizeof (float)) {
	    pixbuf = (float *) malloc (nbytes);
	    if (pixbuf == NULL)
		imfatal ("imgetr: cannot allocate buffer space");
	} else
	    pixbuf = buf;

	/* Read pixels into buffer.  Will abort if cannot seek or read.
	 */
	imseek (im, *x1, *linenum);
	if (read (im->im_pfnum, (char *)pixbuf, nbytes) != nbytes)
	    imfatal ("imgetr: read error");

	/* Do type conversion, if necessary, writing converted pixels to
	 * user buffer.  Otherwise pixels are already in user buffer.
	 */
	if (im->im_pixtype != TY_REAL)
	    acht_ (pixbuf, buf, &npix, &im->im_pixtype, &realpix);

	if (pixbuf != buf)
	    free ((char *)pixbuf);
}


/* IMPUTS -- Put type "short" (16 bit signed integer) pixels to an image.
 * Pixels will be converted from type short to the image pixel datatype if
 * necessary.
 */
imputs_ (u_im, buf, x1, x2, linenum)
unsigned int *u_im;
short	*buf;
int	*x1, *x2;
int	*linenum;
{
	static	int shortpix = TY_SHORT;
	register struct imhdr *im = (struct imhdr *)*u_im;
	short	*pixbuf;
	int	npix, nbytes;
	char	*impakstr();

	/* Verify image descriptor is ok and section is in bounds.
	 */
	imbounds (im, *x1, *x2, *linenum, *linenum);

	/* An extra buffer is necessary is the data must be type converted,
	 * since we cannot modify the caller's buffer.
	 */
	npix = *x2 - *x1 + 1;
	nbytes = im_pixbufsize (npix, im->im_pixtype);

	if (im->im_pixtype != TY_SHORT) {
	    pixbuf = (short *)malloc (nbytes);
	    if (pixbuf == NULL)
		imfatal ("imputs: cannot allocate buffer space");
	} else
	    pixbuf = buf;

	/* Do type conversion, if necessary, writing converted pixels to
	 * temporary buffer.  Otherwise pixels are taken directly from user
	 * buffer.
	 */
	if (im->im_pixtype != TY_SHORT)
	    acht_ (buf, pixbuf, &npix, &shortpix, &im->im_pixtype);

	/* Write pixels to image.  Will abort if cannot seek or write.
	 */
	imseek (im, *x1, *linenum);
	if (write (im->im_pfnum, pixbuf, nbytes) != nbytes)
	    imfatal ("imputs: write error");

	if (pixbuf != buf)
	    free ((char *)pixbuf);
}


/* IMPUTR -- Put type "real" (single precision floating) pixels to an image.
 * Pixels will be converted from type real to the image pixel datatype if
 * necessary.
 */
imputr_ (u_im, buf, x1, x2, linenum)
unsigned int *u_im;
float	*buf;
int	*x1, *x2;
int	*linenum;
{
	static	int realpix = TY_REAL;
	register struct imhdr *im = (struct imhdr *)*u_im;
	float	*pixbuf;
	int	npix, nbytes;
	char	*impakstr();

	/* Verify image descriptor is ok and section is in bounds.
	 */
	imbounds (im, *x1, *x2, *linenum, *linenum);

	/* An extra buffer is necessary is the data must be type converted,
	 * since we cannot modify the caller's buffer.
	 */
	npix = *x2 - *x1 + 1;
	nbytes = im_pixbufsize (npix, im->im_pixtype);

	if (im->im_pixtype != TY_REAL) {
	    pixbuf = (float *)malloc (nbytes);
	    if (pixbuf == NULL)
		imfatal ("imputr: cannot allocate buffer space");
	} else
	    pixbuf = buf;

	/* Do type conversion, if necessary, writing converted pixels to
	 * temporary buffer.  Otherwise pixels are taken directly from user
	 * buffer.
	 */
	if (im->im_pixtype != TY_REAL)
	    acht_ (buf, pixbuf, &npix, &realpix, &im->im_pixtype);

	/* Write pixels to image.  Will abort if cannot seek or write.
	 */
	imseek (im, *x1, *linenum);
	if (write (im->im_pfnum, (char *)pixbuf, nbytes) != nbytes)
	    imfatal ("imputr: write error");

	if (pixbuf != buf)
	    free ((char *)pixbuf);
}


/* IMBOUNDS -- Verify that the specified section is inbounds.
 */
imbounds (im, x1, x2, y1, y2)
register struct imhdr *im;
int	x1, x2;
int	y1, y2;
{
	register int ncols, nlines;

	/* Verify image descriptor is ok.
	 */
	if (im == NULL || strcmp (impakstr (im->im_magic), "imhdr") != 0)
	    imfatal ("imgets: bad image descriptor");

	/* Verify that image section is in bounds.
	 */
	ncols = im->im_len[0];
	nlines = im->im_len[1];

	if (x1 < 1 || x1 > ncols || x2 < 1 || x2 > ncols)
	    imfatal ("image column number out of range");
	if (y1 < 1 || y1 > nlines || y2 < 1 || y2 > nlines)
	    imfatal ("image line number out of range");
}


/* IMPAKSTR -- Pack an SPP string of type XCHAR into a C string.  The packed
 * string is left in a temporary buffer and will be overwritten the nex time
 * we are called.
 */
char *
impakstr (s)
XCHAR	*s;
{
	static	char packed_string[SZ_PAKSTR+1];
	register XCHAR *ip;
	register char *op;
	register int n;

	ip = s;
	op = packed_string;
	n = SZ_PAKSTR;

	while ((*op++ = *ip++) != XEOS && --n >= 0)
	    ;
	*op = '\0';

	return (packed_string);
}


/* IM_PIXBUFSIZE -- Determine the size of a buffer in bytes required to hold
 * the specified number of pixels of the specified SPP datatype.
 */
im_pixbufsize (npix, datatype)
int	npix;
int	datatype;
{
	switch (datatype) {
	case TY_SHORT:
	    return (npix * sizeof (short));
	case TY_INT:
	    return (npix * sizeof (int));
	case TY_LONG:
	    return (npix * sizeof (long));
	case TY_REAL:
	    return (npix * sizeof (float));
	case TY_DOUBLE:
	    return (npix * sizeof (double));
	case TY_COMPLEX:
	    return (npix * sizeof (float) * 2);
	default:
	    imfatal ("unknown image pixel datatype");
	}
}


/* IMSEEK -- Seek to the specified pixel offset in the pixel storage file.
 */
imseek (im, x, y)
register struct imhdr *im;
int	x, y;
{
	long	pixel_offset;		/* 1-indexed	*/
	long	byte_offset;		/* 0-indexed	*/

	pixel_offset = im->im_physlen[0] * (y - 1) + x - 1;
	byte_offset = (im->im_pixels - 1) * sizeof (XCHAR) +
	    pixel_offset * im_pixbufsize (1, im->im_pixtype);

	if (lseek (im->im_pfnum, byte_offset, 0) == ERR)
	    imfatal ("cannot seek on pixel storage file");
}


/* IMFATALS -- Fatal error in Fortran/IRAF image i/o interface.  Format
 * output error message containing an operand string.
 */
imfatals (errmsg, argstr)
char	*errmsg;
char	*argstr;
{
	char	sbuf[SZ_ERRMSG];

	sprintf (sbuf, errmsg, argstr);
	imfatal (sbuf);
}


/* IMFATAL -- Fatal error in Fortran/IRAF image i/o interface.
 */
imfatal (errmsg)
char	*errmsg;
{
	write (2, errmsg, strlen (errmsg));
	write (2, "\n", 1);
	exit (1);
}
