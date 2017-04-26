#include <stdio.h>
#ifdef linux
/*#include <gnu/types.h>*/
#include <sys/types.h>
#endif
#ifdef ULTRIX
#include <sys/types.h>
#endif
#include <unistd.h>
#define  CDL_LIBRARY_SOURCE
#include "cdl.h"


/*
 * CDLIRAF.C -- Routines to read IRAF OIF images.
 *
 *          cdl_displayIRAF  (cdl, fname, band, frame, fbconfig, zscale)
 *               cdl_isIRAF  (fname)
 *             cdl_readIRAF  (fname, band, &pix, &nx, &ny, &bitpix, title);
 */


#define	VERSION_1	1
#define VERSION_2	2

/* Image header parmeters. */
#define SZ_V1PIXFILE 	79
#define SZ_V1TITLE 	79
#define SZ_V1HDR 	2048

/* Offsets into header (in sizeof(int) units) for various parameters */
#define	IM_V1PIXTYPE	4		/* datatype of the pixels 	*/
#define	IM_V1NDIM	5		/* number of dimensions 	*/
#define	IM_V1LEN	6		/* image dimensions 		*/
#define	IM_V1PHYSLEN	13		/* physical length (as stored) 	*/
#define	IM_V1PIXOFF	22		/* offset of the pixels 	*/
#define	IM_V1PIXFILE	103		/* name of pixel storage file 	*/
#define	IM_V1TITLE	183		/* title of image 		*/

/* Image header parmeters. */
#define SZ_V2PIXFILE 	255
#define SZ_V2TITLE 	383
#define SZ_V2HDR 	1024

/* Offsets into header (in sizeof(int) units) for various parameters */
#define	IM_V2PIXTYPE	10		/* datatype of the pixels 	*/
#define	IM_V2SWAPPED	14		/* number of dimensions 	*/
#define	IM_V2NDIM	18		/* number of dimensions 	*/
#define	IM_V2LEN	22		/* image dimensions 		*/
#define	IM_V2PHYSLEN	50		/* physical length (as stored) 	*/
#define	IM_V2PIXOFF	86		/* offset of the pixels 	*/
#define	IM_V2PIXFILE	126		/* name of pixel storage file 	*/
#define	IM_V2TITLE	638		/* title of image		*/

/* IRAF dataype codes */
#define	TY_CHAR		2
#define	TY_SHORT	3
#define	TY_INT		4
#define	TY_LONG		5
#define	TY_REAL		6
#define	TY_DOUBLE	7
#define	TY_USHORT	11
#define	TY_UCHAR	12

#ifdef __STDC__
#include <stddef.h>
#include <stdlib.h>
#endif

#undef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#undef min
#define min(a,b) ((a) < (b) ? (a) : (b))

extern	int	cdl_debug;


#ifdef ANSI_FUNC

static char *cdl_getIRAFPixels(char *fname, int band, uchar **image, int *nx, int *ny, int *bitpix, char *title);
static int cdl_readImageHdr(char *fname, int *version, int *px, int *py, int *nx, int *ny, int *nbands, int *ptype, int *offset, int *swapped, char *pfile, char *title);
static int cdl_getVersion(FILE *fd);
static void cdl_getPixfile(char *fname, char *pfile);
static int cdl_setBitpix(int ptype, int *pix_size, int *bitpix);
static void cdl_swapPixels(int swapped, int nx, int ny, int ptype, uchar **image);
static void cdl_strpak(char *in, char *out, int len);
static void bswap2(char *a, char *b, int nbytes);
static void bswap4(char *a, int aoff, char *b, int boff, int nbytes);
static void bswap8(char *a, int aoff, char *b, int boff, int nbytes);
static int is_swapped_machine(void);

#else

static char  *cdl_getIRAFPixels();
static void  cdl_getPixfile(), cdl_strpak();
static void  bswap2(), bswap4(), bswap8();
static void  cdl_swapPixels ();
static int   cdl_readImageHdr(), cdl_setBitpix();
static int   cdl_getVersion();
static int   is_swapped_machine();

#endif



/*  CDL_DISPLAYIRAF -- Display an IRAF OIF format image to the given frame,
 *  optionally doing a Zscale transform of the image pixels.
 */

#ifdef ANSI_FUNC

int 
cdl_displayIRAF (
    CDLPtr cdl,                         /* package ptr          */
    char *fname,			/* image name		*/
    int band,				/* image band if 3-d	*/
    int frame,				/* display frame	*/
    int fbconfig,			/* frame buffer config	*/
    int zscale				/* do zscale of image?	*/
)
#else

int
cdl_displayIRAF (cdl, fname, band, frame, fbconfig, zscale)
CDLPtr  cdl;                            /* package ptr          */
char	*fname;				/* image name		*/
int	band;				/* image band if 3-d	*/
int	frame;				/* display frame	*/
int	fbconfig;			/* frame buffer config	*/
int	zscale;				/* do zscale of image?	*/
#endif
{
	char	title[128];
	int	status, nx, ny, bitpix;
	uchar	*pix = NULL;
	float	z1 = 0.0, z2 = 0.0;


	/* See if this is a valid IRAF file. */
	if (!cdl_isIRAF (fname)) {
	    fprintf (stderr, "%s: not a valid IRAF file or doesn't exist.\n",
		fname);
	    return (ERR);
	}

	/* Get the raw IRAF image pixels. */
        if (cdl_readIRAF (fname, band, &pix, &nx, &ny, &bitpix, title))
	    return (ERR);

	if (cdl_debug) {
	    printf("[cdl_displayIRAF] '%s[%d] frame=%d zscale=%d\n",
		fname, band, frame, zscale);
	    printf("[cdl_displayIRAF] %dx%d bitpix=%d pixels z1=%g z2=%g\n",
		nx, ny, bitpix, z1, z2);
	}

        (void) cdl_setTitle (cdl, title);
        (void) cdl_setName (cdl, fname);
	status =  cdl_displayPix (cdl, pix, nx, ny, bitpix, frame, fbconfig,
	    zscale);
	free ((char *) pix);
	return (status);
}


/* CDL_ISIRAF -- Test a file to see if it is a IRAF file.  We check for both
 * V1 and V2 header magic numbers and return the appropriate version if found.
 */

#ifdef ANSI_FUNC

int 
cdl_isIRAF (
    char *fname				/* input filename */
)
#else

int 
cdl_isIRAF (fname)
char	*fname;				/* input filename */
#endif
{
	register FILE *fp;
	int value = 0;
	char magic[24];

        if ((fp = fopen (fname, "r"))) {
            fread (magic, sizeof (char), 12, fp);
	    fclose (fp);

	    /* See if this is a valid OIF header file. */
	    if (strncmp(magic, "imhv2", 5) == 0)
	        value = 2;
	    else {
	        cdl_strpak (magic, magic, 5);
	        if (strncmp(magic, "imhdr", 5) == 0)
	            value = 1;
	    }
	}

	return value;
}


/*  CDL_READIRAF -- Read the pixels from an IRAF OIF format image, returning
 *  the pixel array, dimensions, and image type.
 */

#ifdef ANSI_FUNC

int 
cdl_readIRAF (
    char *fname,			/* image name		*/
    int band,				/* image band if 3-d	*/
    uchar **pix,			/* pixel array (output) */
    int *nx,
    int *ny,				/* dimensions (output)	*/
    int *bitpix,			/* pixel size (output)	*/
    char *title				/* image title		*/
)
#else

int
cdl_readIRAF (fname, band, pix, nx, ny, bitpix, title)
char	*fname;				/* image name		*/
int	band;				/* image band if 3-d	*/
uchar	**pix;				/* pixel array (output) */
int	*nx, *ny;			/* dimensions (output)	*/
int	*bitpix;			/* pixel size (output)	*/
char	*title;				/* image title		*/
#endif
{
	char	*errstr;
	char	*cdl_getIRAFPixels();

	/* See if this is a valid IRAF file. */
	if (!cdl_isIRAF (fname)) {
	    fprintf (stderr, "%s: not a valid IRAF file or doesn't exist.\n",
		fname);
	    return (ERR);
	}

	if ((errstr=cdl_getIRAFPixels(fname,band,pix,nx,ny,bitpix,title))) {
	    fprintf (stderr, "%s\n", errstr);
	    return (ERR);
	}

	if (cdl_debug)
	    printf ("[cdl_readIRAF] '%s[%d]' nx=%d ny=%d bitpix=%d\n",
		fname, band, *nx, *ny, *bitpix);

	return (OK);
}



/* ------------------
 * Private routines.
 * ------------------*/

/*  CDL_GETIRAFPIXELS -- Given an IRAF filename return a pointer to the pixel
 *  array and the image dimensions.
 */

#ifdef ANSI_FUNC

static char *
cdl_getIRAFPixels (
    char *fname,                        /* input filename       */
    int band,				/* image band		*/
    uchar **image,                      /* output pixels        */
    int *nx,
    int *ny,                       	/* dimensions           */
    int *bitpix,			/* pixel size		*/
    char *title				/* image title		*/
)
#else

static char *
cdl_getIRAFPixels (fname, band, image, nx, ny, bitpix, title)
char    *fname;                         /* input filename       */
int	band;				/* image band		*/
uchar   **image;                        /* output pixels        */
int     *nx, *ny;                       /* dimensions           */
int	*bitpix;			/* pixel size		*/
char	*title;				/* image title		*/
#endif
{
	register int i;
	int	 npix, px, py, nbands, pix_size, version, swapped = 0;
	FILE 	 *pf;
	char  	 *line, pixfile[256];
	int 	 ptype, offset;


	/* Read the image header params */
	if (cdl_readImageHdr (fname, &version, &px, &py, nx, ny, &nbands,
	    &ptype, &offset, &swapped, pixfile, title) == ERR)
		return "error reading image header";

	/* Do some simple error checking. */
	if (band > nbands)
	    return "Invalid band request.";
	if (access (pixfile, R_OK) != 0)
	    return "Cannot access pixel file";

	/* Open the pixel file and seek to the beginning of the data. */
        if ((pf = fopen (pixfile, "r")) == NULL)
	    return "Cannot open pixel file.";

	npix = (*nx) * (*ny);
	if (cdl_setBitpix (ptype, &pix_size, bitpix) == ERR) {
	    fclose (pf);
	    return "Invalid pixel type in image";
	}

	/* Now suck up the pixels. */
	*image = (uchar *) malloc (npix * pix_size);
	offset += (band - 1) * (px * py) * pix_size;
	lseek (fileno(pf), (off_t)offset, SEEK_SET);

	if (*nx == px)
            fread ((void *)*image, pix_size, npix, pf);
	else {
	    line = (char *) malloc (px * pix_size);
	    for (i=0; i < (*ny); i++) {
                fread (line, pix_size, px, pf);
		bcopy (line, &((*image)[i*(*nx)*pix_size]), (*nx)*pix_size);
	    }
	    free ((char *)line);
	}

	/* See if we need to swap the bytes. */
	if (version == 2)
	    cdl_swapPixels (swapped, *nx, *ny, ptype, image);

	fclose (pf);
	return NULL;
}


/*  CDL_READIMAGEHDR -- Read the image header information.
 */

#ifdef ANSI_FUNC

static int 
cdl_readImageHdr (
    char *fname,			/* image name		 */
    int *version,			/* OIF version number	 */
    int *px,
    int *py,				/* physical storage dims */
    int *nx,
    int *ny,
    int *nbands,			/* image dims 		 */
    int *ptype,				/* pixel type		 */
    int *offset,			/* offset to pixels	 */
    int *swapped,			/* byte-swapped pixels	 */
    char *pfile,			/* pixfile pathname	 */
    char *title 			/* image title		 */
)
#else

static int
cdl_readImageHdr (fname, version, px, py, nx, ny, nbands, ptype, offset, 
    swapped, pfile, title)
char	*fname;				/* image name		 */
int	*version;			/* OIF version number	 */
int	*px, *py;			/* physical storage dims */
int	*nx, *ny, *nbands;		/* image dims 		 */
int	*ptype;				/* pixel type		 */
int	*offset;			/* offset to pixels	 */
int	*swapped;			/* byte-swapped pixels	 */
char	*pfile;				/* pixfile pathname	 */
char	*title; 			/* image title		 */
#endif
{
	FILE 	*hdr;

        if ((hdr = fopen (fname, "r")) == NULL)
	    return (ERR);

	*version = cdl_getVersion (hdr);
	if (*version == VERSION_1) {
	    int 	header_v1[SZ_V1HDR];
	    char 	pixfile_v1[SZ_V1PIXFILE];

	    /* Read in the image header. */
            fread ((void *)header_v1, sizeof (char), SZ_V1HDR, hdr);

	    /* Get the interesting stuff. */
	    *px = header_v1[IM_V1PHYSLEN];
	    *py = header_v1[IM_V1PHYSLEN+1];
	    *nx = header_v1[IM_V1LEN];
	    *ny = header_v1[IM_V1LEN+1];
	    *ptype = header_v1[IM_V1PIXTYPE];
	    *offset = (header_v1[IM_V1PIXOFF] - 1) * sizeof(short);
	    *nbands = header_v1[IM_V1LEN+2];
	    *swapped = 0;

	    /* Find the pixfile and see if it exists. */
	    cdl_strpak ((char *)&header_v1[IM_V1PIXFILE], pixfile_v1,
		SZ_V1PIXFILE);
	    cdl_getPixfile (fname, pixfile_v1);
	    (void) strcpy (pfile, pixfile_v1);

	    /* Find the image title string */
	    if (title == (char *)NULL)
		title = (char *) malloc (SZ_V1TITLE+1);
	    cdl_strpak ((char *)&header_v1[IM_V1TITLE], title, SZ_V1TITLE);

	} else if (*version == VERSION_2) {
	    char	header_v2[SZ_V2HDR], *tp;
	    char	pixfile_v2[SZ_V2PIXFILE];

	    /* Read in the image header. */
            fread (header_v2, sizeof (char), SZ_V2HDR, hdr);

	    /* Get the interesting stuff. */
            
            if (is_swapped_machine())
                bswap4 (&header_v2[IM_V2SWAPPED], 1, (char *)swapped, 
		    1, sizeof(int));
            else
                bcopy ((char *)&header_v2[IM_V2SWAPPED], (char *)swapped, 
		    sizeof(int));

	    if (is_swapped_machine()) {
	        bswap4 (&header_v2[IM_V2PHYSLEN], 1, (char *)px, 1,
		    sizeof(int));
	        bswap4 (&header_v2[IM_V2PHYSLEN+sizeof(int)], 1,
		    (char *)py, 1, sizeof(int));
	        bswap4 (&header_v2[IM_V2LEN], 1, (char *)nx, 1,
		    sizeof(int));
	        bswap4 (&header_v2[IM_V2LEN+sizeof(int)], 1, (char *)ny,
		    1, sizeof(int));
	        bswap4 (&header_v2[IM_V2PIXTYPE], 1, (char *)ptype, 1, 
		    sizeof(int));
	        bswap4 (&header_v2[IM_V2PIXOFF], 1, (char *)offset, 1, 
		    sizeof(int));
	        bswap4 (&header_v2[IM_V2LEN+(2*sizeof(int))], 1,
		    (char *)nbands, 1, sizeof(int));
	    } else {
	        bcopy ((char *)&header_v2[IM_V2PHYSLEN], px, sizeof(int));
	        bcopy ((char *)&header_v2[IM_V2PHYSLEN+sizeof(int)], py,
		    sizeof(int));
	        bcopy ((char *)&header_v2[IM_V2LEN], nx, sizeof(int));
	        bcopy ((char *)&header_v2[IM_V2LEN+sizeof(int)], ny,
		    sizeof(int));
	        bcopy ((char *)&header_v2[IM_V2PIXTYPE], ptype, sizeof(int));
	        bcopy ((char *)&header_v2[IM_V2PIXOFF], offset, sizeof(int));
	        bcopy ((char *)&header_v2[IM_V2LEN+(2*sizeof(int))], nbands,
		    sizeof(int));
	    }
 	    *offset = (*offset - 1) * sizeof(short); 

	    /* Find the pixfile and see if it exists. */
 	    bcopy ((char *)&header_v2[IM_V2PIXFILE], pixfile_v2, SZ_V2PIXFILE); 
	    cdl_getPixfile (fname, pixfile_v2);
	    (void) strcpy (pfile, pixfile_v2);

	    /* Find the image title string */
	    tp = (char *)&header_v2[IM_V2TITLE];
	    if (title == (char *)NULL)
		title = (char *) malloc (SZ_V2TITLE+1);
	    strcpy (title, (char *)tp);
	}

	if (cdl_debug) {
	    printf("[cdl_readImageHdr] px,py=%d,%d nx,ny,nb=%d,%d,%d\n\t",
	        *px, *py, *nx, *ny, *nbands);
	    printf("ptype=%d offset=%d swap=%d pixfile='%s' title='%s'\n",
		*ptype, *offset, swapped, pfile, title);
	}

	fclose (hdr); 			/* we're done with the header */
	return (OK);
}


/* CDL_GETVERSION -- Return image header version.
 */

#ifdef ANSI_FUNC

static int 
cdl_getVersion (
    FILE *fd				/* input filename */
)
#else

static int 
cdl_getVersion (fd)
FILE	*fd;				/* input filename */
#endif
{
	int 	value = 0;
	char 	magic[24];

	rewind (fd);
        fread (magic, sizeof (char), 12, fd);
	rewind (fd);

	/* See if this is a valid OIF header file. */
	if (strncmp(magic, "imhv2", 5) == 0)
            value = 2;
	else {
	    cdl_strpak (magic, magic, 5);
	    if (strncmp(magic, "imhdr", 5) == 0)
	        value = 1;
	}
	return value;
}


/*  CDL_GETPIXFILE -- Get the pixelfile pathname.
 */

#ifdef ANSI_FUNC

static void 
cdl_getPixfile (char *fname, char *pfile)
#else

static void
cdl_getPixfile (fname, pfile)
char	*fname;
char	*pfile;
#endif
{
	char	temp[SZ_V1PIXFILE], *ip;
	int	len;
	char  	*index();

        if (strncmp (pfile, "HDR$", 4) == 0) {
            /* Handle the special case of a HDR$ pixfile path, prepend the
             * current working directory name to the pixfile.
             */
	    ip = pfile + 4;
            (void) strncpy (temp, ip, SZ_V1PIXFILE);
            (void) strncpy (pfile, fname, SZ_V1PIXFILE);

            /* Find the end of the pathname. */
            len = strlen (pfile);
            while ( (len > 0) && (pfile[len-1] != '/') )
              len--;

            /* Add the image name. */
            pfile[len] = '\0';
            (void) strncat (pfile, temp, SZ_V1PIXFILE);

        } else if (index (pfile, '!') != NULL) {
            /* Strip out the leading node! prefix from the pixfile path.  */
            for (ip = pfile; *ip != '!' ; )
                ip++;
            (void) strcpy (pfile, ++ip);
        }
}


/*  CDL_SETBITPIX -- Set the number of pixels for a given pixel type.
 */

#ifdef ANSI_FUNC

static int 
cdl_setBitpix (int ptype, int *pix_size, int *bitpix)
#else

static int
cdl_setBitpix (ptype, pix_size, bitpix)
int	ptype;
int	*pix_size;
int	*bitpix;
#endif
{
	switch (ptype) {
	case TY_USHORT:
	case TY_SHORT:
	    *pix_size = sizeof (short);
	    *bitpix = 16;
	    break;
	case TY_INT:
	    *pix_size = sizeof (int);
	    *bitpix = 32;
	    break;
	case TY_LONG:
	    *pix_size = sizeof (long);
	    *bitpix = 32;
	    break;
	case TY_REAL:
	    *pix_size = sizeof (float);
	    *bitpix = -32;
	    break;
	case TY_DOUBLE:
	    *pix_size = sizeof (double);
	    *bitpix = -64;
	    break;
	default:
	    return (ERR);
	}
	return (OK);
}


/*  CDL_SWAPPIXELS -- Byte swap pixels if necessary.
 */

#ifdef ANSI_FUNC

static void 
cdl_swapPixels (int swapped, int nx, int ny, int ptype, uchar **image)
#else

static void
cdl_swapPixels (swapped, nx, ny, ptype, image)
int	swapped;
int	nx, ny;
int	ptype;
uchar	**image;
#endif
{
	if ((swapped != is_swapped_machine())) {
	    switch (ptype) {
	    case TY_USHORT:
	    case TY_SHORT:
	        bswap2 ((char *)*image, (char *)*image, 
	          	nx * ny * sizeof(short));
	        break;
	    case TY_INT:
	    case TY_LONG:
	        bswap4 ((char *)*image, 1, (char *)*image, 1, 
	          	nx * ny * sizeof(int));
	        break;
	    case TY_REAL:
	        bswap4 ((char *)*image, 1, (char *)*image, 1, 
	          	nx * ny * sizeof(float));
	        break;
	    case TY_DOUBLE:
	        bswap8 ((char *)*image, 1, (char *)*image, 1, 
	          	nx * ny * sizeof(double));
	        break;
	    }
	}
}


/*  CDL_STRPAK -- Convert ASCII string from SPP char per short to C char
 *  per byte.
 */

#ifdef ANSI_FUNC

static void 
cdl_strpak (char *in, char *out, int len)
#else

static void 
cdl_strpak (in, out, len)
char	*in, *out;
int	len;
#endif
{
	int	i, j;

	/* Adaptive byte selection (whichever byte order) chars alternate 
	 * with \0.
	 */
	j = (in[0] == '\0' ? 1 : 0);

	for (i = 0; i < len; i++, j += 2)
	    out[i] = in[j];
	out[i] = '\0';
}


/* BSWAP2 - Move bytes from array "a" to array "b", swapping successive
 * pairs of bytes.  The two arrays may be the same but may not be offset
 * and overlapping.
 */

#ifdef ANSI_FUNC

static void 
bswap2 (
    char *a,
    char *b,         		/* input array                  */
    int nbytes         		/* number of bytes to swap      */
)
#else

static void
bswap2 (a, b, nbytes)
char    *a, *b;         	/* input array                  */
int     nbytes;         	/* number of bytes to swap      */
#endif
{
        register char *ip=a, *op=b, *otop;
        register unsigned temp;

        /* Swap successive pairs of bytes.
         */
        for (otop = op + (nbytes & ~1);  op < otop;  ) {
            temp  = *ip++;
            *op++ = *ip++;
            *op++ = temp;
        }

        /* If there is an odd byte left, move it to the output array.
         */
        if (nbytes & 1)
            *op = *ip;
}


/* BSWAP4 - Move bytes from array "a" to array "b", swapping the four bytes
 * in each successive 4 byte group, i.e., 12345678 becomes 43218765.
 * The input and output arrays may be the same but may not partially overlap.
 */

#ifdef ANSI_FUNC

static void 
bswap4 (
    char *a,			/* input array			*/
    int aoff,			/* first byte in input array	*/
    char *b,			/* output array			*/
    int boff,			/* first byte in output array	*/
    int nbytes			/* number of bytes to swap	*/
)
#else

static void
bswap4 (a, aoff, b, boff, nbytes)
char	*a;			/* input array			*/
int	aoff;			/* first byte in input array	*/
char	*b;			/* output array			*/
int	boff;			/* first byte in output array	*/
int	nbytes;			/* number of bytes to swap	*/
#endif
{
	register char	*ip, *op, *tp;
	register int	n;
	static	char temp[4];

	tp = temp;
	ip = (char *)a + aoff - 1;
	op = (char *)b + boff - 1;

	/* Swap successive four byte groups.
	 */
	for (n = nbytes >> 2;  --n >= 0;  ) {
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	}

	/* If there are any odd bytes left, move them to the output array.
	 * Do not bother to swap as it is unclear how to swap a partial
	 * group, and really incorrect if the data is not modulus 4.
	 */
	for (n = nbytes & 03;  --n >= 0;  )
	    *op++ = *ip++;
}


/* BSWAP8 - Move bytes from array "a" to array "b", swapping the eight bytes
 * in each successive 8 byte group, i.e., 12345678 becomes 87654321.
 * The input and output arrays may be the same but may not partially overlap.
 */
#ifdef ANSI_FUNC

static void 
bswap8 (
    char *a,			/* input array			*/
    int aoff,			/* first byte in input array	*/
    char *b,			/* output array			*/
    int boff,			/* first byte in output array	*/
    int nbytes			/* number of bytes to swap	*/
)
#else

static void
bswap8 (a, aoff, b, boff, nbytes)
char	*a;			/* input array			*/
int	aoff;			/* first byte in input array	*/
char	*b;			/* output array			*/
int	boff;			/* first byte in output array	*/
int	nbytes;			/* number of bytes to swap	*/
#endif
{
	register char	*ip, *op, *tp;
	register int	n;
	static	char temp[8];

	tp = temp;
	ip = (char *)a + aoff - 1;
	op = (char *)b + boff - 1;

	/* Swap successive eight byte groups.
	 */
	for (n = nbytes >> 3;  --n >= 0;  ) {
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	}

	/* If there are any odd bytes left, move them to the output array.
	 * Do not bother to swap as it is unclear how to swap a partial
	 * group, and really incorrect if the data is not modulus 8.
	 */
	for (n = nbytes & 03;  --n >= 0;  )
	    *op++ = *ip++;
}


#ifdef ANSI_FUNC

static int 
is_swapped_machine (void)
#else

static int
is_swapped_machine ()
#endif
{
	union { char ch[4]; int i; } u;
	u.i = 1;
	return (u.ch[0]);
}
