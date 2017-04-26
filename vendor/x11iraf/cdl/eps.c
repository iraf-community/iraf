#include <stdio.h>
#include <math.h>
#include <time.h>
#include <pwd.h>
#define  CDL_LIBRARY_SOURCE
#include "cdl.h"

#ifdef __STDC__
#include <stdlib.h>
#include <string.h>
#endif

#ifdef SOLARIS
#include <sys/systeminfo.h>
#endif

#ifdef SVR4
#include <sys/types.h>
#include <unistd.h>
#endif


/* 
 * EPS.C -- EPS hardcopy routines.  This code takes an input structure
 * containing the pixels, colormap, and output specifications and creates
 * and Encapsulated PostScript file.
 *
 *     #include "eps.h"			# flags, macro defs, etc.
 *
 *	     psim = eps_init ()
 *                 eps_print (psim, fp, &data, xdim, ydim, depth, pad)
 *	           eps_close (psim)
 *
 *               eps_setPage (psim, orient, paper_size, scale, flags)
 *	         eps_setCmap (psim, &r, &g, &b, ncolors)
 *           eps_setCompress (psim, ctype)
 *          eps_setColorType (psim, otype)
 *	        eps_setLabel (psim, &label)
 *	    eps_setTransform (psim, z1, z2, ztype, offset, slope, cmap_name)
 *            eps_setCorners (psim, llx, lly, urx, ury)
 *
 *	    eps_getImageSize (psim, xdim, ydim, &width, &height)
 *	     eps_getImagePos (psim, xdim, ydim, &llx, &llx)
 *
 *     # Read-only Macro functions:	  Result type
 *     ----------------------------	  -----------
 * 		 Orientation (psim)	  EPS_PORTRAIT | EPS_LANDSCAPE
 * 		       Scale (psim)	  float scale value
 * 		    MaxScale (psim)	  float maxaspect scale value
 * 		 DoAutoScale (psim)	  bool
 * 		DoAutoRotate (psim)	  bool
 * 		 DoMaxAspect (psim)	  bool
 * 		    PageType (psim)	  EPS_LETTER | EPS_LEGAL | ...
 *		    PageXdim (psim)	  page pixel width
 *		    PageYdim (psim)	  page pixel height
 *		   PageWidth (psim)	  page size in inches
 *		  PageHeight (psim)	  page size in inches
 *  
 * The first three routines are all that's required to output an array of
 * pixels to an open file descriptor.  The default output produced is a
 * grayscale EPS file, centered in a portrait orientation (unless a rotation
 * is required to make it fit on the page) and auto scaled to fit on a normal
 * 8.5"x11" page.
 *
 * The remaining routines can be used to set various options, new colormaps
 * labels, etc.  They are not required unless you need to override one or
 * more of the output defaults.
 */

#define	MAX_LENLABEL		256
#define	SZ_EPSBUF		8193

#undef  min
#undef  max

static char	*hextab = "0123456789abcdef";
static int	pixnum 	= 0, lpix = 0;
static uchar   *pixbuf;


#define PageXdim(ps)    ( PageInfo[ps->page.page_type].npixx )
#define PageYdim(ps)    ( PageInfo[ps->page.page_type].npixy )       
#define PageWidth(ps)   ( PageInfo[ps->page.page_type].sizex )
#define PageHeight(ps)  ( PageInfo[ps->page.page_type].sizey )       

typedef struct {
        float   sizex, sizey;           /* page size in inches */
        int     npixx, npixy;           /* pix resolution at 72 dpi */
} PSPageInfo;

static PSPageInfo  PageInfo[] = {       /* assumes 300 dpi */
        { 8.500, 11.000,                /* US NORMAL, aka LETTER */
          612, 762 
        },   
        { 8.500, 14.000,                /* US LEGAL */
          612, 1008
        },
        { 8.267, 11.811,                /* A4 */
          595, 850
        },
        { 7.283, 10.630,                /* B5 */
          524, 765
        },
        {11.000, 17.000,                /* B-size */
          762, 1224
        },
        { 3.875,  4.875,                /* 4 by 5 */
          279, 351
        },
        { 0.945,  1.417,                /* 35mm (24x36) */
          68, 102
        }
};



#ifdef ANSI_FUNC

static void eps_pageParams(PSImagePtr psim, float *llx, float *lly, int *icols, int *irows, int *scols, int *srows, int *turnflag);
static void eps_writePix(FILE *fp, uchar *pix, PSCmap *cmap, int npix, int xdim, int pad);
static void eps_writeMono(FILE *fp, uchar *pix, PSCmap *cmap, int npix, int xdim, int pad);
static void eps_writeMonoRGB(FILE *fp, uchar *pix, int npix, int xdim, int depth, int pad);
static void eps_writeRGB(FILE *fp, uchar *pix, PSCmap *cmap, int npix, int xdim, int depth, int pad);
static void eps_writeCmap(PSCmap *cmap, FILE *fp);
static void eps_simpleHeader(PSImagePtr psim, FILE *fp);
static void eps_annotate(PSImagePtr psim, FILE *fp);
static void eps_portLabels(FILE *fp, PSImagePtr psim, int scols, int srows, int icols, int irows, float llx, float lly);
static void eps_landLabels(FILE *fp, PSImagePtr psim, int scols, int srows, int icols, int irows, float llx, float lly);
static void eps_doColorbar(FILE *fp, PSImagePtr psim, int scols, int srows, float llx, float lly, int turnflag);
static void eps_colorHeader(PSImagePtr psim, FILE *fp);
static void eps_writeTrailer(FILE *fp);
static void eps_simpleTrailer(FILE *fp);
static void eps_flushPix(FILE *fp);
static float ticstep(float range, int nsteps);
static char *make_label(void);

#else

static void eps_simpleHeader(), eps_colorHeader();
static void eps_pageParams(), eps_simpleTrailer();
static void eps_writeCmap(), eps_writeTrailer();
static void eps_writeMono(), eps_writePix();
static void eps_writeMonoRGB(), eps_writeRGB();
static void eps_putPix(), eps_flushPix();
static void eps_annotate(), eps_portLabels(), eps_landLabels();
static void eps_doColorbar();
static float ticstep();
static char *make_label();

#endif


static int 	debug = 0;


/* EPS_INIT -- Allocate and return an initialized pointer to a structure
 * containing the default output setup.
 */
#ifdef ANSI_FUNC

PSImage *
eps_init (void)
#else

PSImage *
eps_init()
#endif
{
	register int i;
	register PSImage *ps;

        /* Allocate the structure. */
	ps = (PSImage *) calloc ((unsigned)1, sizeof (PSImage));

        /* Initialize the structure. */
        ps->cols = 0;
        ps->rows = 0;
        ps->colorClass = EPS_GRAYSCALE;
        ps->compression = NoCompression;
        ps->annotate = 1;
        ps->label = (char *) calloc (MAX_LENLABEL, sizeof (uchar));
	ps->z1 = 0.0;
	ps->z2 = 255.0;
	ps->ztype = 0;

	/* Set up a default grayscale colormap. */
        ps->cmap.ncolors = 256;
        for (i=0; i<ps->cmap.ncolors; i++)
            ps->cmap.r[i] = ps->cmap.g[i] = ps->cmap.b[i] = i;
	ps->cmap.IsDefault = 1;

	/* Load the page information defaults. */
	eps_setPage (ps, EPS_PORTRAIT, EPS_LETTER, 100, EPS_AUTOSCALE);

	/* Initialize the pixel buffer array */
	pixbuf = (uchar *) calloc (SZ_EPSBUF, sizeof (uchar));

	return (ps);
}


/* EPS_PRINT -- Dump the given array of pixels to the output file as an EPS
 * format file.  The data array may contain 8-bit, 24-bit RGB triplets, or
 * 32-bit RGBA pixels.  
 */
#ifdef ANSI_FUNC

void 
eps_print (
    PSImage *psim,				/* EPS image structure */
    FILE *fp,				/* output file descriptor */
    uchar *data,				/* array of image pixels */
    int xdim,
    int ydim,			/* image dimensions */
    int depth,				/* bits / pixel, must be 8,24, or 32 */
    int pad				/* bytes per line of padding */
)
#else

void
eps_print (psim, fp, data, xdim, ydim, depth, pad)
PSImage *psim;				/* EPS image structure */
FILE	*fp;				/* output file descriptor */
uchar   *data;				/* array of image pixels */
int	xdim, ydim;			/* image dimensions */
int	depth;				/* bits / pixel, must be 8,24, or 32 */
int	pad;				/* bytes per line of padding */
#endif
{
	int npix = xdim * ydim;
        uchar *pix = data;

	psim->cols = xdim;		/* save it for the headers */
	psim->rows = ydim;

	if (debug)
	    fprintf (stderr, "eps_print: w=%d h=%d d=%d pad=%d\n",
		xdim, ydim, depth, pad);

        switch (psim->colorClass) {
        case EPS_GRAYSCALE:
	    /* Regardless of the color type desired we'll either write the
	     * pixels directly or convert from pseudocolor or RGB.  For a
	     * start dump the header. 
	     */
	    eps_simpleHeader (psim, fp);

	    if (psim->cmap.IsDefault && depth == 8) {
		/* We're using the default grayscale colormap with an 
		 * 8-bit image, so just dump the pixels.
		 */
	        if (debug)
	 	    fprintf (stderr,"eps_print: straight grayscale.\n");
		eps_writePix (fp, pix, &psim->cmap, npix, xdim, pad);

	    } else if (!psim->cmap.IsDefault && depth == 8) {
		/* We're using an arbitrary colormap with an 8-bit image, 
	   	 * so convert to grayscale and write the pixels.
		 */
	        if (debug)
		    fprintf (stderr,"eps_print: pseudo cnv to grayscale.\n");
		eps_writeMono (fp, pix, &psim->cmap, npix, xdim, pad);

	    } else {
                /* Convert RGBA to grayscale. */
	        if (debug)
		    fprintf (stderr,"eps_print: RGB cnv to grayscale.\n");
		eps_writeMonoRGB (fp, pix, npix, xdim, depth, pad);

	    }

	    /* Now that the image is out we restore the graphics context. */
            fprintf (fp, "grestore\n" );

	    /* Write the colorbar. */
	    if (psim->annotate)
	        eps_annotate (psim, fp);

	    /* Write the trailer. */
	    eps_simpleTrailer (fp);
            break;

        case EPS_PSEUDOCOLOR:
	    /* Write the color prolog header. */
	    eps_colorHeader (psim, fp);

	    /* Dump the colormap. */
            eps_writeCmap (&psim->cmap, fp);

            /* Write color indices. */
	    eps_writePix (fp, pix, &psim->cmap, npix, xdim, pad);

	    /* Write the colorbar. */
	    if (psim->annotate)
	        eps_annotate (psim, fp);

	    /* Write the trailer to finish it off. */
	    eps_writeTrailer (fp);
            break;

        case EPS_TRUECOLOR:
            /* Write the seudocolor prolog header. */
            eps_colorHeader (psim, fp);

	    eps_writeRGB (fp, pix, &psim->cmap, npix, xdim, depth, pad);

	    /* Write the colorbar. */
	    if (psim->annotate)
	        eps_annotate (psim, fp);

	    /* Write the trailer to finish it off. */
	    eps_writeTrailer (fp);
            break;

        default:
            perror ("Bad colorClass specification.\n");
            break;
        }

	fflush (fp);
}


/* EPS_CLOSE -- Close down the EPS output structure.
 */
#ifdef ANSI_FUNC

void 
eps_close (
    PSImage *psim				/* EPS image structure */
)
#else

void
eps_close (psim)
PSImage *psim;				/* EPS image structure */
#endif
{
	if (psim->label)
	    (void) free ((char *) psim->label);
	(void) free ((char *) pixbuf);
	(void) free (psim);
}


/* EPS_SETPAGE -- Inialize the page setup with a different size, orientation,
 * or scale options.
 */
#ifdef ANSI_FUNC

void 
eps_setPage (
    PSImage *psim,				/* EPS image structure */
    int orientation,			/* page orientation flag */
    int paper_size,			/* paper size flag */
    int scale,				/* image scale percentage */
    int flags				/* option flags */
)
#else

void
eps_setPage (psim, orientation, paper_size, scale, flags)
PSImage *psim;				/* EPS image structure */
int	orientation;			/* page orientation flag */
int	paper_size;			/* paper size flag */
int	scale;				/* image scale percentage */
int	flags;				/* option flags */
#endif
{
	int NPageTypes = sizeof (PageInfo) / sizeof (PSPageInfo);

	/* Set the orientation of the output */
	if (orientation == EPS_PORTRAIT)
            psim->page.orientation = EPS_PORTRAIT;
	else if (orientation == EPS_LANDSCAPE)
            psim->page.orientation = EPS_LANDSCAPE;
	else 
	    perror ("eps_setPage: Invalid orientation.");

	/* Set the output paper size */
	if (paper_size >= 0 && paper_size <= NPageTypes)
            psim->page.page_type = paper_size;
	else 
	    perror ("eps_setPage: Invalid paper size.");

	/* Set scale and recompute dpi resolution. */
        psim->page.scale = (float) scale / 100.0;
        psim->page.maxscale = (float) scale / 100.0;
        psim->page.dpi = 72;			/* need to recalculate */

	/* Set the option flags */
        psim->page.flags = flags;

	if (debug) {
	    fprintf (stderr,
		"eps_setPage: orientation:%d size:%d scale:%g\n",
	        Orientation(psim), PageType(psim), Scale(psim));
	    fprintf (stderr,"\tautoscale:%d autorotate:%d maxaspect:%d\n",
	        DoAutoScale(psim), DoAutoRotate(psim), DoMaxAspect(psim));
	}
}


/* EPS_SETCMAP -- Define a given colormap to be used on output.
 */
#ifdef ANSI_FUNC

void 
eps_setCmap (
    PSImage *psim,				/* EPS image structure */
    uchar *r,
    uchar *g,
    uchar *b,			/* color components */
    int ncolors				/* number of colors in colormap */
)
#else

void
eps_setCmap (psim, r, g, b, ncolors)
PSImage *psim;				/* EPS image structure */
uchar *r, *g, *b;			/* color components */
int ncolors;				/* number of colors in colormap */
#endif
{
	register int i = 0;

	/* Load the colormap. */
	psim->cmap.ncolors = ncolors;
	/* for (i=0;  i < ncolors;  i++) { */
	for (i=0;  i < 256;  i++) {
	    psim->cmap.r[i] = r[i];
	    psim->cmap.g[i] = g[i];
	    psim->cmap.b[i] = b[i];
	}

	psim->cmap.IsDefault = 0;
}


/* EPS_SETCOMPRESS -- Define the type of output compression to use.
 */
#ifdef ANSI_FUNC

void 
eps_setCompress (
    PSImage *psim,				/* EPS image structure */
    int compress			/* compression type flag */
)
#else

void
eps_setCompress (psim, compress)
PSImage *psim;				/* EPS image structure */
int	compress;			/* compression type flag */
#endif
{
	/* Set the compression type to use. */
	switch (compress) {
	case NoCompression:
	case RLECompression:
	    psim->compression = compress;
	    break;
	case LZWCompression:
	case JPEGCompression:
	default:
	    perror ("eps_setCompress: Invalid compression type.");
	}
}


/* EPS_SETCOLORTYPE -- Set the type of output image to be written, e.g. even
 * though we have an RGB or pseudocolor image we may wish to coerce it to 
 * a grayscale on output.
 */
#ifdef ANSI_FUNC

void 
eps_setColorType (
    PSImage *psim,				/* EPS image structure */
    int color_class			/* output color class */
)
#else

void
eps_setColorType (psim, color_class)
PSImage *psim;				/* EPS image structure */
int	color_class;			/* output color class */
#endif
{
	/* Set the compression type to use. */
	switch (color_class) {
	case EPS_GRAYSCALE:
	case EPS_PSEUDOCOLOR:
	case EPS_TRUECOLOR:
	    psim->colorClass = color_class;
	    break;
	default:
	    perror ("eps_setColorType: Invalid output color type.");
	}
}


/* EPS_SETLABEL -- Set the output label to be used in annotation.
 */
#ifdef ANSI_FUNC

void 
eps_setLabel (
    register PSImage *psim,			/* EPS image structure */
    char *label				/* Label string */
)
#else

void
eps_setLabel (psim, label)
register PSImage *psim;			/* EPS image structure */
char	*label;				/* Label string */
#endif
{
	register int maxlen = MAX_LENLABEL;

	if (!psim->label)
	    psim->label = (char *) malloc (maxlen);

	(void) strncpy (psim->label, label, maxlen-1);
	psim->label[maxlen-1] = '\0';
}


/* EPS_SETTRANSFORM -- Set the color transformation parameters, i.e. the
 * z1/z2 values that map the image pixel values being displayed to the number
 * of colors we have available.  This is used in the annotation when labeling
 * the colorbar.
 */
#ifdef ANSI_FUNC

void 
eps_setTransform (
    PSImage *psim,                          /* EPS image structure  	*/
    float z1,
    float z2,				/* zscale values 		*/
    int ztype,				/* Transformation type  	*/
    float offset,
    float scale,			/* brightness/contrast values 	*/
    char *cmap_name			/* colormap name		*/
)
#else

void
eps_setTransform (psim, z1, z2, ztype, offset, scale, cmap_name)
PSImage *psim;                          /* EPS image structure  	*/
float	z1, z2;				/* zscale values 		*/
int	ztype;				/* Transformation type  	*/
float	offset, scale;			/* brightness/contrast values 	*/
char	*cmap_name;			/* colormap name		*/
#endif
{
        psim->z1 = z1;
        psim->z2 = z2;
        psim->ztype = ztype;
        psim->offset = offset;
        psim->scale = scale;
	if (!psim->cmap.cmap_name)
	    psim->cmap.cmap_name = (char *) malloc (16);
	strcpy (psim->cmap.cmap_name, cmap_name);

	if (debug) {
	    fprintf (stderr, 
	       "setTransform: z1=%g z2=%g zt=%d offset=%g scale=%g name='%s'\n",
		psim->z1, psim->z2, psim->ztype, 
		psim->offset, psim->scale,
		psim->cmap.cmap_name);
	}
}


/* EPS_SETCORNERS -- Set the image corner values.
 */
#ifdef ANSI_FUNC

void 
eps_setCorners (
    PSImage *psim,                          /* EPS image structure  	*/
    int llx,
    int lly,
    int urx,
    int ury		/* image corners		*/
)
#else

void
eps_setCorners (psim, llx, lly, urx, ury)
PSImage *psim;                          /* EPS image structure  	*/
int	llx, lly, urx, ury;		/* image corners		*/
#endif
{
	psim->llx = llx;
	psim->lly = lly;
	psim->urx = urx;
	psim->ury = ury;
}


/* EPS_GETIMAGESIZE -- Given the current page parameters and image dimensions,
 * compute the size of the image (in inches) on the page.
 */       
#ifdef ANSI_FUNC

void 
eps_getImageSize (
    PSImagePtr psim,			/* EPS image struct */
    int xdim,
    int ydim,			/* image dimensions */
    float *width,
    float *height		/* width x height of image on page */
)
#else

void
eps_getImageSize (psim, xdim, ydim, width, height)
PSImagePtr psim;			/* EPS image struct */
int	xdim, ydim;			/* image dimensions */
float	*width, *height;		/* width x height of image on page */
#endif
{
        int icols, irows, scols, srows, turnflag;
	float	llx, lly;

        /* Get the common page parameters. */
        eps_pageParams (psim, &llx, &lly, &icols, &irows, &scols, &srows,
	    &turnflag);

	*width = (scols / (float) psim->page.dpi);
	*height = (srows / (float) psim->page.dpi);
}


/* EPS_GETIMAGEPOS -- Given the current page parameters and image dimensions,
 * compute the position of the image (in pixels) on the page.
 */	        
#ifdef ANSI_FUNC

void 
eps_getImagePos (
    PSImagePtr psim,			/* EPS image struct */
    int xdim,
    int ydim,			/* image dimensions */
    int *llx,
    int *lly			/* LL coords for centered image */
)
#else

void
eps_getImagePos (psim, xdim, ydim, llx, lly)
PSImagePtr psim;			/* EPS image struct */
int	xdim, ydim;			/* image dimensions */
int	*llx, *lly;			/* LL coords for centered image */
#endif
{
        int icols, irows, scols, srows, turnflag;
	float	lx, ly;

        /* Get the common page parameters. */
        eps_pageParams (psim, &lx, &ly, &icols, &irows, &scols, &srows,
	    &turnflag);

	*llx = (int) lx;
	*lly = (int) ly;
}


/************************
 *  Private Procedures  *
 ************************/


/* EPS_PAGEPARAMS -- Compute the EPS page parameters.
 */
#ifdef ANSI_FUNC

static void 
eps_pageParams (
    PSImagePtr psim,			/* EPS image struct */
    float *llx,
    float *lly,			/* LL coords for centered image */
    int *icols,
    int *irows,			/* final image rows/cols */
    int *scols,
    int *srows,			/* scaled rows/cols */
    int *turnflag			/* turn the image? */
)
#else

static void 
eps_pageParams (psim, llx, lly, icols, irows, scols, srows, turnflag)
PSImagePtr psim;			/* EPS image struct */
float	*llx, *lly;			/* LL coords for centered image */
int	*icols, *irows;			/* final image rows/cols */
int	*scols, *srows;			/* scaled rows/cols */
int	*turnflag;			/* turn the image? */
#endif
{
        int devpix, pwidth, pheight, cols=0, rows=0;
        float pixfac, scale = Scale(psim), margin;

        /* See if we need to rotate the image to fit on the page. */
	margin = (psim->annotate ? 0.9 : 0.95);
	pwidth = (int) PageXdim(psim) * margin;
	pheight = (int) PageYdim(psim) * margin;
        *icols = cols = psim->cols;
        *irows = rows = psim->rows;
        *turnflag = 0;
        if (DoAutoRotate(psim)) {
            if (psim->cols > psim->rows && (scale * psim->cols) > pwidth) {
	        if (debug) fprintf (stderr, "Rotating image....\n");
                *turnflag = 1;
                cols = *irows;
                rows = *icols;
	    }
        } else if (psim->page.orientation == EPS_LANDSCAPE) {
	        if (debug) fprintf (stderr, "Rotating image....\n");
                *turnflag = 1;
                cols = *irows;
                rows = *icols;
        }
	
	if (turnflag) {
	    margin = (turnflag ? 0.825 : margin);
	    pwidth = (int) PageXdim(psim) * margin;
	    pheight = (int) PageYdim(psim) * margin;
	}

        /* Figure out the image size. */
        devpix = psim->page.dpi / 72.0 + 0.5;      /* device pixels per unit */
        pixfac = 72.0 / psim->page.dpi * devpix;
        *scols = psim->page.scale * cols * pixfac;
        *srows = psim->page.scale * rows * pixfac;

        /* See if we need to fiddle with the size to get it on the page
	 * the way we want.
	 */
	if (debug) {
	    fprintf (stderr, "before: scale=%g scols=%d srows=%d pixfac=%g\n",
		scale, *scols, *srows, pixfac);
	    fprintf (stderr, "\tpwidth=%d pheight=%d\n", pwidth, pheight);
	    fprintf (stderr,"\tautoscale:%d autorotate:%d maxaspect:%d\n",
	        DoAutoScale(psim), DoAutoRotate(psim), DoMaxAspect(psim));
	}
        if ( *scols > pwidth || *srows > pheight ) {

	    /* Image is larger than we think will fit on the page. If we're
	     * autoscaling reset the scale.
	     */
            if (DoAutoScale(psim)) {
	        if (debug)
		    fprintf (stderr, "Image too big, autoscaling...\n");
                if ( *scols >= pwidth ) {
		    if (*scols >= *srows) {
                        scale *= (float)PageXdim(psim) / (float)*scols * margin;
                        *scols = scale * cols * pixfac;
                        *srows = scale * rows * pixfac;
		    } else {
                        scale *= (float)PageYdim(psim) / (float)*srows * margin;
                        *scols = scale * cols * pixfac;
                        *srows = scale * rows * pixfac;
		    }
                }
	        if (debug)
		    fprintf (stderr, ": scale=%f scols=%d srows=%d pixfac=%g\n",
			scale, *scols, *srows, pixfac);
                if ( *srows >= pheight ) {
		    if (*scols >= *srows) {
                        scale *= (float)PageXdim(psim) / (float)*scols * margin;
                        *scols = scale * cols * pixfac;
                        *srows = scale * rows * pixfac;
		    } else {
                        scale *= (float)PageYdim(psim) / (float)*srows * margin;
                        *scols = scale * cols * pixfac;
                        *srows = scale * rows * pixfac;
		    }
                }
	        if (debug)
		    fprintf (stderr, ": scale=%f scols=%d srows=%d pixfac=%g\n",
			scale, *scols, *srows, pixfac);
	        psim->page.maxscale = scale; 	/* update new scale factor */

            } else {
		/* Notify user that the image won't fit and suggest a scale. */
                if ( *scols > pwidth )
                    scale *= (float) PageXdim(psim) / (float) *scols * margin;
                if ( *srows > pheight )
                    scale *= (float) PageYdim(psim) / (float) *srows * margin;
	        psim->page.maxscale = scale; 	/* update new scale factor */

		fprintf (stderr, 
		    "Image too big for the page, no autoscaling set...\n");
		fprintf (stderr, "Reset autoscale flag or use scale < %g\n",
		    scale);
	    }

        } 

	if (DoMaxAspect(psim)) {
	    /* Image will fit on page, but blow it up to a maximum size for
	     * the page orientation but retain the image aspect.
	     */
	    if (debug)
		fprintf (stderr, "Doing max aspect...");
	    if ( *scols >= *srows ) {
                scale *= (float) PageXdim(psim) / (float) *scols * margin;
                *scols = scale * cols * pixfac;
                *srows = scale * rows * pixfac;
	    } else if ( *srows > *scols ) {
                scale *= (float) PageYdim(psim) / (float) *srows * margin;
                *scols = scale * cols * pixfac;
                *srows = scale * rows * pixfac;
	    }
	    psim->page.maxscale = scale; 	/* update new scale factor */
	}

        /* Center it on the page. */
        *llx = (PageXdim(psim) - *scols) / 2 - (*turnflag ? 10 : 0);
        *lly = (PageYdim(psim) - *srows) / 2 + (*turnflag ? 0 : 10);

	if (debug) {
	    fprintf(stderr,"after: scale=%g scols=%d srows=%d ",
		scale, *scols, *srows);
	    fprintf(stderr,"icols=%d irows=%d llx=%g lly=%g\n",
		*icols, *irows, *llx, *lly);
	}
}


/* EPS_WRITEPIX -- Write the pixels or color indices directly.
 */
#ifdef ANSI_FUNC

static void 
eps_writePix (FILE *fp, uchar *pix, PSCmap *cmap, int npix, int xdim, int pad)
#else

static void
eps_writePix (fp, pix, cmap, npix, xdim, pad)
FILE    *fp;
uchar   *pix;
PSCmap 	*cmap;
int     npix;
int     xdim;
int     pad;
#endif
{
        register int i, min, max;

	min = *pix;
	max = *pix;
        while (npix > 0) {
	    min = (min > *pix ? *pix : min);
	    max = (max < *pix ? *pix : max);

            /* Write the pixels. */
	    for (i=0; i < PIX_PER_LINE && npix--; i+=2) {
        	pixbuf[pixnum++] = (uchar) hextab[((uchar)*pix >> 4)]; 
        	pixbuf[pixnum++] = (uchar) hextab[((uchar)*pix++ & 0x0F)]; 
	    }
            pixbuf[pixnum++] = '\n';

            if (pixnum >= (SZ_EPSBUF - PIX_PER_LINE - 1)) {
                pixbuf[pixnum] = '\0';
                fwrite ((char *)pixbuf, sizeof(char), pixnum, fp);
                pixnum = 0;
            }
            
            /* Skip the pad chars. */
            for (i=0; i<pad && npix; i++, npix--)
                pix++;
        }
	cmap->min = min; 
	cmap->max = max;
	eps_flushPix (fp);
}


/* EPS_WRITEMONO -- Write a pseudocolor image and convert to grayscale.
 */
#ifdef ANSI_FUNC

static void 
eps_writeMono (FILE *fp, uchar *pix, PSCmap *cmap, int npix, int xdim, int pad)
#else

static void
eps_writeMono (fp, pix, cmap, npix, xdim, pad)
FILE	*fp;
uchar   *pix;
PSCmap 	*cmap;
int 	npix;
int	xdim;
int	pad;
#endif
{
	register int i, min, max;
	register uchar pval;

	min = *pix;
	max = *pix;
        while (npix > 0) {
	    min = (min > *pix ? *pix : min);
	    max = (max < *pix ? *pix : max);

            /* Write the pixels. */
            for (i=0; i < PIX_PER_LINE && npix; i+=2, npix-- ) {
                pval = (uchar) MONO (cmap->r[*pix], 
				     cmap->g[*pix],
                                     cmap->b[*pix++]);
                pixbuf[pixnum++] = (uchar) hextab[(pval >> 4)];
                pixbuf[pixnum++] = (uchar) hextab[(pval & 0x0F)];
            }
            pixbuf[pixnum++] = '\n';

            if (pixnum >= (SZ_EPSBUF - PIX_PER_LINE - 1) || !npix) {
                pixbuf[pixnum] = '\0';
                fwrite ((char *)pixbuf, sizeof(char), pixnum, fp);
                pixnum = 0;
            }

            /* skip the pad chars */
            for (i=0; i<pad && npix; i++, npix--)
                pix++;
        }
	cmap->min = min; 
	cmap->max = max;
	eps_flushPix (fp);
}


/* EPS_WRITEMONORGB --  Write RGB data converted to grayscale.
 */
#ifdef ANSI_FUNC

static void 
eps_writeMonoRGB (FILE *fp, uchar *pix, int npix, int xdim, int depth, int pad)
#else

static void
eps_writeMonoRGB (fp, pix, npix, xdim, depth, pad)
FILE    *fp;
uchar   *pix;
int     npix;
int     xdim;
int     depth;
int     pad;
#endif
{
        register int i;
	register uchar pval;

        while (npix > 0) {
            /* Write the pixels. */
	    if (depth < 24) {
                for (i=0; i < PIX_PER_LINE && npix; i+=2, npix-=3 ) {
                    pval = (uchar) MONO (*pix++, *pix++, *pix++);
                    pixbuf[pixnum++] = (uchar) hextab[(pval >> 4)];
                    pixbuf[pixnum++] = (uchar) hextab[(pval & 0x0F)];
                }
            } else {
		/* Skip alpha channel in the loop */
                for (i=0; i < PIX_PER_LINE && npix; i+=2, npix-=3, pix++ ) {
                    pval = (uchar) MONO (*pix++, *pix++, *pix++);
                    pixbuf[pixnum++] = (uchar) hextab[(pval >> 4)];
                    pixbuf[pixnum++] = (uchar) hextab[(pval & 0x0F)];
                }
            }
            pixbuf[pixnum++] = '\n';

            if (pixnum >= (SZ_EPSBUF - PIX_PER_LINE - 1)) {
                pixbuf[pixnum] = '\0';
                fwrite ((char *)pixbuf, sizeof(char), pixnum, fp);
                pixnum = 0;
            }

            /* skip the pad chars */
            for (i=0; i<pad && npix; i++, npix--)
                pix++;
        }
	eps_flushPix (fp);
}


/* EPS_WRITERGB -- Write the pixels in RGB format, skipping a possible alpha
 * channel.
 */
#ifdef ANSI_FUNC

static void 
eps_writeRGB (FILE *fp, uchar *pix, PSCmap *cmap, int npix, int xdim, int depth, int pad)
#else

static void
eps_writeRGB (fp, pix, cmap, npix, xdim, depth, pad)
FILE    *fp;
uchar   *pix;
PSCmap 	*cmap;
int     npix;
int     xdim;
int     depth;
int     pad;
#endif
{
        register int i, min, max;
	register uchar val;

        min = *pix;
        max = *pix;
        while (npix > 0) {
            min = (min > *pix ? *pix : min);
            max = (max < *pix ? *pix : max);

            /* Write the pixels. */
            if (depth < 24) {
                for (i=0; i < PIX_PER_LINE && npix; i+=6, npix--, pix++ ) {
                    val = (uchar) cmap->r[*pix];
                    pixbuf[pixnum++] = (uchar) hextab[((uchar)val >> 4)];
                    pixbuf[pixnum++] = (uchar) hextab[((uchar)val & 0x0F)];
                    val = (uchar) cmap->g[*pix];
                    pixbuf[pixnum++] = (uchar) hextab[((uchar)val >> 4)];
                    pixbuf[pixnum++] = (uchar) hextab[((uchar)val & 0x0F)];
                    val = (uchar) cmap->b[*pix];
                    pixbuf[pixnum++] = (uchar) hextab[((uchar)val >> 4)];
                    pixbuf[pixnum++] = (uchar) hextab[((uchar)val & 0x0F)];
                }
            } else {
                /* Skip alpha channel in the loop */
                for (i=0; i < PIX_PER_LINE && npix; i+=6, npix-=3, pix++ ) {
                    pixbuf[pixnum++] = (uchar) hextab[((uchar)*pix >> 4)];
                    pixbuf[pixnum++] = (uchar) hextab[((uchar)*pix++ & 0x0F)];
                    pixbuf[pixnum++] = (uchar) hextab[((uchar)*pix >> 4)];
                    pixbuf[pixnum++] = (uchar) hextab[((uchar)*pix++ & 0x0F)];
                    pixbuf[pixnum++] = (uchar) hextab[((uchar)*pix >> 4)];
                    pixbuf[pixnum++] = (uchar) hextab[((uchar)*pix++ & 0x0F)];
                }
            }
            pixbuf[pixnum++] = '\n';

            if (pixnum >= (SZ_EPSBUF - PIX_PER_LINE - 1)) {
                pixbuf[pixnum] = '\0';
                fwrite ((char *)pixbuf, sizeof(char), pixnum, fp);
                pixnum = 0;
            }


            /* skip the pad chars */
            for (i=0; i<pad && npix; i++, npix--)
                pix++;
        }
	cmap->min = min; 
	cmap->max = max;
	eps_flushPix (fp);
}


/* EPS_WRITECMAP -- Output Postscript colormap.
 */
#ifdef ANSI_FUNC

static void 
eps_writeCmap (PSCmap *cmap, FILE *fp)
#else

static void
eps_writeCmap (cmap, fp)
PSCmap 	*cmap;
FILE	*fp;
#endif
{
	register int i, j;

	/* Dump number of colors and colormap used. */
       	fprintf (fp, "256\n");
      	for (i=0; i < 256; ) {
      	    for (j=0; j < 10 && i < 256; i++, j++) {
                fprintf (fp, "%02x%02x%02x", 
		    cmap->r[i], cmap->g[i], cmap->b[i]);
	    }
            fprintf (fp, "\n");
	}
}


/* EPS_SIMPLEHEADER -- Output simple Postscript header.
 */
static char *EPSSimpleRLEProlog[]= {
        "/rlestr1 1 string def\n",  
        "/readrlestring {                      		/* s -- nr */\n",
        "  /rlestr exch def                    		/* - */\n",
        "  currentfile rlestr1 readhexstring pop        /* s1 */\n",
        "  0 get                               		/* c */\n",
        "  dup 127 le {                        		/* c */\n",
        "    currentfile rlestr 0              		/* c f s 0 */\n",
        "    4 3 roll                          		/* f s 0 c */\n",
        "    1 add  getinterval                		/* f s */\n",
        "    readhexstring pop                 		/* s */\n",
        "    length                            		/* nr */\n",
        "  } {                                 		/* c */\n",
        "    256 exch sub dup                  		/* n n */\n",
        "    currentfile rlestr1 readhexstring pop     	/* n n s1 */\n",
        "    0 get                             		/* n n c */\n",
        "    exch 0 exch 1 exch 1 sub {        		/* n c 0 1 n-1*/\n",
        "      rlestr exch 2 index put\n",  
        "    } for                             		/* n c */\n",
        "    pop                               		/* nr */\n",
        "  } ifelse                            		/* nr */\n",
        "} bind def\n",  
        "/readstring {                         		/* s -- s */\n",
        "  dup length 0 {                      		/* s l 0 */\n",
        "    3 copy exch                       		/* s l n s n l*/\n",
        "    1 index sub                       		/* s l n s n r*/\n",
        "    getinterval                       		/* s l n ss */\n",
        "    readrlestring                     		/* s l n nr */\n",
        "    add                               		/* s l n */\n",
        "    2 copy le { exit } if             		/* s l n */\n",
        "  } loop                              		/* s l l */\n",
        "  pop pop                             		/* s */\n",
        "} bind def\n",  
      	NULL
};

#ifdef ANSI_FUNC

static void 
eps_simpleHeader (PSImagePtr psim, FILE *fp)
#else

static void
eps_simpleHeader (psim, fp)
PSImagePtr psim;
FILE	   *fp;
#endif
{
        register char **line;
        int 	icols=0, irows=0, scols=0, srows=0, turnflag=0;
        float 	llx = 0.0, lly = 0.0;
        time_t  timer;

        /* Get the common page parameters. */
        eps_pageParams (psim, &llx, &lly, &icols, &irows, &scols, &srows,
	    &turnflag);

	/* Now write header and prolog stuff */
        fprintf (fp, "%%!PS-Adobe-3.0 EPSF-3.0\n" );
	fprintf (fp, "%%%%Title: XImtool Screen Hardcopy\n");
	fprintf (fp, "%%%%Creator: XImtool\n");
  	timer = time ((time_t *) NULL);
	(void) localtime (&timer);
	fprintf (fp, "%%%%CreationDate: %s", ctime(&timer));
        if (psim->annotate)
            fprintf (fp, "%%%%BoundingBox: %d %d %d %d\n",
                (int) (MAX (0, llx - X_ANNOT_MARGIN)),
                (int) (MAX (0, lly - Y_ANNOT_MARGIN)),
                (int) ( llx + scols + 0.5 + X_ANNOT_MARGIN),
                (int) ( lly + srows + 0.5 + Y_ANNOT_MARGIN) );
        else
            fprintf (fp, "%%%%BoundingBox: %d %d %d %d\n",
                (int) (MAX (0, llx - 10)),
                (int) (MAX (0, lly - 10)),
                (int) ( llx + scols + 0.5 + 10),
                (int) ( lly + srows + 0.5 + 10) );
	fprintf (fp, "%%%%Pages: 1\n");
        fprintf (fp, "%%%%EndComments\n" );

    	if ( psim->compression == RLECompression ) {
            for (line = EPSSimpleRLEProlog; *line; line++)
                fprintf (fp, "%s\n",*line);
        } else {
            fprintf (fp, "/readstring {\n" );                    /* s -- s */
            fprintf (fp, "  currentfile exch readhexstring pop\n" );
            fprintf (fp, "} bind def\n" );
        }

    	if (psim->colorClass == EPS_TRUECOLOR) {
            fprintf (fp, "/rpicstr %d string def\n", icols );
            fprintf (fp, "/gpicstr %d string def\n", icols );
            fprintf (fp, "/bpicstr %d string def\n", icols );
        } else
            fprintf (fp, "/picstr %d string def\n", icols );
        fprintf (fp, "%%%%EndProlog\n" );
        fprintf (fp, "%%%%Page: 1 1\n" );
        fprintf (fp, "gsave\n" );

        fprintf (fp, "%g %g translate\n", llx, lly );
        fprintf (fp, "%d %d scale\n", scols, srows );
    	if ( turnflag ) {
	    if (debug) fprintf (stderr, "turnflag enabled....\n");
            fprintf (fp, "0.5 0.5 translate  90 rotate  -0.5 -0.5 translate\n");
	}
        fprintf (fp, "%d %d 8\n", icols, irows);
        fprintf (fp, "[ %d 0 0 -%d 0 %d ]\n", icols, irows, irows );

    	if (psim->colorClass == EPS_TRUECOLOR) {
            fprintf (fp, "{ rpicstr readstring }\n" );
            fprintf (fp, "{ gpicstr readstring }\n" );
            fprintf (fp, "{ bpicstr readstring }\n" );
            fprintf (fp, "true 3\n" );
            fprintf (fp, "colorimage\n" );
        } else {
            fprintf (fp, "{ picstr readstring }\n" );
            fprintf (fp, "image\n" );
        }
}


/* EPS_ANNOTATE --  Annotate the main image window with axis labels,
 * colorbars, and the title string.
 */

#ifdef ANSI_FUNC

static void 
eps_annotate (PSImagePtr psim, FILE *fp)
#else

static void
eps_annotate (psim, fp)
PSImagePtr psim;
FILE       *fp;
#endif
{
        int     icols=0, irows=0, scols=0, srows=0, turnflag=0;
	int	cbar_size;
        float   llx = 0.0, lly = 0.0;


        /* Get the common page parameters. */
        eps_pageParams (psim, &llx, &lly, &icols, &irows, &scols, &srows,
            &turnflag);

	/* Write the annotation prolog stufd. */
        fprintf (fp, "gsave\n" );

	/* Main image title. */
	if (psim->label != NULL) {
            fprintf (fp, 
	      "/labelfont /NewCenturySchlbk-Roman findfont 16 scalefont def\n");
	    if (turnflag) {
                fprintf (fp, "/label { moveto labelfont setfont\n");
	     	fprintf (fp, "         90 rotate show -90 rotate\n} def\n" );
      	        fprintf (fp, "(%s) %g %g label\n", psim->label,
	            llx - TITLE_OFFSET,
	            lly + (srows/2) - 8.25*(strlen(psim->label)/2) );
	    } else {
                fprintf (fp, "/label { moveto labelfont setfont show } def\n" );
      	        fprintf (fp, "(%s) %g %g label\n", psim->label,
	            llx + (scols/2) - 8.25*(strlen(psim->label)/2), 
		    lly + srows + TITLE_OFFSET);
	    }
	}

	/* A border for the image axes. */
	fprintf (fp, "/imBorder {\n");
	fprintf (fp, "   %-4d %-4d moveto	%% Outer axis\n", 
	    (int)(llx - AXIS_OOFFSET), 
	    (int)(lly - AXIS_OOFFSET));
	fprintf (fp, "   0    %-4d rlineto\n", srows + (2 * AXIS_OOFFSET));
	fprintf (fp, "   %-4d  0 rlineto\n", scols + (2 * AXIS_OOFFSET));
	fprintf (fp, "   0  -%-4d rlineto\n", srows + (2 * AXIS_OOFFSET));
	fprintf (fp, "   closepath\n");
	fprintf (fp, "   %g setlinewidth\n", AXIS_OWIDTH);
	fprintf (fp, "   stroke\n");

	fprintf (fp, "   %-4d %-4d moveto	%% Inner axis\n", 
	    (int)(llx - AXIS_IOFFSET), 
	    (int)(lly - AXIS_IOFFSET));
	fprintf (fp, "   0   %-4d rlineto\n", srows + (2 * AXIS_IOFFSET));
	fprintf (fp, "   %-4d  0 rlineto\n", scols + (2 * AXIS_IOFFSET));
	fprintf (fp, "   0   -%-4d rlineto\n", srows + (2 * AXIS_IOFFSET));
	fprintf (fp, "   closepath\n");
	fprintf (fp, "   %g setlinewidth\n", AXIS_IWIDTH);
	fprintf (fp, "   stroke\n");

	fprintf (fp, "} def\n");
	fprintf (fp, "imBorder\n");

	/* Axis label font definitions. */
	fprintf (fp, "/axlabelfont /Times-Roman findfont 8 scalefont def\n");
	fprintf (fp, "/axlabel { moveto axlabelfont setfont %s show %s } def\n",
	    (turnflag ? "90 rotate" : " "),
	    (turnflag ? "-90 rotate" : " "));

	/* Axis ticmark procedures. */
	fprintf (fp, "/xMajorTicmark {\n");
	fprintf (fp, "    moveto\n");
	fprintf (fp, "    0 %d rlineto    ", MAJOR_TICK_SIZE);
	fprintf (fp, "    0 %d rmoveto    ", 
	        srows + MAJOR_TICK_SIZE + (2 * AXIS_IOFFSET) );
	fprintf (fp, "    0 -%d rlineto\n", MAJOR_TICK_SIZE);
	fprintf (fp, "    %g setlinewidth\n    stroke\n", MAJOR_TICK_WIDTH);
	fprintf (fp, "} def\n");
	fprintf (fp, "/yMajorTicmark {\n");
	fprintf (fp, "    moveto\n");
	fprintf (fp, "    %d 0 rlineto    ", MAJOR_TICK_SIZE);
	fprintf (fp, "    %d 0 rmoveto    ", 
	        scols + MAJOR_TICK_SIZE + (2 * AXIS_IOFFSET) );
	fprintf (fp, "    -%d 0 rlineto\n", MAJOR_TICK_SIZE);
	fprintf (fp, "    %g setlinewidth\n    stroke\n", MAJOR_TICK_WIDTH);
	fprintf (fp, "} def\n");
	fprintf (fp, "/xMinorTicmark {\n");
	fprintf (fp, "    moveto\n");
	fprintf (fp, "    0 %d rlineto    ", MINOR_TICK_SIZE);
	fprintf (fp, "    0 %d rmoveto    ", 
	        srows + MAJOR_TICK_SIZE + (2*AXIS_IOFFSET) +
	        (MAJOR_TICK_SIZE - MINOR_TICK_SIZE) );
	fprintf (fp, "    0 -%d rlineto\n", MINOR_TICK_SIZE);
	fprintf (fp, "    %g setlinewidth\n    stroke\n", MINOR_TICK_WIDTH);
	fprintf (fp, "} def\n");
	fprintf (fp, "/yMinorTicmark {\n");
	fprintf (fp, "    moveto\n");
	fprintf (fp, "    %d 0 rlineto    ", MINOR_TICK_SIZE);
	fprintf (fp, "    %d 0 rmoveto    ", 
	        scols + MAJOR_TICK_SIZE + (2*AXIS_IOFFSET) + 
	        (MAJOR_TICK_SIZE - MINOR_TICK_SIZE) );
	fprintf (fp, "    -%d 0 rlineto\n", MINOR_TICK_SIZE);
	fprintf (fp, "    %g setlinewidth\n    stroke\n", MINOR_TICK_WIDTH);
	fprintf (fp, "} def\n");

	/* The axis labeling.  */
	if (turnflag)
	    eps_landLabels (fp, psim, scols, srows, icols, irows, llx, lly);
	else
	    eps_portLabels (fp, psim, scols, srows, icols, irows, llx, lly);
        fprintf (fp, "grestore\n");

	/* The colorbar.  */
    	eps_doColorbar (fp, psim, scols, srows, llx, lly, turnflag);

	/* Print the transform information.  */
        fprintf (fp, "gsave\n");
	fprintf (fp, "/Times-Roman findfont 8 scalefont setfont\n");
	if (turnflag) {
            cbar_size = MIN (512, MAX (256, srows + 2));
            fprintf (fp, "%d %d moveto\n", 
		(int)llx + scols + 26, 
		(int)lly + (srows/2) - (cbar_size/2) - (cbar_size==256?5:1));
	} else {
            cbar_size = MIN (512, MAX (256, scols + 2));
            fprintf (fp, "%d %d moveto\n", 
		(int)llx + (scols/2) - (cbar_size/2) - (cbar_size==256?5:1), 
		(int)(lly-29));
	}
	fprintf (fp, "(z1=%.2f z2=%.2f ztrans=%s Con=%.2f Brt=%.2f cmap=%s ncolors=%d) %s show %s\n",
	    psim->z1, psim->z2,
	    (psim->ztype==EPS_UNITARY ? "unitary": 
		(psim->ztype==EPS_LINEAR ? "linear" : "log")),
	    psim->scale, psim->offset,
	    psim->cmap.cmap_name,
	    psim->cmap.max - psim->cmap.min + 1,
	    (turnflag ? "90 rotate" : " "),
	    (turnflag ? "-90 rotate" : " "));
        fprintf (fp, "grestore\n");

	/* Print the timestamp. */
        fprintf (fp, "gsave\n");
        fprintf (fp, "/Times-Roman findfont 6 scalefont setfont\n");
        fprintf (fp, "20 15 moveto\n(%s) show\n", make_label());
	fprintf (fp, "grestore\n");

	if (debug) { 
	    fprintf (stderr, "colormap: min/max = %d/%d\n", 
		psim->cmap.min, psim->cmap.max);
	    fprintf (stderr, "label: '%s'", make_label());
	}
}


/* EPS_PORTRAITLABELS -- Write out the labeling procedures for a portrait
 * mode image.
 */
#ifdef ANSI_FUNC

static void 
eps_portLabels (FILE *fp, PSImagePtr psim, int scols, int srows, int icols, int irows, float llx, float lly)
#else

static void 
eps_portLabels (fp, psim, scols, srows, icols, irows, llx, lly)
FILE	*fp;
PSImagePtr psim;
int	scols, srows;
int	icols, irows;
float	llx, lly;
#endif
{
	int	start, end, range, nlabels;
	float 	xpos, xstep, ypos, ystep;
        float   Mval, xval=0.0, yval=0.0, tic, mtic, Mtic;

	/* X Axis labeling and ticmarks. */
	nlabels = (scols > 256 ? 5 : 3);
        fprintf (fp, "/axLabelX {\n");
	if (psim->urx > psim->llx) {
	    start = psim->llx;
	    end = psim->urx;
	} else {
	    start = psim->urx;
	    end = psim->llx;
	}
	range = end - start + 1;
	Mtic = tic = ticstep ((float)range, (range>=256 ? NTICMARKS : 3));
	mtic = (float) tic / (tic < 100 ? 5. : 10.);
	Mval = ( (int)(start + Mtic) / (int) Mtic) * Mtic;
	xval = ( (int)(start + mtic) / (int) mtic) * mtic;
	xstep = mtic * ((float)scols/(float)(range));
	if (psim->urx > psim->llx) 
	    xpos = llx + ( (float)(xval - start) / (float)range * scols);
	else {
	    xpos = (llx + scols) - ((float)(xval-start) / (float)range * scols);
	    xstep = -xstep;
	}
	while (xval <= end) {
	    if ( xval == Mval ) {
	        fprintf (fp, "    %4d %4d xMajorTicmark\t", 
		    (int)xpos, (int)(lly-AXIS_OOFFSET));
	        fprintf (fp, "    (%g) %d %d axlabel\n", 
		    xval,
		    (int)xpos-(xval>=1000?8:(xval>=100?5:2)),
		    (int)(lly-14));
	        Mval += Mtic;
	    } else
	        fprintf (fp, "\t%4d %4d xMinorTicmark\n", 
		    (int)xpos, (int)(lly-AXIS_OOFFSET));
	    xval += mtic;
	    xpos += xstep;
	}
        fprintf (fp, "} def\n");
        fprintf (fp, "axLabelX\n");

	/* Y Axis labeling and ticmarks. */
	nlabels = (srows > 256 ? 5 : 3);
        fprintf (fp, "/axLabelY {\n");
	if (psim->ury > psim->lly) {
	    start = psim->lly;
	    end = psim->ury;
	} else {
	    start = psim->ury;
	    end = psim->lly;
	}
	range = end - start + 1;
	Mtic = tic = ticstep ((float)range, (range>=256 ? NTICMARKS : 3));
	mtic = (float) tic / (tic < 100 ? 5. : 10.);
	Mval = ( (int)(start + Mtic) / (int) Mtic) * Mtic;
	yval = ( (int)(start + mtic) / (int) mtic) * mtic;
	ystep = mtic * ((float)srows/(float)(range));
	if (psim->ury > psim->lly) 
	    ypos = lly + ( (float)(yval - start) / (float)range * srows);
	else {
	    ypos = (lly + srows) - ((float)(yval-start) / (float)range * srows);
	    ystep = -ystep;
	}
	while (yval <= end) {
	    if ( yval == Mval ) {
	        fprintf (fp, "    %4d %4d yMajorTicmark\t", 
		    (int)(llx-AXIS_OOFFSET), (int)ypos);
	        fprintf (fp, "    (%g) %d %d axlabel\n", 
		    yval, 
		    (int)(llx-(yval>=1000?27:(yval>=100?20:16))), 
		    (int)ypos-2); 
	        Mval += Mtic;
	    } else
	        fprintf (fp, "\t%4d %4d yMinorTicmark\n", 
		    (int)(llx-AXIS_OOFFSET), (int)ypos);
	    yval += mtic;
	    ypos += ystep;
	}
        fprintf (fp, "} def\n");
        fprintf (fp, "axLabelY\n");
}


/* EPS_LANDSCAPELABELS -- Write out the labeling procedures for a landscape
 * mode image.
 */
#ifdef ANSI_FUNC

static void 
eps_landLabels (FILE *fp, PSImagePtr psim, int scols, int srows, int icols, int irows, float llx, float lly)
#else

static void 
eps_landLabels (fp, psim, scols, srows, icols, irows, llx, lly)
FILE	*fp;
PSImagePtr psim;
int	scols, srows;
int	icols, irows;
float	llx, lly;
#endif
{
	float	xpos, xstep, ypos, ystep;
	int	start, end, range, nlabels;
        float   Mval, xval=0.0, yval=0.0, tic, mtic, Mtic;

	/* X Axis labeling and ticmarks. */
	nlabels = (srows > 256 ? 5 : 3);
        fprintf (fp, "/axLabelX {\n");
        if (psim->ury > psim->lly) {
            start = psim->lly;
            end = psim->ury;
        } else {
            start = psim->ury;
            end = psim->lly;
        }  
	range = end - start + 1;
	Mtic = tic = ticstep ((float)range, (range>=256 ? NTICMARKS : 3));
	mtic = (float) tic / (tic < 100 ? 5. : 10.);
        Mval = ( (int)(start + Mtic) / (int) Mtic) * Mtic;
        xval = ( (int)(start + mtic) / (int) mtic) * mtic;
        xstep = mtic * ((float)scols/(float)(range));
        if (psim->ury > psim->lly)
            xpos = (llx + scols) - ((float)(xval-start) / (float)range * scols);
        else {
            xpos = llx + ((float)(xval-start) / (float)range * scols);
            xstep = -xstep;
        }
	while (xval <= end ) {
	    if ( xval == Mval ) {
	        fprintf (fp, "    %4d %4d xMajorTicmark\t", 
		    (int)xpos, (int)(lly-AXIS_OOFFSET));
	        fprintf (fp, "    (%g) %d %d axlabel\n", 
		    xval,
		    (int)xpos+2,
		    (int)(lly-(yval>=1000?30:(yval>=100?25:20))) );
	        Mval += Mtic;
	    } else
	        fprintf (fp, "\t%4d %4d xMinorTicmark\n", 
		    (int)xpos, (int)(lly-AXIS_OOFFSET));
	    xval += mtic;
	    xpos -= xstep;
	}
        fprintf (fp, "} def\n");
        fprintf (fp, "axLabelX\n");

	/* Y Axis labeling and ticmarks. */
	nlabels = (srows > 256 ? 5 : 3);
        fprintf (fp, "/axLabelY {\n");
        if (psim->urx > psim->llx) {
            start = psim->llx;
            end = psim->urx;
        } else {
            start = psim->urx;
            end = psim->llx;
        }  
	Mtic = tic = ticstep ((float)range, (range>=256 ? NTICMARKS : 3));
	mtic = (float) tic / (tic < 100 ? 5. : 10.);
        Mval = ( (int)(start + Mtic) / (int) Mtic) * Mtic;
        yval = ( (int)(start + mtic) / (int) mtic) * mtic;
        ystep = mtic * ((float)scols/(float)(range));
        if (psim->urx > psim->llx)
            ypos = lly + ( (float)(yval - start) / (float)range * scols);
        else {
            ypos = (lly + srows) - ((float)(yval-start) / (float)range * scols);
            ystep = -ystep;
        }
        while (yval <= end) {  
	    if ( yval == Mval ) {
	        fprintf (fp, "    %4d %4d yMajorTicmark\t", 
		    (int)(llx-AXIS_OOFFSET), (int)ypos);
	        fprintf (fp, "    (%g) %d %d axlabel\n", 
		    yval, 
		    (int)(llx+scols+(2*AXIS_OOFFSET)+2),
		    (int)ypos-(yval>=1000?9:(yval>=100?6:2)) ); 
	        Mval += Mtic;
	    } else
	        fprintf (fp, "\t%4d %4d yMinorTicmark\n", 
		    (int)(llx-AXIS_OOFFSET), (int)ypos);
	    yval += mtic;
	    ypos += ystep;
	}
        fprintf (fp, "} def\n");
        fprintf (fp, "axLabelY\n");
}


/* EPS_DOCOLORBAR --  Annotate the image with a colorbar.
 */

#ifdef ANSI_FUNC

static void 
eps_doColorbar (FILE *fp, PSImagePtr psim, int scols, int srows, float llx, float lly, int turnflag)
#else

static void 
eps_doColorbar (fp, psim, scols, srows, llx, lly, turnflag)
FILE	*fp;
PSImagePtr psim;
int	scols, srows;
float	llx, lly;
int	turnflag;
#endif
{
	register int i, j, cbar_size;
        int     ncolors, nlabels, pos, step, cmel, cmstep, val;

	/* Colorbar label font definitions. */
	fprintf (fp, "/cblabelfont /Times-Roman findfont 10 scalefont def\n");
	fprintf (fp, "/cblabel { moveto cblabelfont setfont %s show %s } def\n",
	    (turnflag ? "90 rotate" : " "),
	    (turnflag ? "-90 rotate" : " "));

	/* A border for the colorbar. */
	fprintf (fp, "/cbarBorder {\n");
	if (turnflag) {
	    cbar_size = MIN (512, MAX (256, srows + 2)) - 1;
	    fprintf (fp, "   %4d %4d moveto\n", 
		(int)llx + scols + 31, 
		(int)lly + (srows/2) - (cbar_size/2) - 1);
	    fprintf (fp, "   17      0 rlineto\n");
	    fprintf (fp, "   0    %4d rlineto\n",  cbar_size + 3);
	    fprintf (fp, "   -17     0 rlineto\n");
	} else {
	    cbar_size = MIN (512, MAX (256, scols + 2));
	    fprintf (fp, "   %-4d %4d moveto\n", 
		(int)llx + (scols/2) - (cbar_size/2) - 1, 
		(int)(lly - 46));
	    fprintf (fp, "   0      14 rlineto\n");
	    fprintf (fp, "   %-4d    0 rlineto\n", cbar_size + 3);
	    fprintf (fp, "   0     -14 rlineto\n");
	}
	fprintf (fp, "   closepath\n");
	fprintf (fp, "   1 setlinewidth\n");
	fprintf (fp, "   stroke\n");
	fprintf (fp, "} def\n");
	fprintf (fp, "cbarBorder\n");

	/* Draw the colorbar labels, but only for grayscale images. */
	ncolors = psim->cmap.max - psim->cmap.min + 1;
	nlabels = 5;
	if (turnflag)
	    pos = (int)lly + (srows/2) - (cbar_size/2);
	else
	    pos = (int)llx + (scols/2) - (cbar_size/2);
	step = cbar_size / nlabels;
	cmel = psim->cmap.min;
	cmstep = ncolors / nlabels;
	for (i=0; i <= nlabels; i++) {
	    val = MONO(psim->cmap.r[cmel],	    /* get grayscale value */
		       psim->cmap.g[cmel],
		       psim->cmap.b[cmel]);
	     if (psim->z1 != 0.0 && psim->z2 != 0.0) {
		/* We have a transformation defined, convert the grayscale
		 * value to original image values.   Assumes a linear
		 * transformation, for a log transformation the wcsbox
	 	 * reports only screen units so we'll do that here as well.
		 */
		if (psim->ztype == EPS_LINEAR)
		    val = ((psim->z2-psim->z1)/(float)ncolors)*cmel + psim->z1;
	    }

	    if (turnflag) 
	        fprintf (fp, "(%d) %d %d cblabel\n", 
		    val, 
		    (int)llx + scols + 57, 
		    pos-(val<100?3:6));
	    else
	        fprintf (fp, "(%d) %d %d cblabel\n", 
		    val, 
		    pos-(val<100?3:6), 
		    (int)(lly-55));
	    cmel += cmstep;
	    cmel = (cmel < psim->cmap.max ? cmel : psim->cmap.max);
	    pos += step;
	}

	/* Draw the colorbar. */
	if (psim->colorClass == EPS_GRAYSCALE) {
            fprintf (fp, "/cbarstr %d string def\n", ncolors);
            fprintf (fp, "gsave\n" );
	    if (turnflag) {
                fprintf (fp, "%d %d translate\n",
		    (int)(llx + scols + 45), 
		    (int)(lly + (srows/2) - (cbar_size/2)) + 1);
	        fprintf (fp, 
		    "0.5 0.5 translate  90 rotate  -0.5 -0.5 translate\n");
	    } else {
                fprintf (fp, "%g %g translate\n", 
		    llx + (scols/2) - (cbar_size/2) + 1, 
		    lly - 45 );
	    }
            fprintf (fp, "%d 12 scale\n", cbar_size);
            fprintf (fp, "%d 1 8  [ %d 0 0 1 0 0 ]\n", ncolors, ncolors);

            fprintf (fp, "{ cbarstr readstring }\n" );
            fprintf (fp, "image\n" );

	} else {
            fprintf (fp, "DisplayImage\n" );
            if (turnflag) {
                fprintf (fp, "%d %d\n", 
                    (int)(llx + scols + 34),
                    (int)(lly + (srows/2) - (cbar_size/2)));
                fprintf (fp, "12 %d\n", cbar_size);
            } else {
                fprintf (fp, "%d %d\n",  
		    (int)llx + (scols/2) - (cbar_size/2), 
		    (int)lly - 45 );
                fprintf (fp, "%d 12\n", cbar_size);
	    }
            fprintf (fp, "%d 1\n", ncolors);
            fprintf (fp, "%d\n", turnflag);
            fprintf (fp, "0\n");
            fprintf (fp, "1\n");
	}

	/* Write out the colormap used. */
	if (psim->colorClass == EPS_GRAYSCALE) {
	    for (i=psim->cmap.min; i <= psim->cmap.max; i++ ) {
	        fprintf (fp, "%02x", 
	            MONO(psim->cmap.r[i],psim->cmap.g[i], psim->cmap.b[i]));
	        if ((i+1) % 32 == 0)
		    fprintf (fp, "\n");
	    }
     	    fprintf (fp, "\n");
            fprintf (fp, "grestore\n");
	} else {
	    for (j=1, i=psim->cmap.min; i <= psim->cmap.max; i++, j++ )
	        fprintf (fp, "%02x%02x%02x\n", 
		    psim->cmap.r[i],psim->cmap.g[i], psim->cmap.b[i]);
		if (j % 12 ==0)
		    fprintf (fp, "\n");
	}
}


/* EPS_COLORHEADER - Write the pseudocolor header prolog and compute EPS page
 * parameters. 
 */
static char *EPSColorProlog[]=
    {
      "%%BeginProlog",
      "%",
      "% Display a color image.  The image is displayed in color on",
      "% Postscript viewers or printers that support color, otherwise",
      "% it is displayed as grayscale.",
      "%",
      "/buffer 512 string def",
      "/byte 1 string def",
      "/color_packet 3 string def",
      "/pixels 768 string def",
      "",
      "/DirectClassPacket",
      "{",
      "  %",
      "  % Get a DirectClass packet.",
      "  %",
      "  % Parameters: ",
      "  %   red.",
      "  %   green.",
      "  %   blue.",
      "  %   length: number of pixels minus one of this color (optional).",
      "  %",
      "  currentfile color_packet readhexstring pop pop",
      "  compression 0 gt",
      "  {",
      "    /number_pixels 3 def",
      "  }",
      "  {",
      "    currentfile byte readhexstring pop 0 get",
      "    /number_pixels exch 1 add 3 mul def",
      "  } ifelse",
      "  0 3 number_pixels 1 sub",
      "  {",
      "    pixels exch color_packet putinterval",
      "  } for",
      "  pixels 0 number_pixels getinterval",
      "} bind def",
      "",
      "/DirectClassImage",
      "{",
      "  %",
      "  % Display a DirectClass image.",
      "  %",
      "  systemdict /colorimage known",
      "  {",
      "    columns rows 8",
      "    [",
      "      columns 0 0",
      "      rows neg 0 rows",
      "    ]",
      "    { DirectClassPacket } false 3 colorimage",
      "  }",
      "  {",
      "    %",
      "    % No colorimage operator;  convert to grayscale.",
      "    %",
      "    columns rows 8",
      "    [",
      "      columns 0 0",
      "      rows neg 0 rows",
      "    ]",
      "    { GrayDirectClassPacket } image",
      "  } ifelse",
      "} bind def",
      "",
      "/GrayDirectClassPacket",
      "{",
      "  %",
      "  % Get a DirectClass packet;  convert to grayscale.",
      "  %",
      "  % Parameters: ",
      "  %   red",
      "  %   green",
      "  %   blue",
      "  %   length: number of pixels minus one of this color (optional).",
      "  %",
      "  currentfile color_packet readhexstring pop pop",
      "  color_packet 0 get 0.299 mul",
      "  color_packet 1 get 0.587 mul add",
      "  color_packet 2 get 0.114 mul add",
      "  cvi",
      "  /gray_packet exch def",
      "  compression 0 gt",
      "  {",
      "    /number_pixels 1 def",
      "  }",
      "  {",
      "    currentfile byte readhexstring pop 0 get",
      "    /number_pixels exch 1 add def",
      "  } ifelse",
      "  0 1 number_pixels 1 sub",
      "  {",
      "    pixels exch gray_packet put",
      "  } for",
      "  pixels 0 number_pixels getinterval",
      "} bind def",
      "",
      "/GrayPseudoClassPacket",
      "{",
      "  %",
      "  % Get a PseudoClass packet;  convert to grayscale.",
      "  %",
      "  % Parameters: ",
      "  %   index: index into the colormap.",
      "  %   length: number of pixels minus one of this color (optional).",
      "  %",
      "  currentfile byte readhexstring pop 0 get",
      "  /offset exch 3 mul def",
      "  /color_packet colormap offset 3 getinterval def",
      "  color_packet 0 get 0.299 mul",
      "  color_packet 1 get 0.587 mul add",
      "  color_packet 2 get 0.114 mul add",
      "  cvi",
      "  /gray_packet exch def",
      "  compression 0 gt",
      "  {",
      "    /number_pixels 1 def",
      "  }",
      "  {",
      "    currentfile byte readhexstring pop 0 get",
      "    /number_pixels exch 1 add def",
      "  } ifelse",
      "  0 1 number_pixels 1 sub",
      "  {",
      "    pixels exch gray_packet put",
      "  } for",
      "  pixels 0 number_pixels getinterval",
      "} bind def",
      "",
      "/PseudoClassPacket",
      "{",
      "  %",
      "  % Get a PseudoClass packet.",
      "  %",
      "  % Parameters: ",
      "  %   index: index into the colormap.",
      "  %   length: number of pixels minus one of this color (optional).",
      "  %",
      "  currentfile byte readhexstring pop 0 get",
      "  /offset exch 3 mul def",
      "  /color_packet colormap offset 3 getinterval def",
      "  compression 0 gt",
      "  {",
      "    /number_pixels 3 def",
      "  }",
      "  {",
      "    currentfile byte readhexstring pop 0 get",
      "    /number_pixels exch 1 add 3 mul def",
      "  } ifelse",
      "  0 3 number_pixels 1 sub",
      "  {",
      "    pixels exch color_packet putinterval",
      "  } for",
      "  pixels 0 number_pixels getinterval",
      "} bind def",
      "",
      "/PseudoClassImage",
      "{",
      "  %",
      "  % Display a PseudoClass image.",
      "  %",
      "  % Parameters: ",
      "  %   colors: number of colors in the colormap.",
      "  %   colormap: red, green, blue color packets.",
      "  %",
      "  currentfile buffer readline pop",
      "  token pop /colors exch def pop",
      "  /colors colors 3 mul def",
      "  /colormap colors string def",
      "  currentfile colormap readhexstring pop pop",
      "  systemdict /colorimage known",
      "  {",
      "    columns rows 8",
      "    [",
      "      columns 0 0",
      "      rows neg 0 rows",
      "    ]",
      "    { PseudoClassPacket } false 3 colorimage",
      "  }",
      "  {",
      "    %",
      "    % No colorimage operator;  convert to grayscale.",
      "    %",
      "    columns rows 8",
      "    [",
      "      columns 0 0",
      "      rows neg 0 rows",
      "    ]",
      "    { GrayPseudoClassPacket } image",
      "  } ifelse",
      "} bind def",
      "",
      "/DisplayImage",
      "{",
      "  %",
      "  % Display a DirectClass or PseudoClass image.",
      "  %",
      "  % Parameters: ",
      "  %   x & y translation.",
      "  %   x & y scale.",
      "  %   image columns & rows.",
      "  %   orientation: 0-Portrait or 1-Landscape",
      "  %   class: 0-DirectClass or 1-PseudoClass.",
      "  %   compression: 0-RunlengthEncodedCompression or 1-NoCompression.",
      "  %   hex color packets.",
      "  %",
      "  gsave",
      "  currentfile buffer readline pop",
      "  token pop /x exch def",
      "  token pop /y exch def pop",
      "  x y translate",
      "  currentfile buffer readline pop",
      "  token pop /x exch def",
      "  token pop /y exch def pop",
      "  x y scale",
      "  currentfile buffer readline pop",
      "  token pop /columns exch def",
      "  token pop /rows exch def pop",
      "  currentfile buffer readline pop",
      "  token pop /orient exch def pop",
      "  orient 0 gt { 0.5 0.5 translate  90 rotate  -0.5 -0.5 translate } if",
      "  currentfile buffer readline pop",
      "  token pop /class exch def pop",
      "  currentfile buffer readline pop",
      "  token pop /compression exch def pop",
      "  class 0 gt { PseudoClassImage } { DirectClassImage } ifelse",
      "  grestore",
      "} bind def",
      "%%EndProlog",
      "%%Page:  1 1",
      NULL
};


#ifdef ANSI_FUNC

static void 
eps_colorHeader (PSImagePtr psim, FILE *fp)
#else

static void
eps_colorHeader (psim, fp)
PSImagePtr psim;
FILE	   *fp;
#endif
{
	register char **line;
	int icols=0, irows=0, scols=0, srows=0, turnflag=0;
	float   llx = 0.0, lly = 0.0;
 	time_t  timer;

	/* Get the common page parameters. */
	eps_pageParams (psim, &llx, &lly, &icols, &irows, &scols, &srows,
	    &turnflag);

	/* Now write header and prolog stuff */
	fprintf (fp, "%%!PS-Adobe-3.0 EPSF-3.0\n");
	fprintf (fp, "%%%%Title: XImtool Screen Hardcopy\n");
	fprintf (fp, "%%%%Creator: XImtool\n");
  	timer = time ((time_t *) NULL);
	(void) localtime (&timer);
	fprintf (fp, "%%%%CreationDate: %s", ctime(&timer));
	if (psim->annotate) 
	    fprintf (fp, "%%%%BoundingBox: %d %d %d %d\n",
                (int) (MAX (0, llx - X_ANNOT_MARGIN)), 
	        (int) (MAX (0, lly - Y_ANNOT_MARGIN)),
                (int) ( llx + scols + 0.5 + X_ANNOT_MARGIN), 
	        (int) ( lly + srows + 0.5 + Y_ANNOT_MARGIN) );
	else
	    fprintf (fp, "%%%%BoundingBox: %d %d %d %d\n",
                (int) (MAX (0, llx - 10)), 
	        (int) (MAX (0, lly - 10)),
                (int) ( llx + scols + 0.5 + 10), 
	        (int) ( lly + srows + 0.5 + 10) );
	fprintf (fp, "%%%%Pages: 1\n");
	fprintf (fp, "%%%%EndComments\n");

  	/* Output remaining Postscript prolog commands.  */
  	for (line = EPSColorProlog; *line; line++)
    	    fprintf (fp, "%s\n",*line);
    	fprintf (fp, "userdict begin\n");
  	fprintf (fp, "%%%%BeginData:\n");

	fprintf (fp, "DisplayImage\n");

	/* Now write the image display parameters, specifically
	 *
	 *   x & y translation.
	 *   x & y scale.
	 *   image columns & rows.
	 *   orientation: 0-Portrait or 1-Landscape
	 *   class: 0-DirectClass or 1-PseudoClass.
	 *   compression: 0-RunlengthEncodedCompression or 1-NoCompression.
	 *   hex color packets.
	 */

 	fprintf (fp, "%d %d\n%u %u\n%u %u\n%d\n%d\n%d\n",
            (int) llx, (int) lly,
	    (int) scols, (int) srows,
	    psim->cols, psim->rows,
	    turnflag,
            psim->colorClass == EPS_PSEUDOCOLOR,
	    psim->compression == NoCompression);
}


/* EPS_WRITETRAILER -- Output Postscript trailer blurb.
 */
#ifdef ANSI_FUNC

static void 
eps_writeTrailer (FILE *fp)
#else

static void
eps_writeTrailer (fp)
FILE	*fp;
#endif
{
	fprintf (fp, "\n");
	fprintf (fp, "showpage\n");
	fprintf (fp, "%%%%EndData\n");
	fprintf (fp, "end\n");
	fprintf (fp, "%%%%PageTrailer\n");
	fprintf (fp, "%%%%Trailer\n");
	fprintf (fp, "%%%%EOF\n");
}


/* EPS_SIMPLETRAILER -- Output Postscript trailer blurb.
 */
#ifdef ANSI_FUNC

static void 
eps_simpleTrailer (FILE *fp)
#else

static void
eps_simpleTrailer (fp)
FILE	*fp;
#endif
{
	fprintf (fp, "showpage\n");
}


#ifdef ANSI_FUNC

static void 
eps_flushPix (FILE *fp)
#else

static void 
eps_flushPix (fp)
FILE	*fp;
#endif
{
	pixbuf[pixnum] = '\0';
	fprintf (fp, "%s", pixbuf);
	pixnum = 0;
	lpix = 0;
}


/* Utility Routines.
 */

/* TICSTEP --  calculate nice intervals for the ticmarks.
 */
#ifdef ANSI_FUNC

static float 
ticstep (float range, int nsteps)
#else

static float
ticstep (range,nsteps)
float	range;
int	nsteps;
#endif
{
	double df, t2, t5, p1, p2, p3;
	float ticstep;
	int logtic;

        df = range / (float)(nsteps + 1);
        t2 = 0.301029996;
        t5 = 0.698970004;
        p1 = log10 ((double)(df > 0.0 ? df : -df));
        p2 = (int) p1;
        p3 = p1 - p2;
        if(p3 < 0.) {
            p3 = p2 + 1.;
            p2 = p2 - 1.;
	}

      if (p3 < 1.0e-10)
         ticstep = pow((double)10.0,(double)(p2));
      else if (p3 > 0. && p3 <= t2)
         ticstep = pow((double)10.0,(double)(p2+t2));
      else if (p3 > t2 && p3 <= t5)
         ticstep = pow((double)10.0,(double)(p2+t5));
      else if (p3 > t5 && p3 <= 1.) 
         ticstep = pow((double)10.0,(double)(p2+1.));
      else
         ticstep = df;

      logtic = (int) log10(ticstep) - 1;
      ticstep = (int)( (ticstep / pow((double)10.,(double)logtic)) *
	   pow((double)10.,(double)logtic) );

      if (ticstep < 0.1) ticstep = 0.10;

      return (ticstep);
}


/* MAKE_LABEL -- Generate the label for the output printer page.
 */
#ifdef ANSI_FUNC

static char *
make_label (void)
#else

static char *
make_label()
#endif
{
	struct	tm  *lt;
	static  char label_buffer[256];
        char    hostname[32], username[32];
        struct  passwd *pw;
	time_t	clock;

#ifdef SOLARIS
        sysinfo (SI_HOSTNAME, hostname, 32);
#else
        gethostname (hostname, 32);
#endif

        clock = time(0);
        pw = getpwuid (getuid());
        strcpy (username, pw->pw_name);
        endpwent();

	lt = localtime (&clock);
        sprintf (label_buffer, "NOAO/IRAF  %s@%s  %s",
            username, hostname, (char *)asctime(lt));

        return (label_buffer);
}
