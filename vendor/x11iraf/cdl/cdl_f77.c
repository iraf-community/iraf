#include <stdio.h>
#include <stdlib.h>
#define  CDL_LIBRARY_SOURCE
#include "cdl.h"
#include "cdl_f77.h"


#define ABS(a)          (((a)<0) ? -(a) : (a))


/*  CDL_F77.C -- Fortran binding for the CDL package.
 */

static CDLPtr	cdl_f = (CDLPtr) NULL;


#ifdef ANSI_FUNC
static char * sstrip (char *instr, int len);
#else
static char * sstrip();
#endif

#ifdef ANSI_FUNC
static void spad (char *outstr, int len);
#else
static void spad ();
#endif



/*  CDF_OPEN -- Open and initialize the CDL package.  */

#ifdef ANSI_FUNC

void 
CDF_OPEN (
    char *imtdev,                       /* connection device	*/
    int *ier,				/* error code		*/
    int len				/* string length	*/
)
#else

void
CDF_OPEN (imtdev, ier, len)
char	*imtdev;                        /* connection device	*/
int	*ier;				/* error code		*/
int	len;				/* string length	*/
#endif
{
	char  *dev;

	if (imtdev == NULL || *imtdev == '\0')
	    dev = NULL;
	else {
	    dev = sstrip (imtdev, len);
	    if (*dev == '\0')
		dev = getenv("IMTDEV");
	}
	*ier = ((cdl_f = (CDLPtr) cdl_open (dev)) == (CDLPtr) NULL ? 1 : 0);
}


/*  CDF_DISPLAYPIX -- Display a raw pixel array to the server.  */

#ifdef ANSI_FUNC

void 
CDF_DISPLAYPIX (
    uchar *pix,				/* pixels to display	*/
    int *nx,
    int *ny,				/* image dimensions	*/
    int *bitpix,			/* pixel size		*/
    int *frame,				/* display frame	*/
    int *fbconfig,			/* FB config number	*/
    int *zscale,			/* do zscale of image?	*/
    int *ier				/* error code		*/
)
#else

void
CDF_DISPLAYPIX (pix, nx, ny, bitpix, frame, fbconfig, zscale, ier)
uchar	*pix;				/* pixels to display	*/
int	*nx, *ny;			/* image dimensions	*/
int	*bitpix;			/* pixel size		*/
int	*frame;				/* display frame	*/
int	*fbconfig;			/* FB config number	*/
int	*zscale;			/* do zscale of image?	*/
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_displayPix (cdl_f, pix, *nx, *ny, *bitpix, *frame, 
	    *fbconfig, *zscale);
} 
 

/*  CDF_READCURSOR --  Read the current cursor position. */

#ifdef ANSI_FUNC

void 
CDF_READCURSOR (
    int *sample,                 	/* wait for keystroke?  */
    float *x,
    float *y,                 		/* position (output)	*/
    int	*wcs,				/* WCS			*/
    char *key,                   	/* keystroke (output)	*/
    int *ier				/* error code		*/
)
#else

void
CDF_READCURSOR (sample, x, y, wcs, key, ier)
int     *sample;                 	/* wait for keystroke?  */
float   *x, *y;                 	/* position (output)	*/
int	*wcs;				/* WCS			*/
char    *key;                   	/* keystroke (output)	*/
int	*ier;				/* error code		*/
#endif
{
	*ier = (cdl_readCursor (cdl_f, *sample, x, y, wcs, key) != 0 ? 0 : 1);
} 


/*  CDF_DISPLAYIRAF -- Display an IRAF OIF format image.  */

#ifdef ANSI_FUNC

void 
CDF_DISPLAYIRAF (
    char *fname,			/* image name		*/
    int *band,				/* image band if 3-d	*/
    int *frame,				/* display frame	*/
    int *fbconfig,			/* frame buffer config	*/
    int *zscale,			/* do zscale of image?	*/
    int *ier,				/* error code		*/
    int len				/* string length	*/
)
#else

void
CDF_DISPLAYIRAF (fname, band, frame, fbconfig, zscale, ier, len)
char	*fname;				/* image name		*/
int	*band;				/* image band if 3-d	*/
int	*frame;				/* display frame	*/
int	*fbconfig;			/* frame buffer config	*/
int	*zscale;			/* do zscale of image?	*/
int	*ier;				/* error code		*/
int	len;				/* string length	*/
#endif
{
	*ier = cdl_displayIRAF(cdl_f, sstrip(fname, len), 
		*band, *frame, *fbconfig, *zscale);
}


/* CDF_ISIRAF -- Test a file to see if it is a IRAF file.  */

#ifdef ANSI_FUNC

void 
CDF_ISIRAF (
    char *fname,			/* input filename 	*/
    int *isiraf,
    int len				/* string length	*/
)
#else

void 
CDF_ISIRAF (fname, isiraf, len)
char	*fname;				/* input filename 	*/
int	*isiraf;
int	len;				/* string length	*/
#endif
{
	*isiraf = cdl_isIRAF (sstrip(fname, len));
}


/*  CDF_READIRAF -- Read the pixels from an IRAF OIF format image.  */

#ifdef ANSI_FUNC

void 
CDF_READIRAF (
    char *fname,			/* image name		*/
    int *band,				/* image band if 3-d	*/
    uchar *pix,				/* pixel array (output) */
    int *nx,
    int *ny,				/* dimensions (output)	*/
    int *bitpix,			/* pixel size (output)	*/
    char *title,			/* image title		*/
    int *ier,				/* error code		*/
    int len				/* string length	*/
)
#else

void
CDF_READIRAF (fname, band, pix, nx, ny, bitpix, title, ier, len)
char	*fname;				/* image name		*/
int	*band;				/* image band if 3-d	*/
uchar	*pix;				/* pixel array (output) */
int	*nx, *ny;			/* dimensions (output)	*/
int	*bitpix;			/* pixel size (output)	*/
char	*title;				/* image title		*/
int	*ier;				/* error code		*/
int	len;				/* string length	*/
#endif
{
	uchar *data = (uchar) NULL;

	*ier = cdl_readIRAF (sstrip(fname, len), 
		   *band, &data, nx, ny, bitpix, title);

	spad(title, len);
	bcopy (data, pix, ((*nx) * (*ny) * (ABS(*bitpix)/8)) );

	free (data);
}


/*  CDF_DISPLAYFITS -- Display a simple FITS format image.  */

#ifdef ANSI_FUNC

void 
CDF_DISPLAYFITS (
    char *fname,
    int *frame,
    int *fbconfig,
    int *zscale,
    int *ier,				/* error code		*/
    int len				/* string length	*/
)
#else

void
CDF_DISPLAYFITS (fname, frame, fbconfig, zscale, ier, len)
char	*fname;
int	*frame;
int	*fbconfig;
int	*zscale;
int	*ier;				/* error code		*/
int	len;				/* string length	*/
#endif
{
	*ier = cdl_displayFITS (cdl_f, sstrip(fname, len), 
		   *frame, *fbconfig, *zscale);
}


/*  CDF_ISFITS -- Test a file to see if it is a simple FITS file.  */

#ifdef ANSI_FUNC

void 
CDF_ISFITS (
    char *fname,			/* input filename 	*/
    int *isfits,
    int len				/* string length	*/
)
#else

void 
CDF_ISFITS (fname, isfits, len)
char	*fname;				/* input filename 	*/
int	*isfits;
int	len;				/* string length	*/
#endif
{
	*isfits = cdl_isFITS (sstrip(fname, len));
}


/*  CDF_READFITS -- Read the pixels from a simple FITS format image.  */

#ifdef ANSI_FUNC

void 
CDF_READFITS (
    char *fname,			/* image name		*/
    uchar *pix,				/* pixel array (output) */
    int *nx,
    int *ny,				/* dimensions (output)	*/
    int *bitpix,			/* pixel size (output)	*/
    char *title,			/* image title		*/
    int *ier,				/* error code		*/
    int len				/* string length	*/
)
#else

void
CDF_READFITS (fname, pix, nx, ny, bitpix, title, ier, len)
char	*fname;				/* image name		*/
uchar	*pix;				/* pixel array (output) */
int	*nx, *ny;			/* dimensions (output)	*/
int	*bitpix;			/* pixel size (output)	*/
char	*title;				/* image title		*/
int	*ier;				/* error code		*/
int	len;				/* string length	*/
#endif
{
	*ier = cdl_readFITS (sstrip(fname, len), &pix, nx, ny, bitpix, title);
}


/*  CDF_SETCURSOR --  Set the current logical cursor position.  */

#ifdef ANSI_FUNC

void 
CDF_SETCURSOR (
    int *x,
    int *y,                 		/* position 		*/
    int *wcs,                   	/* cursor wcs		*/
    int *ier				/* error code		*/
)
#else

void
CDF_SETCURSOR (x, y, wcs, ier)
int   	*x, *y;                 	/* position 		*/
int     *wcs;                   	/* cursor wcs		*/
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_setCursor (cdl_f, *x, *y, *wcs);
} 


/*  CDF_SETWCS --  Set the WCS of the screen.  */

#ifdef ANSI_FUNC

void 
CDF_SETWCS (
    char *name,                  	/* name string          */
    char *title,                 	/* title string         */
    float *a,
    float *b,
    float *c,
    float *d,             		/* WCS values           */
    float *tx,
    float *ty,                 		/* translation          */
    float *z1,
    float *z2,                 		/* zscale values        */
    int *zt,                     	/* transformation type  */
    int *ier,				/* error code		*/
    int nlen,
    int tlen				/* string lengths	*/
)
#else

void
CDF_SETWCS (name, title, a, b, c, d, tx, ty, z1, z2, zt, ier, nlen, tlen)
char    *name;                  	/* name string          */
char    *title;                 	/* title string         */
float   *a, *b, *c, *d;             	/* WCS values           */
float   *tx, *ty;                 	/* translation          */
float   *z1, *z2;                 	/* zscale values        */
int     *zt;                     	/* transformation type  */
int	*ier;				/* error code		*/
int	nlen, tlen;			/* string lengths	*/
#endif
{
	char localname[SZ_FNAME], localtitle[SZ_FNAME];

	strcpy (localname, sstrip (name, min(nlen,SZ_FNAME)));
	strcpy (localtitle, sstrip (title, min(tlen,SZ_FNAME)));
	*ier = cdl_setWCS (cdl_f, localname, localtitle, *a, *b, *c, *d,
	    *tx, *ty, *z1, *z2, *zt);
} 


/*  CDF_GETWCS -- Get the current display frame WCS information.  */

#ifdef ANSI_FUNC

void 
CDF_GETWCS (
    char *name,                  	/* name string          */
    char *title,                 	/* title string         */
    float *a,
    float *b,
    float *c,
    float *d,         			/* WCS values           */
    float *tx,
    float *ty,               		/* translation          */
    float *z1,
    float *z2,               		/* zscale values        */
    int *zt,                    	/* transformation type  */
    int *ier,				/* error code		*/
    int nlen,
    int tlen				/* string lengths	*/
)
#else

void
CDF_GETWCS (name, title, a, b, c, d, tx, ty, z1, z2, zt, ier, nlen, tlen)
char    *name;                  	/* name string          */
char    *title;                 	/* title string         */
float   *a, *b, *c, *d;         	/* WCS values           */
float   *tx, *ty;               	/* translation          */
float   *z1, *z2;               	/* zscale values        */
int     *zt;                    	/* transformation type  */
int	*ier;				/* error code		*/
int	nlen, tlen;			/* string lengths	*/
#endif
{
	*ier = cdl_getWCS (cdl_f, name, title, a, b, c, d, tx, ty, z1, z2, zt);
	spad (name, nlen);
	spad (title, tlen);
} 


/*  CDF_SETMAPPING -- Set the mapping information to be sent with the next
 *  cdl_setWcs() call.
 */
#ifdef ANSI_FUNC

void 
CDF_SETMAPPING  (
    char *region,                  	/* region name 		*/
    float *sx,				/* source rect		*/
    float *sy,
    int *snx, 				/* source extent	*/
    int *sny,
    int *dx, 				/* dest rect		*/
    int *dy,
    int *dnx, 				/* dest extent		*/
    int *dny,
    char *ref,                   	/* reference name	*/
    int *ier,				/* error code		*/
    int reglen,				/* string lengths	*/
    int reflen
)
#else

void 
CDF_SETMAPPING  (region, sx,sy,snx,sny, dx,dy,dnx,dny, ref, ier, reglen, reflen)
char    *region;                  	/* region name 		*/
float	*sx, *sy;			/* source rect		*/
int	*snx, *sny;			/* source extent	*/
int	*dx, *dy;			/* dest rect		*/
int	*dnx, *dny;			/* dest extent		*/
char    *ref;                  		/* reference name	*/
int	*ier;				/* error code		*/
int	reglen, reflen;			/* string lengths	*/
#endif
{
	char objreg[SZ_FNAME], objref[SZ_FNAME];

	strcpy (objreg, sstrip (region, min(reglen, SZ_FNAME)));
	strcpy (objref, sstrip (ref, min(reflen, SZ_FNAME)));
	*ier = cdl_setMapping (cdl_f, objreg, *sx,*sy,*snx,*sny,
		*dx,*dy,*dnx,*dny, objref);
}


/*  CDF_GETMAPPING --  Get the mapping information returned with the last
 *  cdl_getWcs() call.
 */
#ifdef ANSI_FUNC
void 
CDF_GETMAPPING  (
    char *region,                  	/* region name 		*/
    float *sx,				/* source rect		*/
    float *sy,
    int *snx, 				/* source extent	*/
    int *sny,
    int *dx, 				/* dest rect		*/
    int *dy,
    int *dnx, 				/* dest extent		*/
    int *dny,
    char *ref,                   	/* reference name	*/
    int *ier,				/* error code		*/
    int reglen,				/* string lengths	*/
    int reflen
)

#else
void 
CDF_GETMAPPING  (region, sx,sy,snx,sny, dx,dy,dnx,dny, ref, ier, reglen, reflen)
char    *region;                  	/* region name 		*/
float	*sx, *sy;			/* source rect		*/
int	*snx, *sny;			/* source extent	*/
int	*dx, *dy;			/* dest rect		*/
int	*dnx, *dny;			/* dest extent		*/
char    *ref;                  		/* reference name	*/
int	*ier;				/* error code		*/
int	reglen, reflen;			/* string lengths	*/
#endif
{
	*ier = cdl_getMapping (cdl_f, region, sx,sy,snx,sny,
	    dx,dy,dnx,dny, ref);
	spad (region, reglen);
	spad (ref, reflen);
}


/*  CDF_QUERYMAP -- Query a mapping given the wcs number.
 */
#ifdef ANSI_FUNC

void 
CDF_QUERYMAP  (
    int *wcs,                  		/* requested wcs number	*/
    char *region,                   	/* region name		*/
    float *sx,				/* source rect		*/
    float *sy,
    int *snx, 				/* source extent	*/
    int *sny,
    int *dx, 				/* dest rect		*/
    int *dy,
    int *dnx, 				/* dest extent		*/
    int *dny,
    char *objref,                   	/* reference name	*/
    int *ier,				/* error code		*/
    int reglen,				/* string lengths	*/
    int reflen
)
#else

void 
CDF_QUERYMAP (wcs, region, sx,sy,snx,sny, dx,dy,dnx,dny, objref, ier, reglen, reflen)
int     *wcs;                  		/* requested wcs number	*/
char    *region;                  	/* region name 		*/
float	*sx, *sy;			/* source rect		*/
int	*snx, *sny;			/* source extent	*/
int	*dx, *dy;			/* dest rect		*/
int	*dnx, *dny;			/* dest extent		*/
char    *objref;                  	/* reference name	*/
int	*ier;				/* error code		*/
int	reglen, reflen;			/* string lengths	*/
#endif
{
	*ier = cdl_queryMap (cdl_f, *wcs, region, sx,sy,snx,sny,
	    dx,dy,dnx,dny, objref);
	spad (region, reglen);
	spad (objref, reflen);
}


/*  CDF_CLEARFRAME -- Erase the current display frame.  */

#ifdef ANSI_FUNC

void 
CDF_CLEARFRAME (
    int *ier				/* error code		*/
)
#else

void
CDF_CLEARFRAME (ier)
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_clearFrame (cdl_f);
} 


/*  CDF_SELECTFB -- Select an appropriate frame buffer for the image.  */

#ifdef ANSI_FUNC

void 
CDF_SELECTFB (
    int *nx,
    int *ny,				/* image size		*/
    int *fb,				/* frame buffer		*/
    int *w,
    int *h,				/* frame size		*/
    int *nf,				/* number of frames	*/
    int *reset				/* reset after select	*/
)
#else

void
CDF_SELECTFB (nx, ny, fb, w, h, nf, reset)
int	*nx, *ny;			/* image size		*/
int	*fb;				/* frame buffer		*/
int	*w, *h;				/* frame size		*/
int	*nf;				/* number of frames	*/
int	*reset;				/* reset after select	*/
#endif
{
	cdl_selectFB (cdl_f, *nx, *ny, fb, w, h, nf, *reset);
} 


/*  CDF_CLOSE -- Close the CDL package descriptor.  */

#ifdef ANSI_FUNC

void 
CDF_CLOSE (void)
#else

void
CDF_CLOSE ()
#endif
{
	if (cdl_f) cdl_close (cdl_f);
} 


/*  CDF_READIMAGE -- Read the currently displayed image.  */

#ifdef ANSI_FUNC

void 
CDF_READIMAGE (
    uchar *pix,                   	/* image pixels (output)*/
    int *nx,
    int *ny,               		/* dimensions (output)  */
    int *ier				/* error code		*/
)
#else

void
CDF_READIMAGE (pix, nx, ny, ier)
uchar   *pix;                   	/* image pixels (output)*/
int     *nx, *ny;               	/* dimensions (output)  */
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_readImage (cdl_f, &pix, nx, ny);
} 


/*  CDF_READFRAMEBUFFER -- Read the contents of the entire frame buffer.  */

#ifdef ANSI_FUNC

void 
CDF_READFRAMEBUFFER (
    uchar *pix,                   	/* image pixels (output)*/
    int *nx,
    int *ny,               		/* dimensions (output)  */
    int *ier				/* error code		*/
)
#else

void
CDF_READFRAMEBUFFER (pix, nx, ny, ier)
uchar   *pix;                   	/* image pixels (output)*/
int     *nx, *ny;               	/* dimensions (output)  */
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_readFrameBuffer (cdl_f, &pix, nx, ny);
} 


/*  CDF_COMPZSCALE -- Compute the optimal z1/z2 values for an array.  */

#ifdef ANSI_FUNC

void 
CDF_COMPZSCALE (
    uchar *pix,                   	/* data to be sampled   */
    int *nx,
    int *ny,                 		/* image dimensions     */
    int *bitpix,                 	/* bits per pixel       */
    float *z1,
    float *z2               		/* min/max zscale values*/
)
#else

void
CDF_COMPZSCALE (pix, nx, ny, bitpix, z1, z2)
uchar   *pix;                   	/* data to be sampled   */
int     *nx, *ny;                 	/* image dimensions     */
int     *bitpix;                 	/* bits per pixel       */
float   *z1, *z2;               	/* min/max zscale values*/
#endif
{
	cdl_computeZscale (cdl_f, pix, *nx, *ny, *bitpix, z1, z2);
} 


/*  CDF_ZSCALEIMAGE -- Compute the optimal z1/z2 values for an array.  */

#ifdef ANSI_FUNC

void 
CDF_ZSCALEIMAGE (
    uchar *pix,                   	/* data to be sampled   */
    int *nx,
    int *ny,                 		/* image dimensions     */
    int *bitpix,                 	/* bits per pixel       */
    float *z1,
    float *z2               		/* min/max zscale values*/
)
#else

void
CDF_ZSCALEIMAGE (pix, nx, ny, bitpix, z1, z2)
uchar   *pix;                   	/* data to be sampled   */
int     *nx, *ny;                 	/* image dimensions     */
int     *bitpix;                 	/* bits per pixel       */
float   *z1, *z2;               	/* min/max zscale values*/
#endif
{
	cdl_zscaleImage (cdl_f, &pix, *nx, *ny, *bitpix, *z1, *z2);
} 


/*  CDF_PRINTPIX --  Print the given pixels as EPS to the named command. */

#ifdef ANSI_FUNC

void 
CDF_PRINTPIX (
    char *cmd,				/* command string	*/
    uchar *pix,				/* pixel array		*/
    int *nx,
    int *ny,				/* image dimensions	*/
    int *annotate,			/* annotate output?	*/
    int *ier,				/* error code		*/
    int len				/* string length 	*/
)
#else

void
CDF_PRINTPIX (cmd, pix, nx, ny, annotate, ier, len)
char	*cmd;				/* command string	*/
uchar	*pix;				/* pixel array		*/
int	*nx, *ny;			/* image dimensions	*/
int	*annotate;			/* annotate output?	*/
int	*ier;				/* error code		*/
int	len;				/* string length 	*/
#endif
{
	*ier = cdl_printPix (cdl_f, sstrip(cmd, len), pix, *nx, *ny, *annotate);
} 


/*  CDF_PRINTPIXTOFILE -- Print the given pixels as EPS to the named file.  */

#ifdef ANSI_FUNC

void 
CDF_PRINTPIXTOFILE (
    char *fname,			/* filename		*/
    uchar *pix,				/* pixel array		*/
    int *nx,
    int *ny,				/* image dimensions	*/
    int *annotate,			/* annotate output?	*/
    int *ier,				/* error code		*/
    int len				/* string length	*/
)
#else

void
CDF_PRINTPIXTOFILE (fname, pix, nx, ny, annotate, ier, len)
char	*fname;				/* filename		*/
uchar	*pix;				/* pixel array		*/
int	*nx, *ny;			/* image dimensions	*/
int	*annotate;			/* annotate output?	*/
int	*ier;				/* error code		*/
int	len;				/* string length	*/
#endif
{
	*ier = cdl_printPixToFile (cdl_f, sstrip(fname, len), pix, *nx, *ny, *annotate);
} 


/*  CDF_READSUBRASTER -- Read a rectangular region of the frame buffer.  */

#ifdef ANSI_FUNC

void 
CDF_READSUBRASTER (
    int *lx,
    int *ly,              		/* region corner        */
    int *nx,
    int *ny,                 		/* dimensions           */
    uchar *pix,                   	/* image pixels (output)*/
    int *ier				/* error code		*/
)
#else

void
CDF_READSUBRASTER (lx, ly, nx, ny, pix, ier)
int     *lx, *ly;              		/* region corner        */
int     *nx, *ny;                 	/* dimensions           */
uchar   *pix;                   	/* image pixels (output)*/
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_readSubRaster (cdl_f, *lx, *ly, *nx, *ny, &pix);
} 


/*  CDF_WRITESUBRAS -- Write a rectangular region of the frame buffer.  */

#ifdef ANSI_FUNC

void 
CDF_WRITESUBRAS (
    int *lx,
    int *ly,              		/* region corner        */
    int *nx,
    int *ny,                 		/* dimensions           */
    uchar *pix,                   	/* subraster pixels     */
    int *ier				/* error code		*/
)
#else

void
CDF_WRITESUBRAS (lx, ly, nx, ny, pix, ier)
int     *lx, *ly;              		/* region corner        */
int     *nx, *ny;                 	/* dimensions           */
uchar   *pix;                   	/* subraster pixels     */
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_writeSubRaster (cdl_f, *lx, *ly, *nx, *ny, pix);
} 


/*  CDF_SETFBCONFIG -- Set the frame buffer configuration number.  */

#ifdef ANSI_FUNC

void 
CDF_SETFBCONFIG (
    int *configno			/* fb config number	*/
)
#else

void
CDF_SETFBCONFIG (configno)
int	*configno;			/* fb config number	*/
#endif
{
	cdl_setFBConfig (cdl_f, *configno);
} 


/*  CDF_GETFBCONFIG -- Get the frame buffer configuration number.  */

#ifdef ANSI_FUNC

void 
CDF_GETFBCONFIG (
    int *configno,                      /* fb config number     */
    int *w,
    int *h,                         	/* fb frame size        */
    int *nframes                       	/* number of frames     */
)
#else

void
CDF_GETFBCONFIG (configno, w, h, nframes)
int     *configno;                      /* fb config number     */
int     *w, *h;                         /* fb frame size        */
int     *nframes;                       /* number of frames     */
#endif
{
	cdl_getFBConfig (cdl_f, configno, w, h, nframes);
} 


/*  CDF_LOOKUPFBSIZE -- Lookup the frame buffer dimensions.*/

#ifdef ANSI_FUNC

void 
CDF_LOOKUPFBSIZE (
    int *configno,                      /* fb config number     */
    int *w,
    int *h,                         	/* fb frame size        */
    int *nf                            	/* number of frames     */
)
#else

void
CDF_LOOKUPFBSIZE (configno, w, h, nf)
int     *configno;                      /* fb config number     */
int     *w, *h;                         /* fb frame size        */
int     *nf;                            /* number of frames     */
#endif
{
	cdl_lookupFBSize (cdl_f, *configno, w, h, nf);
}


/*  CDF_SETFRAME -- Set the current display frame.  */

#ifdef ANSI_FUNC

void 
CDF_SETFRAME (
    int *frame				/* frame number		*/
)
#else

void
CDF_SETFRAME (frame)
int	*frame;				/* frame number		*/
#endif
{
	cdl_setFrame (cdl_f, *frame);
} 


/*  CDF_SETZTRANS -- Set the current zscale transform parameters.  */

#ifdef ANSI_FUNC

void 
CDF_SETZTRANS (
    int *ztrans				/* z-transform type	*/
)
#else

void
CDF_SETZTRANS (ztrans)
int	*ztrans;			/* z-transform type	*/
#endif
{
	cdl_setZTrans (cdl_f, *ztrans);
} 


/*  CDF_SETZSCLAE -- Set the current zscale transform parameters.  */

#ifdef ANSI_FUNC

void 
CDF_SETZSCALE (
    float *z1,
    float *z2				/* zscale values	*/
)
#else

void
CDF_SETZSCALE (z1, z2)
float	*z1, *z2;			/* zscale values	*/
#endif
{
	cdl_setZScale (cdl_f, *z1, *z2);
} 


/*  CDF_SETSAMPLE -- Set the number of zscale sample points to use.  */

#ifdef ANSI_FUNC

void 
CDF_SETSAMPLE (
    int *nsample			/* no. of sample pts	*/
)
#else

void
CDF_SETSAMPLE (nsample)
int	*nsample;			/* no. of sample pts	*/
#endif
{
	cdl_setSample (cdl_f, *nsample);
} 


/*  CDF_SETSAMPLELINES -- Set the number of zscale sample lines to use.  */

#ifdef ANSI_FUNC

void 
CDF_SETSAMPLELINES (
    int *nlines				/* no. of sample lines	*/
)
#else

void
CDF_SETSAMPLELINES (nlines)
int	*nlines;			/* no. of sample lines	*/
#endif
{
	cdl_setSampleLines (cdl_f, *nlines);
} 


/*  CDF_SETCONTRAST -- Set the zscale contrast value.  */

#ifdef ANSI_FUNC

void 
CDF_SETCONTRAST (
    float *contrast			/* contrast value	*/
)
#else

void
CDF_SETCONTRAST (contrast)
float	*contrast;			/* contrast value	*/
#endif
{
	cdl_setContrast (cdl_f, *contrast);
} 


/*  CDF_SETNAME -- Set the image name for the WCS string.  */

#ifdef ANSI_FUNC

void 
CDF_SETNAME (
    char *imname,			/* image name		*/
    int len				/* string length	*/
)
#else

void
CDF_SETNAME (imname, len)
char	*imname;			/* image name		*/
int	len;				/* string length	*/
#endif
{
	cdl_setName (cdl_f, sstrip(imname, len));
} 


/*  CDF_SETTITLE -- Set the image title for the WCS string.  */

#ifdef ANSI_FUNC

void 
CDF_SETTITLE (
    char *imtitle,			/* image title		*/
    int len				/* string length	*/
)
#else

void
CDF_SETTITLE (imtitle, len)
char	*imtitle;			/* image title		*/
int	len;				/* string length	*/
#endif
{
	cdl_setTitle (cdl_f, sstrip(imtitle, len));
} 


/*  CDF_GETFRAME -- Get the current display frame.  */

#ifdef ANSI_FUNC

void 
CDF_GETFRAME (
    int *frame				/* frame number		*/
)
#else

void
CDF_GETFRAME (frame)
int	*frame;				/* frame number		*/
#endif
{
	cdl_getFrame (cdl_f, frame);
} 


/*  CDF_GETZTRANS -- Get the current zscale transform parameters.  */

#ifdef ANSI_FUNC

void 
CDF_GETZTRANS (
    int *ztrans                        	/* z-transform type     */
)
#else

void
CDF_GETZTRANS (ztrans)
int     *ztrans;                        /* z-transform type     */
#endif
{
	cdl_getZTrans (cdl_f, ztrans);
} 


/*  CDF_GETZSCALE -- Get the current zscale transform parameters.  */

#ifdef ANSI_FUNC

void 
CDF_GETZSCALE (
    float *z1,
    float *z2                       	/* zscale values        */
)
#else

void
CDF_GETZSCALE (z1, z2)
float   *z1, *z2;                       /* zscale values        */
#endif
{
	cdl_getZScale (cdl_f, z1, z2);
} 


/*  CDF_GETSAMPLE -- Get the number of zscale sample points to use.  */

#ifdef ANSI_FUNC

void 
CDF_GETSAMPLE (
    int *nsample                       	/* no. of sample pts    */
)
#else

void
CDF_GETSAMPLE (nsample)
int     *nsample;                       /* no. of sample pts    */
#endif
{
	cdl_getSample (cdl_f, nsample);
} 


/*  CDF_GETSAMPLELINES -- Get the number of zscale sample lines to use.  */

#ifdef ANSI_FUNC

void 
CDF_GETSAMPLELINES (
    int *nlines                        	/* no. of sample lines  */
)
#else

void
CDF_GETSAMPLELINES (nlines)
int     *nlines;                        /* no. of sample lines  */
#endif
{
	cdl_getSampleLines (cdl_f, nlines);
} 


/*  CDF_GETCONTRAST -- Get the zscale contrast value.  */

#ifdef ANSI_FUNC

void 
CDF_GETCONTRAST (
    float *contrast                     /* contrast value       */
)
#else

void
CDF_GETCONTRAST (contrast)
float   *contrast;                      /* contrast value       */
#endif
{
	cdl_getContrast (cdl_f, contrast);
} 


/*  CDF_GETNAME -- Get the image name for the WCS string.  */

#ifdef ANSI_FUNC

void 
CDF_GETNAME (
    char *imname,                       /* image name           */
    int len				/* string length	*/
)
#else

void
CDF_GETNAME (imname, len)
char    *imname;                        /* image name           */
int	len;				/* string length	*/
#endif
{
	cdl_getName (cdl_f, imname); spad (imname, len);
} 


/*  CDF_GETTITLE -- Get the image title for the WCS string.  */

#ifdef ANSI_FUNC

void 
CDF_GETTITLE (
    char *imtitle,                      /* image title          */
    int len				/* string length	*/
)
#else

void
CDF_GETTITLE (imtitle, len)
char    *imtitle;                       /* image title          */
int	len;				/* string length	*/
#endif
{
	cdl_getTitle (cdl_f, imtitle); spad (imtitle, len);
} 


/*  CDF_MAPFRAME -- Map the current frame buffer as an image for overlay.  */

#ifdef ANSI_FUNC

void 
CDF_MAPFRAME (
    int *frame,				/* fb frame to map	*/
    int *ier				/* error code		*/
)
#else

void
CDF_MAPFRAME (frame, ier)
int	*frame;				/* fb frame to map	*/
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_mapFrame (cdl_f, *frame);
} 


/*  CDF_MARKCOORDSFILE -- Mark a list of coords from a file. */

#ifdef ANSI_FUNC

void
CDF_MARKCOORDSFILE (
    char *fname,                        /* file name            */
    int  *type,                         /* marker type          */
    int  *size,                         /* marker size          */
    int  *color,                        /* marker color         */
    int  *label,                        /* label?               */
    int	 *ier,				/* error code		*/
    int	 len				/* string length	*/
)
#else

void
CDF_MARKCOORDSFILE (fname, type, size, color, label, ier, len)
char 	*fname;                        	/* file name            */
int  	*type;                         	/* marker type          */
int  	*size;                         	/* marker size          */
int  	*color;                        	/* marker color         */
int  	*label;                        	/* label?               */
int	*ier;				/* error code		*/
int	len;				/* string length	*/
#endif
{
        *ier = cdl_markCoordsFile (cdl_f, sstrip (fname, len), *type, *size, *color, *label);
}


/*  CDF_MARKPOINT --  Draw a mark on the display at the given point. */

#ifdef ANSI_FUNC

void 
CDF_MARKPOINT (
    int *x,
    int *y,				/* marker position	*/
    int *number,			/* if > 0, label value	*/
    int *size,				/* marker size (pixels) */
    int *type,				/* type to draw		*/
    int *color,				/* marker color		*/
    int *ier				/* error code		*/
)
#else

void
CDF_MARKPOINT (x, y, number, size, type, color, ier)
int	*x, *y;				/* marker position	*/
int	*number;			/* if > 0, label value	*/
int	*size;				/* marker size (pixels) */
int	*type;				/* type to draw		*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_markPoint (cdl_f, *x, *y, *number, *size, *type, *color);
} 


/*  CDF_MARKPOINTLABEL --  Draw a mark on the display at the given point. */

#ifdef ANSI_FUNC

void 
CDF_MARKPOINTLABEL (
    int  *x,
    int  *y,				/* marker position	*/
    char *label,			/* point label string   */
    int  *size,				/* marker size (pixels) */
    int  *type,				/* type to draw		*/
    int  *color,			/* marker color		*/
    int  *ier,				/* error code		*/
    int	 len				/* string length	*/
)
#else

void
CDF_MARKPOINTLABEL (x, y, label, size, type, color, ier, len)
int	*x, *y;				/* marker position	*/
char	*label;				/* point label string   */
int	*size;				/* marker size (pixels) */
int	*type;				/* type to draw		*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
int	len;				/* string length	*/
#endif
{
	sstrip (label, len);
	*ier = cdl_markPointLabel (cdl_f, *x, *y, sstrip(label, len),
	    *size, *type, *color);
} 


/*  CDF_MARKLINE --  Draw a line of given color between two points.  */

#ifdef ANSI_FUNC

void 
CDF_MARKLINE (
    int *xs,
    int *ys,				/* line start points	*/
    int *xe,
    int *ye,				/* line end points	*/
    int *color,				/* marker color		*/
    int *ier				/* error code		*/
)
#else

void
CDF_MARKLINE (xs, ys, xe, ye, color, ier)
int	*xs, *ys;			/* line start points	*/
int	*xe, *ye;			/* line end points	*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_markLine (cdl_f, *xs, *ys, *xe, *ye, *color);
} 


/*  CDF_MARKBOX --  Draw a rectangular box given two corner endpoints.  */

#ifdef ANSI_FUNC

void 
CDF_MARKBOX (
    int *lx,
    int *ly,				/* LL corner points	*/
    int *ux,
    int *uy,				/* UR corner points	*/
    int *fill,				/* fill rectangle?	*/
    int *color,				/* marker color		*/
    int *ier				/* error code		*/
)
#else

void
CDF_MARKBOX (lx, ly, ux, uy, fill, color, ier)
int	*lx, *ly;			/* LL corner points	*/
int	*ux, *uy;			/* UR corner points	*/
int	*fill;				/* fill rectangle?	*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_markBox (cdl_f, *lx, *ly, *ux, *uy, *fill, *color);
} 


/*  CDF_MARKPOLYGON -- Draw a polygon on the display, optionally filling.  */

#ifdef ANSI_FUNC

void 
CDF_MARKPOLYGON (
    int *xarray,
    int *yarray,			/* vertex points	*/
    int *npts,				/* number of corners	*/
    int *fill,				/* fill polygon?	*/
    int *color,				/* marker color		*/
    int *ier				/* error code		*/
)
#else

void
CDF_MARKPOLYGON (xarray, yarray, npts, fill, color, ier)
int	*xarray, *yarray;		/* vertex points	*/
int	*npts;				/* number of corners	*/
int	*fill;				/* fill polygon?	*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_markPolygon (cdl_f, xarray, yarray, *npts, *fill, *color);
} 


/*  CDF_MARKPOLYLINE -- Draw a polyline on the display, optionally filling.  */

#ifdef ANSI_FUNC

void 
CDF_MARKPOLYLINE (
    int *xarray,
    int *yarray,			/* vertex points	*/
    int *npts,				/* number of points	*/
    int *color,				/* marker color		*/
    int *ier				/* error code		*/
)
#else

void
CDF_MARKPOLYLINE (xarray, yarray, npts, color, ier)
int	*xarray, *yarray;		/* vertex points	*/
int	*npts;				/* number of points	*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_markPolyline (cdl_f, xarray, yarray, *npts, *color);
} 


/*  CDF_MARKCIRCLE -- Draw of circle on the display, optionally filling.  */

#ifdef ANSI_FUNC

void 
CDF_MARKCIRCLE (
    int *x,
    int *y,				/* center position	*/
    int *radius,			/* radius of circle	*/
    int *fill,				/* fill circle?		*/
    int *color,				/* marker color		*/
    int *ier				/* error code		*/
)
#else

void
CDF_MARKCIRCLE (x, y, radius, fill, color, ier)
int	*x, *y;				/* center position	*/
int	*radius;			/* radius of circle	*/
int	*fill;				/* fill circle?		*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_markCircle (cdl_f, *x, *y, *radius, *fill, *color);
} 


/*  CDF_MARKCIRCANNULI -- Draw circular annuli on the display.  */

#ifdef ANSI_FUNC

void 
CDF_MARKCIRCANNULI (
    int *x,
    int *y,				/* center position	*/
    int *radius,			/* radius of 1st annulus*/
    int *nannuli,			/* no. of annuli	*/
    int *sep,				/* annuli sep (pixels)	*/
    int *color,				/* marker color		*/
    int *ier				/* error code		*/
)
#else

void
CDF_MARKCIRCANNULI (x, y, radius, nannuli, sep, color, ier)
int	*x, *y;				/* center position	*/
int	*radius;			/* radius of 1st annulus*/
int	*nannuli;			/* no. of annuli	*/
int	*sep;				/* annuli sep (pixels)	*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_markCircAnnuli (cdl_f, *x, *y, *radius, *nannuli,
	    *sep, *color);
} 


/*  CDF_MARKELLIPSE -- Draw an ellipse.  */

#ifdef ANSI_FUNC

void 
CDF_MARKELLIPSE (
    int *x,
    int *y,				/* center position	*/
    int *xrad,
    int *yrad,				/* x and y radii	*/
    float *ang,				/* position angle (deg) */
    int *fill,				/* fill ellipse?	*/
    int *color,				/* marker color		*/
    int *ier				/* error code		*/
)
#else

void
CDF_MARKELLIPSE (x, y, xrad, yrad, ang, fill, color, ier)
int	*x, *y;				/* center position	*/
int	*xrad, *yrad;			/* x and y radii	*/
float	*ang;				/* position angle (deg) */
int	*fill;				/* fill ellipse?	*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_markEllipse (cdl_f, *x, *y, *xrad, *yrad, *ang,
	    *fill, *color);
} 


/*  CDF_MARKELLIPANNULI -- Draw elliptical annuli on the display.  */

#ifdef ANSI_FUNC

void 
CDF_MARKELLIPANNULI (
    int *x,
    int *y,				/* center position	*/
    int *xrad,
    int *yrad,				/* radius of 1st annulus*/
    float *ang,				/* rotation angle	*/
    int *nannuli,			/* no. of annuli	*/
    int *sep,				/* annuli sep (pixels)	*/
    int *color,				/* marker color		*/
    int *ier				/* error code		*/
)
#else

void
CDF_MARKELLIPANNULI (x, y, xrad, yrad, ang, nannuli, sep, color, ier)
int	*x, *y;				/* center position	*/
int	*xrad, *yrad;			/* radius of 1st annulus*/
float	*ang;				/* rotation angle	*/
int	*nannuli;			/* no. of annuli	*/
int	*sep;				/* annuli sep (pixels)	*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_markEllipAnnuli (cdl_f, *x, *y, *xrad, *yrad, *ang,
	    *nannuli, *sep, *color);
} 


/*  CDF_MARKTEXT -- Draw a text string on the display at the given location.  */

#ifdef ANSI_FUNC

void 
CDF_MARKTEXT (
    int *x,
    int *y,                         	/* marker position      */
    char *str,                          /* text string          */
    float *size,                        /* size scale factor    */
    float *angle,			/* text rotation angle	*/
    int *color,                         /* marker color         */
    int *ier,				/* error code		*/
    int len				/* string length	*/
)
#else

void
CDF_MARKTEXT (x, y, str, size, angle, color, ier, len)
int     *x, *y;                         /* marker position      */
char    *str;                           /* text string          */
float   *size;                          /* size scale factor    */
float	*angle;				/* text rotation angle	*/
int     *color;                         /* marker color         */
int	*ier;				/* error code		*/
int	len;				/* string length	*/
#endif
{
	sstrip (str, len);
	*ier = cdl_markText (cdl_f, *x, *y, sstrip(str, len),
	    *size, *angle, *color);
} 


/*  CDF_SETFONT -- Set the font to be used for text. */

#ifdef ANSI_FUNC

void 
CDF_SETFONT (
    int *font				/* font type		*/
)
#else

void
CDF_SETFONT (font)
int	*font;				/* font type		*/
#endif
{
	cdl_setFont (cdl_f, *font);
} 


/*  CDF_SETLINEWIDTH -- Set the line width to be used. */

#ifdef ANSI_FUNC

void 
CDF_SETLINEWIDTH (
    int *width				/* line width		*/
)
#else

void
CDF_SETLINEWIDTH (width)
int	*width;				/* line width		*/
#endif
{
	cdl_setLineWidth (cdl_f, *width);
} 


/*  CDF_SETLINESTYLE -- Set the line style to be used. */

#ifdef ANSI_FUNC

void
CDF_SETLINESTYLE (
    int *style                          /* line style           */
)
#else

void
CDF_SETLINESTYLE (style)
int     *style;                         /* line style           */
#endif
{
        cdl_setLineStyle (cdl_f, *style);
}


/*  CDF_SETTEXTWIDTH -- Set the line width to be used. */

#ifdef ANSI_FUNC

void 
CDF_SETTEXTWIDTH (
    int *width				/* text width		*/
)
#else

void
CDF_SETTEXTWIDTH (width)
int	*width;				/* text width		*/
#endif
{
	cdl_setTextWidth (cdl_f, *width);
} 


/*  CDF_DELETEMARK -- Delete the overlay mark whose center is closest.  */

#ifdef ANSI_FUNC

void 
CDF_DELETEMARK (
    int *x,
    int *y,				/* marker position	*/
    int *ier				/* error code		*/
)
#else

void
CDF_DELETEMARK (x, y, ier)
int	*x, *y;				/* marker position	*/
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_deleteMark (cdl_f, *x, *y);
} 


/*  CDF_CLEAROVERLAY -- Erase all marks in the current display list.  */

#ifdef ANSI_FUNC

void 
CDF_CLEAROVERLAY (
    int *ier				/* error code		*/
)
#else

void
CDF_CLEAROVERLAY  (ier)
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_clearOverlay (cdl_f);
} 
 

/*  CDF_REDRAWOVERLAY -- Redraw all marks in the current display list.  */

#ifdef ANSI_FUNC

void 
CDF_REDRAWOVERLAY (
    int *ier				/* error code		*/
)
#else

void
CDF_REDRAWOVERLAY  (ier)
int	*ier;				/* error code		*/
#endif
{
	*ier = cdl_redrawOverlay (cdl_f);
} 


/* Debug routine. */
#ifdef ANSI_FUNC

void 
CDF_SETDEBUG (int *state)
#else

void CDF_SETDEBUG (state) int	*state;
#endif
 { cdl_setDebug (*state); }


/* -------------------
 * PRIVATE PROCEDURES
 * -------------------*/

/* Support utility to trim trailing blanks from string and add
 * a null terminator.
 */

static char t[SZ_LINE];

#ifdef ANSI_FUNC

static char * 
sstrip (char *instr, int len)
#else

static char *
sstrip (instr, len) 
char 	*instr;
int 	len;
#endif
{
        int i;

	if (instr == NULL)
	    t[0] = '\0';
	else {
	    strncpy (t, instr, min(len, SZ_LINE));
	    i = min(len, SZ_LINE) - 1;
	    while (t[i] == ' ')
	        i = i - 1;
	    t[i+1] = '\0';
	}
	return(t);
}

/*
 * SPAD --- Pad a string to length 'len' with blanks, as Fortran
 * requires.
 */
 
#ifdef ANSI_FUNC
static void
spad (char *outstr, int len)
#else

static void
spad (outstr, len)
char *outstr;
int len;
#endif
{
	int i;
	
	for (i = strlen(outstr); i < len; i++)
	    outstr[i] = ' ';
}
