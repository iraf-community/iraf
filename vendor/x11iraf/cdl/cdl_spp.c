#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#define  CDL_LIBRARY_SOURCE
#include "cdl.h"
#include "cdl_spp.h"



/*  
 *  CDL_SPP.C -- SPP binding for the CDL package.  Even though we mostly
 *  call the F77 routines here we need a unique procedure for each function
 *  so we can map the SPP symbol names correctly.
 */

char    cstr[512], cstr2[512];


/*  CDS_OPEN -- Open and initialize the CDL package.  */

#ifdef ANSI_FUNC

void 
CDS_OPEN (
    char *imtdev,                       /* connection device	*/
    int *ier				/* error code		*/
)
#else

void
CDS_OPEN (imtdev, ier)
char	*imtdev;                        /* connection device	*/
int	*ier;				/* error code		*/
#endif
{
        strpak (imtdev, cstr, SZ_FNAME);
        CDF_OPEN (cstr, ier, strlen(cstr));
}


/*  CDS_DISPLAYPIX -- Display a raw pixel array to the server.  */

#ifdef ANSI_FUNC

void 
CDS_DISPLAYPIX (
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
CDS_DISPLAYPIX (pix, nx, ny, bitpix, frame, fbconfig, zscale, ier)
uchar	*pix;				/* pixels to display	*/
int	*nx, *ny;			/* image dimensions	*/
int	*bitpix;			/* pixel size		*/
int	*frame;				/* display frame	*/
int	*fbconfig;			/* FB config number	*/
int	*zscale;			/* do zscale of image?	*/
int	*ier;				/* error code		*/
#endif
{
	CDF_DISPLAYPIX (pix, nx, ny, bitpix, frame, fbconfig, zscale, ier);
} 
 

/*  CDS_READCURSOR --  Read the current cursor position. */

#ifdef ANSI_FUNC

void 
CDS_READCURSOR (
    int *sample,                 	/* wait for keystroke?  */
    float *x,
    float *y,                 		/* position (output)	*/
    int *wcs,				/* WCS			*/
    char *key,                   	/* keystroke (output)	*/
    int *ier				/* error code		*/
)
#else

void
CDS_READCURSOR (sample, x, y, wcs, key, ier)
int     *sample;                 	/* wait for keystroke?  */
float   *x, *y;                 	/* position (output)	*/
int 	*wcs;				/* WCS			*/
char    *key;                   	/* keystroke (output)	*/
int	*ier;				/* error code		*/
#endif
{
	char	ch[2];
	CDF_READCURSOR (sample, x, y, wcs, ch, ier);
	strupk (ch, key, 2);

	/* If the char read is a control char, return EOS */
	if (iscntrl(key[0]))
	    key[0] = key[1] = '\0';
} 


/*  CDS_DISPLAYIRAF -- Display an IRAF OIF format image.  */

#ifdef ANSI_FUNC

void 
CDS_DISPLAYIRAF (
    char *fname,			/* image name		*/
    int *band,				/* image band if 3-d	*/
    int *frame,				/* display frame	*/
    int *fbconfig,			/* frame buffer config	*/
    int *zscale,			/* do zscale of image?	*/
    int *ier				/* error code		*/
)
#else

void
CDS_DISPLAYIRAF (fname, band, frame, fbconfig, zscale, ier)
char	*fname;				/* image name		*/
int	*band;				/* image band if 3-d	*/
int	*frame;				/* display frame	*/
int	*fbconfig;			/* frame buffer config	*/
int	*zscale;			/* do zscale of image?	*/
int	*ier;				/* error code		*/
#endif
{
        strpak (fname, cstr, SZ_FNAME);
	CDF_DISPLAYIRAF (cstr, band, frame, fbconfig, zscale, ier, SZ_FNAME);
}


/* CDS_ISIRAF -- Test a file to see if it is a IRAF file.  */

#ifdef ANSI_FUNC

void 
CDS_ISIRAF (
    char *fname,			/* input filename 	*/
    int *isiraf
)
#else

void 
CDS_ISIRAF (fname, isiraf)
char	*fname;				/* input filename 	*/
int	*isiraf;
#endif
{
        strpak (fname, cstr, SZ_FNAME);
	CDF_ISIRAF (cstr, isiraf, SZ_FNAME);
}


/*  CDS_READIRAF -- Read the pixels from an IRAF OIF format image.  */

#ifdef ANSI_FUNC

void 
CDS_READIRAF (
    char *fname,			/* image name		*/
    int *band,				/* image band if 3-d	*/
    uchar *pix,				/* pixel array (output) */
    int *nx,
    int *ny,				/* dimensions (output)	*/
    int *bitpix,			/* pixel size (output)	*/
    char *title,			/* image title (output) */
    int *ier				/* error code		*/
)
#else

void
CDS_READIRAF (fname, band, pix, nx, ny, bitpix, title, ier)
char	*fname;				/* image name		*/
int	*band;				/* image band if 3-d	*/
uchar	*pix;				/* pixel array (output) */
int	*nx, *ny;			/* dimensions (output)	*/
int	*bitpix;			/* pixel size (output)	*/
char	*title;				/* image title (output) */
int	*ier;				/* error code		*/
#endif
{
        strpak (fname, cstr, SZ_FNAME);
	CDF_READIRAF (cstr, band, pix, nx, ny, bitpix, cstr2, ier, SZ_FNAME);
	strupk (cstr2, title, SZ_FNAME);
}


/*  CDS_DISPLAYFITS -- Display a simple FITS format image.  */

#ifdef ANSI_FUNC

void 
CDS_DISPLAYFITS (
    char *fname,
    int *frame,
    int *fbconfig,
    int *zscale,
    int *ier				/* error code		*/
)
#else

void
CDS_DISPLAYFITS (fname, frame, fbconfig, zscale, ier)
char	*fname;
int	*frame;
int	*fbconfig;
int	*zscale;
int	*ier;				/* error code		*/
#endif
{
        strpak (fname, cstr, SZ_FNAME);
	CDF_DISPLAYFITS (cstr, frame, fbconfig, zscale, ier, SZ_FNAME);
}


/*  CDS_ISFITS -- Test a file to see if it is a simple FITS file.  */

#ifdef ANSI_FUNC

void 
CDS_ISFITS (
    char *fname,			/* input filename 	*/
    int *isfits				/* return value		*/
)
#else

void 
CDS_ISFITS (fname, isfits)
char	*fname;				/* input filename 	*/
int	*isfits;			/* return value		*/
#endif
{
        strpak (fname, cstr, SZ_FNAME);
	CDF_ISFITS (cstr, isfits, SZ_FNAME);
}


/*  CDS_READFITS -- Read the pixels from a simple FITS format image.  */

#ifdef ANSI_FUNC

void 
CDS_READFITS (
    char *fname,			/* image name		*/
    uchar *pix,				/* pixel array (output) */
    int *nx,
    int *ny,				/* dimensions (output)	*/
    int *bitpix,			/* pixel size (output)	*/
    char *title,			/* image title (output) */
    int *ier				/* error code		*/
)
#else

void
CDS_READFITS (fname, pix, nx, ny, bitpix, title, ier)
char	*fname;				/* image name		*/
uchar	*pix;				/* pixel array (output) */
int	*nx, *ny;			/* dimensions (output)	*/
int	*bitpix;			/* pixel size (output)	*/
char	*title;				/* image title (output) */
int	*ier;				/* error code		*/
#endif
{
        strpak (fname, cstr, SZ_FNAME);
	CDF_READFITS (fname, pix, nx, ny, bitpix, cstr2, ier, SZ_FNAME);
	strupk (cstr2, title, SZ_FNAME);
}


/*  CDS_SETCURSOR --  Set the current logical cursor position.  */

#ifdef ANSI_FUNC

void 
CDS_SETCURSOR (
    int *x,
    int *y,                 		/* position 		*/
    int *wcs,                   	/* cursor wcs		*/
    int *ier				/* error code		*/
)
#else

void
CDS_SETCURSOR (x, y, wcs, ier)
int   	*x, *y;                 	/* position 		*/
int     *wcs;                   	/* cursor wcs		*/
int	*ier;				/* error code		*/
#endif
{
	CDF_SETCURSOR (x, y, wcs, ier);
} 


/*  CDS_SETWCS --  Set the WCS of the screen.  */

#ifdef ANSI_FUNC

void 
CDS_SETWCS (
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
    int *ier				/* error code		*/
)
#else

void
CDS_SETWCS (name, title, a, b, c, d, tx, ty, z1, z2, zt, ier)
char    *name;                  	/* name string          */
char    *title;                 	/* title string         */
float   *a, *b, *c, *d;             	/* WCS values           */
float   *tx, *ty;                 	/* translation          */
float   *z1, *z2;                 	/* zscale values        */
int     *zt;                     	/* transformation type  */
int	*ier;				/* error code		*/
#endif
{
        strpak (name, cstr, SZ_FNAME);
        strpak (title, cstr2, SZ_FNAME);
	CDF_SETWCS (cstr, cstr2, a, b, c, d, tx,ty, z1,z2, zt, ier, 
	    SZ_FNAME, SZ_FNAME);
} 


/*  CDS_GETWCS -- Get the current display frame WCS information.  */

#ifdef ANSI_FUNC

void 
CDS_GETWCS (
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
    int *ier				/* error code		*/
)
#else

void
CDS_GETWCS (name, title, a, b, c, d, tx, ty, z1, z2, zt, ier)
char    *name;                  	/* name string          */
char    *title;                 	/* title string         */
float   *a, *b, *c, *d;         	/* WCS values           */
float   *tx, *ty;               	/* translation          */
float   *z1, *z2;               	/* zscale values        */
int     *zt;                    	/* transformation type  */
int	*ier;				/* error code		*/
#endif
{
	CDF_GETWCS (cstr, cstr2, a, b, c, d, tx,ty, z1,z2, zt, ier, 
	    SZ_FNAME, SZ_FNAME);
        strupk (cstr, name, SZ_FNAME);
        strupk (cstr2, title, SZ_FNAME);
} 


/*  CDS_SETMAPPING -- Set the mapping information to be sent with the next
 *  cdl_setWcs() call.
 */
#ifdef ANSI_FUNC

void 
CDS_SETMAPPING  (
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
CDS_SETMAPPING  (region, sx,sy,snx,sny, dx,dy,dnx,dny, ref, ier, reglen, reflen)
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
        strpak (region, cstr, SZ_FNAME);
        strpak (ref, cstr2, SZ_FNAME);
        CDF_SETMAPPING (cstr, sx,sy,snx,sny, dx,dy,dnx,dny, cstr2, ier,
            SZ_FNAME, SZ_FNAME);
}


/*  CDS_GETMAPPING --  Get the mapping information returned with the last
 *  cdl_getWcs() call.
 */
#ifdef ANSI_FUNC
void 
CDS_GETMAPPING  (
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
CDS_GETMAPPING  (region, sx,sy,snx,sny, dx,dy,dnx,dny, ref, ier, reglen, reflen)
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
        CDF_GETMAPPING (cstr, sx,sy,snx,sny, dx,dy,dnx,dny, cstr2, ier,
            SZ_FNAME, SZ_FNAME);
        strupk (cstr, region, SZ_FNAME);
        strupk (cstr2, ref, SZ_FNAME);
}


/*  CDS_QUERYMAP -- Query a mapping given the wcs number.
 */
#ifdef ANSI_FUNC

void 
CDS_QUERYMAP  (
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
CDS_QUERYMAP (wcs, region, sx,sy,snx,sny, dx,dy,dnx,dny, objref, ier, reglen, reflen)
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
	CDF_QUERYMAP (wcs, cstr, sx,sy,snx,sny, dx,dy,dnx,dny, cstr2, ier,
            SZ_FNAME, SZ_FNAME);
        strupk (cstr, region, SZ_FNAME);
        strupk (cstr2, objref, SZ_FNAME);
}


/*  CDS_CLEARFRAME -- Erase the current display frame.  */

#ifdef ANSI_FUNC

void 
CDS_CLEARFRAME (
    int *ier				/* error code		*/
)
#else

void
CDS_CLEARFRAME (ier)
int	*ier;				/* error code		*/
#endif
{
	CDF_CLEARFRAME (ier);
} 


/*  CDS_SELECTFB -- Select an appropriate frame buffer for the image.  */

#ifdef ANSI_FUNC

void 
CDS_SELECTFB (
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
CDS_SELECTFB (nx, ny, fb, w, h, nf, reset)
int	*nx, *ny;			/* image size		*/
int	*fb;				/* frame buffer		*/
int	*w, *h;				/* frame size		*/
int	*nf;				/* number of frames	*/
int	*reset;				/* reset after select	*/
#endif
{
	CDF_SELECTFB (nx, ny, fb, w, h, nf, reset);
} 


/*  CDS_CLOSE -- Close the CDL package descriptor.  */

#ifdef ANSI_FUNC

void 
CDS_CLOSE (void)
#else

void
CDS_CLOSE ()
#endif
{
	CDF_CLOSE ();
} 


/*  CDS_READIMAGE -- Read the currently displayed image.  */

#ifdef ANSI_FUNC

void 
CDS_READIMAGE (
    uchar *pix,                   	/* image pixels (output)*/
    int *nx,
    int *ny,               		/* dimensions (output)  */
    int *ier				/* error code		*/
)
#else

void
CDS_READIMAGE (pix, nx, ny, ier)
uchar   *pix;                   	/* image pixels (output)*/
int     *nx, *ny;               	/* dimensions (output)  */
int	*ier;				/* error code		*/
#endif
{
	CDF_READIMAGE (pix, nx, ny, ier);
} 


/*  CDS_READFRAMEBUFFER -- Read the contents of the entire frame buffer.  */

#ifdef ANSI_FUNC

void 
CDS_READFRAMEBUFFER (
    uchar *pix,                   	/* image pixels (output)*/
    int *nx,
    int *ny,               		/* dimensions (output)  */
    int *ier				/* error code		*/
)
#else

void
CDS_READFRAMEBUFFER (pix, nx, ny, ier)
uchar   *pix;                   	/* image pixels (output)*/
int     *nx, *ny;               	/* dimensions (output)  */
int	*ier;				/* error code		*/
#endif
{
	CDF_READFRAMEBUFFER (pix, nx, ny, ier);
} 


/*  CDS_COMPZSCALE -- Compute the optimal z1/z2 values for an array.  */

#ifdef ANSI_FUNC

void 
CDS_COMPZSCALE (
    uchar *pix,                   	/* data to be sampled   */
    int *nx,
    int *ny,                 		/* image dimensions     */
    int *bitpix,                 	/* bits per pixel       */
    float *z1,
    float *z2               		/* min/max zscale values*/
)
#else

void
CDS_COMPZSCALE (pix, nx, ny, bitpix, z1, z2)
uchar   *pix;                   	/* data to be sampled   */
int     *nx, *ny;                 	/* image dimensions     */
int     *bitpix;                 	/* bits per pixel       */
float   *z1, *z2;               	/* min/max zscale values*/
#endif
{
	CDF_COMPZSCALE (pix, nx, ny, bitpix, z1, z2);
} 


/*  CDS_ZSCALEIMAGE -- Compute the optimal z1/z2 values for an array.  */

#ifdef ANSI_FUNC

void 
CDS_ZSCALEIMAGE (
    uchar *pix,                   	/* data to be sampled   */
    int *nx,
    int *ny,                 		/* image dimensions     */
    int *bitpix,                 	/* bits per pixel       */
    float *z1,
    float *z2               		/* min/max zscale values*/
)
#else

void
CDS_ZSCALEIMAGE (pix, nx, ny, bitpix, z1, z2)
uchar   *pix;                   	/* data to be sampled   */
int     *nx, *ny;                 	/* image dimensions     */
int     *bitpix;                 	/* bits per pixel       */
float   *z1, *z2;               	/* min/max zscale values*/
#endif
{
	CDF_ZSCALEIMAGE (pix, nx, ny, bitpix, z1, z2);
} 


/*  CDS_PRINTPIX --  Print the given pixels as EPS to the named command. */

#ifdef ANSI_FUNC

void 
CDS_PRINTPIX (
    char *cmd,				/* command string	*/
    uchar *pix,				/* pixel array		*/
    int *nx,
    int *ny,				/* image dimensions	*/
    int *annotate,			/* annotate output?	*/
    int *ier				/* error code		*/
)
#else

void
CDS_PRINTPIX (cmd, pix, nx, ny, annotate, ier)
char	*cmd;				/* command string	*/
uchar	*pix;				/* pixel array		*/
int	*nx, *ny;			/* image dimensions	*/
int	*annotate;			/* annotate output?	*/
int	*ier;				/* error code		*/
#endif
{
        strpak (cmd, cstr, SZ_FNAME);
	CDF_PRINTPIX (cmd, pix, nx, ny, annotate, ier, SZ_FNAME);
} 


/*  CDS_PRINTPIXTOFILE -- Print the given pixels as EPS to the named file.  */

#ifdef ANSI_FUNC

void 
CDS_PRINTPIXTOFILE (
    char *fname,			/* filename		*/
    uchar *pix,				/* pixel array		*/
    int *nx,
    int *ny,				/* image dimensions	*/
    int *annotate,			/* annotate output?	*/
    int *ier				/* error code		*/
)
#else

void
CDS_PRINTPIXTOFILE (fname, pix, nx, ny, annotate, ier)
char	*fname;				/* filename		*/
uchar	*pix;				/* pixel array		*/
int	*nx, *ny;			/* image dimensions	*/
int	*annotate;			/* annotate output?	*/
int	*ier;				/* error code		*/
#endif
{
	strpak (fname, cstr, SZ_FNAME);
	CDF_PRINTPIXTOFILE (cstr, pix, nx, ny, annotate, ier, SZ_FNAME);
} 


/*  CDS_READSUBRAS -- Read a rectangular region of the frame buffer.  */

#ifdef ANSI_FUNC

void 
CDS_READSUBRAS (
    int *lx,
    int *ly,              		/* region corner        */
    int *nx,
    int *ny,                 		/* dimensions           */
    uchar *pix,                   	/* image pixels (output)*/
    int *ier				/* error code		*/
)
#else

void
CDS_READSUBRAS (lx, ly, nx, ny, pix, ier)
int     *lx, *ly;              		/* region corner        */
int     *nx, *ny;                 	/* dimensions           */
uchar   *pix;                   	/* image pixels (output)*/
int	*ier;				/* error code		*/
#endif
{
	CDF_READSUBRAS (lx, ly, nx, ny, pix, ier);
} 


/*  CDS_WRITESUBRAS -- Write a rectangular region of the frame buffer.  */

#ifdef ANSI_FUNC

void 
CDS_WRITESUBRAS (
    int *lx,
    int *ly,              		/* region corner        */
    int *nx,
    int *ny,                 		/* dimensions           */
    uchar *pix,                   	/* subraster pixels     */
    int *ier				/* error code		*/
)
#else

void
CDS_WRITESUBRAS (lx, ly, nx, ny, pix, ier)
int     *lx, *ly;              		/* region corner        */
int     *nx, *ny;                 	/* dimensions           */
uchar   *pix;                   	/* subraster pixels     */
int	*ier;				/* error code		*/
#endif
{
	CDF_WRITESUBRAS (lx, ly, nx, ny, pix, ier);
} 


/*  CDS_SETFBCONFIG -- Set the frame buffer configuration number.  */

#ifdef ANSI_FUNC

void 
CDS_SETFBCONFIG (
    int *configno			/* fb config number	*/
)
#else

void
CDS_SETFBCONFIG (configno)
int	*configno;			/* fb config number	*/
#endif
{
	CDF_SETFBCONFIG (configno);
} 


/*  CDS_GETFBCONFIG -- Get the frame buffer configuration number.  */

#ifdef ANSI_FUNC

void 
CDS_GETFBCONFIG (
    int *configno,                      /* fb config number     */
    int *w,
    int *h,                         	/* fb frame size        */
    int *nframes                        /* number of frames     */
)
#else

void
CDS_GETFBCONFIG (configno, w, h, nframes)
int     *configno;                      /* fb config number     */
int     *w, *h;                         /* fb frame size        */
int     *nframes;                       /* number of frames     */
#endif
{
	CDF_GETFBCONFIG (configno, w, h, nframes);
} 


/*  CDS_LOOKUPFBSIZE -- Lookup the frame buffer dimensions.*/

#ifdef ANSI_FUNC

void 
CDS_LOOKUPFBSIZE (
    int *configno,                      /* fb config number     */
    int *w,
    int *h,                         	/* fb frame size        */
    int *nf                             /* number of frames     */
)
#else

void
CDS_LOOKUPFBSIZE (configno, w, h, nf)
int     *configno;                      /* fb config number     */
int     *w, *h;                         /* fb frame size        */
int     *nf;                            /* number of frames     */
#endif
{
	CDF_LOOKUPFBSIZE (configno, w, h, nf);
}


/*  CDS_SETFRAME -- Set the current display frame.  */

#ifdef ANSI_FUNC

void 
CDS_SETFRAME (
    int *frame				/* frame number		*/
)
#else

void
CDS_SETFRAME (frame)
int	*frame;				/* frame number		*/
#endif
{
	CDF_SETFRAME (frame);
} 


/*  CDS_SETZTRANS -- Set the current zscale transform parameters.  */

#ifdef ANSI_FUNC

void 
CDS_SETZTRANS (
    int *ztrans				/* z-transform type	*/
)
#else

void
CDS_SETZTRANS (ztrans)
int	*ztrans;			/* z-transform type	*/
#endif
{
	CDF_SETZTRANS (ztrans);
} 


/*  CDS_SETZSCLAE -- Set the current zscale transform parameters.  */

#ifdef ANSI_FUNC

void 
CDS_SETZSCALE (
    float *z1,
    float *z2				/* zscale values	*/
)
#else

void
CDS_SETZSCALE (z1, z2)
float	*z1, *z2;			/* zscale values	*/
#endif
{
	CDF_SETZSCALE (z1, z2);
} 


/*  CDS_SETSAMPLE -- Set the number of zscale sample points to use.  */

#ifdef ANSI_FUNC

void 
CDS_SETSAMPLE (
    int *nsample			/* no. of sample pts	*/
)
#else

void
CDS_SETSAMPLE (nsample)
int	*nsample;			/* no. of sample pts	*/
#endif
{
	CDF_SETSAMPLE (nsample);
} 


/*  CDS_SETSAMPLELINES -- Set the number of zscale sample lines to use.  */

#ifdef ANSI_FUNC

void 
CDS_SETSAMPLELINES (
    int *nlines				/* no. of sample lines	*/
)
#else

void
CDS_SETSAMPLELINES (nlines)
int	*nlines;			/* no. of sample lines	*/
#endif
{
	CDF_SETSAMPLELINES (nlines);
} 


/*  CDS_SETCONTRAST -- Set the zscale contrast value.  */

#ifdef ANSI_FUNC

void 
CDS_SETCONTRAST (
    float *contrast			/* contrast value	*/
)
#else

void
CDS_SETCONTRAST (contrast)
float	*contrast;			/* contrast value	*/
#endif
{
	CDF_SETCONTRAST (contrast);
} 


/*  CDS_SETNAME -- Set the image name for the WCS string.  */

#ifdef ANSI_FUNC

void 
CDS_SETNAME (
    char *imname			/* image name		*/
)
#else

void
CDS_SETNAME (imname)
char	*imname;			/* image name		*/
#endif
{
	strpak (imname, cstr, SZ_FNAME);
	CDF_SETNAME (imname, SZ_FNAME);
} 


/*  CDS_SETTITLE -- Set the image title for the WCS string.  */

#ifdef ANSI_FUNC

void 
CDS_SETTITLE (
    char *imtitle			/* image title		*/
)
#else

void
CDS_SETTITLE (imtitle)
char	*imtitle;			/* image title		*/
#endif
{
	strpak (imtitle, cstr, SZ_FNAME);
	CDF_SETTITLE (imtitle, SZ_FNAME);
} 


/*  CDS_GETFRAME -- Get the current display frame.  */

#ifdef ANSI_FUNC

void 
CDS_GETFRAME (
    int *frame				/* frame number		*/
)
#else

void
CDS_GETFRAME (frame)
int	*frame;				/* frame number		*/
#endif
{
	CDF_GETFRAME (frame);
} 


/*  CDS_GETZTRANS -- Get the current zscale transform parameters.  */

#ifdef ANSI_FUNC

void 
CDS_GETZTRANS (
    int *ztrans                        	/* z-transform type     */
)
#else

void
CDS_GETZTRANS (ztrans)
int     *ztrans;                        /* z-transform type     */
#endif
{
	CDF_GETZTRANS (ztrans);
} 


/*  CDS_GETZSCALE -- Get the current zscale transform parameters.  */

#ifdef ANSI_FUNC

void 
CDS_GETZSCALE (
    float *z1,
    float *z2                       	/* zscale values        */
)
#else

void
CDS_GETZSCALE (z1, z2)
float   *z1, *z2;                       /* zscale values        */
#endif
{
	CDF_GETZSCALE (z1, z2);
} 


/*  CDS_GETSAMPLE -- Get the number of zscale sample points to use.  */

#ifdef ANSI_FUNC

void 
CDS_GETSAMPLE (
    int *nsample                       	/* no. of sample pts    */
)
#else

void
CDS_GETSAMPLE (nsample)
int     *nsample;                       /* no. of sample pts    */
#endif
{
	CDF_GETSAMPLE (nsample);
} 


/*  CDS_GETSAMPLELINES -- Get the number of zscale sample lines to use.  */

#ifdef ANSI_FUNC

void 
CDS_GETSAMPLELINES (
    int *nlines                        	/* no. of sample lines  */
)
#else

void
CDS_GETSAMPLELINES (nlines)
int     *nlines;                        /* no. of sample lines  */
#endif
{
	CDF_GETSAMPLELINES (nlines);
} 


/*  CDS_GETCONTRAST -- Get the zscale contrast value.  */

#ifdef ANSI_FUNC

void 
CDS_GETCONTRAST (
    float *contrast                     /* contrast value       */
)
#else

void
CDS_GETCONTRAST (contrast)
float   *contrast;                      /* contrast value       */
#endif
{
	CDF_GETCONTRAST (contrast);
} 


/*  CDS_GETNAME -- Get the image name for the WCS string.  */

#ifdef ANSI_FUNC

void 
CDS_GETNAME (
    char *imname                        /* image name           */
)
#else

void
CDS_GETNAME (imname)
char    *imname;                        /* image name           */
#endif
{
	CDF_GETNAME (cstr, SZ_FNAME);
	strupk (cstr, imname, SZ_FNAME);
} 


/*  CDS_GETTITLE -- Get the image title for the WCS string.  */

#ifdef ANSI_FUNC

void 
CDS_GETTITLE (
    char *imtitle                       /* image title          */
)
#else

void
CDS_GETTITLE (imtitle)
char    *imtitle;                       /* image title          */
#endif
{
	CDF_GETTITLE (cstr, SZ_FNAME);
	strupk (cstr, imtitle, SZ_FNAME);
} 


/*  CDS_MAPFRAME -- Map the current frame buffer as an image for overlay.  */

#ifdef ANSI_FUNC

void 
CDS_MAPFRAME (
    int *frame,				/* fb frame to map	*/
    int *ier				/* error code		*/
)
#else

void
CDS_MAPFRAME (frame, ier)
int	*frame;				/* fb frame to map	*/
int	*ier;				/* error code		*/
#endif
{
	CDF_MAPFRAME (frame, ier);
} 

/*  CDF_MARKCOORDSFILE -- Mark a list of coords from a file. */

#ifdef ANSI_FUNC

void
CDS_MARKCOORDSFILE (
    char *fname,                        /* file name            */
    int  *type,                         /* marker type          */
    int  *size,                         /* marker size          */
    int  *color,                        /* marker color         */
    int  *label,                        /* label?               */
    int  *ier                           /* error code           */
)
#else

void
CDS_MARKCOORDSFILE (fname, type, size, color, label, ier)
char    *fname;                         /* file name            */
int     *type;                          /* marker type          */
int     *size;                          /* marker size          */
int     *color;                         /* marker color         */
int     *label;                         /* label?               */
int     *ier;                           /* error code           */
#endif
{
	strpak (fname, cstr, SZ_FNAME);
        CDF_MARKCOORDSFILE (cstr, type, size, color, label, ier, SZ_FNAME);
}


/*  CDS_MARKPOINT --  Draw a mark on the display at the given point. */

#ifdef ANSI_FUNC

void 
CDS_MARKPOINT (
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
CDS_MARKPOINT (x, y, number, size, type, color, ier)
int	*x, *y;				/* marker position	*/
int	*number;			/* if > 0, label value	*/
int	*size;				/* marker size (pixels) */
int	*type;				/* type to draw		*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	CDF_MARKPOINT (x, y, number, size, type, color, ier);
} 


/*  CDS_MARKPOINTLABEL --  Draw a mark on the display at the given point. */

#ifdef ANSI_FUNC

void 
CDS_MARKPOINTLABEL (
    int *x,
    int *y,				/* marker position	*/
    char *label,			/* point label string	*/
    int *size,				/* marker size (pixels) */
    int *type,				/* type to draw		*/
    int *color,				/* marker color		*/
    int *ier				/* error code		*/
)
#else

void
CDS_MARKPOINTLABEL (x, y, label, size, type, color, ier)
int	*x, *y;				/* marker position	*/
char	*label;				/* point label string	*/
int	*size;				/* marker size (pixels) */
int	*type;				/* type to draw		*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	strpak (label, cstr, SZ_FNAME);
	CDF_MARKPOINTLABEL (x, y, cstr, size, type, color, ier, SZ_FNAME);
} 


/*  CDS_MARKLINE --  Draw a line of given color between two points.  */

#ifdef ANSI_FUNC

void 
CDS_MARKLINE (
    int *xs,
    int *ys,				/* line start points	*/
    int *xe,
    int *ye,				/* line end points	*/
    int *color,				/* marker color		*/
    int *ier				/* error code		*/
)
#else

void
CDS_MARKLINE (xs, ys, xe, ye, color, ier)
int	*xs, *ys;			/* line start points	*/
int	*xe, *ye;			/* line end points	*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	CDF_MARKLINE (xs, ys, xe, ye, color, ier);
} 


/*  CDS_MARKBOX --  Draw a rectangular box given two corner endpoints.  */

#ifdef ANSI_FUNC

void 
CDS_MARKBOX (
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
CDS_MARKBOX (lx, ly, ux, uy, fill, color, ier)
int	*lx, *ly;			/* LL corner points	*/
int	*ux, *uy;			/* UR corner points	*/
int	*fill;				/* fill rectangle?	*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	CDF_MARKBOX (lx, ly, ux, uy, fill, color, ier);
} 


/*  CDS_MARKPOLYGON -- Draw a polygon on the display, optionally filling.  */

#ifdef ANSI_FUNC

void 
CDS_MARKPOLYGON (
    int *xarray,
    int *yarray,			/* vertex points	*/
    int *npts,				/* number of corners	*/
    int *fill,				/* fill polygon?	*/
    int *color,				/* marker color		*/
    int *ier				/* error code		*/
)
#else

void
CDS_MARKPOLYGON (xarray, yarray, npts, fill, color, ier)
int	*xarray, *yarray;		/* vertex points	*/
int	*npts;				/* number of corners	*/
int	*fill;				/* fill polygon?	*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	CDF_MARKPOLYGON (xarray, yarray, npts, fill, color, ier);
} 


/*  CDS_MARKPOLYLINE -- Draw a polyline on the display, optionally filling.  */

#ifdef ANSI_FUNC

void 
CDS_MARKPOLYLINE (
    int *xarray,
    int *yarray,			/* vertex points	*/
    int *npts,				/* number of points	*/
    int *color,				/* marker color		*/
    int *ier				/* error code		*/
)
#else

void
CDS_MARKPOLYLINE (xarray, yarray, npts, color, ier)
int	*xarray, *yarray;		/* vertex points	*/
int	*npts;				/* number of points	*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	CDF_MARKPOLYLINE (xarray, yarray, npts, color, ier);
} 


/*  CDS_MARKCIRCLE -- Draw of circle on the display, optionally filling.  */

#ifdef ANSI_FUNC

void 
CDS_MARKCIRCLE (
    int *x,
    int *y,				/* center position	*/
    int *radius,			/* radius of circle	*/
    int *fill,				/* fill circle?		*/
    int *color,				/* marker color		*/
    int *ier				/* error code		*/
)
#else

void
CDS_MARKCIRCLE (x, y, radius, fill, color, ier)
int	*x, *y;				/* center position	*/
int	*radius;			/* radius of circle	*/
int	*fill;				/* fill circle?		*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	CDF_MARKCIRCLE (x, y, radius, fill, color, ier);
} 


/*  CDS_MARKCIRCANNULI -- Draw circular annuli on the display.  */

#ifdef ANSI_FUNC

void 
CDS_MARKCIRCANNULI (
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
CDS_MARKCIRCANNULI (x, y, radius, nannuli, sep, color, ier)
int	*x, *y;				/* center position	*/
int	*radius;			/* radius of 1st annulus*/
int	*nannuli;			/* no. of annuli	*/
int	*sep;				/* annuli sep (pixels)	*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	CDF_MARKCIRCANNULI (x, y, radius, nannuli, sep, color, ier);
} 


/*  CDS_MARKELLIPSE -- Draw an ellipse.  */

#ifdef ANSI_FUNC

void 
CDS_MARKELLIPSE (
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
CDS_MARKELLIPSE (x, y, xrad, yrad, ang, fill, color, ier)
int	*x, *y;				/* center position	*/
int	*xrad, *yrad;			/* x and y radii	*/
float	*ang;				/* position angle (deg) */
int	*fill;				/* fill ellipse?	*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	CDF_MARKELLIPSE (x, y, xrad, yrad, ang, fill, color, ier);
} 


/*  CDS_MARKELLIPANNULI -- Draw elliptical annuli on the display.  */

#ifdef ANSI_FUNC

void 
CDS_MARKELLIPANNULI (
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
CDS_MARKELLIPANNULI (x, y, xrad, yrad, ang, nannuli, sep, color, ier)
int	*x, *y;				/* center position	*/
int	*xrad, *yrad;			/* radius of 1st annulus*/
float	*ang;				/* rotation angle	*/
int	*nannuli;			/* no. of annuli	*/
int	*sep;				/* annuli sep (pixels)	*/
int	*color;				/* marker color		*/
int	*ier;				/* error code		*/
#endif
{
	CDF_MARKELLIPANNULI (x, y, xrad, yrad, ang, nannuli, sep, color, ier);
} 


/*  CDS_MARKTEXT -- Draw a text string on the display at the given location.  */

#ifdef ANSI_FUNC

void 
CDS_MARKTEXT (
    int *x,
    int *y,                         	/* marker position      */
    char *str,                          /* text string          */
    float *size,                        /* size scale factor    */
    float *angle,			/* text rotation angle	*/
    int *color,                         /* marker color         */
    int *ier				/* error code		*/
)
#else

void
CDS_MARKTEXT (x, y, str, size, angle, color, ier)
int     *x, *y;                         /* marker position      */
char    *str;                           /* text string          */
float   *size;                          /* size scale factor    */
float	*angle;				/* text rotation angle	*/
int     *color;                         /* marker color         */
int	*ier;				/* error code		*/
#endif
{
	strpak (str, cstr, SZ_FNAME);
	CDF_MARKTEXT (x, y, cstr, size, angle, color, ier, SZ_FNAME);
} 


/*  CDS_SETFONT -- Set the font to be used. */

#ifdef ANSI_FUNC

void 
CDS_SETFONT (
    int *font				/* font type		*/
)
#else

void
CDS_SETFONT (font)
int	*font;				/* font type		*/
#endif
{
	CDF_SETFONT (font);
} 


/*  CDS_SETLINEWIDTH -- Set the line width to be used. */

#ifdef ANSI_FUNC

void 
CDS_SETLINEWIDTH (
    int *width				/* line width		*/
)
#else

void
CDS_SETLINEWIDTH (width)
int	*width;				/* line width		*/
#endif
{
	CDF_SETLINEWIDTH (width);
} 


/*  CDS_SETLINESTYLE -- Set the line style to be used. */

#ifdef ANSI_FUNC

void
CDS_SETLINESTYLE (
    int *style                          /* line style           */
)
#else

void
CDS_SETLINESTYLE (style)
int     *style;                         /* line style           */
#endif
{
        CDF_SETLINESTYLE (style);
}


/*  CDS_SETTEXTWIDTH -- Set the line width to be used. */

#ifdef ANSI_FUNC

void 
CDS_SETTEXTWIDTH (
    int *width				/* line width		*/
)
#else

void
CDS_SETTEXTWIDTH (width)
int	*width;				/* line width		*/
#endif
{
	CDF_SETTEXTWIDTH (width);
} 


/*  CDS_DELETEMARK -- Delete the overlay mark whose center is closest.  */

#ifdef ANSI_FUNC

void 
CDS_DELETEMARK (
    int *x,
    int *y,				/* marker position	*/
    int *ier				/* error code		*/
)
#else

void
CDS_DELETEMARK (x, y, ier)
int	*x, *y;				/* marker position	*/
int	*ier;				/* error code		*/
#endif
{
	CDF_DELETEMARK (x, y, ier);
} 


/*  CDS_CLEAROVERLAY -- Erase all marks in the current display list.  */

#ifdef ANSI_FUNC

void 
CDS_CLEAROVERLAY (
    int *ier				/* error code		*/
)
#else

void
CDS_CLEAROVERLAY  (ier)
int	*ier;				/* error code		*/
#endif
{
	CDF_CLEAROVERLAY  (ier);
} 
 

/*  CDS_REDRAWOVERLAY -- Redraw all marks in the current display list.  */

#ifdef ANSI_FUNC

void 
CDS_REDRAWOVERLAY (
    int *ier				/* error code		*/
)
#else

void
CDS_REDRAWOVERLAY  (ier)
int	*ier;				/* error code		*/
#endif
{
	CDF_REDRAWOVERLAY  (ier);
} 


/* Debug routine. */
#ifdef ANSI_FUNC

void 
CDS_SETDEBUG (int *state)
#else

void CDS_SETDEBUG (state) int	*state;
#endif
 { cdl_setDebug (*state); }


/* -------------------
 * PRIVATE PROCEDURES
 * -------------------*/


/* STRUPK -- Unpack a C string into an SPP string.
 */

#ifdef ANSI_FUNC

static void 
strupk (
    char *str,                  	/* C string                     */
    char *outstr,              		/* SPP string                   */
    int maxch                  		/* max chars out, incl EOS      */
)
#else

static void
strupk (str, outstr, maxch)
char    *str;                   	/* C string                     */
char   *outstr;                		/* SPP string                   */
int     maxch;                  	/* max chars out, incl EOS      */
#endif
{
        register char   *ip = str;
        register short  *op = (short *)outstr;
        register int      n = maxch-1;

        /* Is is necessary to determine the length of the string in order to
         * be able to unpack the string in place, i.e., from right to left.
         */
        if (maxch)
            if (str != (char *)outstr) {
                n = min (n, strlen(ip));
                op[n] = '\0';

                for (n = n - 1;  n >= 0;  --n)
                    op[n] = ip[n];
            }
}


/* STRPAK -- Pack an SPP string (type XCHAR) into a C string in a user
 * supplied buffer.  Return a pointer to the output buffer.
 */

#ifdef ANSI_FUNC

static void 
strpak (
    char *sppstr,              		/* SPP string                   */
    char *cstr,                 	/* C string                     */
    int maxch                  		/* max chars out, incl EOS      */
)
#else

static void
strpak (sppstr, cstr, maxch)
char    *sppstr;                	/* SPP string                   */
char    *cstr;                  	/* C string                     */
int     maxch;                  	/* max chars out, incl EOS      */
#endif
{
        register short  *ip = (short *)sppstr;
        register char   *op = cstr;
        register int      n = maxch-1;

        if (maxch)
            if ((char *)sppstr != cstr) {
                while (--n >= 0 && (*op++ = *ip++) != '\0')
                    ;
                cstr[maxch-1] = '\0';
            }
}
