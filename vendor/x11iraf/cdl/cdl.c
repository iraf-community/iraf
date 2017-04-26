#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#define  CDL_LIBRARY_SOURCE
#define  CDL_NEED_COLORMAPS
#include "cdl.h"


/*
 * CDL -- Client Display Library.  This package provides a general interface
 * for client applications to do IRAF-like image display and interaction.
 * It is layered upon other interfaces for handling basic display, cursor
 * and frame buffer operations, and low-level server communications.  
 *
 *           cdl = cdl_open  (imtdev)
 *           cdl_displayPix  (cdl, pix, nx, ny, bitpix, frame, fbconfig, zscale)
 *           cdl_readCursor  (cdl, sample, &x, &y, &wcs, &key)
 *            cdl_setCursor  (cdl, x, y, wcs)
 *           cdl_clearFrame  (cdl)
 *                cdl_close  (cdl)
 *
 *          cdl_displayIRAF  (cdl, fname, band, frame, fbconfig, zscale)
 *               cdl_isIRAF  (fname)
 *             cdl_readIRAF  (fname, band, &pix, &nx, &ny, &bitpix, title);
 *
 *          cdl_displayFITS  (cdl, fname, frame, fbconfig, zscale)
 *               cdl_isFITS  (fname)
 *             cdl_readFITS  (fname, &pix, &nx, &ny, &bitpix, title);
 *
 *        cdl_computeZscale  (cdl, pix, nx, ny, bitpix, &z1, &z2)
 *          cdl_zscaleImage  (cdl, &pix, nx, ny, bitpix, z1, z2)
 *
 *             cdl_printPix  (cdl, cmd, pix, nx, ny, annotate)
 *       cdl_printPixToFile  (cdl, fname, pix, nx, ny, annotate)
 *
 *            cdl_readImage  (cdl, &pix, &nx, &ny)
 *      cdl_readFrameBuffer  (cdl, &pix, &nx, &ny)
 *        cdl_readSubRaster  (cdl, lx, ly, nx, ny, &pix)
 *       cdl_writeSubRaster  (cdl, lx, ly, nx, ny, pix)
 *  
 *             cdl_selectFB  (cdl, nx, ny, &fb, &fb_w, &fb_h, &nframes, reset)
 *          cdl_setFBConfig  (cdl, configno)
 *          cdl_getFBConfig  (cdl, &configno, &w, &h, &nframes)
 *         cdl_lookupFBSize  (cdl, configno, &w, &h, &nframes)
 *
 *         cdl_[set|get]WCS  (cdl, name, title, a, b, c, d, tx, ty, z1, z2, zt)
 *     cdl_[set|get]Mapping  (cdl, region, sx,sy,snx,sny, dx,dy,dnx,dny, ref)
 *             cdl_queryMap  (cdl, wcs, region, sx,sy,snx,sny, 
 *					dx,dy,dnx,dny, objref)
 *
 *       cdl_[set|get]Frame  (cdl, frame)
 *      cdl_[set|get]ZTrans  (cdl, ztrans)
 *      cdl_[set|get]ZScale  (cdl, z1, z2)
 *      cdl_[set|get]Sample  (cdl, nsample)
 * cdl_[set|get]SampleLines  (cdl, nlines)
 *    cdl_[set|get]Contrast  (cdl, contrast)
 *        cdl_[set|get]Name  (cdl, imname)
 *       cdl_[set|get]Title  (cdl, imtitle)
 *
 *
 *   GRAPHICS OVERLAY ROUTINES:
 *   --------------------------
 *             cdl_mapFrame  (cdl, frame)
 *            cdl_markPoint  (cdl, x, y, number, size, type, color)
 *             cdl_markLine  (cdl, xs, ys, xe, ye, color)
 *              cdl_markBox  (cdl, lx, ly, ux, uy, fill, color)
 *         cdl_markPolyline  (cdl, xpts, ypts, npts, color)
 *          cdl_markPolygon  (cdl, xpts, ypts, npts, fill, color)
 *           cdl_markCircle  (cdl, x, y, radius, fill, color)
 *       cdl_markCircAnnuli  (cdl, x, y, radius, nannuli, sep, color)
 *          cdl_markEllipse  (cdl, x, y, xrad, yrad, ang, fill, color)
 *      cdl_markEllipAnnuli  (cdl, x, y, xrad, yrad, ang, nannuli, sep, color)
 *             cdl_markText  (cdl, x, y, str, size, angle, color)
 *
 *              cdl_setFont  (cdl, type)
 *
 *           cdl_deleteMark  (cdl, x, y)
 *         cdl_clearOverlay  (cdl)
 *        cdl_redrawOverlay  (cdl)
 *
 *
 *	Client applications begin with a cdl_open() call to initialize the
 * interface.  The "imtdev" argument is used to specify a connection at device
 * open time, or if NULL the procedure will attempt to first connect on a unix
 * socket or fifo pipe if that fails.  The syntax for the imtdev argument is
 * as follows:
 *              	<domain> : <address>
 *
 * where <domain> is one of "inet" (internet tcp/ip socket), "unix" (unix
 * domain socket) or "fifo" (named pipe).   The form of the address depends
 * upon the domain, see the IMD interface code comments or documentation
 * for examples.
 *
 *	The library assumes a logical coordinate system that has the image or
 * raster origin in the lower-left corner. All I/O routines should be passed
 * or will return a pixel pointer set to the LL corner of the raster, all
 * cursor positions will similarly use this coordinate system.  Initially the
 * [0,0] origin is defined as the LL of the frame buffer, this will remain
 * the case until the WCS is redefined either explicitly through a cdl_setWCS()
 * call or by using one of the high level cdl_display*() procedures to display
 * an image smaller that the current frame buffer.  Applications wishing to
 * retain this initial origin or those wanting to explicitly place the image
 * in the frame buffer should use the cdl_writeSubRaster() for display.  This
 * is to allow cursor and subraster positions to be specified in image coord-
 * inates more easily.  Negative positions are allowed and will either refer 
 * to empty pixels if the frame buffer is larger than the image, or pixels 
 * outside the frame buffer boundaries.  Raster I/O requests will be clipped
 * to the frame buffer endpoints, a request completely outside the frame buffer
 * is an error.
 *
 *	The high-level display routines cdl_displayIRAF() and cdl_displayFITS()
 * can be used to display images directly by name.  A WCS will automatically
 * be defined for each image after the optional zscaling hass been computed.  
 * Applications wishing to define their own WCS need to call cdl_setWCS()
 * after the display call to redefine the default WCS.
 */

/* Function prototypes */
#ifdef __STDC__
#include <stddef.h>
#endif

#ifdef ANSI_FUNC

void cdl_zscale(uchar *im, int nx, int ny, int bitpix, float *z1, float *z2, float contrast, int opt_size, int len_stdline);
int  cdl_freeDisplayList(CDLPtr cdl, MarkerPtr head);

extern  char *mktemp(char *template);
extern  int  system(const char *cmd);
extern  void bcopy(char *b1, char *b2, int length);

#endif

/* Display list declarations.  We keep a separate list for each frame that
 * is freed whenever a new image is displayed.  The list is maintained as
 * a doubly-linked list of Marker structs.
 */
extern MarkerPtr  DLHead[MAX_FRAMES];          /* diplay list head     */
extern MarkerPtr  DLTail[MAX_FRAMES];          /* diplay list tail     */


int	cdl_debug = 0;

static void 	cdl_applyZscale(), cdl_flip();


/*  CDL_OPEN -- Open and initialize the CDL package.
 */

#ifdef ANSI_FUNC

CDLPtr 
cdl_open (
    char *imtdev                        /* connection device	*/
)
#else

CDLPtr
cdl_open (imtdev)
char	*imtdev;                        /* connection device	*/
#endif
{
	register int i;
        CDLPtr  cdl;
	IMDPtr  imd_open();

	/* If the debug flag isn't set in the code, see if it's set in the
	 * runtime environment.
	 */
	if (cdl_debug == 0) {
	    if (getenv("CDL_DEBUG") != NULL)
	        cdl_debug = atoi(getenv("CDL_DEBUG"));
	    else 
		cdl_debug = 0;
	    cdl_setDebug (cdl_debug);
	}

	if (cdl_debug)
	    printf ("%s\n[cdl_open] imtdev='%s'\n", CDL_VERSION, 
		(imtdev ? imtdev : ""));

        /* Allocate the cdl structure. */
        cdl = (struct CDL *) calloc (1, sizeof (struct CDL));

	/* Open the connection to the server. */
	cdl->imd = imd_open ((imtdev == NULL) ? getenv("IMTDEV") : imtdev);
	if (cdl->imd == (IMDPtr) NULL) {
	    free ((char *)cdl);
	    return (NULL);
	}

        /* Initialize the CDL structure. */
        cdl->frame      = 1;
        cdl->fbconfig   = 1;
	cdl->fbwidth    = 512;
	cdl->fbheight   = 512;
	cdl->fbnf       = 2;
	cdl->im_nx      = 512;
	cdl->im_ny      = 512;

	cdl->contrast   = DEF_CONTRAST;
	cdl->nsample    = DEF_NSAMPLE;
	cdl->nsamplines = DEF_NSAMPLINES;

	cdl->font       = F_ROMAN;
	cdl->textwidth  = 1;
	cdl->linewidth  = 1;
	cdl->linestyle  = L_SOLID;

        /* Initialize a WCS. */
        cdl->a          = 1.0;
        cdl->b          = 0.0;
        cdl->c          = 0.0;
        cdl->d          = -1.0;
        cdl->tx         = 1.0;                  /* default for 512x512 fb */
        cdl->ty         = 512.0;                /* default for 512x512 fb */
        cdl->z1         = 0.0;
        cdl->z2         = 255.0;
        cdl->ztrans     = CDL_LINEAR;

        /* Initialize the mapping. */
        cdl->sx         = 1.0;
        cdl->sy         = 1.0;
        cdl->snx        = 512;
        cdl->sny        = 512;
        cdl->dx         = 0;
        cdl->dy         = 0;
        cdl->dnx        = 511;
        cdl->dny        = 511;

        cdl->iis_version = cdl->imd->iis_version;
        cdl->iis_valid   = cdl->imd->iis_valid;

        cdl->imname     = (char *) calloc (SZ_NAME, sizeof(char));
        cdl->imtitle    = (char *) calloc (SZ_NAME, sizeof(char));
        cdl->region     = (char *) calloc (SZ_NAME, sizeof(char));
        cdl->ref        = (char *) calloc (SZ_NAME, sizeof(char));

	/* Initialize the display list. */
	for (i=0; i < MAX_FRAMES; i++)
	    DLHead[i] = DLTail[i] = (MarkerPtr) NULL;

        return (cdl);   
}


/*  CDL_DISPLAYPIX -- Display a raw pixel array to the server.  Pixels may
 *  be larger than 8-bits; they will be scaled for display either through
 *  a simple minmax scaling to 8-bits if zscale=0, or an optimal Z-transform
 *  will be computed and used if zscale=1.
 */

#ifdef ANSI_FUNC

int 
cdl_displayPix (
    CDLPtr cdl,                         /* package ptr          */
    uchar *pix,				/* pixels to display	*/
    int nx,
    int ny,				/* image dimensions	*/
    int bitpix,				/* pixel size		*/
    int frame,				/* display frame	*/
    int fbconfig,			/* frame bvuffer config */
    int zscale				/* do zscale of image?	*/
)
#else

int
cdl_displayPix (cdl, pix, nx, ny, bitpix, frame, fbconfig, zscale)
CDLPtr  cdl;                            /* package ptr          */
uchar	*pix;				/* pixels to display	*/
int	nx, ny;				/* image dimensions	*/
int	bitpix;				/* pixel size		*/
int	frame;				/* display frame	*/
int	fbconfig;			/* frame bvuffer config */
int	zscale;				/* do zscale of image?	*/
#endif
{
	float	z1 = 0.0, z2 = 0.0;
        int     fb = fbconfig, w, h, nframes;

	if (cdl_debug)
	    printf ("[cdl_displayPix] %dx%d-%d frame=%d fb=%d zscale=%d\n",
		nx, ny, bitpix, frame, fbconfig, zscale);

	/* Sanity check. */
	if (frame < 1 || frame > MAX_FRAMES) {
	    perror ("cdl_displayPix: invalid frame number");
	    return (ERR);
	}

	if (zscale) {
	    /* Compute the optimal zscale values. */
            cdl_computeZscale  (cdl, pix, nx, ny, bitpix, &z1, &z2);
	    cdl_zscaleImage (cdl, &pix, nx, ny, bitpix, z1, z2);
	    cdl_setZScale (cdl, z1, z2);
	    cdl->imd->z1 = z1;
	    cdl->imd->z2 = z2;
	}

	/* See if we need to free the display list for the frame. */
	if (DLHead[frame-1] != (MarkerPtr) NULL)
	    (void) cdl_freeDisplayList (cdl, DLHead[frame-1]);

        /* Do the frame buffer configuration if needed. */
        if (fbconfig == FB_AUTO) {
            cdl_selectFB (cdl, nx, ny, &fb, &w, &h, &nframes, True);
        } else if (fbconfig != cdl->fbconfig) {
            cdl_setFBConfig (cdl, fbconfig);
	    fb = max (1, fbconfig);
	}

	cdl->frame = frame;
	cdl->im_nx = nx;
	cdl->im_ny = ny;

	return (imd_displayImage (cdl->imd, pix, nx, ny, frame, fb, 1));
}


/*  CDL_READCURSOR --  Read the current cursor position.  If sample is defined
 *  logical cursor position will be sampled and returned immediately, otherwise
 *  the server will block until a key is hit and we return that value as well.
 */

#ifdef ANSI_FUNC

char 
cdl_readCursor (
    CDLPtr cdl,                         /* package ptr          */
    int sample,                 	/* wait for keystroke?  */
    float *x,
    float *y,                 		/* position (output)	*/
    int *wcs,				/* WCS			*/
    char *key                   	/* keystroke (output)	*/
)
#else

char
cdl_readCursor (cdl, sample, x, y, wcs, key)
CDLPtr  cdl;                            /* package ptr          */
int     sample;                 	/* wait for keystroke?  */
float   *x, *y;                 	/* position (output)	*/
int	*wcs;				/* WCS			*/
char    *key;                   	/* keystroke (output)	*/
#endif
{
	int status;

	if (cdl_debug)
	    printf ("[cdl_readCursor]\n");

	status =  imd_readCursor (cdl->imd, sample, x, y, wcs, key);
	return (*key);
}


/*  CDL_SETCURSOR --  Set the current logical cursor position.  If 'wcs' is
 *  non-zero the cursor position is assumed to be defined in terms of the
 *  current image WCS, otherwise it is in frame buffer coordinates.
 */

#ifdef ANSI_FUNC

int 
cdl_setCursor (
    CDLPtr cdl,                         /* package ptr          */
    int x,
    int y,                 		/* position 		*/
    int wcs                   		/* cursor wcs		*/
)
#else

int
cdl_setCursor (cdl, x, y, wcs)
CDLPtr  cdl;                            /* package ptr          */
int   	x, y;                 		/* position 		*/
int     wcs;                   		/* cursor wcs		*/
#endif
{
	if (cdl_debug)
	    printf ("[cdl_setCursor]  x=%d y=%d wcs=%d\n", x, y, wcs);

	return (imd_setCursor (cdl->imd, x, y, wcs));
}


/*  CDL_SETWCS --  Set the WCS of the screen.  The WCS is passed in a string
 *  defined as:
 *              Image_Name_String\n a b c d tx ty z1 z2 zt
 *  where:
 *              X' = a*X + c*Y + tx
 *              Y' = b*X + d*Y + ty
 *
 *  z1 is the minimum pixel value, z2 is the maximum pixel value,  zt
 *  defines the type of transformation to use.
 */

#ifdef ANSI_FUNC

int cdl_setWCS (
    CDLPtr cdl,                         /* package ptr          */
    char *imname,                  	/* name string          */
    char *imtitle,                 	/* title string         */
    float a,
    float b,
    float c,
    float d,             		/* WCS values           */
    float tx,
    float ty,                 		/* translation          */
    float z1,
    float z2,                 		/* zscale values        */
    int zt                     		/* transformation type  */
)
#else

int
cdl_setWCS (cdl, imname, imtitle, a, b, c, d, tx, ty, z1, z2, zt)
CDLPtr  cdl;                            /* package ptr          */
char    *imname;                  	/* name string          */
char    *imtitle;                 	/* title string         */
float	 a, b, c, d;             	/* WCS values           */
float	 tx, ty;                 	/* translation          */
float	 z1, z2;                 	/* zscale values        */
int	 zt;                     	/* transformation type  */
#endif
{
	int  status = 0;


	if (cdl_debug) {
	    printf ("[cdl_setWCS] name='%s' title='%s'\n", (imname?imname:""),
		(imtitle?imtitle:""));
            printf ("\ta=%g b=%g c=%g d=%g tx=%g ty=%g z1=%g z2=%g zt=%d\n",
               a, b, c, d, tx, ty, z1, z2, zt);
        }

	strcpy (cdl->imname, (imname ? imname : ""));
	strcpy (cdl->imtitle, (imtitle ? imtitle : ""));
	cdl->a 	    = a;
	cdl->b 	    = b;
	cdl->c 	    = c;
	cdl->d 	    = d;
	cdl->tx     = tx;
	cdl->ty     = ty;
	cdl->z1     = z1;
	cdl->z2     = z2;
	cdl->ztrans = zt;

	/* Send the WCS. */
	status = imd_setWCS (cdl->imd, imname, imtitle, a, b, c, d, tx, ty,
	    z1, z2, zt);

	/* Invalidate the mapping once it's been sent. */
	cdl->iis_valid = 0;

	return (status);
}


/*  CDL_GETWCS -- Get the current display frame WCS information.
 */

#ifdef ANSI_FUNC

int 
cdl_getWCS (
    CDLPtr cdl,                         /* package ptr          */
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
    int *zt                    		/* transformation type  */
)
#else

int
cdl_getWCS (cdl, name, title, a, b, c, d, tx, ty, z1, z2, zt)
CDLPtr  cdl;                            /* package ptr          */
char    *name;                  	/* name string          */
char    *title;                 	/* title string         */
float   *a, *b, *c, *d;         	/* WCS values           */
float   *tx, *ty;               	/* translation          */
float   *z1, *z2;               	/* zscale values        */
int     *zt;                    	/* transformation type  */
#endif
{
	int wcs = 0;			/* get the frame WCS	*/
	int status = imd_getWCS (cdl->imd, wcs, name, title, a, b, c, d,
	    tx, ty, z1, z2, zt);

	cdl->iis_valid = cdl->imd->iis_valid;

	if (cdl_debug) {
	    printf ("[cdl_getWCS] name='%s' title='%s'\n", (name ? name : ""),
		(title ? title : ""));
            printf ("\ta=%g b=%g c=%g d=%g tx=%g ty=%g z1=%g z2=%g zt=%d\n",
               *a, *b, *c, *d, *tx, *ty, *z1, *z2, *zt);
        }

	return (status);
}


/*  CDL_SETMAPPING -- Set the mapping information to be sent with the next
 *  cdl_setWcs() call.
 */
#ifdef ANSI_FUNC

int 
cdl_setMapping  (
    CDLPtr  cdl,                        /* package ptr          */
    char    *region,                  	/* region name 		*/
    float   sx,				/* source rect		*/
    float   sy,
    int	    snx, 			/* source extent	*/
    int	    sny,
    int	    dx, 			/* dest rect		*/
    int	    dy,
    int	    dnx, 			/* dest extent		*/
    int	    dny,
    char    *ref                   	/* reference name	*/
)
#else

int 
cdl_setMapping  (cdl, region, sx,sy,snx,sny, dx,dy,dnx,dny, ref)
CDLPtr  cdl;                            /* package ptr          */
char    *region;                  	/* region name 		*/
float	sx, sy;				/* source rect		*/
int	snx, sny;			/* source extent	*/
int	dx, dy;				/* dest rect		*/
int	dnx, dny;			/* dest extent		*/
char    *ref;                  		/* reference name	*/
#endif
{
        if (cdl_debug) {
            printf ("[cdl_setMapping] region='%s' ref='%s'\n",
		(region ? region : ""), (ref ? ref : ""));
            printf ("\tsrc = %g,%g,%d,%d   dest = %d,%d,%d,%d\n",
               sx, sy, snx, sny, dx, dy, dnx, dny);
        }

        strcpy (cdl->region, (region ? region : ""));
        strcpy (cdl->ref, (ref ? ref : ""));
        cdl->sx   = sx;
        cdl->sy   = sy;
        cdl->snx  = snx;
        cdl->sny  = sny;
        cdl->dx   = dx;
        cdl->dy   = dy;
        cdl->dnx  = dnx;
        cdl->dny  = dny;

        return ((cdl->iis_valid = imd_setMapping (cdl->imd, 
		region, sx,sy,snx,sny, dx,dy,dnx,dny, ref)));
}


/*  CDL_GETMAPPING --  Get the mapping information returned with the last
 *  cdl_getWcs() call.
 */
#ifdef ANSI_FUNC
int 
cdl_getMapping  (
    CDLPtr  cdl,                        /* package ptr          */
    char    *region,                  	/* region name 		*/
    float   *sx,			/* source rect		*/
    float   *sy,
    int	    *snx, 			/* source extent	*/
    int	    *sny,
    int	    *dx, 			/* dest rect		*/
    int	    *dy,
    int	    *dnx, 			/* dest extent		*/
    int	    *dny,
    char    *ref                   	/* reference name	*/
)

#else
int 
cdl_getMapping  (cdl, region, sx,sy,snx,sny, dx,dy,dnx,dny, ref)
CDLPtr  cdl;                            /* package ptr          */
char    *region;                  	/* region name 		*/
float	*sx, *sy;			/* source rect		*/
int	*snx, *sny;			/* source extent	*/
int	*dx, *dy;			/* dest rect		*/
int	*dnx, *dny;			/* dest extent		*/
char    *ref;                  		/* reference name	*/
#endif
{
        int  
	status = imd_getMapping (cdl->imd, region, 
			sx,sy,snx,sny, dx,dy,dnx,dny, ref);

        if (cdl_debug) {
            printf ("[cdl_getMapping] region='%s' ref='%s'\n",
		(region ? region : ""), (ref ? ref : ""));
            printf ("\tsrc = %g,%g,%d,%d   dest = %d,%d,%d,%d\n",
               *sx, *sy, *snx, *sny, *dx, *dy, *dnx, *dny);
        }

        return (status);
}


/*  CDL_QUERYMAP -- Query a mapping given the wcs number.
 */
#ifdef ANSI_FUNC

int 
cdl_queryMap  (
    CDLPtr  cdl,                        /* package ptr          */
    int     wcs,                  	/* requested wcs number	*/
    char    *region,			/* region name		*/
    float   *sx,			/* source rect		*/
    float   *sy,
    int	    *snx, 			/* source extent	*/
    int	    *sny,
    int	    *dx, 			/* dest rect		*/
    int	    *dy,
    int	    *dnx, 			/* dest extent		*/
    int	    *dny,
    char    *objref                   	/* reference name	*/
)
#else

int 
cdl_queryMap  (cdl, wcs, region, sx,sy,snx,sny, dx,dy,dnx,dny, objref)
CDLPtr  cdl;                            /* package ptr          */
int     wcs;                  		/* requested wcs number	*/
char    *region;                  	/* region name 		*/
float	*sx, *sy;			/* source rect		*/
int	*snx, *sny;			/* source extent	*/
int	*dx, *dy;			/* dest rect		*/
int	*dnx, *dny;			/* dest extent		*/
char    *objref;                  	/* reference name	*/
#endif
{
	float a, b, c, d, tx, ty, z1, z2;
	int   zt;
	char  name[256], title[256];
	int   wcs_status = ERR;
	int   frame = wcs / 100;


	if (cdl->iis_version > 0) {
	    /* Do a WCS query so we get the requested mapping. */
	    wcs_status = imd_getWCS (cdl->imd, wcs, name, title, 
		&a, &b, &c, &d, &tx, &ty, &z1, &z2, &zt);

	    /* What we really want is the mapping info for the WCS */
	    wcs_status = cdl_getMapping (cdl, region, sx,sy,snx,sny,
		dx,dy,dnx,dny, objref);

	    if (cdl_debug) {
	        printf ("[cdl_queryMap] wcs=%d name='%s' title='%s'\n",
		    wcs, (name ? name : ""), (title ? title : ""));
                printf ("\ta=%g b=%g c=%g d=%g tx=%g ty=%g z1=%g z2=%g zt=%d\n",
                   a, b, c, d, tx, ty, z1, z2, zt);
                printf ("\tregion='%s' ref='%s'\n",
		    (region ? region : ""), (objref ? objref : ""));
                printf ("\tsrc = %g,%g,%d,%d   dest = %d,%d,%d,%d\n",
                   sx, sy, snx, sny, dx, dy, dnx, dny);
            }
	}

	return (wcs_status);
}



/*  CDL_CLEARFRAME -- Erase the current display frame.
 */

#ifdef ANSI_FUNC

int 
cdl_clearFrame (
    CDLPtr cdl                          /* package ptr          */
)
#else

int
cdl_clearFrame (cdl)
CDLPtr  cdl;                            /* package ptr          */
#endif
{
	if (cdl_debug)
	    printf ("[cdl_clearFrame]\n");

	return (imd_clearFrame (cdl->imd));
}


/*  CDL_SELECTFB -- Select a frame buffer large enough to contain an image
 *  of the given size.  Instead of finding jsut the first buffer large
 *  enough to hold the image look for one with minimal edge space.
 */

#ifdef ANSI_FUNC

void 
cdl_selectFB (
    CDLPtr cdl,                         /* package ptr          */
    int nx,
    int ny,				/* image size		*/
    int *fb,				/* frame buffer		*/
    int *w,
    int *h,				/* frame size		*/
    int *nf,				/* number of frames	*/
    int reset				/* reset after select	*/
)
#else

void
cdl_selectFB (cdl, nx, ny, fb, w, h, nf, reset)
CDLPtr  cdl;                            /* package ptr          */
int	nx, ny;				/* image size		*/
int	*fb;				/* frame buffer		*/
int	*w, *h;				/* frame size		*/
int	*nf;				/* number of frames	*/
int	reset;				/* reset after select	*/
#endif
{
	register int i, edges, mintab = -1, tmin = 100000;
	FBTab 	 tab;

	if (cdl_debug)
	    printf ("[cdl_selectFb] nx=%d ny=%d  ", nx, ny);

	for (i=0; i < MAX_FBCONFIG; i++) {
	    tab = *cdl->imd->fbtab[i];
	    if (tab.width == nx && tab.height == ny) {
		/* Get an exact match first. */
	        tab = *cdl->imd->fbtab[i];
	        goto found;

	    } else if (tab.width >= nx && tab.height >= ny) {
		/* Look for match with smallest padding. */
		edges = tab.width - nx + tab.height - ny;
		if (edges < tmin) {
		    tmin = edges;
		    mintab = i;
		}
	    }
	}
	if (mintab >= 0) {
	    tab = *cdl->imd->fbtab[mintab];
	    goto found;
	}

	/* Couldn't find one, punt and use the default. */
	fprintf (stderr,
	    "Warning: cannot find adequate frame buffer, using default.\n");
	tab = *cdl->imd->fbtab[0];
found:
	*fb = tab.config;
	*w  = tab.width;
	*h  = tab.height;
	*nf = tab.nframes;
	if (reset && cdl->fbconfig != *fb)
	    cdl_setFBConfig (cdl, *fb);

	if (cdl_debug)
	    printf ("->  fb=%d w=%d h=%d nf=%d\n", *fb, *w, *h, *nf);
}


/*  CDL_CLOSE -- Close the CDL package descriptor.
 */

#ifdef ANSI_FUNC

void 
cdl_close (
    CDLPtr cdl                          /* package ptr          */
)
#else

void
cdl_close (cdl)
CDLPtr  cdl;                            /* package ptr          */
#endif
{
	register int i;

	if (cdl_debug)
	    printf ("[cdl_close]\n");

	for (i=0; i < MAX_FRAMES; i++)
	    if (DLHead[i] != (MarkerPtr) NULL)
	        cdl_freeDisplayList (cdl, DLHead[i]);

	free ((char *) cdl->imname);
	free ((char *) cdl->imtitle);
	free ((char *) cdl->region);
	free ((char *) cdl->ref);

	imd_close (cdl->imd);
	free ((CDLPtr) cdl);
}



/*  CDL_READIMAGE -- Read the currently displayed image and return a pointer to
 *  the array and it's dimensions.  Since we know where the image was written
 *  in the frame buffer this is really just a large subregion read.
 */

#ifdef ANSI_FUNC

int 
cdl_readImage (
    CDLPtr cdl,                         /* package ptr          */
    uchar **pix,                   	/* image pixels (output)*/
    int *nx,
    int *ny               		/* dimensions (output)  */
)
#else

int
cdl_readImage (cdl, pix, nx, ny)
CDLPtr  cdl;                            /* package ptr          */
uchar   **pix;                   	/* image pixels (output)*/
int     *nx, *ny;               	/* dimensions (output)  */
#endif
{
	int	status;

	if (*pix == NULL)
	    *pix = (uchar *) malloc (cdl->im_nx * cdl->im_ny);

	status = imd_readImage (cdl->imd, *pix, nx, ny);
	if (cdl_debug)
	    printf ("[cdl_readImage] %dx%d pixels\n", *nx, *ny);

	return (status);
}


/*  CDL_READFRAMEBUFFER -- Read the contents of the entire frame buffer and
 *  return a pointer to the array and it's dimensions. 
 */

#ifdef ANSI_FUNC

int 
cdl_readFrameBuffer (
    CDLPtr cdl,                         /* package ptr          */
    uchar **pix,                   	/* image pixels (output)*/
    int *nx,
    int *ny               		/* dimensions (output)  */
)
#else

int
cdl_readFrameBuffer (cdl, pix, nx, ny)
CDLPtr  cdl;                            /* package ptr          */
uchar   **pix;                   	/* image pixels (output)*/
int     *nx, *ny;               	/* dimensions (output)  */
#endif
{
	int	status;

	if (*pix == NULL)
	    *pix = (uchar *) malloc (cdl->fbwidth * cdl->fbheight);

	status =  imd_readFrameBuffer (cdl->imd, *pix, nx, ny);
	if (cdl_debug)
	    printf ("[cdl_readFrameBuffer] %dx%d pixels\n", *nx, *ny);

	return (status);
}


/*  CDL_COMPUTEZSCALE -- Compute the optimal z1/z2 values for an array. We
 *  don't transform the pixels, just compute the values.
 */

#ifdef ANSI_FUNC

void 
cdl_computeZscale (
    CDLPtr cdl,                         /* package ptr          */
    uchar *pix,                   	/* data to be sampled   */
    int nx,
    int ny,                 		/* image dimensions     */
    int bitpix,                 	/* bits per pixel       */
    float *z1,
    float *z2               		/* min/max zscale values*/
)
#else

void
cdl_computeZscale (cdl, pix, nx, ny, bitpix, z1, z2)
CDLPtr  cdl;                            /* package ptr          */
uchar   *pix;                   	/* data to be sampled   */
int     nx, ny;                 	/* image dimensions     */
int     bitpix;                 	/* bits per pixel       */
float   *z1, *z2;               	/* min/max zscale values*/
#endif
{
	register float	np = nx * ny;

        cdl_zscale ((uchar *)pix, 
	    nx, ny, 
	    bitpix, 
	    z1, z2, 
	    cdl->contrast,
	    cdl->nsample, 
	    MAX(2,
	 	(int)(cdl->nsample / (cdl->nsamplines > 0 ?  
		    cdl->nsamplines :
	 	    (int)((float)ny / sqrt(np / (float)cdl->nsample))) )));

	if (cdl_debug)
	    printf ("[cdl_computeZscale] %dx%d-%d  --> z1=%g z2=%g zt=%d\n",
		nx, ny, bitpix, *z1, *z2, cdl->ztrans);
}


/*  CDL_ZSCALEIMAGE -- Compute the optimal z1/z2 values for an array and then
 *  scale the pixels.   We only compute the scale values if they're not input,
 *  otherwise we used the values passed in.
 */

#ifdef ANSI_FUNC

void 
cdl_zscaleImage (
    CDLPtr cdl,                         /* package ptr          */
    uchar **pix,                   	/* data to be sampled   */
    int nx,
    int ny,                 		/* image dimensions     */
    int bitpix,                 	/* bits per pixel       */
    float z1,
    float z2               		/* min/max zscale values*/
)
#else

void
cdl_zscaleImage (cdl, pix, nx, ny, bitpix, z1, z2)
CDLPtr  cdl;                            /* package ptr          */
uchar   **pix;                   	/* data to be sampled   */
int     nx, ny;                 	/* image dimensions     */
int     bitpix;                 	/* bits per pixel       */
float   z1, z2;               		/* min/max zscale values*/
#endif
{
	if (cdl_debug)
	    printf ("[cdl_zscaleImage]\n");

        cdl_setZScale (cdl, z1, z2);
        cdl_applyZscale (cdl, pix, nx, ny, bitpix);
}


/*  CDL_PRINTPIX --  Print the given pixels as EPS to the named command. 
 *  We assume that the 'cmd' argument is a unix command string that takes
 *  input from the stdin.
 */

#ifdef ANSI_FUNC

int 
cdl_printPix (
    CDLPtr cdl,                         /* package ptr          */
    char *cmd,				/* command string	*/
    uchar *pix,				/* pixel array		*/
    int nx,
    int ny,				/* image dimensions	*/
    int annotate			/* annotate output?	*/
)
#else

int
cdl_printPix (cdl, cmd, pix, nx, ny, annotate)
CDLPtr  cdl;                            /* package ptr          */
char	*cmd;				/* command string	*/
uchar	*pix;				/* pixel array		*/
int	nx, ny;				/* image dimensions	*/
int	annotate;			/* annotate output?	*/
#endif
{
	FILE 	*fp;
        PSImagePtr psim, eps_init();
	char	tmpfile[SZ_NAME], text[SZ_NAME];
	extern	char *mktemp();
	extern  int  system();

	if (cdl_debug)
	    printf ("[cdl_printPix] cmd='%s' %dx%d  annotate=%d\n",
		cmd, nx, ny, annotate);

	/* Open the image pointer each time since we assume it's not going to
	 * happen too often.
	 */
	psim = eps_init();

        /* Print to a printer device. */
        strcpy (tmpfile, "/tmp/cdlXXXXXX");
        if (mktemp(tmpfile) == (char *)NULL) {
	    eps_close (psim);
            return (ERR);
	}

        if (!(fp = fopen (tmpfile, "w"))) {
	    eps_close (psim);
            return (ERR);
	}
	psim->annotate = annotate;
	cdl_flip (pix, nx, ny);
	eps_setLabel (psim, cdl->imname);
	eps_setCorners (psim, 0, 0, nx, ny);
	eps_setColorType (psim, EPS_PSEUDOCOLOR);
	eps_setCmap (psim, cmap_r, cmap_g, cmap_b, 256);
        eps_print (psim, fp, pix, nx, ny, 8, 0);

        sprintf (text, "cat %s | %s", tmpfile, (cmd ? cmd : "lpr"));
        (void)system (text);                /* dispose to printer */
        unlink (tmpfile);                   /* delete tmp file */
 
	/* Clean up. */
        fclose (fp);
	eps_close (psim);
	return (OK);
}


/*  CDL_PRINTPIXTOFILE -- Print the given pixels as EPS to the named file.
 */

#ifdef ANSI_FUNC

int 
cdl_printPixToFile (
    CDLPtr cdl,                         /* package ptr          */
    char *fname,			/* filename		*/
    uchar *pix,				/* pixel array		*/
    int nx,
    int ny,				/* image dimensions	*/
    int annotate			/* annotate output?	*/
)
#else

int
cdl_printPixToFile (cdl, fname, pix, nx, ny, annotate)
CDLPtr  cdl;                            /* package ptr          */
char	*fname;				/* filename		*/
uchar	*pix;				/* pixel array		*/
int	nx, ny;				/* image dimensions	*/
int	annotate;			/* annotate output?	*/
#endif
{
	FILE 	*fp;
        PSImagePtr psim, eps_init();

	if (cdl_debug)
	    printf ("[cdl_printPixToFile] fname='%s' %dx%d  annotate=%d\n",
		fname, nx, ny, annotate);

	/* Open the image pointer each time since we assume it's not going to
	 * happen too often.
	 */
	psim = eps_init();

	/* Dump the EPS to the file. */
        if (access (fname, F_OK) < 0) {
            if ((fp = fopen (fname, "w"))) {
		eps_setLabel (psim, cdl->imname);
		psim->annotate = annotate;
		cdl_flip (pix, nx, ny);
		eps_setColorType (psim, EPS_PSEUDOCOLOR);
		eps_setCmap (psim, cmap_r, cmap_g, cmap_b, 256);
		eps_setCorners (psim, 0, 0, nx, ny);
                eps_print (psim, fp, pix, nx, nx, 8, 0);
                fclose (fp);
            } else {
                fprintf (stderr, "Could not open file %s", fname);
		eps_close (psim);
	        return (ERR);
	    }
	}

	eps_close (psim);
	return (OK);
}



/*  CDL_READSUBRASTER -- Read a rectangular region of the frame buffer.
 */

#ifdef ANSI_FUNC

int 
cdl_readSubRaster (
    CDLPtr cdl,				/* package ptr		*/
    int lx,
    int ly,               		/* region corner        */
    int nx,
    int ny,                 		/* dimensions           */
    uchar **pix                   	/* image pixels (output)*/
)
#else

int
cdl_readSubRaster (cdl, lx, ly, nx, ny, pix)
CDLPtr	cdl;				/* package ptr		*/
int     lx, ly;               		/* region corner        */
int     nx, ny;                 	/* dimensions           */
uchar   **pix;                   	/* image pixels (output)*/
#endif
{
	register int llx, lly;

	if (*pix == NULL)
	    *pix = (uchar *) malloc (nx * ny);

        llx = lx + (cdl->fbwidth / 2) - (cdl->im_nx / 2);
        lly = ly + cdl->fbheight - ((cdl->fbheight / 2) + (cdl->im_ny / 2));

	if (cdl_debug)
	    printf ("[cdl_readSubRaster] %dx%d at [%d,%d] offset [%d,%d]\n",
		nx, ny, lx, ly, llx, lly);

	return (imd_readSubRaster (cdl->imd, lx, ly, nx, ny, *pix));
}


/*  CDL_WRITESUBRASTER -- Write a rectangular region of the frame buffer.
 */

#ifdef ANSI_FUNC

int 
cdl_writeSubRaster (
    CDLPtr cdl,				/* package ptr		*/
    int lx,
    int ly,               		/* region corner        */
    int nx,
    int ny,                 		/* dimensions           */
    uchar *pix                   	/* subraster pixels     */
)
#else

int
cdl_writeSubRaster (cdl, lx, ly, nx, ny, pix)
CDLPtr	cdl;				/* package ptr		*/
int     lx, ly;               		/* region corner        */
int     nx, ny;                 	/* dimensions           */
uchar   *pix;                   	/* subraster pixels     */
#endif
{
	if (cdl_debug)
	    printf ("[cdl_writeSubRaster] %dx%d at [%d,%d]\n",
		nx, ny, lx, ly);

	return (imd_writeSubRaster (cdl->imd, lx, ly, nx, ny, pix));
}



/*  CDL_SETFBCONFIG -- Set the frame buffer configuration number.
 */

#ifdef ANSI_FUNC

void 
cdl_setFBConfig (
    CDLPtr cdl,				/* package ptr		*/
    int configno			/* fb config number	*/
)
#else

void
cdl_setFBConfig (cdl, configno)
CDLPtr	cdl;				/* package ptr		*/
int	configno;			/* fb config number	*/
#endif
{
	int	cfg = max (1, configno);

	if (cdl_debug)
	    printf ("[cdl_setFBConfig] configno=%d\n", configno);

	cdl->fbconfig = cfg;
	cdl->fbwidth = cdl->imd->fbtab[cfg-1]->width;
	cdl->fbheight = cdl->imd->fbtab[cfg-1]->height;
	cdl->fbnf = cdl->imd->fbtab[cfg-1]->nframes;
	(void) imd_setFBConfig (cdl->imd, cdl->fbconfig);
}


/*  CDL_GETFBCONFIG -- Get the frame buffer configuration number.
 */

#ifdef ANSI_FUNC

void 
cdl_getFBConfig (
    CDLPtr cdl,				/* package ptr		*/
    int *configno,                      /* fb config number     */
    int *w,
    int *h,				/* fb frame size	*/
    int *nframes			/* number of frames	*/
)
#else

void
cdl_getFBConfig (cdl, configno, w, h, nframes)
CDLPtr	cdl;				/* package ptr		*/
int     *configno;                      /* fb config number     */
int	*w, *h;				/* fb frame size	*/
int	*nframes;			/* number of frames	*/
#endif
{
	*configno = cdl->fbconfig;
	*w        = cdl->fbwidth;
	*h        = cdl->fbheight;
	*nframes  = cdl->fbnf;

	if (cdl_debug)
	    printf ("[cdl_getFBConfig] configno=%d\n", *configno);
}


/*  CDL_LOOKUPFBSIZE -- Get the frame buffer dimensions given a config number.
 */

#ifdef ANSI_FUNC

void 
cdl_lookupFBSize (
    CDLPtr cdl,				/* package ptr		*/
    int configno,                       /* fb config number     */
    int *w,
    int *h,				/* fb frame size	*/
    int *nf				/* number of frames	*/
)
#else

void
cdl_lookupFBSize (cdl, configno, w, h, nf)
CDLPtr	cdl;				/* package ptr		*/
int     configno;                       /* fb config number     */
int	*w, *h;				/* fb frame size	*/
int	*nf;				/* number of frames	*/
#endif
{
	*w  = cdl->imd->fbtab[configno-1]->width;
	*h  = cdl->imd->fbtab[configno-1]->height;
	*nf = cdl->imd->fbtab[configno-1]->nframes;

	if (cdl_debug) {
	    printf ("[cdl_lookupFBSize] configno=%d size=%dx%d\n", configno,
		*w, *h);
	}
}



/*  CDL_SETFRAME -- Set the current display frame.
 */

#ifdef ANSI_FUNC

void 
cdl_setFrame (
    CDLPtr cdl,				/* package ptr		*/
    int frame				/* frame number		*/
)
#else

void
cdl_setFrame (cdl, frame)
CDLPtr	cdl;				/* package ptr		*/
int	frame;				/* frame number		*/
#endif
{
	if (cdl_debug)
	    printf ("[cdl_setFrame] frame=%d\n", frame);

        /* Sanity check. */
        if (frame < 1 || frame > MAX_FRAMES) {
            perror ("cdl_setFrame: invalid frame number - resetting.");
        }

	cdl->frame = max(1,min(MAX_FRAMES,frame));
	(void) imd_setFrame (cdl->imd, cdl->frame);
}


/*  CDL_SETZTRANS -- Set the current zscale transform parameters.
 */

#ifdef ANSI_FUNC

void 
cdl_setZTrans (
    CDLPtr cdl,				/* package ptr		*/
    int ztrans				/* z-transform type	*/
)
#else

void
cdl_setZTrans (cdl, ztrans)
CDLPtr	cdl;				/* package ptr		*/
int	ztrans;				/* z-transform type	*/
#endif
{
	if (cdl_debug)
	    printf ("[cdl_setZTrans] zt=%d\n", ztrans);

	cdl->ztrans = ztrans;
}


/*  CDL_SETZSCLAE -- Set the current zscale transform parameters.
 */

#ifdef ANSI_FUNC

void 
cdl_setZScale (
    CDLPtr cdl,				/* package ptr		*/
    float z1,
    float z2				/* zscale values	*/
)
#else

void
cdl_setZScale (cdl, z1, z2)
CDLPtr	cdl;				/* package ptr		*/
float	z1, z2;				/* zscale values	*/
#endif
{
	if (cdl_debug)
	    printf ("[cdl_setZScale] z1=%g z2=%g\n", z1, z2);

	cdl->z1 = z1;
	cdl->z2 = z2;
	cdl->imd->z1 = z1;
	cdl->imd->z2 = z2;
}


/*  CDL_SETSAMPLE -- Set the number of zscale sample points to use.
 */

#define	cdl_setNSamples (cdl, ns)	cdl_setSample(cdl,ns)
#define	cdl_setSamples (cdl, ns)	cdl_setSample(cdl,ns)

#ifdef ANSI_FUNC

void 
cdl_setSample (
    CDLPtr cdl,				/* package ptr		*/
    int nsample				/* no. of sample pts	*/
)
#else

void
cdl_setSample (cdl, nsample)
CDLPtr	cdl;				/* package ptr		*/
int	nsample;			/* no. of sample pts	*/
#endif
{
	if (cdl_debug)
	    printf ("[cdl_setSample] nsample=%d\n", nsample);

	cdl->nsample = MAX(5,nsample);
}


/*  CDL_SETSAMPLELINES -- Set the number of zscale sample lines to use.
 */

#ifdef ANSI_FUNC

void 
cdl_setSampleLines (
    CDLPtr cdl,				/* package ptr		*/
    int nlines				/* no. of sample lines	*/
)
#else

void
cdl_setSampleLines (cdl, nlines)
CDLPtr	cdl;				/* package ptr		*/
int	nlines;				/* no. of sample lines	*/
#endif
{
	if (cdl_debug)
	    printf ("[cdl_setSampleLines] nlines=%d\n", nlines);

	cdl->nsamplines = MAX(1,nlines);
}


/*  CDL_SETCONTRAST -- Set the zscale contrast value.
 */

#ifdef ANSI_FUNC

void 
cdl_setContrast (
    CDLPtr cdl,				/* package ptr		*/
    float contrast			/* contrast value	*/
)
#else

void
cdl_setContrast (cdl, contrast)
CDLPtr	cdl;				/* package ptr		*/
float	contrast;			/* contrast value	*/
#endif
{
	if (cdl_debug)
	    printf ("[cdl_setContrast] contrast=%g\n", contrast);

	cdl->contrast = contrast;
}


/*  CDL_SETNAME -- Set the image name for the WCS string.
 */

#ifdef ANSI_FUNC

void 
cdl_setName (
    CDLPtr cdl,				/* package ptr		*/
    char *imname			/* image name		*/
)
#else

void
cdl_setName (cdl, imname)
CDLPtr	cdl;				/* package ptr		*/
char	*imname;			/* image name		*/
#endif
{
	if (cdl_debug)
	    printf ("[cdl_setName] name='%s'\n", (imname?imname:""));

	(void) imd_setName(cdl->imd, imname);
}


/*  CDL_SETTITLE -- Set the image title for the WCS string.
 */

#ifdef ANSI_FUNC

void 
cdl_setTitle (
    CDLPtr cdl,				/* package ptr		*/
    char *imtitle			/* image title		*/
)
#else

void
cdl_setTitle (cdl, imtitle)
CDLPtr	cdl;				/* package ptr		*/
char	*imtitle;			/* image title		*/
#endif
{
	if (cdl_debug)
	    printf ("[cdl_setTitle] title='%s'\n", (imtitle?imtitle:""));

	(void) imd_setTitle(cdl->imd, imtitle);
}


/*  CDL_GETFRAME -- Get the current display frame.
 */

#ifdef ANSI_FUNC

void 
cdl_getFrame (
    CDLPtr cdl,				/* package ptr		*/
    int *frame				/* frame number		*/
)
#else

void
cdl_getFrame (cdl, frame)
CDLPtr	cdl;				/* package ptr		*/
int	*frame;				/* frame number		*/
#endif
{
	float 	x, y;
	int	wcs;
	char 	key;

	(void) imd_readCursor (cdl->imd, 1, &x, &y, &wcs, &key);
	*frame = (int) (wcs / 100);

	if (*frame == 0)
	    *frame = cdl->frame;

	if (cdl_debug)
	    printf ("[cdl_getFrame] frame=%d\n", *frame);
}


/*  CDL_GETZTRANS -- Get the current zscale transform parameters.
 */

#ifdef ANSI_FUNC

void 
cdl_getZTrans (
    CDLPtr cdl,				/* package ptr		*/
    int *ztrans                         /* z-transform type     */
)
#else

void
cdl_getZTrans (cdl, ztrans)
CDLPtr	cdl;				/* package ptr		*/
int     *ztrans;                        /* z-transform type     */
#endif
{
	*ztrans = cdl->ztrans;

	if (cdl_debug)
	    printf ("[cdl_getZTrans] zt=%d\n", *ztrans);
}


/*  CDL_GETZTRANS -- Get the current zscale transform parameters.
 */

#ifdef ANSI_FUNC

void 
cdl_getZScale (
    CDLPtr cdl,				/* package ptr		*/
    float *z1,
    float *z2                       	/* zscale values        */
)
#else

void
cdl_getZScale (cdl, z1, z2)
CDLPtr	cdl;				/* package ptr		*/
float   *z1, *z2;                       /* zscale values        */
#endif
{
	*z1	= cdl->z1;
	*z2	= cdl->z2;

	if (cdl_debug)
	    printf ("[cdl_getZScale] z1=%g z2=%g\n", *z1, *z2);
}


/*  CDL_GETSAMPLE -- Get the number of zscale sample points to use.
 */

#ifdef ANSI_FUNC

void 
cdl_getSample (
    CDLPtr cdl,				/* package ptr		*/
    int *nsample                       	/* no. of sample pts    */
)
#else

void
cdl_getSample (cdl, nsample)
CDLPtr	cdl;				/* package ptr		*/
int     *nsample;                       /* no. of sample pts    */
#endif
{
	*nsample = cdl->nsample;

	if (cdl_debug)
	    printf ("[cdl_getSample] nsample=%d\n", *nsample);
}


/*  CDL_GETSAMPLELINES -- Get the number of zscale sample lines to use.
 */

#ifdef ANSI_FUNC

void 
cdl_getSampleLines (
    CDLPtr cdl,				/* package ptr		*/
    int *nlines                        	/* no. of sample lines  */
)
#else

void
cdl_getSampleLines (cdl, nlines)
CDLPtr	cdl;				/* package ptr		*/
int     *nlines;                        /* no. of sample lines  */
#endif
{
	*nlines = cdl->nsamplines;

	if (cdl_debug)
	    printf ("[cdl_getSampleLines] nlines=%d\n", *nlines);
}


/*  CDL_GETCONTRAST -- Get the zscale contrast value.
 */

#ifdef ANSI_FUNC

void 
cdl_getContrast (
    CDLPtr cdl,				/* package ptr		*/
    float *contrast                     /* contrast value       */
)
#else

void
cdl_getContrast (cdl, contrast)
CDLPtr	cdl;				/* package ptr		*/
float   *contrast;                      /* contrast value       */
#endif
{
	*contrast = cdl->contrast;

	if (cdl_debug)
	    printf ("[cdl_getContrast] contrast=%g\n", *contrast);
}


/*  CDL_GETNAME -- Get the image name for the WCS string.
 */

#ifdef ANSI_FUNC

void 
cdl_getName (
    CDLPtr cdl,				/* package ptr		*/
    char *imname                        /* image name           */
)
#else

void
cdl_getName (cdl, imname)
CDLPtr	cdl;				/* package ptr		*/
char    *imname;                        /* image name           */
#endif
{
	if (imname) 
	    strcpy (imname, cdl->imname);

	if (cdl_debug)
	    printf ("[cdl_setName] name='%s'\n", (imname?imname:""));
}


/*  CDL_GETTITLE -- Get the image title for the WCS string.
 */

#ifdef ANSI_FUNC

void 
cdl_getTitle (
    CDLPtr cdl,				/* package ptr		*/
    char *imtitle                       /* image title          */
)
#else

void
cdl_getTitle (cdl, imtitle)
CDLPtr	cdl;				/* package ptr		*/
char    *imtitle;                       /* image title          */
#endif
{
	if (imtitle) 
	    strcpy (imtitle, cdl->imtitle);

	if (cdl_debug)
	    printf ("[cdl_setTitle] title='%s'\n", (imtitle?imtitle:""));
}


/* --------------------
 * PRIVATE PROCEDURES
 * ------------------ */



/* CDL_SETDEBUG -- Set the state of the debug flag.
 */

#ifdef ANSI_FUNC

void 
cdl_setDebug (int state)
#else

void
cdl_setDebug (state)
int     state;
#endif
{
        cdl_debug = state;
	if (state >= 1)
	    imd_setDebug (state);
	if (state >= 2)
	    com_setDebug (state);
} 


/*  CDL_APPLYZSCALE -- Compute the Zscale transform image given an array of
 *  raw pixels and image size.  We use the z1/z2/ztrans values assumed to be
 *  previously stored.
 */

#ifdef ANSI_FUNC

static void 
cdl_applyZscale (
    CDLPtr cdl,                         /* package ptr          */
    uchar **pix,                   	/* input image pixels 	*/
    int nx,
    int ny,               		/* image dimensions   	*/
    int bitpix				/* pixel type		*/
)
#else

static void
cdl_applyZscale (cdl, pix, nx, ny, bitpix)
CDLPtr  cdl;                            /* package ptr          */
uchar   **pix;                   	/* input image pixels 	*/
int     nx, ny;               		/* image dimensions   	*/
int	bitpix;				/* pixel type		*/
#endif
{
	register int	i, n = nx * ny, pmin = 1, pmax = 200;
        register int    smax, smin;
	float 		pval, scale = 0.0, dscale = (200.0 / 3.0);
	uchar	 	*outpix ;
	float	 	z1, z2;			/* zscale values 	*/
	int	 	zt;
	extern		void bcopy();


       if (cdl_debug)
            printf ("[cdl_applyZscale] nx=%d by=%d bitpix=%d\n", nx,ny,bitpix);

	/* Move the input array to the output array before zscaling. */
	outpix = (uchar *) malloc (nx * ny);
	bcopy ((char *)*pix, (char *)outpix, nx * ny);

	/* Get the zscale transform values. */
	cdl_getZTrans (cdl, &zt);
	cdl_getZScale (cdl, &z1, &z2);
        smin = (int) z1;
        smax = (int) z2;
	if (bitpix < 0)
            scale = (z2 == z1) ? 0. : 200. / (z2 - z1);
	else
            scale = (smax == smin) ? 0. : 200. / (z2 - z1);


	/* Now scale the pixels. */
	if (bitpix == 16) {
            register short int  *buffer = (short *)(*pix);

	    if (zt == CDL_LINEAR) {
                for (i=0; i < n; i++)
                    outpix[i] = max (pmin, min (pmax,
                        (int)(scale * (float)((int)buffer[i] - smin)) ));
	    } else if (zt == CDL_LOG) {
                scale = (smax == smin) ? 0. : (1000.0 - 1.0) / (z2 - z1);
                for (i=0; i < n; i++) {
		    /* Scale that to the range 1-1000 and take the log. */
                    pval = max (1.0,min(1000.0, (scale * (buffer[i] - smin)) ));
                    pval = log10 (pval);

		    /* Now scale back to the display range */
                    outpix[i] = (uchar) max (pmin, min (pmax,
			(uchar)(dscale * pval) ));
		}
	    } else if (zt == CDL_UNITARY) {
		for (i=0; i < n; i++) 
		    outpix[i] = buffer[i];
	    }
 
	} else if (bitpix == 32) {
            register int      *buffer = (int *)(*pix);

	    if (zt == CDL_LINEAR) {
                for (i=0; i < n; i++)
                    outpix[i] = max (pmin, min (pmax,
                        (int)(scale * (float)((int)buffer[i] - smin)) ));
	    } else if (zt == CDL_LOG) {
                scale = (smax == smin) ? 0. : (1000.0 - 1.0) / (z2 - z1);
                for (i=0; i < n; i++) {
		    /* Scale that to the range 1-1000 and take the log. */
                    pval = max (1.0,min(1000.0, (scale * (buffer[i] - smin)) ));
                    pval = log10 (pval);

		    /* Now scale back to the display range */
                    outpix[i] = (uchar) max (pmin, min (pmax,
			(uchar)(dscale * pval) ));
		}
	    } else if (zt == CDL_UNITARY) {
		for (i=0; i < n; i++) 
		    outpix[i] = buffer[i];
	    }
 
	} else if (bitpix == 64) {
            register long int  *buffer = (long *)(*pix);

	    if (zt == CDL_LINEAR) {
                for (i=0; i < n; i++)
                    outpix[i] = max (pmin, min (pmax,
                        (int)(scale * (float)((int)buffer[i] - smin)) ));
	    } else if (zt == CDL_LOG) {
                scale = (smax == smin) ? 0. : (1000.0 - 1.0) / (z2 - z1);
                for (i=0; i < n; i++) {
		    /* Scale that to the range 1-1000 and take the log. */
                    pval = max (1.0,min(1000.0, (scale * (buffer[i] - smin)) ));
                    pval = log10 (pval);

		    /* Now scale back to the display range */
                    outpix[i] = (uchar) max (pmin, min (pmax,
			(uchar)(dscale * pval) ));
		}
	    } else if (zt == CDL_UNITARY) {
		for (i=0; i < n; i++) 
		    outpix[i] = buffer[i];
	    }
 
	} else if (bitpix == -32) {
            register float      *buffer = (float *)(*pix);

	    if (zt == CDL_LINEAR) {
                for (i=0; i < n; i++)
                    outpix[i] = max (pmin, min (pmax,
                        (int)(scale * (float)((float)buffer[i] - z1)) ));
	    } else if (zt == CDL_LOG) {
                scale = (smax == smin) ? 0. : (1000.0 - 1.0) / (z2 - z1);
                for (i=0; i < n; i++) {
		    /* Scale that to the range 1-1000 and take the log. */
                    pval = max (1.0,min(1000.0, (scale * (buffer[i] - z1)) ));
                    pval = log10 (pval);

		    /* Now scale back to the display range */
                    outpix[i] = (uchar) max (pmin, min (pmax,
			(uchar)(dscale * pval) ));
		}
	    } else if (zt == CDL_UNITARY) {
		for (i=0; i < n; i++) 
		    outpix[i] = buffer[i];
	    }
 
	} else if (bitpix == -64) {
            register double      *buffer = (double *)(*pix);

	    if (zt == CDL_LINEAR) {
                for (i=0; i < n; i++)
                    outpix[i] = max (pmin, min (pmax,
                        (int)(scale * (float)((double)buffer[i] - z1)) ));
	    } else if (zt == CDL_LOG) {
                scale = (smax == smin) ? 0. : (1000.0 - 1.0) / (z2 - z1);
                for (i=0; i < n; i++) {
		    /* Scale that to the range 1-1000 and take the log. */
                    pval = max (1.0,min(1000.0, (scale * (buffer[i] - z1)) ));
                    pval = log10 (pval);

		    /* Now scale back to the display range */
                    outpix[i] = (uchar) max (pmin, min (pmax,
			(uchar)(dscale * pval) ));
		}
	    } else if (zt == CDL_UNITARY) {
		for (i=0; i < n; i++) 
		    outpix[i] = buffer[i];
	    }
	}

	/* Copy the scaled pixel array back to the original raster.  We don't
	 * realloc the pointer in case it's some static array.
	 */
	bcopy ((char *)outpix, (char *)*pix, n);

	if (cdl_debug)
	    printf ("[cdl_zscaleImage] %dx%d-%d  --> z1=%g z2=%g zt=%d\n",
		nx, ny, bitpix, z1, z2, zt);

	free ((char *) outpix);
}


/* CDL_FLIP -- Reverse order of lines in image.
 */

#ifdef ANSI_FUNC

static void 
cdl_flip (uchar *buffer, int nx, int ny)
#else

static void
cdl_flip (buffer, nx, ny)
uchar 	*buffer;
int     nx;
int     ny;
#endif
{
        register int    i, j, v;
        register uchar  *buff1, *buff2;

        for (i = 0; i < ny / 2; i++) {
            buff1 = &buffer[i*nx];
            buff2 = &buffer[(ny-1-i)*nx];
            for (j = 0; j < nx; j++) {
                v = *buff1;
                *(buff1++) = *buff2;
                *(buff2++) = v;
            }
        }
}
