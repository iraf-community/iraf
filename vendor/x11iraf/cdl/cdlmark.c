#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#define  CDL_NEED_LINESTYLES
#define  CDL_LIBRARY_SOURCE
#include "cdl.h"
#include "cdlfont.h"

/*
 *  CDL Marker Procedures -- Procedures for doing overlay graphics.
 *
 *             cdl_mapFrame  (cdl, frame)
 *       cdl_markCoordsFile  (cdl, fname, type, size, color, label)
 *            cdl_markPoint  (cdl, x, y, number, size, type, color)
 *       cdl_markPointLabel  (cdl, x, y, label, size, type, color)
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
 *              cdl_setFont  (cdl, font)
 *         cdl_setTextWidth  (cdl, width)
 *         cdl_setLineWidth  (cdl, width)
 *         cdl_setLineStyle  (cdl, style)
 *
 *           cdl_deleteMark  (cdl, x, y)
 *         cdl_clearOverlay  (cdl)
 *        cdl_redrawOverlay  (cdl)
 *
 *            cdl_beginDList (frame)		   Not Yet Implemented
 *            cdl_clearDList (frame)		   Not Yet Implemented
 *             cdl_drawDList (frame)		   Not Yet Implemented
 *              cdl_endDList (frame, flush)	   Not Yet Implemented
 */

/* Function prototypes */
#ifdef __STDC__
#include <stddef.h>
#endif

#ifdef ANSI_FUNC

static int cdl_getPixRegion(CDLPtr cdl, int lx, int ly, int nx, int ny, uchar *ras);
static void cdl_doPointMark(int color, int type, uchar *pix, int sz);
static void cdl_doLineMark(int color, int width, int style, uchar *pix, int xs, int ys, int xe, int ye);
static void cdl_doBoxMark(int color, int width, int style, int fill, uchar *pix, int lx, int ly, int nx, int ny);
static void cdl_doPolygonMark(int color, int width, int style, int fill, int *x, int *y, int npts, uchar *pix, int lx, int ly, int nx, int ny, int close);
static void cdl_doCircleMark(int x, int y, int radius, int color, int width, int style, int fill, uchar *pix, int lx, int ly, int nx, int ny);
static void cdl_doCircAnnuliMark(int x, int y, int radius, int nannuli, int sep, int color, int width, int style, uchar *pix, int lx, int ly, int nx, int ny);
static void cdl_doEllipseMark(int color, int width, int style, int fill, int xc, int yc, int xrad, int yrad, float ang, uchar *pix, int lx, int ly, int nx, int ny);
static void cdl_doEllipAnnuliMark(int x, int y, int xrad, int yrad, float ang, int nannuli, int sep, int color, int width, int style, uchar *pix, int lx, int ly, int nx, int ny);
int cdl_doTextMarker(int x, int y, char *string, float size, float angle, int color, int width, int font, uchar *pix, int lx, int ly, int nx, int ny);
int cdl_freeDisplayList(CDLPtr cdl, MarkerPtr head);
static void cdl_insertMarker(CDLPtr cdl, MarkerPtr back, MarkerPtr new);
static void cdl_removeMarker(CDLPtr cdl, MarkerPtr mk);
static MarkerPtr cdl_findNearest(MarkerPtr head, int x, int y);
static void cdl_printDisplayList(FILE *fd, MarkerPtr head);
static MarkerPtr cdl_initPointMarker(int x, int y, int number, int size, int type, int color);
static MarkerPtr cdl_initPointLabelMarker(int x, int y, char *label, int size, int type, int color, int font);
static MarkerPtr cdl_initLineMarker(int x1, int y1, int x2, int y2, int color, int width, int style);
static MarkerPtr cdl_initBoxMarker(int lx, int ly, int ux, int uy, int fill, int color, int width, int style);
static MarkerPtr cdl_initPolylineMarker(int *x, int *y, int npts, int color, int width, int style);
static MarkerPtr cdl_initPolygonMarker(int *x, int *y, int npts, int fill, int color, int width, int style);
static MarkerPtr cdl_initCircleMarker(int x, int y, int radius, int fill, int color, int width, int style);
static MarkerPtr cdl_initCircAnnMarker(int x, int y, int radius, int nannuli, int sep, int color, int width, int style);
static MarkerPtr cdl_initEllipseMarker(int x, int y, int xrad, int yrad, float ang, int fill, int color, int width, int style);
static MarkerPtr cdl_initEllAnnMarker(int x, int y, int xrad, int yrad, float ang, int nannuli, int sep, int color, int width, int style);
static MarkerPtr cdl_initTextMarker(int x, int y, char *string, float size, int color, int font, int width);
static void cdl_initMarkPos(MarkerPtr mk, int nx, int ny, int lx, int ly);
static void cdl_doLineInSubRas(uchar *pix, int color, int width, int style, int x1, int x2, int y1, int y2, int lx, int ly, int nx, int ny);
static void cdl_doDashedLine(uchar *pix, int color, int width, int style, int *x, int *y, int np, int lx, int ly, int nx, int ny);
static void cdl_drawThickVector (uchar *pix, int color, int width, int style, int x1, int x2, int y1, int y2, int lx, int ly, int nx, int ny);
static void cdl_drawVector(uchar *pix, int color, int x1, int x2, int y1, int y2, int lx, int ly, int nx, int ny);
static void cdl_drawThickDashVec (uchar *pix, int color, int width, int style, int x1, int x2, int y1, int y2, int lx, int ly, int nx, int ny);
static void cdl_drawDashVec(uchar *pix, int color, int style, int x1, int x2, int y1, int y2, int lx, int ly, int nx, int ny);
static uchar cdl_setpixel(uchar pix, int style, int color);
static void cdl_fillArea(uchar *pix, int nx, int ny, int color);
static void cdl_getCircleCoords(int xcen, int ycen, int radius, int *x, int *y, int npts);
static void cdl_getEllipseCoords(int xcen, int ycen, int xradius, int yradius, float rotang, int *x, int *y);
static void cdl_minmax(int *array, int npts, int *amin, int *amax);
static int cdl_strlen(char *str, float size, int font);
static int bitupk(unsigned int wordp, int offset, int nbits);

#else

int		cdl_freeDisplayList();
void		cdl_setFont(), cdl_setLineWidth(), cdl_setLineStyle();
void		cdl_beginDList(), cdl_drawDList();
void		cdl_clearDList(), cdl_endDList(), cdl_setTextWidth();

static int	cdl_getPixRegion();
static uchar	cdl_setpixel();
static void	cdl_insertMarker(), cdl_removeMarker(); 
static void	cdl_doLineInSubRas(), cdl_drawThickVector(), cdl_drawVector();
static void	cdl_doPointMark(), cdl_doLineMark(), cdl_doBoxMark();
static void	cdl_doPolygonMark(), cdl_doCircleMark(), cdl_doCircAnnuliMark();
static void	cdl_doEllipseMark(), cdl_doEllipAnnuliMark();
static void	cdl_getCircleCoords(), cdl_getEllipseCoords();
static void	cdl_printDisplayList(), cdl_initMarkPos();
static void	cdl_fillArea(), cdl_minmax(), cdl_doDashedLine();
static void 	cdl_drawThickDashVec(), cdl_drawDashVec();

static int 	cdl_strlen(), bitupk();

static MarkerPtr cdl_initPointMarker(), cdl_initLineMarker();
static MarkerPtr cdl_initBoxMarker(), cdl_initPolylineMarker();
static MarkerPtr cdl_initPolygonMarker(), cdl_initCircleMarker();
static MarkerPtr cdl_initCircAnnMarker(), cdl_initEllipseMarker();
static MarkerPtr cdl_initEllAnnMarker(), cdl_findNearest();
static MarkerPtr cdl_initTextMarker(), cdl_initPointLabelMarker();

#endif


/* Display list declarations.  We keep a separate list for each frame that
 * is freed whenever a new image is displayed.  The list is maintained as
 * a doubly-linked list of Marker structs.
 */
MarkerPtr  DLHead[MAX_FRAMES];          /* diplay list head     */
MarkerPtr  DLTail[MAX_FRAMES];          /* diplay list tail     */
int	   DLFlag[] = { 		/* display list flags.  */
		    0, 0, 0, 0,		/* If set we're doing   */
		    0, 0, 0, 0		/* markers in memory	*/
	       };
uchar	   *DLFBPix[MAX_FRAMES];	/* frame buffer pixels  */


/* absolute value of a */
#define ABS(a)          (((a)<0) ? -(a) : (a))

/* take binary sign of a, either -1, or 1 if >= 0 */
#define SGN(a)          (((a)<0) ? -1 : 1)

#define	N_CIRCLE_PTS	48	/* no. of points defining unit circle  	*/
#define	N_ELLIPSE_PTS	64	/* no. of points defining unit ellipse 	*/
#define FONT_SCALE      0.36	/* default text font scale		*/
#define FONT_SPACE      4	/* default text font spacing		*/

#ifndef M_PI
#define M_PI    3.14159265358979323846
#endif

extern int	cdl_debug;



/*  CDL_MAPFRAME -- Map the current frame buffer as an image for overlay.
 *  This routine is a bit of a hack since we're trying to make some guesses
 *  about what the server has displayed, there's no way to actually query it.
 *  We assume the fbconfig has already been reset if it's not the default,
 *  and we assume the WCS defined is for a single image centered in the frame.
 */

#ifdef ANSI_FUNC

int 
cdl_mapFrame (
    CDLPtr cdl,				/* package ptr		*/
int frame				/* fb frame to map	*/
)
#else

int
cdl_mapFrame (cdl, frame)
CDLPtr	cdl;				/* package ptr		*/
int	frame;				/* fb frame to map	*/
#endif
{
	int	xo, yo;
	float   a, b, c, d, tx, ty, z1, z2;
	int     zt;
	char	*name = NULL, *title = NULL;

        name  = (char *) malloc (SZ_NAME);
        title = (char *) malloc (SZ_NAME);

	/* Select the requested frame. */
	cdl_setFrame (cdl, frame);

	/* Get/Set the current WCS. */
	(void) cdl_getWCS (cdl, name, title, &a, &b, &c, &d, &tx, &ty,
	    &z1, &z2, &zt);

	/* If no WCS is defined for the frame make up a default. */
	if ((a+b+c+d+tx+ty+z1+z2+zt) == 0.0) {
            a  =  1.0;
            b  =  0.0;
            c  =  0.0;
            d  = -1.0;
            tx =  1.0;
            ty = (float) cdl->fbheight;
            z1 =  0.0;
            z2 = 255.0;
            zt = CDL_LINEAR;
	}
	(void) cdl_setWCS (cdl, name, title, a, b, c, d, tx, ty, z1, z2, zt);

	/* Reset the image corners, assume image is centered in the frame.  */
	xo = - cdl->tx + 1;
	yo = cdl->fbheight - cdl->ty;
	cdl->imd->xs = xo;
	cdl->imd->ys = yo;
	cdl->imd->xe = cdl->fbwidth - xo + 1;
	cdl->imd->ye = cdl->fbheight - yo + 1;

	free (name);
	free (title);
	return (OK);
}


/*  CDL_MARKPOINT --  Draw a mark on the display at the given point.  Marks
 *  be specified as either a '+', 'x', or '*'.  If the number argument is
 *  positive that value will be drawn next to the mark as a label string.
 */

#ifdef ANSI_FUNC

int 
cdl_markPoint (
    CDLPtr cdl,				/* package ptr		*/
    int x,
    int y,				/* marker position	*/
    int number,				/* if > 0, label value	*/
    int size,				/* marker size (pixels) */
    int type,				/* type to draw		*/
    int color				/* marker color		*/
)
#else

int
cdl_markPoint (cdl, x, y, number, size, type, color)
CDLPtr	cdl;				/* package ptr		*/
int	x, y;				/* marker position	*/
int	number;				/* if > 0, label value	*/
int	size;				/* marker size (pixels) */
int	type;				/* type to draw		*/
int	color;				/* marker color		*/
#endif
{
	register int lx, ly, sz;
	uchar	*pix = NULL;
	MarkerPtr mk = cdl_initPointMarker (x, y, number, size, type, color);

	if (cdl_debug)
	    printf ("[cdl_markPoint] (%dx%d) num=%d size=%d type=%d color=%d\n",
		x, y, number, size, type, color);

	sz = (size % 2 ? size : size + 1);
	sz = max (sz, 3);
	lx = x - (sz / 2);
	ly = y - (sz / 2);
	cdl_initMarkPos (mk, sz, sz, lx, ly);

	/* Get the pixel region we'll be marking. */
	pix = (uchar *) malloc (sz * sz);
	if (cdl_getPixRegion (cdl, lx, ly, sz, sz, pix))
	    return (ERR);

	/* Save the image pixels in the display list for erasure later. */
	mk->refpix = (uchar *) malloc (sz * sz);
	mk->markpix = (uchar *) malloc (sz * sz);
	bcopy (pix, mk->refpix, sz * sz);
	cdl_insertMarker (cdl, DLTail[cdl->frame-1], mk);

	/* Edit the pixels with the requested color and save the raster. */
	cdl_doPointMark (color, type, pix, sz);
	bcopy (pix, mk->markpix, sz * sz);

	/* Number the point if requested. */
	if (number > 0) {
	    char   lab[64];
	    int	   cx, cy, llen;

	    sprintf (lab, "%d", number);
	    llen = strlen(lab) * (CHARACTER_WIDTH * FONT_SCALE) + strlen(lab);

	    /* Position the label so it isn't clipped.
	     */
	    cx = x + (sz / 2) - 1;
	    cy = y + (sz / 2) - 1;
	    if ((cx + llen) > cdl->im_nx)   cx = x - llen + 1;
	    if ((cy + llen) > cdl->im_ny)   cy = y - llen + 1;
	    cdl_markText (cdl, cx, cy, lab, 1., 0.0, color);
	}

	/* Write the region back to the display. */
	if (cdl_writeSubRaster (cdl, lx, ly, sz, sz, pix)) {
	    free ((char *)pix);
	    return (ERR);
	}

	free ((char *)pix);
	return (OK);
}


/*  CDL_MARKPOINTLABEL --  Draw a mark on the display at the given point. 
 *  Marks be specified as either a '+', 'x', or '*'.  If the number argument
 *  is positive that value will be drawn next to the mark as a label string.
 */

#ifdef ANSI_FUNC

int 
cdl_markPointLabel (
    CDLPtr cdl,				/* package ptr		*/
    int x,
    int y,				/* marker position	*/
    char *label,			/* point label str	*/
    int size,				/* marker size (pixels) */
    int type,				/* type to draw		*/
    int color				/* marker color		*/
)
#else

int
cdl_markPointLabel (cdl, x, y, label, size, type, color)
CDLPtr	cdl;				/* package ptr		*/
int	x, y;				/* marker position	*/
char 	*label;				/* point label str	*/
int	size;				/* marker size (pixels) */
int	type;				/* type to draw		*/
int	color;				/* marker color		*/
#endif
{
	register int lx, ly, sz;
	uchar	*pix = NULL;
	MarkerPtr mk = cdl_initPointLabelMarker(x,y, label, size, type,
	    color, cdl->font);

	if (cdl_debug)
	    printf("[cdl_pointLabel] (%dx%d) lab=%s size=%d type=%d color=%d\n",
		x, y, label, size, type, color);

	sz = (size % 2 ? size : size + 1);
	sz = max (sz, 3);
	lx = x - (sz / 2);
	ly = y - (sz / 2);
	cdl_initMarkPos (mk, sz, sz, lx, ly);

	/* Get the pixel region we'll be marking. */
	pix = (uchar *) malloc (sz * sz);
	if (cdl_getPixRegion (cdl, lx, ly, sz, sz, pix))
	    return (ERR);

	/* Save the image pixels in the display list for erasure later. */
	mk->refpix = (uchar *) malloc (sz * sz);
	mk->markpix = (uchar *) malloc (sz * sz);
	bcopy (pix, mk->refpix, sz * sz);
	cdl_insertMarker (cdl, DLTail[cdl->frame-1], mk);

	/* Edit the pixels with the requested color and save the raster. */
	cdl_doPointMark (color, type, pix, sz);
	bcopy (pix, mk->markpix, sz * sz);

	/* Number the point if requested. */
	if (label) {
	    int	   cx, cy, llen;

	    llen = cdl_strlen (label, mk->txsize, mk->font);

	    /* Position the label so it isn't clipped. */
	    cx = x + (sz / 2) - 1;
	    cy = y + (sz / 2) - 1;
	    if ((cx + llen) > cdl->im_nx)   cx = x - llen + 1;
	    if ((cy + llen) > cdl->im_ny)   cy = y - llen + 1;
	    cdl_markText (cdl, cx, cy, label, 1., 0.0, color);
	}

	/* Write the region back to the display. */
	if (cdl_writeSubRaster (cdl, lx, ly, sz, sz, pix)) {
	    free ((char *)pix);
	    return (ERR);
	}

	free ((char *)pix);
	return (OK);
}


/*  CDL_MARKLIST --  Given a filename of (x,y) and possibly label strings
 *  mark all points on the display with a point marker of the given type,
 *  size and color.
 */

#ifdef ANSI_FUNC

int
cdl_markCoordsFile (
    CDLPtr 	cdl, 				/* package ptr		*/
    char    	*fname,                         /* file name		*/
    int		type, 				/* marker type		*/
    int		size, 				/* marker size		*/
    int		color, 				/* marker color		*/
    int		label 				/* label?		*/
)
#else

int
cdl_markCoordsFile (cdl, fname, type, size, color, label)
CDLPtr 	cdl;					/* package ptr		*/
char    *fname;                                 /* file name		*/
int	type;					/* marker type		*/
int	size;					/* marker size		*/
int	color;					/* marker color		*/
int	label;					/* label?		*/
#endif
{
        if (fname == (char *)NULL) 
	    return (ERR);

        /* If a coordinate file was specified read the file and mark those
         * coords with points of the specified size, type and color.
         */
        if (access (fname, F_OK) == 0) {
            FILE    *fp;
            float   rx, ry;
            int     i, x, y;

            if ((fp = fopen (fname, "r"))) {
                /* The coord file is assumed to be simply a file containing
                 * (x,y) pairs, one per line.  Scan the file and mark each
                 * point.  We do no bounds checking to see if the coords
                 * are correct for the frame.
                 */
                i = 1;
                while (fscanf (fp, "%g %g", &rx, &ry) != EOF) {
		    x = (int) (rx + 0.5);
		    y = (int) (ry + 0.5);
                    if (label)
                        cdl_markPoint (cdl, x, y, i++, size, type, color);
                    else
                        cdl_markPoint (cdl, x, y, 0, size, type, color);
                }

            } else {
                fprintf (stderr, "cannot open coord file '%s'.\n", fname);
	        return (ERR);
	    }
        } else {
            fprintf (stderr, "'%s': coord file doesn't exist, ignoring.\n", 
                fname);
	    return (ERR);
	}
	return (OK);
}


/*  CDL_MARKLINE --  Draw a line of given color between the two points
 *  specified.
 */

#ifdef ANSI_FUNC

int 
cdl_markLine (
    CDLPtr cdl,				/* package ptr		*/
    int xs,
    int ys,				/* line start points	*/
    int xe,
    int ye,				/* line end points	*/
    int color				/* marker color		*/
)
#else

int
cdl_markLine (cdl, xs, ys, xe, ye, color)
CDLPtr	cdl;				/* package ptr		*/
int	xs, ys;				/* line start points	*/
int	xe, ye;				/* line end points	*/
int	color;				/* marker color		*/
#endif
{
	register int nx, ny, lx, ly;
	register int width = cdl->linewidth, style = cdl->linestyle;
	uchar	*pix = NULL;
	MarkerPtr mk = cdl_initLineMarker (xs, ys, xe, ye, color, width, style);

	if (cdl_debug)
	    printf ("[cdl_markLine] (%d,%d) -> (%d,%d) color=%d\n",
		xs, ys, xe, ye, color);

	if (style >= L_HOLLOW) width = HOLLOW_LINE_WIDTH;
	nx = ABS(xe-xs) + width + 1;   
	ny = ABS(ye-ys) + width + 1;   
	lx = min(xs,xe) - width/2 - 1;
	ly = min(ys,ye) - width/2 - 1;
	cdl_initMarkPos (mk, nx, ny, lx, ly);

	/* Get the pixel region we'll be marking. */
	pix = (uchar *) malloc (nx * ny);
	if (cdl_getPixRegion (cdl, lx, ly, nx, ny, pix))
	    return (ERR);

	/* Save the image pixels in the display list for erasure later. */
	mk->refpix = (uchar *) malloc (nx * ny);
	mk->markpix = (uchar *) malloc (nx * ny);
	bcopy (pix, mk->refpix, nx * ny);
	cdl_insertMarker (cdl, DLTail[cdl->frame-1], mk);

	/* Edit the pixels with the requested color and save the raster. */
	cdl_doLineMark (color, width, style, pix, xs, ys, xe, ye);
	bcopy (pix, mk->markpix, nx * ny);

	/* Write the region back to the display. */
	if (cdl_writeSubRaster (cdl, lx, ly, nx, ny, pix)) {
	    free ((char *)pix);
	    return (ERR);
	}

	free ((char *)pix);
	return (OK);
}


/*  CDL_MARKBOX --  Draw a rectangular box given two corner endpoints.  The
 *  box may be optionally filled with the specified color.
 */

#ifdef ANSI_FUNC

int 
cdl_markBox (
    CDLPtr cdl,				/* package ptr		*/
    int lx,
    int ly,				/* LL corner points	*/
    int ux,
    int uy,				/* UR corner points	*/
    int fill,				/* fill rectangle?	*/
    int color				/* marker color		*/
)
#else

int
cdl_markBox (cdl, lx, ly, ux, uy, fill, color)
CDLPtr	cdl;				/* package ptr		*/
int	lx, ly;				/* LL corner points	*/
int	ux, uy;				/* UR corner points	*/
int	fill;				/* fill rectangle?	*/
int	color;				/* marker color		*/
#endif
{
	register int tmp, nx, ny;
	register int width = cdl->linewidth, style = cdl->linestyle;
	uchar	*pix = NULL;
	MarkerPtr mk = cdl_initBoxMarker (lx, ly, ux, uy, fill, color, 
	    width, style);

	if (cdl_debug)
	    printf ("[cdl_markBox] (%d,%d) -> (%d,%d) fill=%d color=%d\n",
		lx, ly, ux, uy, fill, color);

	/* Take care of a corner specified the wrong way. */
	if (lx > ux) { tmp = ux;   ux = lx;  lx = tmp; }
	if (ly > uy) { tmp = uy;   uy = ly;  ly = tmp; }
	nx = ux - lx + width + 1;
	ny = uy - ly + width + 1;
	lx = lx - width/2 - 1;
	ly = ly - width/2 - 1;
	cdl_initMarkPos (mk, nx, ny, lx, ly);

	/* Get the pixel region we'll be marking. */
	pix = (uchar *) malloc (nx * ny);
	if (cdl_getPixRegion (cdl, lx, ly, nx, ny, pix))
	    return (ERR);

	/* Save the image pixels in the display list for erasure later. */
	mk->refpix = (uchar *) malloc (nx * ny);
	mk->markpix = (uchar *) malloc (nx * ny);
	bcopy (pix, mk->refpix, nx * ny);
	cdl_insertMarker (cdl, DLTail[cdl->frame-1], mk);

	/* Edit the pixels with the requested color and save the raster. */
	cdl_doBoxMark (color, width, style, fill, pix, lx, ly, nx, ny);
	bcopy (pix, mk->markpix, nx * ny);

	/* Write the region back to the display. */
	if (cdl_writeSubRaster (cdl, lx, ly, nx, ny, pix)) {
	    free ((char *)pix);
	    return (ERR);
	}

	free ((char *)pix);
	return (OK);
}


/*  CDL_MARKPOLYGON -- Draw a polygon on the display, optionally filling with
 *  the specified color.
 */

#ifdef ANSI_FUNC

int 
cdl_markPolygon (
    CDLPtr cdl,				/* package ptr		*/
    int xarray[],
    int yarray[],			/* vertex points	*/
    int npts,				/* number of corners	*/
    int fill,				/* fill polygon?	*/
    int color				/* marker color		*/
)
#else

int
cdl_markPolygon (cdl, xarray, yarray, npts, fill, color)
CDLPtr	cdl;				/* package ptr		*/
int	xarray[], yarray[];		/* vertex points	*/
int	npts;				/* number of corners	*/
int	fill;				/* fill polygon?	*/
int	color;				/* marker color		*/
#endif
{
	register int nx, ny, lx, ly;
	register int width = cdl->linewidth, style = cdl->linestyle;
	int 	xmin, xmax, ymin, ymax;
	uchar 	*pix;
	MarkerPtr mk = cdl_initPolygonMarker (xarray, yarray, npts,
	    fill, color, width, style);

	/* Find the boundaries of the polygon. */
	cdl_minmax (xarray, npts, &xmin, &xmax);
	cdl_minmax (yarray, npts, &ymin, &ymax);
        nx = ABS(xmax-xmin) + width + 1;
        ny = ABS(ymax-ymin) + width + 1;
        lx = min(xmin,xmax) - width/2 - 1;
        ly = min(ymin,ymax) - width/2 - 1;
	cdl_initMarkPos (mk, nx, ny, lx, ly);

	if (cdl_debug)
	    printf ("[cdl_markPolygon] npts=%d fill=%d color=%d nx=%d ny=%d\n",
		npts, fill, color, nx, ny);

	/* Get the pixel region we'll be marking. */
	pix = (uchar *) malloc (nx * ny);
	if (cdl_getPixRegion (cdl, lx, ly, nx, ny, pix))
	    return (ERR);

	/* Save the image pixels in the display list for erasure later. */
	mk->refpix = (uchar *) malloc (nx * ny);
	mk->markpix = (uchar *) malloc (nx * ny);
	bcopy (pix, mk->refpix, nx * ny);
	cdl_insertMarker (cdl, DLTail[cdl->frame-1], mk);

	/* Edit the pixels with the requested color and save the raster. */
	cdl_doPolygonMark (color, width, style, fill, xarray, yarray, npts,
	    pix, lx, ly, nx, ny, True);
	bcopy (pix, mk->markpix, nx * ny);

	/* Write the region back to the display. */
	if (cdl_writeSubRaster (cdl, lx, ly, nx, ny, pix)) {
	    free ((char *)pix);
	    return (ERR);
	}

	free ((char *)pix);
	return (OK);
}


/*  CDL_MARKPOLYLINE -- Draw a polyline on the display, optionally filling with
 *  the specified color.
 */

#ifdef ANSI_FUNC

int 
cdl_markPolyline (
    CDLPtr cdl,				/* package ptr		*/
    int *xarray,
    int *yarray,			/* vertex points	*/
    int npts,				/* number of points	*/
    int color				/* marker color		*/
)
#else

int
cdl_markPolyline (cdl, xarray, yarray, npts, color)
CDLPtr	cdl;				/* package ptr		*/
int	*xarray, *yarray;		/* vertex points	*/
int	npts;				/* number of points	*/
int	color;				/* marker color		*/
#endif
{
	register int nx, ny, lx, ly;
	register int width = cdl->linewidth, style = cdl->linestyle;
	int 	xmin, xmax, ymin, ymax;
	uchar 	*pix;
	MarkerPtr mk = cdl_initPolylineMarker (xarray, yarray, npts, color,
	    width, style);

	/* Find the boundaries of the polyline. */
	cdl_minmax (xarray, npts, &xmin, &xmax);
	cdl_minmax (yarray, npts, &ymin, &ymax);
        nx = ABS(xmax-xmin) + width + 1;
        ny = ABS(ymax-ymin) + width + 1;
        lx = min(xmin,xmax) - width/2 - 1;
        ly = min(ymin,ymax) - width/2 - 1;
	cdl_initMarkPos (mk, nx, ny, lx, ly);

	if (cdl_debug)
	    printf ("[cdl_markPolyline] npts=%d color=%d nx=%d ny=%d\n",
		npts, color, nx, ny);

	/* Get the pixel region we'll be marking. */
	pix = (uchar *) malloc (nx * ny);
	if (cdl_getPixRegion (cdl, lx, ly, nx, ny, pix))
	    return (ERR);

	/* Save the image pixels in the display list for erasure later. */
	mk->refpix = (uchar *) malloc (nx * ny);
	mk->markpix = (uchar *) malloc (nx * ny);
	bcopy (pix, mk->refpix, nx * ny);
	cdl_insertMarker (cdl, DLTail[cdl->frame-1], mk);

	/* Edit the pixels with the requested color and save the raster.  */
	cdl_doPolygonMark (color, width, style, False, xarray, yarray, npts,
	    pix, lx, ly, nx, ny, False);
	bcopy (pix, mk->markpix, nx * ny);

	/* Write the region back to the display. */
	if (cdl_writeSubRaster (cdl, lx, ly, nx, ny, pix)) {
	    free ((char *)pix);
	    return (ERR);
	}

	free ((char *)pix);
	return (OK);
}


/*  CDL_MARKCIRCLE -- Draw of circle on the display, optionally filling with
 *  the specified color.
 */

#ifdef ANSI_FUNC

int 
cdl_markCircle (
    CDLPtr cdl,				/* package ptr		*/
    int x,
    int y,				/* center position	*/
    int radius,				/* radius of circle	*/
    int fill,				/* fill circle?		*/
    int color				/* marker color		*/
)
#else

int
cdl_markCircle (cdl, x, y, radius, fill, color)
CDLPtr	cdl;				/* package ptr		*/
int	x, y;				/* center position	*/
int	radius;				/* radius of circle	*/
int	fill;				/* fill circle?		*/
int	color;				/* marker color		*/
#endif
{
	register int nx, ny, lx, ly;
	register int width = cdl->linewidth, style = cdl->linestyle;
	uchar 	*pix;
	MarkerPtr mk = cdl_initCircleMarker (x, y, radius, fill, color,
	    width, style);

	nx = ny = 2 * radius + width + 3;
	lx = x - radius - width/2 - 1;
	ly = y - radius - width/2 - 1;
	cdl_initMarkPos (mk, nx, ny, lx, ly);

	if (cdl_debug)
	    printf ("[cdl_markCircle] (%d,%d) radius=%d fill=%d color=%d\n",
		x, y, radius, fill, color);

	/* Get the pixel region we'll be marking. */
	pix = (uchar *) malloc (nx * ny);
	if (cdl_getPixRegion (cdl, lx, ly, nx, ny, pix))
	    return (ERR);

	/* Save the image pixels in the display list for erasure later. */
	mk->refpix = (uchar *) malloc (nx * ny);
	mk->markpix = (uchar *) malloc (nx * ny);
	bcopy (pix, mk->refpix, nx * ny);
	cdl_insertMarker (cdl, DLTail[cdl->frame-1], mk);

	/* Edit the pixels with the requested color and save the raster. */
	cdl_doCircleMark (x, y, radius, color, width, style, fill, pix, 
	    lx, ly, nx, ny);
	bcopy (pix, mk->markpix, nx * ny);

	/* Write the region back to the display. */
	if (cdl_writeSubRaster (cdl, lx, ly, nx, ny, pix)) {
	    free ((char *)pix);
	    return (ERR);
	}

	free ((char *)pix);
	return (OK);
}


/*  CDL_MARKCIRCANNULI -- Draw circular annuli on the display.
 */

#ifdef ANSI_FUNC

int 
cdl_markCircAnnuli (
    CDLPtr cdl,				/* package ptr		*/
    int x,
    int y,				/* center position	*/
    int radius,				/* radius of 1st annulus*/
    int nannuli,			/* no. of annuli	*/
    int sep,				/* annuli sep (pixels)	*/
    int color				/* marker color		*/
)
#else

int
cdl_markCircAnnuli (cdl, x, y, radius, nannuli, sep, color)
CDLPtr	cdl;				/* package ptr		*/
int	x, y;				/* center position	*/
int	radius;				/* radius of 1st annulus*/
int	nannuli;			/* no. of annuli	*/
int	sep;				/* annuli sep (pixels)	*/
int	color;				/* marker color		*/
#endif
{
	register int nx, ny, lx, ly;
	register int width = cdl->linewidth, style = cdl->linestyle;
	uchar 	*pix;
	MarkerPtr mk = cdl_initCircAnnMarker(x, y, radius, nannuli, sep,
	    color, width, style);

	nx = ny = 2 * (radius + nannuli * sep) + width + 3;
	lx = x - (nx / 2) - width/2;
	ly = y - (ny / 2) - width/2;
	cdl_initMarkPos (mk, nx, ny, lx, ly);

	if (cdl_debug)
	    printf("[cdl_markAnnuli] (%d,%d) radius=%d N=%d sep=%d color=%d\n",
		x, y, radius, nannuli, sep, color);

	/* Get the pixel region we'll be marking. */
	pix = (uchar *) malloc (nx * ny);
	if (cdl_getPixRegion (cdl, lx, ly, nx, ny, pix))
	    return (ERR);

	/* Save the image pixels in the display list for erasure later. */
	mk->refpix = (uchar *) malloc (nx * ny);
	mk->markpix = (uchar *) malloc (nx * ny);
	bcopy (pix, mk->refpix, nx * ny);
	cdl_insertMarker (cdl, DLTail[cdl->frame-1], mk);

	/* Edit the pixels with the requested color and save the raster. */
	cdl_doCircAnnuliMark (x, y, radius, nannuli, sep, color, width, style,
	    pix, lx, ly, nx, ny);
	bcopy (pix, mk->markpix, nx * ny);

	/* Write the region back to the display. */
	if (cdl_writeSubRaster (cdl, lx, ly, nx, ny, pix)) {
	    free ((char *)pix);
	    return (ERR);
	}

	free ((char *)pix);
	return (OK);
}


/*  CDL_MARKELLIPSE -- Draw an ellipse with a given center, eccentricity
 *  and position on the display, optionally filling with the specified color.
 */

#ifdef ANSI_FUNC

int 
cdl_markEllipse (
    CDLPtr cdl,				/* package ptr		*/
    int x,
    int y,				/* center position	*/
    int xrad,
    int yrad,				/* x and y radii	*/
    float rotang,			/* position angle (deg) */
    int fill,				/* fill ellipse?	*/
    int color				/* marker color		*/
)
#else

int
cdl_markEllipse (cdl, x, y, xrad, yrad, rotang, fill, color)
CDLPtr	cdl;				/* package ptr		*/
int	x, y;				/* center position	*/
int	xrad, yrad;			/* x and y radii	*/
float	rotang;				/* position angle (deg) */
int	fill;				/* fill ellipse?	*/
int	color;				/* marker color		*/
#endif
{
	register int nx, ny, lx, ly;
	register int width = cdl->linewidth, style = cdl->linestyle;
	uchar 	*pix;
	MarkerPtr mk = cdl_initEllipseMarker (x, y, xrad, yrad, rotang,
	    fill, color, width, style);

	nx = 2 * max(xrad,yrad) + width + 3;
	ny = 2 * max(xrad,yrad) + width + 3;
	lx = x - (nx / 2) - width/2;
	ly = y - (ny / 2) - width/2;
	cdl_initMarkPos (mk, nx, ny, lx, ly);

	if (cdl_debug)
	    printf("[cdl_markEllipse] (%d,%d) R=(%d,%d) pa=%g fill=%d col=%d\n",
		x, y, xrad, yrad, rotang, fill, color);

	/* Get the pixel region we'll be marking. */
	pix = (uchar *) malloc (nx * ny);
	if (cdl_getPixRegion (cdl, lx, ly, nx, ny, pix))
	    return (ERR);

	/* Save the image pixels in the display list for erasure later. */
	mk->refpix = (uchar *) malloc (nx * ny);
	mk->markpix = (uchar *) malloc (nx * ny);
	bcopy (pix, mk->refpix, nx * ny);
	cdl_insertMarker (cdl, DLTail[cdl->frame-1], mk);

	/* Edit the pixels with the requested color and save the raster. */
	cdl_doEllipseMark (color, width, style, fill, x, y, xrad, yrad, rotang,
	    pix, lx, ly, nx, ny);
	bcopy (pix, mk->markpix, nx * ny);

	/* Write the region back to the display. */
	if (cdl_writeSubRaster (cdl, lx, ly, nx, ny, pix)) {
	    free ((char *)pix);
	    return (ERR);
	}

	free ((char *)pix);
	return (OK);
}


/*  CDL_MARKELLIPANNULI -- Draw elliptical annuli on the display.
 */

#ifdef ANSI_FUNC

int 
cdl_markEllipAnnuli (
    CDLPtr cdl,				/* package ptr		*/
    int x,
    int y,				/* center position	*/
    int xrad,
    int yrad,				/* radius of 1st annulus*/
    float ang,				/* rotation angle	*/
    int nannuli,			/* no. of annuli	*/
    int sep,				/* annuli sep (pixels)	*/
    int color				/* marker color		*/
)
#else

int
cdl_markEllipAnnuli (cdl, x, y, xrad, yrad, ang, nannuli, sep, color)
CDLPtr	cdl;				/* package ptr		*/
int	x, y;				/* center position	*/
int	xrad, yrad;			/* radius of 1st annulus*/
float	ang;				/* rotation angle	*/
int	nannuli;			/* no. of annuli	*/
int	sep;				/* annuli sep (pixels)	*/
int	color;				/* marker color		*/
#endif
{
	register int nx, ny, lx, ly;
	register int width = cdl->linewidth, style = cdl->linestyle;
	uchar 	*pix;
	MarkerPtr mk = cdl_initEllAnnMarker (x, y, xrad, yrad, ang, nannuli,
	    sep, color, width, style);

	nx = 2 * max(xrad + nannuli * sep, yrad + nannuli * sep) + width + 3;
	ny = 2 * max(xrad + nannuli * sep, yrad + nannuli * sep) + width + 3;
	lx = x - (nx / 2) - width/2;
	ly = y - (ny / 2) - width/2;
	cdl_initMarkPos (mk, nx, ny, lx, ly);

	if (cdl_debug)
	    printf("[cdl_markElAnn] (%d,%d) R=(%d,%d) pa=%g N=%d S=%d col=%d\n",
		x, y, xrad, yrad, ang, nannuli, sep, color);

	/* Get the pixel region we'll be marking. */
	pix = (uchar *) malloc (nx * ny);
	if (cdl_getPixRegion (cdl, lx, ly, nx, ny, pix))
	    return (ERR);

	/* Save the image pixels in the display list for erasure later. */
	mk->refpix = (uchar *) malloc (nx * ny);
	mk->markpix = (uchar *) malloc (nx * ny);
	bcopy (pix, mk->refpix, nx * ny);
	cdl_insertMarker (cdl, DLTail[cdl->frame-1], mk);

	/* Edit the pixels with the requested color and save the raster. */
	cdl_doEllipAnnuliMark (x, y, xrad, yrad, ang, nannuli,
	    sep, color, width, style, pix, lx, ly, nx, ny);
	bcopy (pix, mk->markpix, nx * ny);

	/* Write the region back to the display. */
	if (cdl_writeSubRaster (cdl, lx, ly, nx, ny, pix)) {
	    free ((char *)pix);
	    return (ERR);
	}

	free ((char *)pix);
	return (OK);
}



/*  CDL_MARKTEXT -- Draw a text string on the display at the given location.
 */

#ifdef ANSI_FUNC

int 
cdl_markText (
    CDLPtr cdl,                         /* package ptr          */
    int x,
    int y,                           	/* marker position      */
    char *str,                          /* text string          */
    float size,                         /* size scale factor    */
    float angle,			/* text rotation angle	*/
    int color                           /* marker color         */
)
#else

int
cdl_markText (cdl, x, y, str, size, angle, color)
CDLPtr  cdl;                            /* package ptr          */
int     x, y;                           /* marker position      */
char    *str;                           /* text string          */
float   size;                           /* size scale factor    */
float	angle;				/* text rotation angle	*/
int     color;                          /* marker color         */
#endif
{
        register int lx, ly, nx, ny, w = cdl->textwidth;
        register int len, font = cdl->font;
        uchar    *pix = NULL;
	float 	 cw, ch;
        MarkerPtr mk = cdl_initTextMarker (x, y, str, size, color, font, w);
        extern  int     cdl_getPixRegion();
        extern  void    cdl_insertMarker(), cdl_doLineInSubRas();

        if (cdl_debug)
            printf ("[cdl_markText] (%d,%d) str='%s' size=%g color=%d ang=%g\n",
                x, y, str, size, color, angle);

        len = cdl_strlen (str, size, mk->font);
        cw = CHARACTER_WIDTH * (size * FONT_SCALE) + FONT_SPACE;
        ch = CHARACTER_HEIGHT * (size * FONT_SCALE);
	if (angle == 0.0) {
	    /* Optimize a bit for horizontal text. */
            lx = x;
            ly = y - (2 * size);
            nx = cdl_strlen (str, size, mk->font);
            ny = FONT_HEIGHT * (size * FONT_SCALE) + (6 * size);
	    if (strstr(str, "\\fU")) {
		ny += ch - (ch * 0.666) / 2;
	    }
	    if (strstr(str, "\\fD")) {
		ly -= ch - (ch * 0.666) / 2 ;
		ny += ch - (ch * 0.666) / 2;
	    }
	} else {
	    /* Compute the four corners of the rotated and scaled text.  Then
	     * define the raster needed to get all of the text in the box.
	     */
	    int 	xc[4], yc[4];
	    int		i, tx, ty, xmin, xmax, ymin, ymax;
	    double 	coso, sino;

            coso = cos ((double)(angle * (double)(M_PI/180.0)));
            sino = sin ((double)(angle * (double)(M_PI/180.0)));

	    xc[0] = -cw / 2;				/* LL corner */
	    yc[0] = -ch / 2;
	    xc[1] = -cw / 2;				/* UL corner */
	    yc[1] = ch + ch / 2;
	    xc[2] = len + cw / 2;   			/* UR corner */
	    yc[2] = ch + ch / 2;
	    xc[3] = len + cw / 2;   			/* LR corner */
	    yc[3] = -ch / 2;

	    if (strstr(str, "\fU"))
		yc[1] = yc[2] = yc[1] + (ch + ch / 2);
	    if (strstr(str, "\fD"))
		yc[0] = yc[3] = yc[1] - (ch + ch / 2);

	    /* Rotate the bounding box. */
	    for (i=0; i < 4; i++) {
		tx = xc[i], ty = yc[i];
    		xc[i] = tx * coso - ty * sino + x + 0.5;
    		yc[i] = tx * sino + ty * coso + y + 0.5;
	    }

	    cdl_minmax (xc, 4, &xmin, &xmax);
	    cdl_minmax (yc, 4, &ymin, &ymax);

	    lx = xmin - cw / 2,  ly = ymin - ch / 2;
	    nx = xmax - xmin + cw / 2;
	    ny = ymax - ymin + ch / 2;
/*cdl_markBox (cdl, lx, ly, lx+nx, ly+ny, 0, 205);*/
	}
        cdl_initMarkPos (mk, nx, ny, lx, ly);

        /* Get the pixel region we'll be marking. */
        pix = (uchar *) malloc (nx * ny);
        if (cdl_getPixRegion (cdl, lx, ly, nx, ny, pix))
            return (ERR);

        /* Save the image pixels in the display list for erasure later. */
        mk->refpix = (uchar *) malloc (nx * ny);
        mk->markpix = (uchar *) malloc (nx * ny);
        bcopy (pix, mk->refpix, nx * ny);
        cdl_insertMarker (cdl, DLTail[cdl->frame-1], mk);

        /* Edit the pixels with the requested color and save the raster. */
        cdl_doTextMarker (x, y, str, size, angle, color, w, mk->font,
	    pix, lx, ly, nx, ny);
	bcopy (pix, mk->markpix, nx * ny);

        /* Write the region back to the display. */
        if (cdl_writeSubRaster (cdl, lx, ly, nx, ny, pix)) {
	    free ((char *)pix);
            return (ERR);
	}

	free ((char *)pix);
        return (OK);
}


/*  CDL_SETFONT -- Set the default font type.
 */

#ifdef ANSI_FUNC

void
cdl_setFont (
    CDLPtr cdl,
    int    font
)
#else

void
cdl_setFont (cdl, font)
CDLPtr cdl;
int    font;
#endif
{
	if (font == F_ROMAN)       cdl->font = F_ROMAN;
	else if (font == F_GREEK)  cdl->font = F_GREEK;
	else if (font == F_TIMES)  cdl->font = F_TIMES;
	else if (font == F_FUTURA) cdl->font = F_FUTURA;
	else if (font == F_BOLD)   
	    cdl_setTextWidth (cdl, cdl->textwidth+1);
	else
	    fprintf (stderr, "Invalid font specification.\n");
}


/*  CDL_SETTEXTWIDTH -- Set the default text width.
 */

#ifdef ANSI_FUNC

void
cdl_setTextWidth (
    CDLPtr cdl,
    int    width
)
#else

void
cdl_setTextWidth (cdl, width)
CDLPtr cdl;
int    width;
#endif
{
	cdl->textwidth = (width > 0 ? width : 1);
}



/*  CDL_SETLINEWIDTH -- Set the default line width.
 */

#ifdef ANSI_FUNC

void
cdl_setLineWidth (
    CDLPtr cdl,
    int    width
)
#else

void
cdl_setLineWidth (cdl, width)
CDLPtr cdl;
int    width;
#endif
{
	cdl->linewidth = (width > 0 ? width : 1);
}  


/*  CDL_SETLINESTYLE -- Set the default line style.
 */

#ifdef ANSI_FUNC

void
cdl_setLineStyle (
    CDLPtr cdl,
    int    style
)
#else

void
cdl_setLineStyle (cdl, style)
CDLPtr cdl;
int    style;
#endif
{
	cdl->linestyle = (style >= 0 ? style : 0);
}  


/*  CDL_DELETEMARK -- Delete the overlay mark whose center is closest to
 *  the given position.
 */

#ifdef ANSI_FUNC

int 
cdl_deleteMark (
    CDLPtr cdl,				/* package ptr		*/
    int x,
    int y				/* marker position	*/
)
#else

int
cdl_deleteMark (cdl, x, y)
CDLPtr	cdl;				/* package ptr		*/
int	x, y;				/* marker position	*/
#endif
{
	MarkerPtr	mk, next, back;

	if (cdl_debug)
	    printf ("[cdl_deleteMark] (%d,%d)\n", x, y);

	if (DLHead[cdl->frame-1] == (MarkerPtr) NULL)
	    return (OK);

	/* Get the marker closest to the given point. */
	mk = cdl_findNearest (DLHead[cdl->frame-1], x, y);

        /* Write the region back to the display. */
        if (cdl_writeSubRaster(cdl, mk->lx, mk->ly, mk->nx, mk->ny, mk->refpix))
            return (ERR);

        /* For point markers we may need to also delete an associated label. */
        if (mk->type == MK_POINT && mk->number > 0 && mk->next != NULL) {
	    next = mk->next;
            if (cdl_writeSubRaster(cdl, next->lx, next->ly,
                next->nx, next->ny, next->refpix))
                    return (ERR);
            cdl_removeMarker (cdl, next);
        } else if (mk->type == MK_TEXT && mk->back != NULL) {
	    /*  If it's a text marker, see if the previous on was a point
	     *  with a label.
	     */
	    back = mk->back;
	    if (back->type == MK_POINT && back->number > 0) {
                if (cdl_writeSubRaster(cdl, back->lx, back->ly,
                    back->nx, back->ny, back->refpix))
                        return (ERR);
                cdl_removeMarker (cdl, back);
	    }
	}
 
	/* Delete the marker from the display list. */
	cdl_removeMarker (cdl, mk);

	return (OK);
}


/*  CDL_CLEAROVERLAY -- Erase all marks in the current display list.  Work
 *  from the tail of the list so we handle overlapping marks correctly.
 */

#ifdef ANSI_FUNC

int 
cdl_clearOverlay (
    CDLPtr cdl				/* package ptr		*/
)
#else

int
cdl_clearOverlay  (cdl)
CDLPtr	cdl;				/* package ptr		*/
#endif
{
	MarkerPtr	mk, back;

	if (cdl_debug)
	    printf ("[cdl_clearOverlay]\n");

	/* To clear the overlay we simply walk the display list and delete
	 * each point.
	 */
	for (mk = DLTail[cdl->frame-1]; mk != (MarkerPtr) NULL; mk = back) {
            /* Write the region back to the display. */
            if (cdl_writeSubRaster (cdl, mk->lx, mk->ly, mk->nx, mk->ny,
		mk->refpix))
                    return (ERR);

	    /* Delete the marker from the display list.  */
	    back = mk->back;
	    cdl_removeMarker (cdl, mk);
	}
	DLHead[cdl->frame-1] = DLTail[cdl->frame-1] = (MarkerPtr) NULL;

        return (OK);
}


/*  CDL_REDRAWOVERLAY -- Redraw all marks in the current display list.
 */

#ifdef ANSI_FUNC

int 
cdl_redrawOverlay (
    CDLPtr cdl				/* package ptr		*/
)
#else

int
cdl_redrawOverlay  (cdl)
CDLPtr	cdl;				/* package ptr		*/
#endif
{
	MarkerPtr	mk;

	if (cdl_debug)
	    printf ("[cdl_redrawOverlay]\n");

	/*  Just walk the list redisplaying the markers.  */
	for (mk = DLHead[cdl->frame-1]; mk != (MarkerPtr) NULL; mk = mk->next)
            /* Write the region back to the display. */
            if (cdl_writeSubRaster (cdl, mk->lx, mk->ly, mk->nx, mk->ny,
		mk->markpix))
                    return (ERR);

	return (OK);
}


/*  CDL_BEGINDLIST --
 */

#ifdef ANSI_FUNC

void 
cdl_beginDList (
    int	frame 					/* frame number		*/
)
#else

void
cdl_beginDList (frame)
int	frame;					/* frame number		*/
#endif
{
/*
	if (DLFlag[frame])
	    error "Display list already open for frame %d";
*/
}


/*  CDL_ENDDLIST --
 */

#ifdef ANSI_FUNC

void 
cdl_endDList (
    int	frame,					/* frame number		*/
    int	flush 					/* flush on close	*/
)
#else

void
cdl_endDList (frame, flush)
int	frame;					/* frame number		*/
int	flush;					/* flush on close	*/
#endif
{
}


/*  CDL_CLEARDLIST --
 */

#ifdef ANSI_FUNC

void 
cdl_clearDList (
    int	frame 					/* frame number		*/
)
#else

void
cdl_clearDList (frame)
int	frame;					/* frame number		*/
#endif
{
}


/*  CDL_DRAWDLIST --
 */

#ifdef ANSI_FUNC

void 
cdl_drawDList (
    int	frame 					/* frame number		*/
)
#else

void
cdl_drawDList (frame)
int	frame;					/* frame number		*/
#endif
{
}



/* -------------------
 * PRIVATE PROCEDUURES
 * ------------------- */


/*  CDL_GETPIXREGION -- Given a corner position and size, return a subraster
 *  of the image pixels in that region. 
 */

#ifdef ANSI_FUNC

static int 
cdl_getPixRegion (
    CDLPtr cdl,				/* package ptr		*/
    int lx,
    int ly,				/* corner position	*/
    int nx,
    int ny,				/* corner position	*/
    uchar *ras				/* subraster pixels	*/
)
#else

static int
cdl_getPixRegion (cdl, lx, ly, nx, ny, ras)
CDLPtr	cdl;				/* package ptr		*/
int	lx, ly;				/* corner position	*/
int	nx, ny;				/* corner position	*/
uchar	*ras;				/* subraster pixels	*/
#endif
{
	if (ras == NULL)
	    ras = (uchar *) malloc (nx * ny);

	/* Do a server query to get the pixels. */
	return (cdl_readSubRaster (cdl, lx, ly, nx, ny, &ras));
}


/*  CDL_DOPOINTMARK -- Edit a given raster with the requested mark.
 */

#ifdef ANSI_FUNC

static void 
cdl_doPointMark (
    int color,				/* overlay color	*/
    int type,				/* type of mark to draw */
    uchar *pix,				/* edit region raster	*/
    int sz				/* mark size		*/
)
#else

static void
cdl_doPointMark (color, type, pix, sz)
int	color;				/* overlay color	*/
int	type;				/* type of mark to draw */
uchar	*pix;				/* edit region raster	*/
int	sz;				/* mark size		*/
#endif
{
	register int i, j, k, npts, center, cpix, fill = 0;
	int 	x, y, width, xp[4], yp[4];

	/* Save the central pixel value, we don't overwrite that. */
	x = y = (sz / 2);
	center = ((sz / 2) * sz) + (sz / 2);
	cpix = pix[center];

	/* Check flags */
	if (type & M_POINT) {
	    cpix = color;
	    pix[center] = cpix;
	}
	if (type & M_FILL)
	    fill++;

	/* Draw the types requested. */
	if (type & M_BOX)
	     cdl_doBoxMark (color, 1, L_SOLID, fill, pix, 0, 0, sz, sz);

	if (type & M_PLUS || type & M_STAR) {
	    j = (y * sz);
	    for (i=0; i < sz; i++)
	        pix[j+i] = color;
	    j = ((sz - 1) * sz) + x;
	    for (i=0; i < sz; i++, j -= sz)
		pix[j] = color;
	}

	if (type & M_HLINE || type & M_HBLINE) {
	    npts = (sz - (sz / 3)) / 2;
	    if (type & M_HBLINE) {
	        j = ((y - 1) * sz);
		width = 3;
	    } else {
	        j = (y * sz);
		width = 1;
	    }
	    for (k=0; k < width; k++, j += sz) {
	        for (i=0; i < npts; i++) {
		    pix[j+i] = color;
		    pix[j+sz-i-1] = color;
	        }
	    }
	}

	if (type & M_VLINE || type & M_VBLINE) {
	    npts = (sz - (sz / 3)) / 2;
	    if (type & M_VBLINE) {
	        for (k=-1; k < 2; k++) {
	            j = ((sz - 1) * sz) + x;
	            for (i=0; i < npts; i++, j -= sz) {
		        pix[j+k] = color;
		        pix[(i*sz)+x+k] = color;
	            }
	        }
	    } else {
	        j = ((sz - 1) * sz) + x;
	        for (i=0; i < npts; i++, j -= sz) {
		    pix[j] = color;
		    pix[(i*sz)+x] = color;
	        }
	    }
	}

	if (type & M_CROSS || type & M_STAR) {
	    for (i=0, j=((sz-1)*sz); i < sz; i++, j-=sz) {
		pix[(i*sz)+i] = color; 			/* main diagonal */
		pix[j+i] = color;			/* off diagonal	 */
	    }
	}

	if (type & M_DIAMOND) {
            xp[0] = x - (sz/2);     yp[0] = y;
            xp[1] = x;              yp[1] = y + (sz/2);
            xp[2] = x + (sz/2);     yp[2] = y;
            xp[3] = x;              yp[3] = y - (sz/2);
            cdl_doPolygonMark (color, 1, L_SOLID, fill, xp, yp, 4, pix,
		0, 0, sz, sz, True);
	    pix[x] = color;
	}

	if (type & M_CIRCLE)
	    cdl_doCircleMark(x, y, (sz/2)+1, color, 1, L_SOLID, fill, pix,
		0, 0, sz, sz);

	/* Restore the central pixel value, we don't usually overwrite that.
	 */
	if (!fill && !(type & M_POINT))
	    pix[center] = cpix;
}


/*  CDL_DOLINEMARK -- Edit a given raster with the requested mark.
 */

#ifdef ANSI_FUNC

static void 
cdl_doLineMark (
    int color,				/* overlay color	*/
    int width,				/* line width		*/
    int style,				/* line style		*/
    uchar *pix,				/* edit region raster	*/
    int xs,
    int ys,
    int xe,
    int ye				/* line endpoints	*/
)
#else

static void
cdl_doLineMark (color, width, style, pix, xs, ys, xe, ye)
int	color;				/* overlay color	*/
int 	width;				/* line width		*/
int 	style;				/* line style		*/
uchar	*pix;				/* edit region raster	*/
int	xs, ys, xe, ye;			/* line endpoints	*/
#endif
{
	register int 	nx, ny, lx, ly;

	nx = ABS(xe-xs) + width + 1;
	ny = ABS(ye-ys) + width + 1;
	lx = min(xs,xe) - width/2 - 1;
	ly = min(ys,ye) - width/2 - 1;

	/*  Handle a line between arbitrary endpoints of any style. */
        if (style == L_SOLID || style >= L_HOLLOW) {
	    cdl_doLineInSubRas (pix, color, width, style, xs, xe, ys, ye,
	        lx, ly, nx ,ny);
        } else {
	    int	x[2], y[2];

	    switch (style) {
	    case L_DASHED:     CLEAR_DASH_COUNT;    break;
	    case L_DOTTED:     CLEAR_DOT_COUNT;     break;
	    case L_DOTDASH:    CLEAR_DOTDASH_COUNT; break;
	    }

	    x[0] = xs, x[1] = xe;
	    y[0] = ys, y[1] = ye;
	    cdl_doDashedLine (pix, color, width, style, x, y, 2,
		lx, ly, nx ,ny);
	}
}


/*  CDL_DOBOXMARK -- Edit a given raster with the requested mark.
 */

#ifdef ANSI_FUNC

static void 
cdl_doBoxMark (
    int color,				/* overlay color	*/
    int width,				/* line width		*/
    int style,				/* line style		*/
    int fill,				/* fill the box?	*/
    uchar *pix,				/* edit region raster	*/
    int lx,
    int ly,				/* subraster corner	*/
    int nx,
    int ny				/* box size		*/
)
#else

static void
cdl_doBoxMark (color, width, style, fill, pix, lx, ly, nx, ny)
int	color;				/* overlay color	*/
int 	width;				/* line width		*/
int 	style;				/* line style		*/
int	fill;				/* fill the box?	*/
uchar	*pix;				/* edit region raster	*/
int	lx, ly;				/* box corner		*/
int	nx, ny;				/* box size		*/
#endif
{
	register int i=0, j=0, k=0, np = (nx * ny);

	/* Edit the pixels with the requested color. */
	if (fill) {
	    /* Fill the entire rectangle with the color. */
	    for (i=0; i < np ; i++)
		pix[i] = color;
	} else {
	    if (style == L_SOLID) {
	        /* Change just the borders of the box to requested color. */
	        for (k=0; k < width; k++)
	            for (i=0, j = (ny-k-1) * nx; i < nx; i++, j++)
		        pix[(k*nx)+i] = pix[j] = color;
	        for (k=0; k < width; k++)
	            for (i=0+k, j = (nx-k-1); i < np; i+=nx, j+=nx)
	                pix[i] = pix[j] = color;

	    } else if (style == L_HOLLOW) {
	        /* Change just the borders of the box to requested style. */
	        for (k=0; k < HOLLOW_LINE_WIDTH; k++)
	            for (i=0, j = (ny-k-1) * nx; i < nx; i++, j++)
		        pix[(k*nx)+i] = pix[j] = (k == 2 ? 0 : color);
	        for (k=0; k < HOLLOW_LINE_WIDTH; k++)
	            for (i=0+k, j = (nx-k-1); i < np; i+=nx, j+=nx)
	                pix[i] = pix[j] = color;

	        /* Redraw the center of the box to create the hollow.  */
	        for (i=3, j = (ny-3) * nx + 2; i < nx - 2; i++, j++)
		    pix[2*nx+i] = pix[j] = 0;
	        for (i=(2*nx) + 2, j = (nx-3); i < np - (2 * nx); i+=nx, j+=nx)
	            pix[i] = pix[2*nx+j] = 0;

	    } else if (style == L_SHADOW) {
	        /* Change just the borders of the box to requested style.  */
	        for (k=0; k < SHADOW_LINE_WIDTH; k++)
	            for (i=0, j = (ny-k-1) * nx; i < nx; i++, j++)
		        pix[(k*nx)+i] = pix[j] = (k > 1 ? 0 : color);
	        for (k=0; k < SHADOW_LINE_WIDTH; k++)
	            for (i=(2*nx)+k, j = (3*nx-k-1); i < np-(2*nx); i+=nx,j+=nx)
	                pix[i] = pix[j] = (k > 1 ? 0 : color);

	    } else {
		int	x[2], y[2], w = width, s = style;

		dash = 0,    dot = 0,    dotdash = 0;

/* bottom */ 	x[0] = lx+width,            y[0] = ly+w/2;
		x[1] = lx+nx-width,         y[1] = ly+w/2;
		cdl_doDashedLine (pix, color, w, s, x, y, 2, lx, ly, nx, ny);
		dash = 0,    dot = 0,    dotdash = 0;

/* right  */ 	x[0] = lx+nx-w/2-1,         y[0] = ly+width;
		x[1] = lx+nx-w/2-1,         y[1] = ly+ny-width;
		cdl_doDashedLine (pix, color, w, s, x, y, 2, lx, ly, nx, ny);
		dash = 0,    dot = 0,    dotdash = 0;

/* top    */ 	x[0] = lx+nx-width,    	    y[0] = ly+ny-w/2-1;
		x[1] = lx+width,       	    y[1] = ly+ny-w/2-1;
		cdl_doDashedLine (pix, color, w, s, x, y, 2, lx, ly, nx, ny);
		dash = 0,    dot = 0,    dotdash = 0;

/* left   */ 	x[0] = lx+w/2,      	    y[0] = ly+ny-width;
		x[1] = lx+w/2,      	    y[1] = ly+width;
		cdl_doDashedLine (pix, color, w, s, x, y, 2, lx, ly, nx, ny);
		dash = 0,    dot = 0,    dotdash = 0;
	    }
	}
}


/*  CDL_DOPOLYGONMARK -- Edit a given raster with the requested mark.
 */

#ifdef ANSI_FUNC

static void 
cdl_doPolygonMark (
    int color,				/* overlay color	*/
    int width,				/* line width		*/
    int style,				/* line style		*/
    int fill,				/* fill polygon 	*/
    int *x,
    int *y,				/* coordinate arrays	*/
    int npts,				/* npts to plot		*/
    uchar *pix,				/* edit region raster	*/
    int lx,
    int ly,				/* subraster corner	*/
    int nx,
    int ny,				/* subraster size  	*/
    int close				/* close the polygon 	*/
)
#else

static void
cdl_doPolygonMark (color, width, style, fill, x, y, npts, pix, lx, ly, nx, ny, close)
int	color;				/* overlay color	*/
int 	width;				/* line width		*/
int 	style;				/* line style		*/
int	fill;				/* fill polygon 	*/
int	*x, *y;				/* coordinate arrays	*/
int	npts;				/* npts to plot		*/
uchar	*pix;				/* edit region raster	*/
int	lx, ly;				/* subraster corner	*/
int	nx, ny;				/* subraster size  	*/
int	close;				/* close the polygon 	*/
#endif
{
	register int i, nseg = (npts - 1);

	if (cdl_debug)
	    printf ("[cdl_doPolygonMark] npts=%d lx=%d ly=%d nseg=%d\n",
	 	npts, lx, ly, nseg);

        /* For solid lines just draw the segments */
        if (style == L_SOLID || style >= L_HOLLOW) {

	    /* Connect all the vertices. */
	    if (style == L_HOLLOW) {
	        for (i=0; i < nseg; i++)
	            cdl_doLineInSubRas (pix, color, HOLLOW_LINE_WIDTH, L_SOLID,
		        x[i], x[i+1], y[i], y[i+1], lx, ly, nx, ny);
	        for (i=0; i < nseg; i++)
	            cdl_doLineInSubRas (pix, 0, 1, L_SOLID,
		        x[i], x[i+1], y[i], y[i+1], lx, ly, nx, ny);
	    } else {
	        for (i=0; i < nseg; i++)
	            cdl_doLineInSubRas (pix, color, width, style, x[i], x[i+1],
		        y[i], y[i+1], lx, ly, nx, ny);
	    }

	    /* Now connect the last point to the first to close the polygon.  */
	    if (close) {
	        if (x[0] != x[npts] && y[0] != y[npts])
		    if (style == L_HOLLOW) {
	                cdl_doLineInSubRas (pix, color, HOLLOW_LINE_WIDTH, 
			    L_SOLID, x[0], x[nseg], y[0], y[nseg],
			    lx, ly, nx, ny);
	                cdl_doLineInSubRas (pix, 0, 1, L_SOLID,
		 	    x[0], x[nseg], y[0], y[nseg], lx, ly, nx, ny);
		    } else {
	                cdl_doLineInSubRas (pix, color, width, style,
			    x[0], x[nseg], y[0], y[nseg], lx, ly, nx, ny);
		    }

	        /* Flood fill the polygon if requested. */
	        if (fill)
	            cdl_fillArea (pix, nx, ny, color);
	    }

        } else {
	    switch (style) {
	    case L_DASHED:     CLEAR_DASH_COUNT;    break;
	    case L_DOTTED:     CLEAR_DOT_COUNT;     break;
	    case L_DOTDASH:    CLEAR_DOTDASH_COUNT; break;
	    }

	    if (close) {
		register int i, *xp, *yp;

		xp = (int *) malloc ((npts+1) * sizeof(int));
		yp = (int *) malloc ((npts+1) * sizeof(int));
		for (i=0; i < npts; i++) {
		    xp[i] = x[i];
		    yp[i] = y[i];
		}
		xp[i] = x[0];
		yp[i] = y[0];
                cdl_doDashedLine (pix, color, width, style, xp, yp, npts+1,
                    lx, ly, nx ,ny);

		free ((char *) xp);
		free ((char *) yp);
	    } else
                cdl_doDashedLine (pix, color, width, style, x, y, npts,
                    lx, ly, nx ,ny);
        }
}


/*  CDL_DOCIRCLEMARK -- Edit a given raster with the requested mark.
 */

#ifdef ANSI_FUNC

static void 
cdl_doCircleMark (
    int x,
    int y,				/* center point		*/
    int radius,				/* circle radius	*/
    int color,				/* overlay color	*/
    int width,				/* line width		*/
    int style,				/* line style		*/
    int fill,				/* fill the box?	*/
    uchar *pix,				/* edit region raster	*/
    int lx,
    int ly,				/* subraster corner	*/
    int nx,
    int ny				/* box size		*/
)
#else

static void
cdl_doCircleMark (x, y, radius, color, width, style, fill, pix, lx, ly, nx, ny)
int	x, y;				/* center point		*/
int 	radius;				/* circle radius	*/
int	color;				/* overlay color	*/
int	width;				/* line width		*/
int	style;				/* line style		*/
int	fill;				/* fill the box?	*/
uchar	*pix;				/* edit region raster	*/
int	lx, ly;				/* subraster corner	*/
int	nx, ny;				/* box size		*/
#endif
{
  	int xp[N_CIRCLE_PTS+1], yp[N_CIRCLE_PTS+1];

	/* Get the coordinates for drawing the circle. */
	cdl_getCircleCoords (x, y, radius, xp, yp, N_CIRCLE_PTS);

	/* Connect the dots to draw the circle. */
	cdl_doPolygonMark (color, width, style, fill, xp, yp, N_CIRCLE_PTS,
	    pix, lx, ly, nx, ny, True);
}


/*  CDL_DOCIRCANNULIMARK -- Edit a given raster with the requested mark. 
 */

#ifdef ANSI_FUNC

static void 
cdl_doCircAnnuliMark (
    int x,
    int y,				/* center point		*/
    int radius,				/* circle radius	*/
    int nannuli,			/* No. of annuli	*/
    int sep,				/* annulus separation	*/
    int color,				/* overlay color	*/
    int width,				/* line width		*/
    int style,				/* line style		*/
    uchar *pix,				/* edit region raster	*/
    int lx,
    int ly,				/* subraster corner	*/
    int nx,
    int ny				/* box size		*/
)
#else

static void
cdl_doCircAnnuliMark(x, y, radius, nannuli, sep, color, width, style, pix, lx, ly, nx, ny)
int	x, y;				/* center point		*/
int 	radius;				/* circle radius	*/
int	nannuli;			/* No. of annuli	*/
int	sep;				/* annulus separation	*/
int	color;				/* overlay color	*/
int	width;				/* line width		*/
int	style;				/* line style		*/
uchar	*pix;				/* edit region raster	*/
int	lx, ly;				/* subraster corner	*/
int	nx, ny;				/* box size		*/
#endif
{
  	register int i;

	/* For each annulus radius draw the circle. */
	for (i=0; i <= nannuli; i++)
	    cdl_doCircleMark (x, y, radius + (i* sep), color, width, style,
		False, pix, lx, ly, nx, ny);
}


/*  CDL_DOELLIPSEMARK -- Edit a given raster with the requested mark.
 */

#ifdef ANSI_FUNC

static void 
cdl_doEllipseMark (
    int color,				/* overlay color	*/
    int width,				/* line width		*/
    int style,				/* line style		*/
    int fill,				/* fill the box?	*/
    int xc,
    int yc,				/* ellipse center	*/
    int xrad,
    int yrad,				/* semimajor axes	*/
    float ang,				/* rotation angle	*/
    uchar *pix,				/* edit region raster	*/
    int lx,
    int ly,				/* subraster corner	*/
    int nx,
    int ny				/* box size		*/
)
#else

static void
cdl_doEllipseMark (color, width, style, fill, xc, yc, xrad, yrad, ang, pix,lx,ly,nx,ny)
int	color;				/* overlay color	*/
int	width;				/* line width		*/
int	style;				/* line style		*/
int	fill;				/* fill the box?	*/
int	xc, yc;				/* ellipse center	*/
int	xrad, yrad;			/* semimajor axes	*/
float	ang;				/* rotation angle	*/
uchar	*pix;				/* edit region raster	*/
int	lx, ly;				/* subraster corner	*/
int	nx, ny;				/* box size		*/
#endif
{
  	int xp[N_ELLIPSE_PTS+1], yp[N_ELLIPSE_PTS+1];

	/* Get the coordinates for drawing the ellipse. */
	cdl_getEllipseCoords (xc, yc, xrad, yrad, ang, xp, yp);

	/* Connect the dots to draw the ellipse.  */
	cdl_doPolygonMark (color, width, style, fill, xp, yp, N_ELLIPSE_PTS,
	    pix, lx, ly, nx, ny, True);
}


/*  CDL_DOELLIPANNULIMARK -- Edit a given raster with the requested mark. 
 */

#ifdef ANSI_FUNC

static void 
cdl_doEllipAnnuliMark (
    int x,
    int y,				/* ellipse center	*/
    int xrad,
    int yrad,				/* semimajor axes	*/
    float ang,				/* rotation angle	*/
    int nannuli,			/* No. of annuli	*/
    int sep,				/* annulus separation	*/
    int color,				/* overlay color	*/
    int width,				/* line width		*/
    int style,				/* line style		*/
    uchar *pix,				/* edit region raster	*/
    int lx,
    int ly,				/* subraster corner	*/
    int nx,
    int ny				/* box size		*/
)
#else

static void
cdl_doEllipAnnuliMark(x, y, xrad, yrad, ang, nannuli, sep, color, width,
	style, pix, lx, ly, nx, ny)
int	x, y;				/* ellipse center	*/
int	xrad, yrad;			/* semimajor axes	*/
float	ang;				/* rotation angle	*/
int	nannuli;			/* No. of annuli	*/
int	sep;				/* annulus separation	*/
int	color;				/* overlay color	*/
int	width;				/* line width		*/
int	style;				/* line style		*/
uchar	*pix;				/* edit region raster	*/
int	lx, ly;				/* subraster corner	*/
int	nx, ny;				/* box size		*/
#endif
{
  	register int i, xr, yr;

	/* For each annulus compute the ellipse and draw it... */
	for (i=0; i <= nannuli; i++) {
	    xr = xrad + (i * sep);
	    yr = yrad + (i * sep);
	    cdl_doEllipseMark (color, width, style, False, x, y, xr, yr, ang,
		pix, lx, ly, nx, ny);
	}
}


/*  CDL_DOTEXTMARKER -- Draw a text string to the display.
 */

#ifdef ANSI_FUNC

int 
cdl_doTextMarker (
    int x,
    int y,				/* LL text position	*/
    char *string,			/* string to draw	*/
    float txtsize,			/* relative scale size	*/
    float angle,			/* text rotation angle	*/
    int color,				/* overlay color	*/
    int width,				/* line width		*/
    int txtfont,			/* text font		*/
    uchar *pix,                         /* edit region raster   */
    int lx,
    int ly,                             /* subraster corner     */
    int nx,
    int ny                              /* box size             */
)
#else

int
cdl_doTextMarker (x,y,string,txtsize,angle,color,width,txtfont,pix,lx,ly,nx,ny)
int     x, y;				/* LL text position	*/
char    *string;			/* string to draw	*/
float   txtsize;			/* relative scale size	*/
float	angle;				/* text rotation angle	*/
int     color;				/* overlay color	*/
int     width;				/* line width		*/
int     txtfont;			/* text font		*/
uchar   *pix;                           /* edit region raster   */
int     lx, ly;                         /* subraster corner     */
int     nx, ny;                         /* box size             */
#endif
{
        char    chr, *sp = string;
        int     mx, my, stroke, tab1, tab2, i, j, pen, len;
        int     x1, x2, y1, y2;
	int	oldfont, font, offset = 0;
        register int w, cw, ch, cx, cy, ip;
        float   px, py, size = txtsize;
	double	coso, sino;
        int     bitupk();

	font = oldfont = txtfont;
	coso = cos ((double) (-angle * (double)(M_PI / 180.0)));
	sino = sin ((double) (-angle * (double)(M_PI / 180.0)));

        /* Loop over each character in the string. */
        len = strlen (string);
        x1 = cx = x;
        y1 = cy = y;

        cw = CHARACTER_WIDTH * (size * FONT_SCALE) + FONT_SPACE;
        ch = CHARACTER_HEIGHT * (size * FONT_SCALE);
        for (j = 0; j < len && *sp; j++, sp++) {

escape:
	    /* Check for an inline font escape. */
	    if (*sp == '\\' && *(sp+1) == 'f') {
		sp++; sp++;
		if (*sp != 'P')
		    oldfont = font;

		switch (*sp) {
		case 'B':
		    width = width + 1;
		    break;
		case 'R':
		    font = F_ROMAN;
		    break;
		case 'F':
		    font = F_FUTURA;
		    break;
		case 'T':
		    font = F_TIMES;
		    break;
		case 'G':
		    font = F_GREEK;
		    break;
		case 'P':
		    if (width > 1)
		        width = width - 1;
		    font = oldfont;
		    break;

	        /* Check for a super/subscript escape. */
		case 'U':
                    offset += ch - (ch * 0.666) / 2;
		    break;
		case 'D':
                    offset -= ch - (ch * 0.666) / 2;
		    break;
		}
		sp++;
	        if (*sp == '\\')
		    goto escape;
	    }

	    /* Adjust the size if we're doing sub/superscript. */
	    if (offset != 0)
		size = txtsize * 0.666;
	    else
		size = txtsize;

            if (*sp < CHARACTER_START || *sp > CHARACTER_END)
                chr = i = '?' - CHARACTER_START;
            else
                chr = i = *sp - CHARACTER_START;

	    switch (font) {
	    case F_ROMAN:
                tab1 = chridx[i] - 1;
                tab2 = chridx[i+1] - 1;
		break;
	    case F_GREEK:
                tab1 = gchidx[i] - 1;
                tab2 = gchidx[i+1] - 1;
		break;
	    case F_TIMES:
                tab1 = timidx[i] - 1;
                tab2 = timidx[i+1] - 1;
		break;
	    case F_FUTURA:
                tab1 = futidx[i] - 1;
                tab2 = futidx[i+1] - 1;
		break;
	    }

            for (i=tab1; i <= tab2; i++) {
	        switch (font) {
	        case F_ROMAN:
                    stroke = chrtab[i]; break;
	        case F_GREEK:
                    stroke = gchtab[i]; break;
	        case F_TIMES:
                    stroke = timtab[i]; break;
	        case F_FUTURA:
                    stroke = futtab[i]; break;
		}
                px  = (float) bitupk (stroke, COORD_X_START, COORD_X_LEN);
                py  = (float) bitupk (stroke, COORD_Y_START, COORD_Y_LEN);
                pen = bitupk (stroke, COORD_PEN_START, COORD_PEN_LEN);

                /* Scale size of character.  */
                px = px * (size * FONT_SCALE);
                py = py * (size * FONT_SCALE) + offset;

                /* Shift and rotate.
		 */
                mx = cx + px * coso + py * sino;
                my = cy - px * sino + py * coso;

                if (pen == 0) {
                    /* Move start point. */
                    x1 = mx;
                    y1 = my;
                } else {
                    /* Draw to this point and update. */
                    x2 = mx;
                    y2 = my;
                    cdl_doLineInSubRas (pix, color, width, L_SOLID,
		 	x1, x2, y1, y2, lx, ly, nx, ny);
                    x1 = x2;
                    y1 = y2;
                }
            }

	    /* Update the character position. */
	    ip = (int) chr;
	    switch (font) {
	    case F_ROMAN:
	        cx += cw * coso;
	        cy -= cw * sino;

	        /* Now correct a bit to kludge a nice spacing. */
	        if (index("il1:;.,!|'`", *sp)) {
	            cx -= FONT_SPACE * coso;
	            cy += FONT_SPACE * sino;
	        } else if (*sp == 'r') {
	            cx -= (FONT_SPACE - 2) * coso;
	            cy += (FONT_SPACE - 2) * sino;
	        }
		break;
	    case F_GREEK:
        	w = (gchwid[ip] / 2) * (size * FONT_SCALE) + FONT_SPACE + 2;
	        cx += w * coso;
	        cy -= w * sino;
		break;
	    case F_TIMES:
        	w = (timwid[ip] - 12) * (size * FONT_SCALE) + FONT_SPACE + 2;
	        cx += w * coso;
	        cy -= w * sino;
		break;
	    case F_FUTURA:
        	w = (futwid[ip] / 2) * (size * FONT_SCALE) + FONT_SPACE + 2;
	        cx += w * coso;
	        cy -= w * sino;
		break;
	    }
        }

	return (OK);
}




/*  ------------------------
 *  Display List Procedures
 *  ------------------------*/



/*  CDL_FREEDISPLAYLIST -- Free up the named display list.
 */

#ifdef ANSI_FUNC

int 
cdl_freeDisplayList (
    CDLPtr cdl,				/* package ptr		*/
    MarkerPtr head
)
#else

int
cdl_freeDisplayList  (cdl, head)
CDLPtr	cdl;				/* package ptr		*/
MarkerPtr head;
#endif
{
	MarkerPtr	mk;

	if (cdl_debug)
	    printf ("[cdl_freeDisplayList]\n");

	/*  Just walk the list freeing the markers.  */
	for (mk = head; mk != (MarkerPtr) NULL; mk = mk->next)
            cdl_removeMarker (cdl, mk);

	return (OK);
}


/*  CDL_INSERTMARKER -- Insert a marker in the display list after the given
 *  entry.  The display list is maintained as a doubly linked list of markers.
 */

#ifdef ANSI_FUNC

static void 
cdl_insertMarker (
    CDLPtr cdl,				/* package ptr		*/
    MarkerPtr back,			/* insert point		*/
    MarkerPtr new			/* marker to insert	*/
)
#else

static void
cdl_insertMarker (cdl, back, new)
CDLPtr		cdl;			/* package ptr		*/
MarkerPtr	back;			/* insert point		*/
MarkerPtr	new;			/* marker to insert	*/
#endif
{
	MarkerPtr	tmp;

	/* If the back pointer is null it means we're creating the head. */
	if (back == (MarkerPtr) NULL) {
	    DLHead[cdl->frame-1] = DLTail[cdl->frame-1] = new;
	    new->back = new->next = (MarkerPtr) NULL;

	} else {
	    /* Otherwise reset the pointers to insert the marker. */
	    tmp = back->next;
	    back->next = new;
	    new->back = back;

	    /* If the beck marker pointed at something, take care of that. */
	    if (tmp) {
	        new->next = tmp;
	        tmp->back = new;
	    } else {
	        new->next = (MarkerPtr) NULL;
		DLTail[cdl->frame-1] = new;
	    }
	}
}


/*  CDL_REMOVEMARKER -- Remove a marker from the display list.
 */

#ifdef ANSI_FUNC

static void 
cdl_removeMarker (
    CDLPtr cdl,				/* package ptr		*/
    MarkerPtr mk			/* marker to delete	*/
)
#else

static void
cdl_removeMarker (cdl, mk)
CDLPtr		cdl;			/* package ptr		*/
MarkerPtr	mk;			/* marker to delete	*/
#endif
{
	if (mk->back) 			/* handle the back ptr  */
	    mk->back->next = mk->next;
	else
	    DLHead[cdl->frame-1] = mk->next;
	if (mk->next) 			/* handle the next ptr  */
	    mk->next->back = mk->back;
	else
	    DLTail[cdl->frame-1] = mk->back;

	/* Now free up the marker and internal pointers. */
	if (mk->str)     free ((uchar *) mk->str);
	if (mk->refpix)  free ((uchar *) mk->refpix);
	if (mk->markpix) free ((uchar *) mk->markpix);
	if (mk->xp) 	 free ((uchar *) mk->xp);
	if (mk->yp) 	 free ((uchar *) mk->yp);
	free ((MarkerPtr) mk);
}


/*  CDL_FINDNEAREST -- Find the marker in the display list that is closest
 *  to the indicated point.  For markers with a defined center we check the
 *  distance to the central point.  For polylines and polygons we check for
 *  a vertex ir knot point that is closest.  The marker is then returned.
 */

#ifdef ANSI_FUNC

static MarkerPtr 
cdl_findNearest (
    MarkerPtr head,			/* display list head	*/
    int x,
    int y				/* pixel coords		*/
)
#else

static MarkerPtr
cdl_findNearest (head, x, y)
MarkerPtr	head;			/* display list head	*/
int		x, y;			/* pixel coords		*/
#endif
{
	MarkerPtr	mk, nearest;
	register float  dmin = 10.0e9, mk_dist, dist = 10.0e9, A, C, cx, cy;
	register int	i;

	if (head == (MarkerPtr) NULL)
	    return ((MarkerPtr) NULL);

	nearest = head;
	for (mk = head; mk ; mk = mk->next) {
	    switch (mk->type) {
	    case MK_POLYLINE:
	    case MK_POLYGON:
		/* Find the nearest point in the poly. */
		for (i=0; i < mk->npts; i++) {
		    mk_dist = sqrt ((double)(ABS(x - mk->x) * ABS(x - mk->x) +
			        	     ABS(y - mk->y) * ABS(y - mk->y)) );
	  	    if (mk_dist < dmin)
		        dmin = mk_dist;
		}
		mk_dist = dmin;
		break;

	    case MK_BOX:
		/* Select the box center closest to the point. */
		cx = (mk->xp[1] + mk->xp[0]) / 2;
		cy = (mk->yp[1] + mk->yp[0]) / 2;
		mk_dist = sqrt ((double)(ABS(x - cx) * ABS(x - cx) +
			        	 ABS(y - cy) * ABS(y - cy)) );
		break;

	    case MK_LINE:
		A = (float) ((mk->yp[1] - mk->yp[0]) / (mk->xp[1] - mk->xp[0]));
		C = mk->yp[0] - A * x;
		mk_dist = ABS( (A * x - y + C) / sqrt (A * A + 1));
		break;

	    case MK_POINT:
	    case MK_CIRCLE:
	    case MK_CIRCANN:
	    case MK_ELLIPSE:
	    case MK_ELLIPANN:
	    case MK_TEXT:
		/* Get the distance from the point to the center. */
		mk_dist = sqrt ((double)(ABS(x - mk->x) * ABS(x - mk->x) +
			                 ABS(y - mk->y) * ABS(y - mk->y)) );
	    }
	    if (mk_dist < dist) {
		dist = mk_dist;
	        nearest = mk;
	    }

	    if (cdl_debug) 
		printf ("Nearest: Marker type=%2d  dist=%g  mk_dist=%g  N=%d\n",
		    mk->type, dist, mk_dist, nearest->type);
	}
	return (nearest);
}


/*  CDL_PRINTDISPLAYLIST -- Print the display list to a file.
 */

#ifdef ANSI_FUNC

static void 
cdl_printDisplayList (
    FILE *fd,				/* file descriptor	*/
    MarkerPtr head			/* display list head	*/
)
#else

static void
cdl_printDisplayList (fd, head)
FILE		*fd;			/* file descriptor	*/
MarkerPtr	head;			/* display list head	*/
#endif
{
	MarkerPtr	mk;

	for (mk = head; mk ; mk = mk->next) {
	    switch (mk->type) {
	    case MK_POINT: 	printf ("point\t\t"); 	   break;
	    case MK_LINE: 	printf ("line\t\t"); 	   break;
	    case MK_BOX: 	printf ("box\t\t"); 	   break;
	    case MK_POLYLINE: 	printf ("polyline\t\t");   break;
	    case MK_POLYGON: 	printf ("polygon\t\t");    break;
	    case MK_CIRCLE: 	printf ("circle\t\t"); 	   break;
	    case MK_CIRCANN: 	printf ("circann\t\t");    break;
	    case MK_ELLIPSE: 	printf ("ellipse\t\t");    break;
	    case MK_ELLIPANN: 	printf ("ellipann\t\t");   break;
	    case MK_TEXT:	printf ("text\t\t"); 	   break;
	    }
	    printf ("mk=%d back=%10d  next=%10d\n",
		(int)mk, (int)mk->back, (int)mk->next);
	}	
}


/*  CDL_INITPOINTMARKER -- Allocate and initialize the space for a point marker.
 */

#ifdef ANSI_FUNC

static MarkerPtr 
cdl_initPointMarker (
    int x,
    int y,                           	/* marker position      */
    int number,                         /* if > 0, label value  */
    int size,                           /* marker size (pixels) */
    int type,                           /* type to draw         */
    int color                          	/* marker color         */
)
#else

static MarkerPtr
cdl_initPointMarker (x, y, number, size, type, color)
int     x, y;                           /* marker position      */
int     number;                         /* if > 0, label value  */
int     size;                           /* marker size (pixels) */
int     type;                           /* type to draw         */
int     color;                          /* marker color         */
#endif
{
	MarkerPtr mk = (MarkerPtr) calloc (1, sizeof(struct Marker));

	mk->type    	= MK_POINT;
	mk->x       	= x;
	mk->y       	= y;
	mk->number  	= number;
	mk->size    	= size;
	mk->pt_type 	= type;
	mk->color   	= color;
	return mk;
}



/*  CDL_INITPOINTLABELMARKER -- Allocate and initialize the space for a point
 * with a text label marker.
 */

#ifdef ANSI_FUNC

static MarkerPtr 
cdl_initPointLabelMarker (
    int  x,
    int  y,                           /* marker position      */
    char *label,                      /* point label str      */
    int  size,                        /* marker size (pixels) */
    int  type,                        /* type to draw         */
    int  color,                       /* marker color         */
    int  font                         /* label font	      */
)
#else

static MarkerPtr
cdl_initPointLabelMarker (x, y, label, size, type, color, font)
int     x, y;                         /* marker position      */
char    *label;                       /* point label str      */
int     size;                         /* marker size (pixels) */
int     type;                         /* type to draw         */
int     color;                        /* marker color         */
int     font;                         /* label font	      */
#endif
{
	MarkerPtr mk = (MarkerPtr) calloc (1, sizeof(struct Marker));

	mk->type    	= MK_POINT;
	mk->x       	= x;
	mk->y       	= y;
	mk->size    	= size;
	mk->pt_type 	= type;
	mk->color   	= color;
	mk->font   	= font;
        mk->str         = (char *) malloc (strlen(label) + 1);
        strcpy (mk->str, label);
	return mk;
}


/*  CDL_INITLINEMARKER -- Allocate and initialize the space for a line marker.
 */

#ifdef ANSI_FUNC

static MarkerPtr 
cdl_initLineMarker (
    int x1,
    int y1,                         	/* line start points    */
    int x2,
    int y2,                         	/* line end points      */
    int color,                         	/* marker color         */
    int width,			 	/* line width 		*/
    int style			 	/* line style 		*/
)
#else

static MarkerPtr
cdl_initLineMarker (x1, y1, x2, y2, color, width, style)
int     x1, y1;                         /* line start points    */
int     x2, y2;                         /* line end points      */
int     color;                          /* marker color         */
int     width;			 	/* line width 		*/
int     style;			 	/* line width 		*/
#endif
{
	MarkerPtr mk = (MarkerPtr) calloc (1, sizeof(struct Marker));

	mk->type	= MK_LINE;
	mk->x 		= x1;
	mk->y 		= y1;
	mk->xp 		= (int *) malloc (2 * sizeof(int));
	mk->xp[0] 	= x1;
	mk->xp[1] 	= x2;
	mk->yp 		= (int *) malloc (2 * sizeof(int));
	mk->yp[0] 	= y1;
	mk->yp[1] 	= y2;
	mk->color	= color;
	mk->linewidth	= width;
	mk->linestyle	= style;
	return mk;
}


/*  CDL_INITBOXMARKER -- Allocate and initialize the space for a box marker.
 */

#ifdef ANSI_FUNC

static MarkerPtr 
cdl_initBoxMarker (
    int lx,
    int ly,                         	/* LL corner points     */
    int ux,
    int uy,                         	/* UR corner points     */
    int fill,                           /* fill rectangle?      */
    int color,                         	/* marker color         */
    int width,                         	/* line width		*/
    int style                          	/* line style		*/
)
#else

static MarkerPtr
cdl_initBoxMarker (lx, ly, ux, uy, fill, color, width, style)
int     lx, ly;                         /* LL corner points     */
int     ux, uy;                         /* UR corner points     */
int     fill;                           /* fill rectangle?      */
int     color;                          /* marker color         */
int 	width;                         	/* line width		*/
int 	style;                         	/* line style		*/
#endif
{
	MarkerPtr mk = (MarkerPtr) calloc (1, sizeof(struct Marker));

	mk->type	= MK_BOX;
	mk->x 		= lx;
	mk->y 		= ly;
	mk->xp 		= (int *) malloc (2 * sizeof(int));
	mk->xp[0] 	= lx;
	mk->xp[1] 	= ux;
	mk->yp 		= (int *) malloc (2 * sizeof(int));
	mk->yp[0] 	= ly;
	mk->yp[1] 	= uy;
	mk->fill	= fill;
	mk->color	= color;
	mk->linewidth	= width;
	mk->linestyle	= style;
	return mk;
}


/*  CDL_INITPOLYLINEMARKER -- Allocate and initialize the space for a 
 *  polyine  marker.
 */

#ifdef ANSI_FUNC

static MarkerPtr 
cdl_initPolylineMarker (
    int *x,
    int *y,               		/* vertex points        */
    int npts,                           /* number of points     */
    int color,                         	/* marker color         */
    int width,                         	/* line width		*/
    int style                          	/* line style		*/
)
#else

static MarkerPtr
cdl_initPolylineMarker (x, y, npts, color, width, style)
int     *x, *y;               		/* vertex points        */
int     npts;                           /* number of points     */
int     color;                          /* marker color         */
int 	width;                         	/* line width		*/
int 	style;                         	/* line style		*/
#endif
{
	MarkerPtr mk = (MarkerPtr) calloc (1, sizeof(struct Marker));

	mk->type	= MK_POLYLINE;
	mk->xp 		= (int *) malloc (npts * sizeof(int));
	mk->yp 		= (int *) malloc (npts * sizeof(int));
	bcopy (x, mk->xp, npts * sizeof(int));
	bcopy (y, mk->yp, npts * sizeof(int));
	mk->x 		= mk->xp[0];
	mk->y 		= mk->yp[0];
	mk->npts	= npts;
	mk->color	= color;
	mk->linewidth	= width;
	mk->linestyle	= style;
	return mk;
}


/*  CDL_INITPOLYGONMARKER -- Allocate and initialize the space for a 
 *  polygon marker.
 */

#ifdef ANSI_FUNC

static MarkerPtr 
cdl_initPolygonMarker (
    int *x,
    int *y,             		/* vertex points        */
    int npts,                           /* number of corners    */
    int fill,                           /* fill polygon?        */
    int color,                         	/* marker color         */
    int width,                         	/* line width		*/
    int style                          	/* line style		*/
)
#else

static MarkerPtr
cdl_initPolygonMarker (x, y, npts, fill, color, width, style)
int     *x, *y;             		/* vertex points        */
int     npts;                           /* number of corners    */
int     fill;                           /* fill polygon?        */
int     color;                          /* marker color         */
int 	width;                         	/* line width		*/
int 	style;                         	/* line style		*/
#endif
{
	MarkerPtr mk = (MarkerPtr) calloc (1, sizeof(struct Marker));

	mk->type	= MK_POLYGON;
	mk->xp 		= (int *) malloc (npts * sizeof(int));
	mk->yp 		= (int *) malloc (npts * sizeof(int));
	bcopy (x, mk->xp, npts * sizeof(int));
	bcopy (y, mk->yp, npts * sizeof(int));
	mk->x 		= mk->xp[0];
	mk->y 		= mk->yp[0];
	mk->npts	= npts;
	mk->fill	= fill;
	mk->color	= color;
	mk->linewidth	= width;
	mk->linestyle	= style;
	return mk;
}


/*  CDL_INITCIRCLEMARKER -- Allocate and initialize the space for a
 *  circle marker.
 */

#ifdef ANSI_FUNC

static MarkerPtr 
cdl_initCircleMarker (
    int x,
    int y,                           	/* center position      */
    int radius,                         /* radius of circle     */
    int fill,                           /* fill circle?         */
    int color,                         	/* marker color         */
    int width,                          /* line width		*/
    int style                          	/* line style		*/
)
#else

static MarkerPtr
cdl_initCircleMarker (x, y, radius, fill, color, width, style)
int     x, y;                           /* center position      */
int     radius;                         /* radius of circle     */
int     fill;                           /* fill circle?         */
int     color;                          /* marker color         */
int     width;                          /* line width		*/
int     style;                          /* line style		*/
#endif
{
	MarkerPtr mk = (MarkerPtr) calloc (1, sizeof(struct Marker));

	mk->type	= MK_CIRCLE;
	mk->x       	= x;
	mk->y       	= y;
	mk->radius	= radius;
	mk->fill	= fill;
	mk->color	= color;
	mk->linewidth	= width;
	mk->linestyle	= style;
	return mk;
}


/*  CDL_INITCIRCANNMARKER -- Allocate and initialize the space for a 
 *  circular annuli marker.
 */

#ifdef ANSI_FUNC

static MarkerPtr 
cdl_initCircAnnMarker (
    int x,
    int y,                           	/* center position      */
    int radius,                         /* radius of 1st annulus*/
    int nannuli,                        /* no. of annuli        */
    int sep,                            /* annuli sep (pixels)  */
    int color,                         	/* marker color         */
    int width,                          /* line width		*/
    int style                          	/* line style		*/
)
#else

static MarkerPtr
cdl_initCircAnnMarker (x, y, radius, nannuli, sep, color, width, style)
int     x, y;                           /* center position      */
int     radius;                         /* radius of 1st annulus*/
int     nannuli;                        /* no. of annuli        */
int     sep;                            /* annuli sep (pixels)  */
int     color;                          /* marker color         */
int     width;                          /* line width		*/
int     style;                          /* line style		*/
#endif
{
	MarkerPtr mk = (MarkerPtr) calloc (1, sizeof(struct Marker));

	mk->type	= MK_CIRCANN;
	mk->x       	= x;
	mk->y       	= y;
	mk->radius	= radius;
	mk->nannuli	= nannuli;
	mk->sep		= sep;
	mk->color	= color;
	mk->linewidth	= width;
	mk->linestyle	= style;
	return mk;
}


/*  CDL_INITELLIPSEMARKER -- Allocate and initialize the space for a 
 *  ellipse marker.
 */

#ifdef ANSI_FUNC

static MarkerPtr 
cdl_initEllipseMarker (
    int x,
    int y,                           	/* center position      */
    int xrad,
    int yrad,                     	/* x and y radii        */
    float ang,                         	/* position angle (deg) */
    int fill,                          	/* fill ellipse?        */
    int color,                         	/* marker color         */
    int width,                         	/* line width		*/
    int style                          	/* line style		*/
)
#else

static MarkerPtr
cdl_initEllipseMarker (x, y, xrad, yrad, ang, fill, color, width, style)
int     x, y;                           /* center position      */
int     xrad, yrad;                     /* x and y radii        */
float   ang;                            /* position angle (deg) */
int     fill;                           /* fill ellipse?        */
int     color;                          /* marker color         */
int     width;                          /* line width		*/
int     style;                          /* line style		*/
#endif
{
	MarkerPtr mk = (MarkerPtr) calloc (1, sizeof(struct Marker));

	mk->type	= MK_ELLIPSE;
	mk->x       	= x;
	mk->y       	= y;
	mk->xrad	= xrad;
	mk->yrad	= yrad;
	mk->ang 	= ang;
	mk->fill	= fill;
	mk->color 	= color;
	mk->linewidth 	= width;
	mk->linestyle 	= style;
	return mk;
}


/*  CDL_INITELLANNMARKER -- Allocate and initialize the space for a 
 *  elliptical annuli marker.
 */

#ifdef ANSI_FUNC

static MarkerPtr 
cdl_initEllAnnMarker (
    int x,
    int y,                           	/* center position      */
    int xrad,
    int yrad,                     	/* radius of 1st annulus*/
    float ang,                          /* rotation angle       */
    int nannuli,                        /* no. of annuli        */
    int sep,                            /* annuli sep (pixels)  */
    int color,                         	/* marker color         */
    int width,                         	/* line width		*/
    int style                         	/* line style		*/
)
#else

static MarkerPtr
cdl_initEllAnnMarker (x, y, xrad, yrad, ang, nannuli, sep, color, width, style)
int     x, y;                           /* center position      */
int     xrad, yrad;                     /* radius of 1st annulus*/
float   ang;                            /* rotation angle       */
int     nannuli;                        /* no. of annuli        */
int     sep;                            /* annuli sep (pixels)  */
int     color;                          /* marker color         */
int     width;                          /* line width		*/
int     style;                          /* line style		*/
#endif
{
	MarkerPtr mk = (MarkerPtr) calloc (1, sizeof(struct Marker));

	mk->type	= MK_ELLIPANN;
	mk->x       	= x;
	mk->y       	= y;
	mk->xrad	= xrad;
	mk->yrad	= yrad;
	mk->ang 	= ang;
	mk->nannuli	= nannuli;
	mk->sep		= sep;
	mk->color 	= color;
	mk->linewidth 	= width;
	mk->linestyle 	= style;
	return mk;
}


/*  CDL_INITTEXTMARKER -- Allocate and initialize the space for a text marker.
 */

#ifdef ANSI_FUNC

static MarkerPtr 
cdl_initTextMarker (
    int x,
    int y,                             /* marker position      */
    char *string,                      /* string to draw       */
    float size,                        /* marker size (pixels) */
    int color,                         /* marker color         */
    int font,                          /* text font            */
    int width                          /* line width	       */
)
#else

static MarkerPtr
cdl_initTextMarker (x, y, string, size, color, font, width)
int     x, y;                           /* marker position      */
char    *string;                        /* string to draw       */
float   size;                           /* marker size (pixels) */
int     color;                          /* marker color         */
int     font;                           /* text font            */
int     width;                          /* line width           */
#endif
{
        MarkerPtr mk = (MarkerPtr) calloc (1, sizeof(struct Marker));

        mk->type        = MK_TEXT;
        mk->x           = x;
        mk->y           = y;
        mk->txsize      = size;
        mk->color       = color;
        mk->font        = font;
        mk->textwidth   = width;
        mk->str         = (char *) malloc (strlen(string) + 1);
        strcpy (mk->str, string);
        return mk;
}



#ifdef ANSI_FUNC

static void 
cdl_initMarkPos (MarkerPtr mk, int nx, int ny, int lx, int ly)
#else

static void
cdl_initMarkPos (mk, nx, ny, lx, ly)
MarkerPtr	mk;
int		nx, ny;
int		lx, ly;
#endif
{
	mk->nx = nx;
	mk->ny = ny;
	mk->lx = lx;
	mk->ly = ly;
}



/*  ------------------------
 *  Misc. Utility Procedures
 *  ------------------------*/

/*  CDL_DOLINEINSUBRAS --  Draw a line between two arbitrary endpoints in a 
 *  subraster.  Line widths and styles are passed through to specialized
 #  routines.
 */

#ifdef ANSI_FUNC

static void 
cdl_doLineInSubRas (
    uchar *pix,				/* edit region raster	*/
    int color,				/* overlay color	*/
    int width,				/* line width		*/
    int style,				/* line style		*/
    int x1,
    int x2,
    int y1,
    int y2,				/* line endpoints	*/
    int lx,
    int ly,				/* subraster LL corner  */
    int nx,
    int ny				/* subraster size  	*/
)
#else

static void
cdl_doLineInSubRas (pix, color, width, style, x1, x2, y1, y2, lx, ly, nx, ny)
uchar	*pix;				/* edit region raster	*/
int	color;				/* overlay color	*/
int 	width;				/* line width		*/
int 	style;				/* line style		*/
int	x1, y1, x2, y2;			/* line endpoints	*/
int	lx, ly;				/* subraster LL corner  */
int	nx, ny;				/* subraster size  	*/
#endif
{
	if (style == L_SOLID || style >= L_HOLLOW) {
	    if (width == 1 && style == L_SOLID)
	        cdl_drawVector (pix, color, x1, x2, y1, y2, lx, ly, nx, ny);
	    else {
		cdl_drawThickVector (pix, color, width, style, x1, x2, y1, y2,
		    lx, ly, nx, ny);
	    }
	} else {
	    ;
	}
}


/*  CDL_DOLINEINSUBRAS --  Draw a line between two arbitrary endpoints in a 
 *  subraster.  Line widths and styles are passed through to specialized
 #  routines.
 */

#ifdef ANSI_FUNC

static void 
cdl_doDashedLine (
    uchar *pix,				/* edit region raster	*/
    int color,				/* overlay color	*/
    int width,				/* line width		*/
    int style,				/* line style		*/
    int *x,
    int *y,				/* line vertices	*/
    int npts,				/* number of vertices	*/
    int lx,
    int ly,				/* subraster LL corner  */
    int nx,
    int ny				/* subraster size  	*/
)
#else

static void
cdl_doDashedLine (pix, color, width, style, x, y, npts, lx, ly, nx, ny)
uchar	*pix;				/* edit region raster	*/
int	color;				/* overlay color	*/
int 	width;				/* line width		*/
int 	style;				/* line style		*/
int	*x, *y;				/* line vertices	*/
int	npts;				/* number of vertices	*/
int	lx, ly;				/* subraster LL corner  */
int	nx, ny;				/* subraster size  	*/
#endif
{
        register int i, nseg = (npts - 1);

        if (cdl_debug)
            printf ("[cdl_doDashedLine] npts=%d lx=%d ly=%d nseg=%d style=%d\n",
                npts, lx, ly, nseg, style);

        /* Connect all the vertices. */
	if (width == 1) {
            for (i=0; i < nseg; i++)
                cdl_drawDashVec (pix, color, style, x[i], x[i+1],
                    y[i], y[i+1], lx, ly, nx, ny);
	} else {
            for (i=0; i < nseg; i++) {
                cdl_drawThickDashVec (pix, color, width, style, x[i], x[i+1],
                    y[i], y[i+1], lx, ly, nx, ny);

		/* Restore the dashed line counters so we get a smooth line */
		dash = p_dash,	dot = p_dot,  dotdash = p_dotdash;
	    }
	}
}


/*  CDL_DRAWTHICKVECTOR --  Draw a line between two arbitrary endpoints in a 
 *  subraster.  We draw multiple vectors to achieve the line thickness.
 */

#ifdef ANSI_FUNC

static void 
cdl_drawThickVector (
    uchar *pix,				/* edit region raster	*/
    int color,				/* overlay color	*/
    int width,				/* line width		*/
    int style,				/* line style		*/
    int x1,
    int x2,
    int y1,
    int y2,				/* line endpoints	*/
    int lx,
    int ly,				/* subraster LL corner  */
    int nx,
    int ny				/* subraster size  	*/
)
#else

static void
cdl_drawThickVector (pix, color, width, style, x1, x2, y1, y2, lx, ly, nx, ny)
uchar	*pix;				/* edit region raster	*/
int	color;				/* overlay color	*/
int 	width;				/* line width		*/
int 	style;				/* line style		*/
int	x1, y1, x2, y2;			/* line endpoints	*/
int	lx, ly;				/* subraster LL corner  */
int	nx, ny;				/* subraster size  	*/
#endif
{
	register int 	i, new_x = x2, new_y = y2, dx = 0, dy = 0;
	register int 	xshift = 0, yshift = 0, w = width, col = color;
	register int 	xx1, xx2, yy1, yy2, n = 0, sign = 1;

        if (abs (new_x - x1) > abs (new_y - y1))
            dx = 0, dy = 1;
        else
            dx = 1, dy = 0;

	if (style == L_HOLLOW)
	    w = HOLLOW_LINE_WIDTH;
	else if (style == L_SHADOW)
	    w = SHADOW_LINE_WIDTH;

	for (i=1; i <= w; i++) {
            xx1 = x1 + xshift;
            yy1 = y1 + yshift;
            xx2 = new_x + xshift;
            yy2 = new_y + yshift;

	    if (style == L_HOLLOW && i == 1)
		col = 0;
	    else if ((style == L_SHADOW && i % 2 && dx) ||
		     (style == L_SHADOW && !(i % 2) && dy))
		col = 0;
	    else
		col = color;

	    cdl_drawVector (pix, col, xx1, xx2, yy1, yy2, lx, ly, nx, ny);

            n = (i + 1 + 0.5) / 2;
	    sign = ((i % 2 == 0) ? 1 : -1);
	    if (x1 == x2)
                xshift = sign * dx * n;
	    else if (y1 == y2)
                yshift = sign * dy * n;
	    else {
                xshift = sign * dx * n;
                yshift = sign * dy * n;
            }
        }
}


/*  CDL_DRAWVECTOR --  Draw a line between two arbitrary endpoints in a 
 *  subraster.  Endpoints are given in image coordinates along with the
 *  subraster boundary, we translate to subraster coords before marking the
 *  pixel.  This allows us to get a subraster for a larger object such as
 *  polygon and use the line primitive to draw each vertex before writing
 *  back a single subraster rather than use the less efficient sequence of
 *  server I/O calls for each line segment.  Uses Bresenham's algorithm to
 *  get a smoother line.
 */

#ifdef ANSI_FUNC

static void 
cdl_drawVector (
    uchar *pix,				/* edit region raster	*/
    int color,				/* overlay color	*/
    int x1,
    int x2,
    int y1,
    int y2,				/* line endpoints	*/
    int lx,
    int ly,				/* subraster LL corner  */
    int nx,
    int ny				/* subraster size  	*/
)
#else

static void
cdl_drawVector (pix, color, x1, x2, y1, y2, lx, ly, nx, ny)
uchar	*pix;				/* edit region raster	*/
int	color;				/* overlay color	*/
int	x1, y1, x2, y2;			/* line endpoints	*/
int	lx, ly;				/* subraster LL corner  */
int	nx, ny;				/* subraster size  	*/
#endif
{
	register int  i, d, x, y;
    	register int  ax, ay, sx, sy, dx, dy;

	/* Use a Bresenham algorith do draw the line. */
    	dx = x2 - x1;    ax = ABS(dx) << 1;    sx = SGN(dx);
    	dy = y2 - y1;    ay = ABS(dy) << 1;    sy = SGN(dy);
	x = x1;
	y = y1;
    	if (ax > ay) {			/* X span is dominant */
            for (d = ay - (ax >> 1); x != x2; x += sx, d += ay) {
	        i = max (((y-ly) * nx) + min(nx-1,(x-lx)), 0);
	        pix[i] = color;
            	if (d >= 0) {
                    y += sy;
                    d -= ax;
            	}
            }
    	} else {     			/* Y span is dominant */
            for (d = ax - (ay >> 1); y != y2; y += sy, d += ax) {
	        i = max (((y-ly) * nx) + min(nx-1,(x-lx)), 0);
	        pix[i] = color;
            	if (d >= 0) {
                    x += sx;
                    d -= ay;
            	}
            }
    	}
 	i = max (((y-ly) * nx) + min(nx-1,(x-lx)), 0);
	pix[i] = color;
}


/*  CDL_DRAWTHICKDASHVEC --  Draw a line between two arbitrary endpoints in
 *  a subraster.  We draw multiple vectors to achieve the line thickness.
 */

#ifdef ANSI_FUNC

static void 
cdl_drawThickDashVec (
    uchar *pix,				/* edit region raster	*/
    int color,				/* overlay color	*/
    int width,				/* line width		*/
    int style,				/* line style		*/
    int x1,
    int x2,
    int y1,
    int y2,				/* line endpoints	*/
    int lx,
    int ly,				/* subraster LL corner  */
    int nx,
    int ny				/* subraster size  	*/
)
#else

static void
cdl_drawThickDashVec (pix, color, width, style, x1, x2, y1, y2, lx, ly, nx, ny)
uchar	*pix;				/* edit region raster	*/
int	color;				/* overlay color	*/
int 	width;				/* line width		*/
int 	style;				/* line style		*/
int	x1, y1, x2, y2;			/* line endpoints	*/
int	lx, ly;				/* subraster LL corner  */
int	nx, ny;				/* subraster size  	*/
#endif
{
	register int 	i, new_x = x2, new_y = y2, dx = 0, dy = 0;
	register int 	xshift = 0, yshift = 0;
	register int 	xx1, xx2, yy1, yy2, n = 0, sign = 1;

        if (abs (new_x - x1) > abs (new_y - y1))
            dx = 0, dy = 1;
        else
            dx = 1, dy = 0;

	switch (style) {
	case L_DASHED:	 SAVE_DASH_COUNT;    break;
	case L_DOTTED:	 SAVE_DOT_COUNT;     break;
	case L_DOTDASH:  SAVE_DOTDASH_COUNT; break;
	}

	for (i=1; i <= width; i++) {
            xx1 = x1 + xshift;
            yy1 = y1 + yshift;
            xx2 = new_x + xshift;
            yy2 = new_y + yshift;

	    cdl_drawDashVec (pix, color, style, xx1, xx2, yy1, yy2,
		lx, ly, nx, ny);

	    if (i == 1) {
		/* Save the dashed line counters so we get a smooth line */
		p_dash = dash,  p_dot = dot,  p_dotdash = dotdash;
	    }

	    switch (style) {
	    case L_DASHED:   RESTORE_DASH_COUNT;    break;
	    case L_DOTTED:   RESTORE_DOT_COUNT;     break;
	    case L_DOTDASH:  RESTORE_DOTDASH_COUNT; break;
	    }

            n = (i + 1 + 0.5) / 2;
	    sign = ((i % 2 == 0) ? 1 : -1);
	    if (x1 == x2)
                xshift = sign * dx * n;
	    else if (y1 == y2)
                yshift = sign * dy * n;
	    else {
                xshift = sign * dx * n;
                yshift = sign * dy * n;
            }
        }
}


/*  CDL_DRAWDASHVEC --  Draw a line between two arbitrary endpoints in a 
 *  subraster.  Endpoints are given in image coordinates along with the
 *  subraster boundary, we translate to subraster coords before marking the
 *  pixel.  This allows us to get a subraster for a larger object such as
 *  polygon and use the line primitive to draw each vertex before writing
 *  back a single subraster rather than use the less efficient sequence of
 *  server I/O calls for each line segment.  Uses Bresenham's algorithm to
 *  get a smoother line.
 */

#ifdef ANSI_FUNC

static void 
cdl_drawDashVec (
    uchar *pix,				/* edit region raster	*/
    int color,				/* overlay color	*/
    int style,				/* line style		*/
    int x1,
    int x2,
    int y1,
    int y2,				/* line endpoints	*/
    int lx,
    int ly,				/* subraster LL corner  */
    int nx,
    int ny				/* subraster size  	*/
)
#else

static void
cdl_drawDashVec (pix, color, style, x1, x2, y1, y2, lx, ly, nx, ny)
uchar	*pix;				/* edit region raster	*/
int	color;				/* overlay color	*/
int	style;				/* line style		*/
int	x1, y1, x2, y2;			/* line endpoints	*/
int	lx, ly;				/* subraster LL corner  */
int	nx, ny;				/* subraster size  	*/
#endif
{
	register int  i, d, x, y;
    	register int  ax, ay, sx, sy, dx, dy;

	/* Use a Bresenham algorith do draw the line. */
    	dx = x2 - x1;    ax = ABS(dx) << 1;    sx = SGN(dx);
    	dy = y2 - y1;    ay = ABS(dy) << 1;    sy = SGN(dy);
	x = x1;
	y = y1;
    	if (ax > ay) {			/* X span is dominant */
            for (d = ay - (ax >> 1); x != x2; x += sx, d += ay) {
	        i = ((y-ly) * nx) + min(nx-1,(x-lx));
		i = max (i, 0);
	        pix[i] = cdl_setpixel (pix[i], style, color);
            	if (d >= 0) {
                    y += sy;
                    d -= ax;
            	}
            }
    	} else {     			/* Y span is dominant */
            for (d = ax - (ay >> 1); y != y2; y += sy, d += ax) {
	        i = ((y-ly) * nx) + min(nx-1,(x-lx));
		i = max (i, 0);
	        pix[i] = cdl_setpixel (pix[i], style, color);
            	if (d >= 0) {
                    x += sx;
                    d -= ay;
            	}
            }
    	}
}


/*  CDL_SETPIXEL --  Set the pixel to the desired color and keep track of
 *  which pixels have been changed so we get a dashed line effect.
 */

#ifdef ANSI_FUNC

static uchar
cdl_setpixel (
    uchar pix,				/* input pixel 		*/
    int	  style,			/* line style 		*/
    int   color				/* line color		*/
)
#else

static uchar
cdl_setpixel (pix, style, color)
uchar	pix;				/* input pixel 		*/
int	style;				/* line style 		*/
int 	color;				/* line color		*/
#endif
{
	/* If the current pixel is already set just ignore it.  We need
	 * to do this so that when drawing many small segments in a poly-
	 * line (e.g. a circle) we don't increment the dash counters un-
	 * necessarily so we can maintain the spacing.
	 */
	if (pix == color)
	    return (pix);

	switch (style) {
	case L_DASHED:
	    return ((DASH_PIXEL ? color : pix));
	case L_DOTTED:
	    return ((DOT_PIXEL ? color : pix));
	case L_DOTDASH:
	    return ((DOTDASH_PIXEL ? color : pix));
	}

	return (color);
}

/*  CDL_FILLAREA -- Scan the pixel array and fill the area enclosed by the
 *  given color, or replace with those pixels from the reference raster.
 *  We assume the area to be filled is a simple closed convex polygon.
 */

#ifdef ANSI_FUNC

static void 
cdl_fillArea (
    uchar *pix,				/* edit region raster	*/
    int nx,
    int ny,				/* subraster size  	*/
    int color				/* overlay color	*/
)
#else

static void
cdl_fillArea (pix, nx, ny, color)
uchar	*pix;				/* edit region raster	*/
int	nx, ny;				/* subraster size  	*/
int	color;				/* overlay color	*/
#endif
{
	register int i, j, l, r;

	for (i=0; i < ny; i++) {
	    l = r  = 0;

	    /* Find the left endpoint. */
	    while (l < nx && pix[(i * nx) + l] != color)
		l++;
	    if (l != nx) {

	        /* Find the right endpoint. */
	        r = nx - 1;
	        while (r > l && pix[(i * nx) + r] != color)
		    r--;

	        if (r > l) {
	            /* Fill the area in between. */
	            for (j=l; j <= r; j++)
	  	        pix[(i*nx)+j] = color;
		}
	    }
	}
}


/*  CDL_GETCIRCLECOORDS --  Compute the coords for points needed to draw a
 *  circle of a given radius at the given center.
 */

#ifdef ANSI_FUNC

static void 
cdl_getCircleCoords (
    int xcen,
    int ycen,				/* circle center	*/
    int radius,				/* circle radius	*/
    int *x,
    int *y,				/* output coords	*/
    int npts				/* number of coords	*/
)
#else

static void
cdl_getCircleCoords (xcen, ycen, radius, x, y, npts)
int	xcen, ycen;			/* circle center	*/
int	radius;				/* circle radius	*/
int	*x, *y;				/* output coords	*/
int 	npts;				/* number of coords	*/
#endif
{
  	register int i, count, xoff, yoff;
  	register int a1, a2, a3, b1, b2, b3, b4;
        static double UnitCircleX[12] = {
            -0.06540312923014, -0.19509032201613,
            -0.32143946530316, -0.44228869021900,
            -0.55557023301960, -0.65934581510007,
            -0.75183980747898, -0.83146961230255,
            -0.89687274153269, -0.94693012949511,
            -0.98078528040323, -0.99785892323860
        };
        static double UnitCircleY[12] = {
            -0.99785892323860, -0.98078528040323,
            -0.94693012949511, -0.89687274153269,
            -0.83146961230255, -0.75183980747898,
            -0.65934581510007, -0.55557023301960,
            -0.44228869021900, -0.32143946530316,
            -0.19509032201613, -0.06540312923014
        };  

 
  	/*  Set up array indexes for 8 fold symetry  */
  	/*  8 indexes radiating both ways from each of the four axes  */
  	a1 = npts / 4;         		/*  (1 * cnt) / 4  */
  	a2 = a1 + a1;                  	/*  (2 * cnt) / 4  */
  	a3 = a2 + a1;                  	/*  (3 * cnt) / 4  */
  	b1 = a1 - 1;
  	b2 = a2 - 1;
  	b3 = a3 - 1;
  	b4 = a3 + a1 - 1;              	/*  (4 * cnt) / 4 - 1  */

  	/*  Calculate points on circumference for 1/8th of circle  */
  	/*  Apply to each of 8 pairs  */
  	count = npts / 8;
	xcen -= 1;			/* fix for mysterious off-by-one bug */
  	for( i=0; i <= count; i++ ) {
    	    xoff    = UnitCircleX[i] * radius + 0.5;
    	    yoff    = UnitCircleY[i] * radius + 0.5;
    	    x[i]    = xcen + xoff;
    	    y[i]    = ycen + yoff;
    	    x[a1+i] = xcen + yoff;
    	    y[a1+i] = ycen - xoff;
    	    x[a2+i] = xcen - xoff;
    	    y[a2+i] = ycen - yoff;
    	    x[a3+i] = xcen - yoff;
    	    y[a3+i] = ycen + xoff;
    	    x[b1-i] = xcen + yoff;
    	    y[b1-i] = ycen + xoff;
    	    x[b2-i] = xcen + xoff;
    	    y[b2-i] = ycen - yoff;
    	    x[b3-i] = xcen - yoff;
    	    y[b3-i] = ycen - xoff;
    	    x[b4-i] = xcen - xoff;
    	    y[b4-i] = ycen + yoff;
  	}
  	/*  Close the circle (end point same as starting point)  */
  	x[npts-1] = x[0];
  	y[npts-1] = y[0];  
}


/*  CDL_GETELLIPSECOORDS --  Compute the coords for points needed to draw a
 *  ellipse w/ given axes and angle at the desired center.
 */

#ifdef ANSI_FUNC

static void 
cdl_getEllipseCoords (
    int xcen,
    int ycen,				/* ellipse center	*/
    int xradius,
    int yradius,			/* ellipse axes		*/
    float rotang,			/* angle from X axis	*/
    int *x,
    int *y				/* output coords	*/
)
#else

static void
cdl_getEllipseCoords (xcen, ycen, xradius, yradius, rotang, x, y)
int	xcen, ycen;			/* ellipse center	*/
int	xradius, yradius;		/* ellipse axes		*/
float	rotang;				/* angle from X axis	*/
int	*x, *y;				/* output coords	*/
#endif
{
  	int 	loop;
  	float 	xoff, yoff;
	double	rotsin, rotcos;
  	register int i;
  	register int x1, x2, y1, y2;
  	register int a2, b1, b3;
	static double UnitEllipseX[16] = {
  	    -0.04906767432742, -0.14673047445536, 
	    -0.24298017990326, -0.33688985339222,
  	    -0.42755509343028, -0.51410274419322, 
	    -0.59569930449243, -0.67155895484702,
  	    -0.74095112535496, -0.80320753148064, 
	    -0.85772861000027, -0.90398929312344,
  	    -0.94154406518302, -0.97003125319454, 
	    -0.98917650996478, -0.99879545620517
	};
	static double UnitEllipseY[16] = {
  	    -0.99879545620517, -0.98917650996478, 
	    -0.97003125319454, -0.94154406518302,
  	    -0.90398929312344, -0.85772861000027, 
	    -0.80320753148064, -0.74095112535496,
  	    -0.67155895484702, -0.59569930449243, 
	    -0.51410274419322, -0.42755509343028,
  	    -0.33688985339222, -0.24298017990326, 
	    -0.14673047445536, -0.04906767432742
	}; 

  	/*  Set up array indexes for 8 fold symetry  */
  	/*  4 indexes radiating both ways from each of the two axis rays  */
  	a2 = N_ELLIPSE_PTS / 2;
  	b1 = a2 - 1;
  	b3 = N_ELLIPSE_PTS - 1;

	rotsin	= sin ((double) ((double)rotang * 0.017453293));
	rotcos	= cos ((double) ((double)rotang * 0.017453293));

  	/*  Recalculate points on circumference  */
  	/*  Apply to each of 4 pairs  */
  	loop = N_ELLIPSE_PTS / 4;
  	for( i=0; i < loop; i++ ) {
    	    /*  Calculate ray lengths for orthogonal case  */
    	    xoff    = UnitEllipseX[i] * xradius;
    	    yoff    = UnitEllipseY[i] * yradius;

    	    /*  Calculate offset of two points on the right (rotate)  */
    	    x2      = (xoff * rotcos) + (yoff * rotsin) + 0.5;
    	    y2      = (xoff * rotsin) - (yoff * rotcos) - 0.5;
    	    x1      = (xoff * rotcos) - (yoff * rotsin) - 0.5;
    	    y1      = (xoff * rotsin) + (yoff * rotcos) + 0.5;
    	    x[i]    = xcen + x1;
    	    y[i]    = ycen + y1;
    	    x[a2+i] = xcen - x1;
    	    y[a2+i] = ycen - y1;
    	    x[b1-i] = xcen + x2;
    	    y[b1-i] = ycen + y2;
    	    x[b3-i] = xcen - x2;
    	    y[b3-i] = ycen - y2;
  	}
  	/*  Close the circle (end point same as starting point)  */
  	x[N_ELLIPSE_PTS-1] = x[0];
  	y[N_ELLIPSE_PTS-1] = y[0]; 
}


/*  CDL_MINMAX -- Find the array extrema.  */

#ifdef ANSI_FUNC

static void 
cdl_minmax (
    int *array,				/* array to check	*/
    int npts,				/* no of points		*/
    int *amin,
    int *amax				/* extremes		*/
)
#else

static void
cdl_minmax (array, npts, amin, amax)
int	*array;				/* array to check	*/
int	npts;				/* no of points		*/
int	*amin, *amax;			/* extremes		*/
#endif
{
	register int i;

	*amin = *amax = array[0];
	for (i=1; i < npts; i++) {
	    if (*amin > array[i])   *amin = array[i];
	    if (*amax < array[i])   *amax = array[i];
	}
}


/*  CDL_STRLEN -- Find the length of a string to be drawn.  */

#ifdef ANSI_FUNC

static int 
cdl_strlen (
char	*str,				/* string to draw	*/
float	txtsize,			/* string size		*/
int	txtfont 			/* font			*/
)
#else

static int
cdl_strlen (str, txtsize, txtfont)
char	*str;				/* string to draw	*/
float	txtsize;			/* string size		*/
int	txtfont;			/* font			*/
#endif
{
        char    chr, *sp = str;
	int	ip, oldfont, font = txtfont, offset = 0;
        register int i, cw, ch, len;
        float   size = txtsize;

        /* Loop over each character in the string. */
        len = 0;
        cw = CHARACTER_WIDTH * (size * FONT_SCALE) + FONT_SPACE;
        ch = CHARACTER_HEIGHT * (size * FONT_SCALE);
        for (; *sp; sp++) {

	    /* Check for an inline font escape. */
	    if (*sp == '\\' && *(sp+1) == 'f') {
		sp++; sp++;
		if (*sp != 'P')
		    oldfont = font;

		switch (*sp) {
		case 'R':
		    font = F_ROMAN;   break;
		case 'F':
		    font = F_FUTURA;  break;
		case 'T':
		    font = F_TIMES;   break;
		case 'G':
		    font = F_GREEK;   break;
		case 'P':
		    font = oldfont;   break;

	        /* Check for a super/subscript escape. */
		case 'U':
                    offset += ch / 2;
		    break;
		case 'D':
                    offset -= ch / 2;
		    break;
		}
		sp++;
	    }

	    /* Adjust the size if we're doing sub/superscript. */
	    if (offset != 0)
		size = txtsize * 0.666;
	    else
		size = txtsize;

            if (*sp < CHARACTER_START || *sp > CHARACTER_END)
                chr = i = '?' - CHARACTER_START;
            else
                chr = i = *sp - CHARACTER_START;

	    /* Update the character position. */
	    ip = (int) chr;
	    switch (font) {
	    case F_ROMAN:
	        len += cw;
		break;
	    case F_GREEK:
        	len += (gchwid[ip] / 2) * (size * FONT_SCALE) + FONT_SPACE + 2;
		break;
	    case F_TIMES:
        	len += (timwid[ip] -12) * (size * FONT_SCALE) + FONT_SPACE + 2;
		break;
	    case F_FUTURA:
        	len += (futwid[ip] / 2) * (size * FONT_SCALE) + FONT_SPACE + 2;
		break;
	    }
        }
	return (len);
}


/* BITUPK -- Unpack an unsigned integer bit field from a longword.
 */

unsigned bitmask[] = {  0,                      /* MACHDEP              */
        01,             03,             07,
        017,            037,            077,
        0177,           0377,           0777,
        01777,          03777,          07777,
        017777,         037777,         077777,
        0177777,        0377777,        0777777,
        01777777,       03777777,       07777777,
        017777777,      037777777,      077777777,
        0177777777,     0377777777,     0777777777,
        01777777777,    03777777777,    07777777777,
        017777777777,   037777777777
};

#ifdef ANSI_FUNC

static int 
bitupk (
    unsigned int wordp,       /* longword to be examined              */
    int offset,               /* one-indexed offset of first bit      */
    int nbits                 /* number of bits to be set             */
)
#else

static int
bitupk (wordp, offset, nbits)
unsigned int   wordp;         /* longword to be examined              */
int    offset;                /* one-indexed offset of first bit      */
int    nbits;                 /* number of bits to be set             */
#endif
{
        return ((wordp >> (offset-1)) & bitmask[nbits]);
}

