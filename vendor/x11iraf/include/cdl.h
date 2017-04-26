/*  
 *  CDL.H -- Global definitions for the Client Display Library.
 */

#ifndef	_CDL_Defined
#define	_CDL_Defined

#define	CDL_VERSION	"Client Display Library V1.8 07/28/01"

/* Declare prototypes if using ANSI C */
#ifdef  CDL_ANSIC
#define ANSI_FUNC
#endif

#define MAX_FBCONFIG     128            /* max size of FB config table  */
#define MAX_FRAMES        16            /* max frames support by server */
#define MAX_MAPPINGS      32            /* max image mappings per frame */
#define DEF_CONTRAST    0.25            /* default zscale contrast      */
#define DEF_NSAMPLE      600            /* default number of samples    */
#define DEF_NSAMPLINES    -1            /* default no. of sample lines  */
#define INDEF           -999            /* INDEF value flag             */


/* Include private definitions when compiling library sources. */
#ifdef   CDL_LIBRARY_SOURCE
#include "eps.h"
#include "cdlP.h"
#endif

/* Frame buffer selection code. */
#define	FB_AUTO		-1		/* autoconfig the frame buffer	*/

/* Types of greyscale transformations. */
#define CDL_UNITARY       0            	/* values map without change	*/
#define CDL_LINEAR        1            	/* linear mapping		*/
#define CDL_LOG           2            	/* logarithmic mapping		*/

/* Overlay colors. */
#define	C_BLACK		202		/* Static overlay color defs	*/
#define	C_WHITE		203
#define	C_RED		204
#define	C_GREEN		205
#define	C_BLUE		206
#define C_YELLOW	207
#define	C_CYAN		208
#define C_MAGENTA	209
#define C_CORAL		210
#define	C_MAROON	211
#define C_ORANGE	212
#define	C_KHAKI		213
#define C_ORCHID	214
#define C_TURQUOISE	215
#define C_VIOLET	216
#define	C_WHEAT		217

#define M_FILL             1 		/* Overlay point mark types. 	*/
#define M_POINT            2
#define M_BOX              4
#define M_PLUS             8
#define M_CROSS           16
#define M_DIAMOND         32
#define M_CIRCLE          64
#define M_STAR           128
#define M_HLINE          256
#define M_VLINE          512
#define M_HBLINE        1024
#define M_VBLINE        2048

#define F_ROMAN		   0 		/* Font types. 			*/
#define F_GREEK		   1
#define F_FUTURA	   2
#define F_TIMES		   3
#define F_BOLD		   4

#define L_SOLID            0            /* Polyline attribute values. 	*/
#define L_DASHED           1
#define L_DOTTED           2
#define L_DOTDASH          3
#define L_HOLLOW           4
#define L_SHADOW           5


#define MOD_FAST	   1		/* SubRaster display model	*/
#define MOD_NORMAL	   2	
#define DEF_MODEL	   MOD_FAST


/* Local type definitions. */
typedef	struct CDL 	*CDLPtr;
typedef	struct Marker 	*MarkerPtr;


#ifndef AIXV3
#ifndef OSF1
typedef unsigned char   uchar;
#endif
#endif



/*  The main CDL package structure.
 */
struct CDL {
#ifdef   CDL_LIBRARY_SOURCE
	IMDPtr	imd;			/* IMD package pointer	*/
	int	memModel;		/* Memory model to use	*/
#endif

	int	frame;			/* display frame number	*/
	int	fbconfig;		/* fb config number 	*/
	int	fbwidth;		/* current FB width	*/
	int	fbheight;		/* current FB height	*/
	int	fbnf;			/* current FB nframes   */

	float	contrast;		/* zscale contrast value*/
	int	nsample;		/* opt. sample points	*/
	int	nsamplines;		/* opt. sample lines	*/

	int	im_nx;	   		/* current image width	*/
	int	im_ny;	   		/* current image height	*/

	/* Overlay graphics attributes. 			*/
	int	font;			/* overlay text font    */
	int	textwidth;		/* text width	    	*/
	int	linewidth;		/* line width	    	*/
	int	linestyle;		/* line style	    	*/

	/* WCS descriptor stuff. 				*/
	char	*imname;		/* image name		*/
	char	*imtitle;		/* image title		*/
	float	a, b, c, d;		/* WCS values 		*/
	float	tx, ty;			/* translation values	*/
	float	z1, z2;			/* zscale values	*/
	int	ztrans;			/* Z trans type		*/

	/* Coordinate mappings on the frame buffer. */
	char	*ref;			/* img reference	*/
	char	*region;		/* region name		*/
	float	sx, sy;			/* source rect		*/
	int	snx, sny;
	int	dx, dy;			/* destination rect	*/
	int	dnx, dny;
	int	iis_version;		/* server IIS version 	*/
	int	iis_valid;		/* valid mapping flag	*/
};


/* A element of the display list describing the marker.  We throw in the
 * kitchen sinke here to cover all possible marker types, in reality only
 * a few of these are used for any given marker type.
 */
struct Marker {
	short	type;			/* marker type		*/
	int	x, y;			/* center coords	*/
	int	number;			/* label a point	*/
	int	pt_type;		/* point type		*/
	int	size;			/* point marker size	*/
	int	fill;			/* fill marker 		*/
	int	color;			/* marker color		*/
	int	*xp, *yp;		/* coords		*/
	int	npts;			/* npts in array	*/
	int	radius;			/* circle radius	*/
	int	nannuli;		/* num of annuli	*/
	int	sep;			/* annulus separation	*/
	int	xrad, yrad;		/* ellipse axes		*/
	float	ang;			/* ellipse rotation	*/
	float	txsize;			/* text marker size	*/
	char	*str;			/* text marker string	*/
	int	font;			/* text font type    	*/
	int	textwidth;		/* text width	    	*/
	int	linewidth;		/* line width	    	*/
	int	linestyle;		/* line style	    	*/

	int	nx, ny;			/* marker region size	*/
	int	lx, ly;			/* marker LL corner	*/

	unsigned char	*refpix;	/* orig image pixels	*/
	unsigned char	*markpix;	/* marked image pixels	*/

	MarkerPtr  back;		/* linked list pointers	*/
	MarkerPtr  next;
	int	markerID;		/* assigned id number 	*/
};


/* Marker types used internally. */
#define MK_POINT	0
#define MK_LINE		1
#define MK_BOX		2
#define MK_POLYLINE	3
#define MK_POLYGON	4
#define MK_CIRCLE	5
#define MK_CIRCANN	6
#define MK_ELLIPSE	7
#define MK_ELLIPANN	8
#define MK_TEXT		9


/* Function definitions. */
#ifdef __cplusplus 
extern "C" {
#endif

#ifndef ANSI_FUNC

CDLPtr  cdl_open();

char	cdl_readCursor();

int	cdl_displayPix(), cdl_displayIRAF(), cdl_displayFITS();
int	cdl_isIRAF(), cdl_isFITS();
int	cdl_readIRAF(), cdl_readFITS(), cdl_clearFrame();
int	cdl_readImage(), cdl_readFrameBuffer(), cdl_readSubRaster();
int	cdl_writeSubRaster(), cdl_setCursor();
int	cdl_printPix(), cdl_printPixToFile();
int	cdl_setWCS(), cdl_getWCS();
int 	cdl_getMapping(), cdl_setMapping(), cdl_queryMap();

void	cdl_selectFB(), cdl_close(), cdl_computeZscale(), cdl_zscaleImage();
void	cdl_setFrame(), cdl_setFBConfig(), cdl_setZTrans(), cdl_setZoom();
void	cdl_setSample(), cdl_setContrast(), cdl_setName(), cdl_setTitle();
void	cdl_getFrame(), cdl_getFBConfig(), cdl_getZTrans(), cdl_getZoom();
void	cdl_getSample(), cdl_getContrast(), cdl_getName(), cdl_getTitle();
void	cdl_getSampleLines(), cdl_setSampleLines(), cdl_zscale();
void	cdl_setZScale(), cdl_getZScale(), cdl_lookupFBSize();
void	cdl_setDebug();

/* Marker function definitions. */
int	cdl_markPoint(), cdl_markLine(), cdl_markBox(), cdl_markPolyline();
int	cdl_markPolygon(), cdl_markCircle(), cdl_markCircAnnuli();
int	cdl_markEllipse(), cdl_markEllipAnnuli(), cdl_markText();
int	cdl_deleteMark(), cdl_clearOverlay(), cdl_redrawOverlay();
int	cdl_markCoordsFile(), cdl_mapFrame(), cdl_markPointLabel();

void	cdl_setFont(), cdl_setTextWidth();
void	cdl_setLineWidth(), cdl_setLineStyle();

#endif


/* Include function prototypes for all public CDL functions when using ANSI C */

#ifdef ANSI_FUNC

CDLPtr cdl_open(char *imtdev);
int cdl_displayPix(CDLPtr cdl, uchar *pix, int nx, int ny, int bitpix, int frame, int fbconfig, int zscale);
char cdl_readCursor(CDLPtr cdl, int sample, float *x, float *y, int *wcs, char *key);
int cdl_setCursor(CDLPtr cdl, int x, int y, int wcs);
int cdl_setWCS(CDLPtr cdl, char *imname, char *imtitle, float a, float b, float c, float d, float tx, float ty, float z1, float z2, int zt);
int cdl_getWCS(CDLPtr cdl, char *name, char *title, float *a, float *b, float *c, float *d, float *tx, float *ty, float *z1, float *z2, int *zt);
int cdl_getMapping(CDLPtr cdl, char *region, float *sx, float *sy, int *snx, int *sny, int *dx, int *dy, int *dnx, int *dny, char *ref);
int cdl_setMapping(CDLPtr cdl, char *region, float sx, float sy, int snx, int sny, int dx, int dy, int dnx, int dny, char *ref);
int cdl_queryMap(CDLPtr cdl, int wcs, char *region, float *sx, float *sy, int *snx, int *sny, int *dx, int *dy, int *dnx, int *dny, char *objref);
int cdl_clearFrame(CDLPtr cdl);
void cdl_selectFB(CDLPtr cdl, int nx, int ny, int *fb, int *w, int *h, int *nf, int reset);
void cdl_close(CDLPtr cdl);
int cdl_readImage(CDLPtr cdl, uchar **pix, int *nx, int *ny);
int cdl_readFrameBuffer(CDLPtr cdl, uchar **pix, int *nx, int *ny);
void cdl_computeZscale(CDLPtr cdl, uchar *pix, int nx, int ny, int bitpix, float *z1, float *z2);
void cdl_zscaleImage(CDLPtr cdl, uchar **pix, int nx, int ny, int bitpix, float z1, float z2);
int cdl_printPix(CDLPtr cdl, char *cmd, uchar *pix, int nx, int ny, int annotate);
int cdl_printPixToFile(CDLPtr cdl, char *fname, uchar *pix, int nx, int ny, int annotate);
int cdl_readSubRaster(CDLPtr cdl, int lx, int ly, int nx, int ny, uchar **pix);
int cdl_writeSubRaster(CDLPtr cdl, int lx, int ly, int nx, int ny, uchar *pix);
void cdl_setFBConfig(CDLPtr cdl, int configno);
void cdl_getFBConfig(CDLPtr cdl, int *configno, int *w, int *h, int *nframes);
void cdl_lookupFBSize(CDLPtr cdl, int configno, int *w, int *h, int *nf);
void cdl_setFrame(CDLPtr cdl, int frame);
void cdl_setZTrans(CDLPtr cdl, int ztrans);
void cdl_setZScale(CDLPtr cdl, float z1, float z2);
void cdl_setSample(CDLPtr cdl, int nsample);
void cdl_setSampleLines(CDLPtr cdl, int nlines);
void cdl_setContrast(CDLPtr cdl, float contrast);
void cdl_setName(CDLPtr cdl, char *imname);
void cdl_setTitle(CDLPtr cdl, char *imtitle);
void cdl_getFrame(CDLPtr cdl, int *frame);
void cdl_getZTrans(CDLPtr cdl, int *ztrans);
void cdl_getZScale(CDLPtr cdl, float *z1, float *z2);
void cdl_getSample(CDLPtr cdl, int *nsample);
void cdl_getSampleLines(CDLPtr cdl, int *nlines);
void cdl_getContrast(CDLPtr cdl, float *contrast);
void cdl_getName(CDLPtr cdl, char *imname);
void cdl_getTitle(CDLPtr cdl, char *imtitle);
void cdl_setDebug(int state);


int cdl_mapFrame(CDLPtr cdl, int frame);
int cdl_markCoordsFile(CDLPtr cdl, char *fname, int type, int size, int color, int label);
int cdl_markPoint(CDLPtr cdl, int x, int y, int number, int size, int type, int color);
int cdl_markPointLabel(CDLPtr cdl, int x, int y, char *label, int size, int type, int color);
int cdl_markLine(CDLPtr cdl, int xs, int ys, int xe, int ye, int color);
int cdl_markBox(CDLPtr cdl, int lx, int ly, int ux, int uy, int fill, int color);
int cdl_markPolygon(CDLPtr cdl, int xarray[], int yarray[], int npts, int fill, int color);
int cdl_markPolyline(CDLPtr cdl, int *xarray, int *yarray, int npts, int color);
int cdl_markCircle(CDLPtr cdl, int x, int y, int radius, int fill, int color);
int cdl_markCircAnnuli(CDLPtr cdl, int x, int y, int radius, int nannuli, int sep, int color);
int cdl_markEllipse(CDLPtr cdl, int x, int y, int xrad, int yrad, float rotang, int fill, int color);
int cdl_markEllipAnnuli(CDLPtr cdl, int x, int y, int xrad, int yrad, float ang, int nannuli, int sep, int color);
int cdl_markText(CDLPtr cdl, int x, int y, char *str, float size, float angle, int color);
void cdl_setFont(CDLPtr cdl, int font);
void cdl_setTextWidth(CDLPtr cdl, int width);
void cdl_setLineWidth(CDLPtr cdl, int width);
void cdl_setLineStyle(CDLPtr cdl, int style);
int cdl_deleteMark(CDLPtr cdl, int x, int y);
int cdl_clearOverlay(CDLPtr cdl);
int cdl_redrawOverlay(CDLPtr cdl);
void cdl_beginDList(int frame);
void cdl_drawDList(int frame);
void cdl_clearDList(int frame);
void cdl_endDList(int frame, int flush);
int cdl_doTextMarker(int x, int y, char *string, float size, float angle, int color, int width, int font, uchar *pix, int lx, int ly, int nx, int ny);
int cdl_freeDisplayList(CDLPtr cdl, MarkerPtr head);

int cdl_displayFITS(CDLPtr cdl, char *fname, int frame, int fbconfig, int zscale);
int cdl_isFITS(char *fname);
int cdl_readFITS(char *fname, uchar **pix, int *nx, int *ny, int *bitpix,
char *title);

int cdl_displayIRAF(CDLPtr cdl, char *fname, int band, int frame, int
fbconfig, int zscale);
int cdl_isIRAF(char *fname);
int cdl_readIRAF(char *fname, int band, uchar **pix, int *nx, int *ny, int *bitpix, char *title);

#endif

#ifdef __cplusplus
}
#endif


#ifdef CDL_LIBRARY_SOURCE
#include "cdlProto.h"
#endif

/* _CDL_Defined */
#endif
