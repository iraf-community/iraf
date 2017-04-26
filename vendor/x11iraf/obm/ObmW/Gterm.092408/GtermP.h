#ifndef _GtermP_h
#define _GtermP_h

#include "Gterm.h"

/*
 * GtermP -- Private definitions for the Gterm graphics widget.
 */

#define	DEF_WIDTH		640
#define	DEF_HEIGHT		480
#define	MAX_RASTERS		512
#define	MAX_MAPPINGS		32
#define	SZ_NUMBER		64
#define	SZ_STATIC_CMAP		10		/* bg+fg+NColors */
#define MAX_SZCMAP		256		/* max size colormap */
#define	DEF_MAXCOLORS		216		/* max dynamic colors */
#define MAX_WMWIN		32		/* max WM colormaps */
#define MAX_REGIONS		64		/* setMapping regions */
#define MAX_AUXTRANS		8		/* auxiliary translations */
#define	DEF_BASEPIXEL		38		/* base of custom colormap */
#define	DEF_CMAPUPDATE		60		/* seconds */
#define	DEF_CMAPSHADOW		10		/* seconds */
#define	DEF_COPYONRESIZE	True		/* copy pixmap on resize */
#define	DEF_WARPCURSOR		False		/* enable warp cursor */
#define	DEF_RAISEWINDOW		False		/* enable raise window */
#define	DEF_DEICONIFYWINDOW	False		/* enable deiconfify window */
#define	DEF_USETIMERS		True		/* ok to use timers */
#define	MAX_DRAW		64		/* max mappings for a draw  */
#define	MAX_POINTS		4096		/* max points in polyline */
#define	GM_MAXVERTICES		64		/* max GM points w/o malloc */
#define	GM_NPTSCIRCLE		48		/* npoints circle or ellipse */
#define	GM_MAXCALLBACKS		16		/* max GM callbacks */
#define	GM_UPDATE		30		/* marker update interval */
#define	MAXNDC			32767		/* GKI/NDC scale factor */
#define V_DIST			4		/* Close to vertex, pixels */
#define E_DIST			1		/* Close to edge, pixels */


#define	RasterDepth		8
#define	ColormapDepth		8
#define	RGBDepth		24

#define	NAlphaFonts		8
#define	NDialogFonts		8
#define	NColors			8

typedef void (*GmVMethod)();
typedef int  (*GmIMethod)();
#define uchar	unsigned char
#define ushort	unsigned short

/* Raster definitions. */
#define	ImageRaster		1
#define	PixmapRaster		2

struct raster {
	int type;
	int delete;
	int width, height;
	union {
	    Pixmap pixmap;
	    XImage *ximage;
	} r;
};

/* Colormap structure. */
struct colormap {
	int map;
	int ncells;
	struct colormap *next;
	unsigned short r[MAX_SZCMAP];
	unsigned short g[MAX_SZCMAP];
	unsigned short b[MAX_SZCMAP];
};

/* mapExtent - Range of dst pixels affected by a src pixel. */
typedef struct {
	Position lo;
	Position hi;
} mapExtent, *MapExtent;

/* Mappings map a source to a destination.  A src or dst of zero refers to
 * the window, a nonzero value is the raster number.
 */
struct mapping {
	int mapping;				/* mapping number */
	int enabled;				/* update destination */
	int defined;				/* mapping is defined */
	int updated;				/* internal params ready */
	int refresh;				/* refresh entire dest */
	int rop;				/* rasterop */
	int src;				/* source rect */
	int st;
	int sx, sy;
	int snx, sny;
	int dst;				/* destination rect */
	int dt;
	int dx, dy;
	int dnx, dny;
	int scaling;				/* internal parameters */
	float xscale, yscale;
	mapExtent *x_extent, *y_extent;
	int *x_srcpix, *y_srcpix;
	float *x_src, *y_src;
	uchar *mapdata;
	int datalen;
	struct mapping *prev;			/* previous in stack order */
	struct mapping *next;			/* next in stack order */
};

#define	M_NOSCALING	0
#define	M_ZOOM		1
#define	M_INTZOOM	2
#define	M_DEZOOM	3

/* The drawing context defines what happens when a drawing operation (e.g.
 * polyline) takes place.  In the simplest case (raster=0) one simply draws
 * into the display window with no transformation or clipping.  When a
 * raster provides the drawing context, the graphics are drawn once for each
 * active mapping defined on the raster, using the scaling and drawable
 * defined by the mapping.
 */
struct drawContext {
	int valid;
	int raster;
	struct raster *rp;
	int nmappings;
	struct mappingContext {
	    int mapping;
	    struct mapping *mp;
	    int scale;
	    float xoffset, xscale;
	    float yoffset, yscale;
	    int use_backing_store;
	    Pixmap pixmap;
	    GC drawGC;
	    int GC_private;
	} mapContext[MAX_DRAW];
};

/* Graphics Markers.  A marker is an active graphics object displayed on
 * top of a drawing to mark a region.  Markers can respond to events and
 * move, resize, or modify themselves, optionally executing callback
 * procedures when the marker changes state.
 */

/* Callback descriptor. */
struct markerCallback {
	int events;
	GmIMethod func;
	XtPointer client_data;
};

/* Marker selection. */
struct markerSelection {
	int type;
	int vertex;
};

/* Main Marker descriptor. */
struct marker {
	GtermWidget w;				/* backpointer to widget */
	int type;				/* marker type */
	int flags;				/* bitflags */
	int x, y;				/* position */
	int width, height;			/* size */
	double rotangle;			/* orientation */
	XtTranslations translations;		/* marker translations */
	XRectangle old_rect;			/* old bounding box */
	Region old_region;			/* old screen region */
	XRectangle cur_rect;			/* current bounding box */
	Region cur_region;			/* current screen region */
	Time time;				/* time of last marker edit */
	struct marker *next;			/* next marker */
	struct marker *prev;			/* previous marker */
	struct marker *parent;			/* set if copy */

	int lineColor, lineWidth, lineStyle;	/* marker attributes */
	int highlightColor;
	int knotColor, knotSize;
	int fill, fillStyle;
	int fillColor, fillBgColor;
	Pixmap fillPattern;
	int imageText, textBorder;
	int textColor, textBgColor;
	int rotIndicator;					/* MF020 */
	XFontStruct *font;

	int npoints;				/* marker data */
	XPoint *points;
	XPoint point_data[GM_MAXVERTICES+1];
	struct dPoint *pgon;
	int pgon_npts;
	char *text;

	GmIMethod select;			/* class methods */
	GmVMethod markpos;
	GmVMethod redraw;
	GmVMethod update;
	GmVMethod addPt;
	GmVMethod deletePt;
	GmVMethod movePt;
	GmVMethod move;
	GmVMethod resize;
	GmVMethod rotate;

	int ncallbacks;				/* callbacks */
	struct markerCallback callback[GM_MAXCALLBACKS];
	XtIntervalId focus_id;
	int constraints;
};

/* Graphics marker bitflags. */
#define Gm_Activated		000001
#define Gm_Visible		000002
#define Gm_Sensitive		000004
#define Gm_AutoRedraw		000010
#define Gm_PgonInit		000020
#define Gm_Smooth		000040
#define Gm_Modified		000100
#define Gm_BeingDestroyed	000200

/* Attribute value type codes. */
#define Gt_Bool			1
#define Gt_Int			2
#define Gt_DFloatP		3
#define Gt_Pointer		4
#define Gt_String		5

/* Attribute name codes. */
#define	Ga_Type			1
#define	Ga_Activated		2
#define	Ga_Visible		3
#define	Ga_Sensitive		4
#define	Ga_AutoRedraw		5
#define Ga_Translations		6
#define	Ga_X			7
#define	Ga_Y			8
#define	Ga_Width		9
#define	Ga_Height		10
#define	Ga_Rotangle		11
#define	Ga_HighlightColor	12
#define	Ga_LineColor		13
#define	Ga_LineWidth		14
#define	Ga_LineStyle		15
#define	Ga_KnotColor		16
#define	Ga_KnotSize		17
#define	Ga_Fill			18
#define	Ga_FillColor		19
#define	Ga_FillBgColor		20
#define	Ga_FillPattern		21
#define	Ga_FillStyle		22
#define	Ga_TextColor		23
#define	Ga_TextBgColor		24
#define	Ga_TextBorder		25
#define	Ga_ImageText		26
#define	Ga_Font			27
#define	Ga_Text			28
#define	Ga_RotIndicator		29				/* MF020 */

/* Bitflags for selected attributes. */
#define	Gb_X			00001
#define	Gb_Y			00002
#define	Gb_Width		00004
#define	Gb_Height		00010
#define	Gb_Rotangle		00020

/* Codes for marker selection types. */
#define	Ge_Marker		1
#define	Ge_Point		2
#define	Ge_Edge			3

/* Auxiliary translation tables. */
#define	T_replace		0
#define	T_augment		1
#define	T_override		2

typedef struct raster *Raster;
typedef struct mapping *Mapping;
typedef struct drawContext *DrawContext;
typedef struct mappingContext *MappingContext;
typedef struct marker *Marker;
typedef struct markerSelection gmSelection;
typedef struct markerSelection *GmSelection;


/* Gterm callbacks. */
typedef void (*GtCallbackProc)();
struct gtCallback {
	GtCallbackProc proc;
	XtPointer client_data;
	struct gtCallback *next;
};
typedef struct gtCallback GtCallback;


/* Main Gterm widget instance descriptor.
 */
typedef struct {
	/* resources */
	XFontStruct	*alphaFont1;		/* graphics fonts             */
	XFontStruct	*alphaFont2;
	XFontStruct	*alphaFont3;
	XFontStruct	*alphaFont4;
	XFontStruct	*alphaFont5;
	XFontStruct	*alphaFont6;
	XFontStruct	*alphaFont7;
	XFontStruct	*alphaFont8;

	XFontStruct	*dialogFont1;		/* dialog fonts               */
	XFontStruct	*dialogFont2;
	XFontStruct	*dialogFont3;
	XFontStruct	*dialogFont4;
	XFontStruct	*dialogFont5;
	XFontStruct	*dialogFont6;
	XFontStruct	*dialogFont7;
	XFontStruct	*dialogFont8;

	Pixel		dialogBgColor;		/* default colors             */
	Pixel		dialogFgColor;
	Pixel		idleCursorBgColor;
	Pixel		idleCursorFgColor;
	Pixel		busyCursorBgColor;
	Pixel		busyCursorFgColor;
	Pixel		ginmodeCursorBgColor;
	Pixel		ginmodeCursorFgColor;
	int		ginmodeBlinkInterval;
	XColor		ginmodeColors[2];
	Pixel		crosshairCursorColor;
	String		idleCursor;
	String		busyCursor;
	String		ginmodeCursor;
	Boolean		warpCursor;
	Boolean		raiseWindow;
	Boolean		deiconifyWindow;
	Boolean		useTimers;

	Pixel		color0;
	Pixel		color1;
	Pixel		color2;
	Pixel		color3;
	Pixel		color4;
	Pixel		color5;
	Pixel		color6;
	Pixel		color7;
	Pixel		color8;
	Pixel		color9;

	String		cacheRasters;
	int		maxRasters;		/* raster display stuff       */
	int		maxMappings;
	int		maxColors;

	/* private state */
	Display		*display;
	Screen		*screen;
	Window		window;
	Window		root;

        int		w_depth;		/* screen depth and visual    */
	int		w_visual_class;

	int		raster;			/* used for drawing context   */
	int		delay;			/* wait for display           */
	Pixmap		pixmap;			/* used to refresh window     */
	Pixmap		d_pixmap;		/* used to erase dialog area  */
	int		d_saved;		/* set when d_pixmap filled   */
	GC		clearGC;		/* clear pixmap               */
	GC		exposeGC;		/* copy pixmap to window      */
	GC		drawGC;			/* graphics drawing           */
	GC		dialogGC;		/* dialog box                 */
	GC		cursorGC;		/* crosshair cursor           */
	int		cursor_type;		/* type of cursor to display  */
	Cursor		cursor;			/* current cursor             */
	int		full_crosshair;		/* crosshair enabled          */
	int		preserve_screen;	/* cursor preserves screen    */
	int		preserve_valid;		/* saved data is valid        */
	Cursor		idle_cursor;		/* application is idle        */
	Cursor		busy_cursor;		/* application is busy        */
	Cursor		ginmode_cursor;		/* graphics input mode        */
	Cursor		crosshair_cursor;	/* graphics input mode        */
	int		cursor_drawn;		/* crosshair cursor drawn     */
	int		cur_x, cur_y;		/* crosshair cursor coords    */
	int		old_width, old_height;	/* size before resize         */
	int		save_root;		/* root window of saved cur   */
	int		save_x, save_y;		/* saved cursor location      */
	int		last_x, last_y;		/* x,y of last event          */
	int		interactive;		/* set if cursor read         */
	int		char_size;		/* not used                   */
	int		data_level;		/* draw or erase graphics     */
	int		line_style;		/* solid or patterned line    */
	int		line_width;		/* width of line in pixels    */
	int		fill_type;		/* not used                   */
	int		color_index;		/* current color index        */
	int		xres, yres;		/* tek logical resolution     */
	int		d_xoff, d_yoff;		/* dialog area offset         */
	int		d_height;		/* dialog area height         */
	int		optcols, optrows;	/* optimum screen size, chars */
	int		alpha_font;		/* current alpha font index   */
	int		dialog_font;		/* current dialog font index  */

	int		ncolors;		/* current cmap size          */
	int		haveColormap;		/* colormap initialized       */
	Boolean		copyOnResize;		/* copy old pixmap on resize  */
	int		useDefaultCM;		/* use default colormap       */
	Pixel		base_pixel;		/* used for custom colormap   */
	String		cmapName;		/* private colormap name      */
        Boolean         useGlobalCmap;          /* use global data struct?    */
	Boolean		cmapInitialize;		/* forcibly install colormap  */
	Atom		cmapAtom;		/* atom for cmap property     */
	int		cmapShadow;		/* update default colormap    */
	Time		cmapLastShadow;		/* time of last update        */
	Boolean		cmapInterpolate;	/* interpolate colormap       */
	int		cmapUpdate;		/* update interval, seconds   */
	Time		cmapLastUpdate;		/* time of last update        */

	Pixel		*cmap;			/* map color number to pixval */
	XColor		*color;			/* RGB color assignments      */

	ushort		iomap[MAX_SZCMAP];	/* client i/o color map       */
	Pixel		cmap_in[MAX_SZCMAP];	/* umap and cmap combined     */
	Pixel		cmap_out[MAX_SZCMAP];	/* umap and cmap combined     */
	int		cmap_in_valid;		/* set when cmap_in computed  */
	int		cmap_out_valid;		/* set when cmap_out computed */
	struct colormap *colormaps;		/* list of client colormaps   */
	Window		wmTop;			/* top level window           */
	Window		wmWindows[MAX_WMWIN];	/* custom colormap windows    */
	int		n_wmWindows;		/* number of WM windows       */
	int		in_window;		/* pointer is in window       */
	XWindowAttributes wa;			/* window attributes          */
	int		wa_defined;		/* set when above is defined  */


        /* Deep Frame */
        Visual*         visual;                 /* ptr to non-default visual  */
        int             forcePseudo8;           /* force use of Pseudo 8 vis  */
        /* Deep Frame */


	XFontStruct *alpha_fonts[NAlphaFonts];	/* alpha font index           */
	XFontStruct *dialog_fonts[NDialogFonts];/* dialog font index          */

	GtCallback	*resetCallback;		/* client setGterm callbacks  */
	GtCallback	*resizeCallback;	/* client resize callback     */
	GtCallback	*inputCallback;		/* client event input cb      */

	Raster		rasters;		/* raster descriptors         */
	int		nrasters;		/* number of alloced rasters  */
	Mapping		mappings;		/* mapping descriptors        */
	int		nmappings;		/* number of mappings         */
	Mapping		mp_head;		/* head of mapping list       */
	Mapping		mp_tail;		/* tail of mapping list       */
	struct drawContext draw;		/* drawing context            */

	/* Markers */
	Marker		gm_head;		/* head of marker list        */
	Marker		gm_tail;		/* head of marker list        */
	Marker		gm_create;		/* set if creating marker     */
	Marker		gm_active;		/* marker that has focus      */
	gmSelection	gm_selection;		/* active portion of marker   */
	GC		gm_drawGC;		/* marker drawing GC          */
	GC		gm_rubberGC;		/* marker rubber-band GC      */
	Cursor		gm_markerCursor;	/* pointer in marker          */
	Cursor		gm_edgeCursor;		/* pointer on marker edge     */
	Cursor		gm_pointCursor;		/* pointer near marker point  */
	int		gm_redisplay;		/* redisplay needed           */
	int		gm_initialized;		/* set after init             */

	XtTranslations	defTranslations;	/* gterm translations         */
	XtTranslations	auxTrans[MAX_AUXTRANS];	/* auxiliary translations     */
	int		auxTType[MAX_AUXTRANS];	/* translation type           */
	int		nauxTrans;		/* number of auxilary trans   */
	String		gm_translations;	/* Marker translations        */
	XtTranslations	gm_defTranslations;	/* default marker trans       */
	Marker		gm_curTranslations;	/* current translations       */
	Marker		gm_reqTranslations;	/* requested translations     */
	XtIntervalId	gm_timer_id;		/* translation request timer  */

	String		gm_defaultMarker;	/* default marker type name   */
	int		gm_defaultType;		/* default marker type        */
	int		gm_nearEdge;		/* defines area near edge     */
	int		gm_nearVertex;		/* defines area near Vertex   */

	int		gm_lineWidth;		/* shared attributes          */
	int		gm_lineStyle;
	Boolean		gm_fill;
	Pixel		gm_fillColor;
	Pixel		gm_fillBgColor;
	int		gm_fillStyle;
	Boolean		gm_xorFill;		/* fill with GXxor            */
	int		gm_xorFillColor;	/* xor-fill color             */
	int		gm_xorFillBgColor;	/* xor-fill background color  */
	int		gm_highlightWidth;	/* highlight width, pixels    */
	int		gm_highlightColor;	/* highlight color            */
	Pixel		gm_cursorFgColor;	/* marker cursors             */
	Pixel		gm_cursorBgColor;	/* marker cursors             */

	Pixel		gm_LineLineColor;	/* Lines, Polylines           */
	Pixel		gm_LineKnotColor;
	int		gm_LineKnotSize;
	Pixel		gm_TextLineColor;	/* Text markers               */
	Pixel		gm_TextColor;
	Pixel		gm_TextBgColor;		/* bkg color, image text      */
	int		gm_TextBorder;		/* border around text         */
	XFontStruct	*gm_TextFont;		/* default font               */
	String		gm_TextString;		/* default text               */

	Pixel		gm_RectLineColor;	/* Rectangle markers          */
	Pixel		gm_RectKnotColor;
	int		gm_RectKnotSize;
	Pixel		gm_BoxLineColor;	/* Box markers                */
	Pixel		gm_BoxKnotColor;
	int		gm_BoxKnotSize;
	Pixel		gm_CircleLineColor;	/* Circle markers             */
	Pixel		gm_CircleKnotColor;
	int		gm_CircleKnotSize;
	Pixel		gm_EllipseLineColor;	/* Ellipse markers            */
	Pixel		gm_EllipseKnotColor;
	int		gm_EllipseKnotSize;
	Pixel		gm_PgonLineColor;	/* Polygon markers            */
	Pixel		gm_PgonKnotColor;
	int		gm_PgonKnotSize;

        /* Deep Frame */
        String          dialogBgColorStr;       /* default colors             */
        String          dialogFgColorStr;
        String          idleCursorBgColorStr;
        String          idleCursorFgColorStr;
        String          busyCursorBgColorStr;
        String          busyCursorFgColorStr;
        String          ginmodeCursorBgColorStr;
        String          ginmodeCursorFgColorStr;
        String          crosshairCursorColorStr;

        String          color0Str;
        String          color1Str;
        String          color2Str;
        String          color3Str;
        String          color4Str;
        String          color5Str;
        String          color6Str;
        String          color7Str;
        String          color8Str;
        String          color9Str;

        String          gm_highlightColorStr;   /* highlight color            */
        String          gm_fillColorStr;
        String          gm_fillBgColorStr;
        String          gm_cursorFgColorStr;    /* marker cursors             */
        String          gm_cursorBgColorStr;
        String          gm_LineLineColorStr;    /* Lines, Polylines           */
        String          gm_LineKnotColorStr;
        String          gm_TextLineColorStr;    /* Text markers               */
        String          gm_TextColorStr;
        String          gm_TextBgColorStr;      /* bkg color, image text      */
        String          gm_RectLineColorStr;    /* Rectangle markers          */
        String          gm_RectKnotColorStr;
        String          gm_BoxLineColorStr;     /* box                        */
        String          gm_BoxKnotColorStr;
        String          gm_CircleLineColorStr;  /* Circle markers             */
        String          gm_CircleKnotColorStr;
        String          gm_EllipseLineColorStr; /* Ellipse markers            */
        String          gm_EllipseKnotColorStr;
        String          gm_PgonLineColorStr;    /* Polygon markers            */
        String          gm_PgonKnotColorStr;
        String          gm_PointLineColorStr;   /* Point markers              */
        String          gm_PointKnotColorStr;
        /* Deep Frame */

} GtermPart;

typedef struct _GtermRec {
	CorePart	core;
	GtermPart	gterm;
} GtermRec;

typedef struct {int dummy;} GtermClassPart;

typedef struct _GtermClassRec {
	CoreClassPart	core_class;
	GtermClassPart	gterm_class;
} GtermClassRec;

extern GtermClassRec gtermClassRec;

#endif /* _GtermP_h */
