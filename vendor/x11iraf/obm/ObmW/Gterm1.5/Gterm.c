#include <stdio.h>
#include <ctype.h>
#include <math.h>

#include <X11/Xlib.h>
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/cursorfont.h>
#include <X11/Xaw/XawInit.h>
#include "GtermP.h"

/*
 * Gterm -- Graphics terminal widget.  This widget implements only the
 * window specific graphics output and graphics window input functions.
 * Protocol translation (e.g. Tek emulation) and i/o is done elsewhere;
 * see for example gtermio.c.
 */

#define	DefaultAlphaFont	3
#define	DefaultDialogFont	3
#define	DefaultMarkerTextFont	3
#define	ZOOM_TOL		0.0001

#define	CacheXImage		1				/* MF004 */

static Dimension defXDim = DEF_WIDTH;
static Dimension defYDim = DEF_HEIGHT;

/* Default translations for Gterm window. */
/* Omitted for now: Ctrl ~Meta <Btn3Down>: popup-menu(tekMenu) */

static char defaultGtermTranslations[] =
"\
		       <Btn1Down>:	m_create()                   \n\
		       <Btn2Down>:	crosshair(on)                \n\
		     <Btn2Motion>:	crosshair(on)                \n\
		         <Btn2Up>:	crosshair(off)               \n\
		    <EnterWindow>:	enter-window()               \n\
		    <LeaveWindow>:	leave-window()               \n\
		       <KeyPress>:	graphics-input()             \n\
		         <Motion>:	track-cursor()               \n\
";

/* Default translations when pointer is over a marker. */
static char defaultMarkerTranslations[] =
"\
	       !Shift <Btn1Motion>: 	m_rotateResize()  	     \n\
		      <Btn1Motion>: 	m_moveResize()               \n\
		 !Shift <Btn1Down>: 	m_raise()  m_markpos()       \n\
			<Btn1Down>: 	m_raise()  m_markposAdd()    \n\
			  <Btn1Up>: 	m_redraw() m_destroyNull()   \n\
			<Btn2Down>: 	m_lower()                    \n\
		    <Key>BackSpace: 	m_deleteDestroy()            \n\
		       <Key>Delete: 	m_deleteDestroy()            \n\
			<KeyPress>: 	m_input()                    \n\
			  <Motion>: 	track-cursor()               \n\
";

static XtResource resources[] = {
    {XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
	XtOffset(Widget,core.width), XtRDimension, (caddr_t)&defXDim},
    {XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
	XtOffset(Widget,core.height), XtRDimension, (caddr_t)&defYDim},

    {XtNalphaFont1, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.alphaFont1), XtRString, "nil2"},
    {XtNalphaFont2, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.alphaFont2), XtRString, "5x8"},
    {XtNalphaFont3, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.alphaFont3), XtRString, "6x10"},
    {XtNalphaFont4, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.alphaFont4), XtRString, "7x13"},
    {XtNalphaFont5, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.alphaFont5), XtRString, "8x13"},
    {XtNalphaFont6, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.alphaFont6), XtRString, "9x15"},
    {XtNalphaFont7, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.alphaFont7), XtRString, "9x15"},
    {XtNalphaFont8, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.alphaFont8), XtRString, "9x15"},

    {XtNdialogFont1, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.dialogFont1), XtRString, "nil2"},
    {XtNdialogFont2, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.dialogFont2), XtRString, "5x8"},
    {XtNdialogFont3, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.dialogFont3), XtRString, "6x13"},
    {XtNdialogFont4, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.dialogFont4), XtRString, "7x13"},
    {XtNdialogFont5, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.dialogFont5), XtRString, "8x13"},
    {XtNdialogFont6, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.dialogFont6), XtRString, "9x15"},
    {XtNdialogFont7, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.dialogFont7), XtRString, "9x15"},
    {XtNdialogFont8, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.dialogFont8), XtRString, "9x15"},

    {XtNdialogBgColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.dialogBgColor), XtRString, "yellow"},
    {XtNdialogFgColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.dialogFgColor), XtRString, "black"},
    {XtNidleCursorBgColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.idleCursorBgColor), XtRString, "white"},
    {XtNidleCursorFgColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.idleCursorFgColor), XtRString, "black"},
    {XtNbusyCursorBgColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.busyCursorBgColor), XtRString, "white"},
    {XtNbusyCursorFgColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.busyCursorFgColor), XtRString, "black"},
    {XtNginmodeCursorBgColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.ginmodeCursorBgColor), XtRString, "black"},
    {XtNginmodeCursorFgColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.ginmodeCursorFgColor), XtRString, "white"},
    {XtNginmodeBlinkInterval, XtCInt, XtRInt, sizeof(int),
        XtOffset(GtermWidget,gterm.ginmodeBlinkInterval), XtRImmediate, 0},
    {XtNcrosshairCursorColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.crosshairCursorColor), XtRString, "red"},

    {XtNidleCursor, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.idleCursor), XtRString, "plus"},
    {XtNbusyCursor, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.busyCursor), XtRString, "watch"},
    {XtNginmodeCursor, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.ginmodeCursor), XtRString, "full_crosshair"},
    {XtNwarpCursor, XtCBoolean, XtRBoolean, sizeof(Boolean),
	XtOffset(GtermWidget,gterm.warpCursor), XtRImmediate,
	(caddr_t)DEF_WARPCURSOR},
    {XtNraiseWindow, XtCBoolean, XtRBoolean, sizeof(Boolean),
	XtOffset(GtermWidget,gterm.raiseWindow), XtRImmediate,
	(caddr_t)DEF_RAISEWINDOW},
    {XtNdeiconifyWindow, XtCBoolean, XtRBoolean, sizeof(Boolean),
	XtOffset(GtermWidget,gterm.deiconifyWindow), XtRImmediate,
	(caddr_t)DEF_DEICONIFYWINDOW},
    {XtNuseTimers, XtCBoolean, XtRBoolean, sizeof(Boolean),
	XtOffset(GtermWidget,gterm.useTimers), XtRImmediate,
	(caddr_t)DEF_USETIMERS},

    {XtNcolor0, XtCBackground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.color0), XtRString, "black"},
    {XtNcolor1, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.color1), XtRString, "white"},
    {XtNcolor2, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.color2), XtRString, "red"},
    {XtNcolor3, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.color3), XtRString, "green"},
    {XtNcolor4, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.color4), XtRString, "blue"},
    {XtNcolor5, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.color5), XtRString, "cyan"},
    {XtNcolor6, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.color6), XtRString, "yellow"},
    {XtNcolor7, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.color7), XtRString, "magenta"},
    {XtNcolor8, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.color8), XtRString, "purple"},
    {XtNcolor9, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.color9), XtRString, "darkslategray"},

    {XtNcopyOnResize, XtCBoolean, XtRBoolean, sizeof(Boolean),
	XtOffset(GtermWidget,gterm.copyOnResize), XtRImmediate,
	(caddr_t)DEF_COPYONRESIZE},
    {XtNcmapName, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.cmapName), XtRImmediate,
	(caddr_t)"default"},
    {XtNcmapInitialize, XtCBoolean, XtRBoolean, sizeof(Boolean),
	XtOffset(GtermWidget,gterm.cmapInitialize), XtRImmediate,
	(caddr_t)FALSE},
    {XtNbasePixel, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.base_pixel), XtRImmediate,
	(caddr_t)DEF_BASEPIXEL},
    {XtNcmapUpdate, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.cmapUpdate), XtRImmediate,
	(caddr_t)DEF_CMAPUPDATE},
    {XtNcmapShadow, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.cmapShadow), XtRImmediate,
	(caddr_t)DEF_CMAPSHADOW},
    {XtNcmapInterpolate, XtCBoolean, XtRBoolean, sizeof(Boolean),
	XtOffset(GtermWidget,gterm.cmapInterpolate), XtRImmediate,
	(caddr_t)True},
    {XtNcacheRasters, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.cacheRasters), XtRImmediate,
	(caddr_t)"whenNeeded"},
    {XtNmaxRasters, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.maxRasters), XtRImmediate,
	(caddr_t)MAX_RASTERS},
    {XtNmaxMappings, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.maxMappings), XtRImmediate,
	(caddr_t)MAX_MAPPINGS},
    {XtNmaxColors, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.maxColors), XtRImmediate,
	(caddr_t)DEF_MAXCOLORS},

    {XtNmarkerTranslations, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.gm_translations), XtRImmediate,
	(caddr_t)defaultMarkerTranslations},
    {XtNdefaultMarker, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.gm_defaultMarker), XtRImmediate,
	(caddr_t)"rectangle"},
    {XtNnearEdge, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_nearEdge), XtRImmediate,
	(caddr_t)E_DIST},
    {XtNnearVertex, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_nearVertex), XtRImmediate,
	(caddr_t)V_DIST},

    {XtNmarkerLineWidth, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_lineWidth), XtRImmediate,
	(caddr_t)1},
    {XtNmarkerLineStyle, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_lineStyle), XtRImmediate,
	(caddr_t)LineSolid},
    {XtNmarkerFill, XtCBoolean, XtRBoolean, sizeof(Boolean),
	XtOffset(GtermWidget,gterm.gm_fill), XtRImmediate,
	(caddr_t)False},
    {XtNmarkerFillColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_fillColor), XtRString,
	"SlateGray"},
    {XtNmarkerFillBgColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_fillBgColor), XtRString,
	"black"},
    {XtNmarkerFillStyle, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_fillStyle), XtRImmediate,
	(caddr_t)FillSolid},
    {XtNxorFill, XtCBoolean, XtRBoolean, sizeof(Boolean),
	XtOffset(GtermWidget,gterm.gm_xorFill), XtRImmediate,
	(caddr_t)False},
    {XtNxorFillColor, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_xorFillColor), XtRImmediate,
	(caddr_t)2},
    {XtNxorFillBgColor, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_xorFillBgColor), XtRImmediate,
	(caddr_t)255},
    {XtNmarkerHighlightWidth, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_highlightWidth), XtRImmediate,
	(caddr_t)2},
    {XtNmarkerHighlightColor, XtCForeground, XtRPixel, sizeof(Pixel),
	XtOffset(GtermWidget,gterm.gm_highlightColor), XtRString,
	"green"},
    {XtNmarkerCursorFgColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_cursorFgColor), XtRString,
	"yellow"},
    {XtNmarkerCursorBgColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_cursorBgColor), XtRString,
	"black"},

    {XtNmarkerLineLineColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_LineLineColor), XtRString,
	"green"},
    {XtNmarkerLineKnotColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_LineKnotColor), XtRString,
	"blue"},
    {XtNmarkerLineKnotSize, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_LineKnotSize), XtRImmediate,
	(caddr_t)5},

    {XtNmarkerTextLineColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_TextLineColor), XtRString,
	"green"},
    {XtNmarkerTextColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_TextColor), XtRString,
	"yellow"},
    {XtNmarkerTextBgColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_TextBgColor), XtRString,
	"SlateGray"},
    {XtNmarkerTextBorder, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_TextBorder), XtRImmediate,
	(caddr_t)2},
    {XtNmarkerTextFont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
        XtOffset(GtermWidget,gterm.gm_TextFont), XtRString,
	"6x13"},
    {XtNmarkerTextString, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.gm_TextString), XtRImmediate,
	(caddr_t)NULL},

    {XtNmarkerRectLineColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_RectLineColor), XtRString,
	"green"},
    {XtNmarkerRectKnotColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_RectKnotColor), XtRString,
	"blue"},
    {XtNmarkerRectKnotSize, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_RectKnotSize), XtRImmediate,
	(caddr_t)0},
    {XtNmarkerBoxLineColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_BoxLineColor), XtRString,
	"green"},
    {XtNmarkerBoxKnotColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_BoxKnotColor), XtRString,
	"blue"},
    {XtNmarkerBoxKnotSize, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_BoxKnotSize), XtRImmediate,
	(caddr_t)0},
    {XtNmarkerCircleLineColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_CircleLineColor), XtRString,
	"green"},
    {XtNmarkerCircleKnotColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_CircleKnotColor), XtRString,
	"blue"},
    {XtNmarkerCircleKnotSize, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_CircleKnotSize), XtRImmediate,
	(caddr_t)0},
    {XtNmarkerEllipseLineColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_EllipseLineColor), XtRString,
	"green"},
    {XtNmarkerEllipseKnotColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_EllipseKnotColor), XtRString,
	"blue"},
    {XtNmarkerEllipseKnotSize, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_EllipseKnotSize), XtRImmediate,
	(caddr_t)0},
    {XtNmarkerPgonLineColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_PgonLineColor), XtRString,
	"green"},
    {XtNmarkerPgonKnotColor, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffset(GtermWidget,gterm.gm_PgonKnotColor), XtRString,
	"blue"},
    {XtNmarkerPgonKnotSize, XtCInt, XtRInt, sizeof(int),
	XtOffset(GtermWidget,gterm.gm_PgonKnotSize), XtRImmediate,
	(caddr_t)5},
};

/* extern void HandlePopupMenu(); */
static Boolean SetValues();
static void Initialize(), Realize(), Destroy(), Redisplay(), Resize();
static void HandleIgnore(), HandleGraphicsInput(), HandleDisplayCrosshair();
static void HandleSoftReset(), HandleGraphicsContext();
static void HandleEnterWindow(), HandleLeaveWindow();
static void color_crosshair(), color_ginmodeCursor();
static void HandleTrackCursor();
static void savepos(), blink_cursor();
static void mp_linkafter(), mp_unlink();

Marker GmSelect();
static void M_create(), GtMarkerFree();
static void gm_focusin(), gm_focusout(), gm_refocus();
static void gm_request_translations(), gm_load_translations();
static int gm_curpos();

static set_default_color_index();
static inherit_default_colormap();
static update_default_colormap();
static update_transients(), update_cursor();
static request_colormap_focus(), restore_colormap_focus();
static refresh_source(), refresh_destination(), get_regions();
static get_rects(), scale_zoom(), scale_intzoom(), scale_boxcar();
static lw_convolve(), bx_boxcar(), bx_extract(), bx_interp();
static mf_getpixel(), mf_getinten();
static scale_lowpass(), scale_nearest(), scale_bilinear();
static save_mapping(), load_mapping(), get_pixel_mapping();
static update_mapping(), free_mapping(), valid_mapping(), rect_intersect();
static initialize_mapping(), draw_crosshair(), erase_crosshair();
static DrawContext get_draw_context();
static invalidate_draw_context();
static XPoint *mapVector();
static Colormap get_colormap();
static Cursor get_cursor();
static void init_iomap(), invalidate_cmap();
static void gm_rotate_indicator();				/* MF020 */
static Pixel get_pixel(), *get_cmap_in(), *get_cmap_out();

static void NewCachedXImage();					/* MF004 */
static void DestroyCachedXImage();				/* MF004 */
static XImage *GetCachedXImage();				/* MF004 */

extern double atof();

static XtActionsRec gtermActionsList[] = {
	{ "ignore",			HandleIgnore },
	{ "graphics-input",		HandleGraphicsInput },
	{ "crosshair",			HandleDisplayCrosshair },
	{ "track-cursor",		HandleTrackCursor },
	{ "enter-window",		HandleEnterWindow },
	{ "leave-window",		HandleLeaveWindow },
/*	{ "popup-menu",			HandlePopupMenu },	*/
	{ "reset",             		HandleSoftReset },
	{ "m_create",			M_create },
};

GtermClassRec gtermClassRec = {
    { /* core fields */
    /* superclass		*/	&widgetClassRec,
    /* class_name		*/	"Gterm",
    /* widget_size		*/	sizeof(GtermRec),
    /* class_initialize		*/	XawInitializeWidgetSet,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	False,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	gtermActionsList,
    /* num_actions		*/	XtNumber(gtermActionsList),
    /* resources		*/	resources,
    /* resource_count		*/	XtNumber(resources),
    /* xrm_class		*/	(XrmClass)NULL,
    /* compress_motion		*/	True,
    /* compress_exposure	*/	True,
    /* compress_enterleave	*/	True,
    /* visible_interest		*/	False,
    /* destroy			*/	Destroy,
    /* resize			*/	Resize,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	defaultGtermTranslations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
    }
};

WidgetClass gtermWidgetClass = (WidgetClass) &gtermClassRec;
#define	abs(a)		(((a)<0)?(-(a)):(a))
#define max(a,b)	((a)>=(b)?(a):(b))
#define min(a,b)	((a)<(b)?(a):(b))
#define ERR		(-1)
#define OK		0
#define SQR(a)		((a)*(a))

/*
 * Widget class procedures.
 * --------------------------
 */

/* ARGSUSED */
static void
Initialize (request, new)
    Widget request, new;
{
    register GtermWidget w = (GtermWidget)new;
    register GC gc;

    XColor fg_color, bg_color;
    XFontStruct **fp;
    Font cursor_font;
    Display *display;
    Screen *screen;
    Pixel *pp;
    int i;

    w->gterm.resetCallback = NULL;
    w->gterm.resizeCallback = NULL;
    w->gterm.inputCallback = NULL;

    w->gterm.display = display = XtDisplay (w);
    w->gterm.screen = screen = XtScreen (w);
    w->gterm.root = RootWindowOfScreen (screen);
    XtVaSetValues ((Widget)w, XtNbackground, (XtArgVal)w->gterm.color0, NULL);

    /* Initialize color map. */
    pp = &w->gterm.color0;
    for (i=0;  i < SZ_STATIC_CMAP;  i++)
	w->gterm.color[i].pixel = w->gterm.cmap[i] = *pp++;
    for (   ;  i < MAX_SZCMAP;  i++) {
	memset ((char *)&w->gterm.color[i], 0, sizeof(XColor));
	w->gterm.color[i].pixel = w->gterm.cmap[i] = i;
    }
    XQueryColors (display, w->core.colormap, w->gterm.color, SZ_STATIC_CMAP);
    w->gterm.ncolors = SZ_STATIC_CMAP;
    init_iomap (w);

    w->gterm.useDefaultCM = (strcmp (w->gterm.cmapName, "default") == 0);
    w->gterm.haveColormap = w->gterm.useDefaultCM;
    w->gterm.cmapLastUpdate = 0;
    w->gterm.cmapLastShadow = 0;
    w->gterm.in_window = 0;
    w->gterm.n_wmWindows = 0;					/* MF012 */

    /* Get clear pixmap GC. */
    gc = XCreateGC (display, w->gterm.root, 0, NULL);
    XSetBackground (display, gc, w->gterm.color0);
    XSetForeground (display, gc, w->gterm.color0);
    w->gterm.clearGC = gc;
    XSetGraphicsExposures (display, gc, 0);			/* MF029 */

    /* Get expose GC. */
    gc = XCreateGC (display, w->gterm.root, 0, NULL);
    w->gterm.exposeGC = gc;
    XSetGraphicsExposures (display, gc, 0);			/* MF029 */

    /* Get graphics drawing GC. */
    gc = XCreateGC (display, w->gterm.root, 0, NULL);
    XSetBackground (display, gc, w->gterm.color0);
    XSetForeground (display, gc, w->gterm.color1);
    XSetLineAttributes (display, gc, 1, LineSolid, CapButt, JoinMiter);
    w->gterm.drawGC = gc;

    /* Get dialog box GC. */
    gc = XCreateGC (display, w->gterm.root, 0, NULL);
    XSetBackground (display, gc, w->gterm.dialogBgColor);
    XSetForeground (display, gc, w->gterm.dialogFgColor);
    /* XSetFunction   (display, gc, GXcopyInverted); */
    w->gterm.dialogGC = gc;

    /* Get crosshair cursor GC. */
    gc = XCreateGC (display, w->gterm.root, 0, NULL);
    XSetBackground (display, gc, w->gterm.color0);
    XSetForeground (display, gc, w->gterm.crosshairCursorColor);
    XSetLineAttributes (display, gc, 0, LineSolid, CapButt, JoinMiter);
    w->gterm.cursorGC = gc;

    /* Get special cursors. */
    bg_color.pixel = w->gterm.idleCursorBgColor;
    XQueryColor (display, w->core.colormap, &bg_color);
    fg_color.pixel = w->gterm.idleCursorFgColor;
    XQueryColor (display, w->core.colormap, &fg_color);
    w->gterm.idle_cursor = get_cursor (w, w->gterm.idleCursor);
    XRecolorCursor (display, w->gterm.idle_cursor, &fg_color, &bg_color);
								/* MF030 */

    bg_color.pixel = w->gterm.busyCursorBgColor;
    XQueryColor (display, w->core.colormap, &bg_color);
    fg_color.pixel = w->gterm.busyCursorFgColor;
    XQueryColor (display, w->core.colormap, &fg_color);
    w->gterm.busy_cursor = get_cursor (w, w->gterm.busyCursor);
    XRecolorCursor (display, w->gterm.busy_cursor, &fg_color, &bg_color);
								/* MF030 */

    bg_color.pixel = w->gterm.color0;
    XQueryColor (display, w->core.colormap, &bg_color);
    fg_color.pixel = w->gterm.crosshairCursorColor;
    XQueryColor (display, w->core.colormap, &fg_color);
    cursor_font = XLoadFont (display, "cursor");
    w->gterm.crosshair_cursor = XCreateGlyphCursor (display,
	cursor_font, cursor_font, XC_crosshair, XC_crosshair,
	&fg_color, &bg_color);

    w->gterm.ginmodeCursor = XtNewString (w->gterm.ginmodeCursor);
    if (strcmp (w->gterm.ginmodeCursor, "full_crosshair") != 0) {
	bg_color.pixel = w->gterm.ginmodeCursorBgColor;
	XQueryColor (display, w->core.colormap, &bg_color);
	fg_color.pixel = w->gterm.ginmodeCursorFgColor;
	XQueryColor (display, w->core.colormap, &fg_color);
	w->gterm.ginmode_cursor = get_cursor (w, w->gterm.ginmodeCursor);
 	XRecolorCursor (display,
	    w->gterm.ginmode_cursor, &fg_color, &bg_color);	/* MF030 */
	w->gterm.ginmodeColors[0] = bg_color;
	w->gterm.ginmodeColors[1] = fg_color;
    } else
	w->gterm.ginmode_cursor = w->gterm.crosshair_cursor;

    w->gterm.full_crosshair = 
	(strcmp (w->gterm.ginmodeCursor, "full_crosshair") == 0);

    /* Make sure we have all the fonts we need. */
    for (fp = &w->gterm.alphaFont1, i=0;  i < NAlphaFonts;  i++, fp++) {
	if (*fp == NULL) {
	    *fp = XQueryFont (display,
		XGContextFromGC (DefaultGCOfScreen(screen)));
	}
	w->gterm.alpha_fonts[i] = *fp;
    }
    for (fp = &w->gterm.dialogFont1, i=0;  i < NDialogFonts;  i++, fp++) {
	if (*fp == NULL) {
	    *fp = XQueryFont (display,
		XGContextFromGC (DefaultGCOfScreen(screen)));
	}
	w->gterm.dialog_fonts[i] = *fp;
    }

    /* Raster initialization. */
    w->gterm.rasters = NULL;
    w->gterm.nrasters = 0;
    w->gterm.mappings = NULL;
    w->gterm.nmappings = 0;
    w->gterm.mp_head = NULL;
    w->gterm.mp_tail = NULL;
    w->gterm.colormaps = NULL;
    w->gterm.wa_defined = 0;
    memset ((char *)&w->gterm.draw, 0, sizeof (struct drawContext));

    /* Marker initialization. */
    w->gterm.gm_head = NULL;
    w->gterm.gm_tail = NULL;
    w->gterm.gm_create = NULL;
    w->gterm.gm_active = NULL;
    w->gterm.defTranslations = NULL;
    w->gterm.nauxTrans = 0;
    w->gterm.gm_defTranslations = NULL;
    w->gterm.gm_curTranslations = NULL;
    w->gterm.gm_reqTranslations = NULL;
    w->gterm.gm_timer_id = (XtIntervalId) NULL;
    w->gterm.gm_initialized = False;

    /* Set defaults (some of these are clobbered anyway by Realize/Resize). */
    w->gterm.raster = 0;
    w->gterm.cur_x = 0;
    w->gterm.cur_y = 0;
    w->gterm.last_x = 0;
    w->gterm.last_y = 0;
    w->gterm.cursor_drawn = 0;
    w->gterm.cursor_type = GtIdleCursor;
    w->gterm.pixmap = (Pixmap)NULL;
    w->gterm.d_pixmap = (Pixmap)NULL;
    w->gterm.preserve_screen = 0;
    w->gterm.preserve_valid = 0;
    w->gterm.d_saved = 0;
    w->gterm.alpha_font = DefaultAlphaFont;
    w->gterm.dialog_font = DefaultDialogFont;
    w->gterm.optcols = 80;
    w->gterm.optrows = 35;
    w->gterm.line_width = 1;
    w->gterm.data_level = GtSet;
    w->gterm.line_style = GtSolid;
    w->gterm.fill_type  = GtSolid;
    set_default_color_index (w);

    /* Disable input until window is ready. */
    w->gterm.delay = 1;
}

static void
Realize (gw, valueMask, attrs)
    Widget gw;
    XtValueMask *valueMask;
    XSetWindowAttributes *attrs;
{
    GtermWidget w = (GtermWidget) gw;

    /* Set default window size. */
    XtMakeResizeRequest (gw, w->core.width, w->core.height,
	&w->core.width, &w->core.height);

    /* Should define pseudocolor visual here, if truecolor or directcolor
     * default visual.
     */

    XtCreateWindow (gw, InputOutput, (Visual *)CopyFromParent,
	*valueMask, attrs);

    w->gterm.window = XtWindow (gw);
    w->gterm.old_width = w->gterm.xres = w->core.width;
    w->gterm.old_height = w->gterm.yres = w->core.height;

    GtRasterInit (w);
    GtMarkerInit (w);

    if (w->core.visible)
	XDefineCursor (w->gterm.display, w->gterm.window,
	    w->gterm.cursor = w->gterm.idle_cursor);

    Resize (gw);
}

static void
Destroy (gw)
    Widget gw;
{
    GtermWidget w = (GtermWidget) gw;
    register GtCallback *cb, *cb_next;
    Display *display = w->gterm.display;

    /* Get rid of any raster stuff. */
    GtRasterInit (w);						/* MF008 */
    XtFree ((char *)w->gterm.rasters);
    XtFree ((char *)w->gterm.mappings);

    /* Destroy any markers. */
    GtMarkerFree (w);

    /* Can't use XtDestroyGC here; the source says it is broken and will
     * work only for applications that have only 1 display, and we have 2.
     * Also the documentation in Asente&Swick documents the calling sequence
     * incorrectly.
     */
    XFreeGC (display, w->gterm.clearGC);
    XFreeGC (display, w->gterm.exposeGC);
    XFreeGC (display, w->gterm.drawGC);
    XFreeGC (display, w->gterm.dialogGC);
    XFreeGC (display, w->gterm.cursorGC);

    /* This one also proves problematic.  When there are multiple gterm
     * widgets allocating the same cursor, succeeding calls for the same
     * cursor return the same cursor ID.  When these widgets are later
     * destroyed, the first XFreeCursor succeeds but subsequent ones find
     * the referenced cursor undefined and the application boms with a
     * BadCursor error.  This must be some problem with reference counts
     * in the X server.  Cursors use minor amounts of resources and they
     * will probably be freed anyway when the display is closed, so we just
     * leave them defined here.
     *
    XFreeCursor (display, w->gterm.idle_cursor);
    XFreeCursor (display, w->gterm.busy_cursor);
    XFreeCursor (display, w->gterm.crosshair_cursor);
    if (w->gterm.ginmode_cursor != w->gterm.crosshair_cursor)
	XFreeCursor (display, w->gterm.ginmode_cursor);
     */

    if (w->gterm.pixmap)
	XFreePixmap (w->gterm.display, w->gterm.pixmap);
    if (w->gterm.d_pixmap)
	XFreePixmap (w->gterm.display, w->gterm.d_pixmap);

    /* Destroy callback lists. */
    for (cb = w->gterm.resetCallback;  cb;  cb = cb_next) {
	cb_next = cb->next;
	XtFree ((char *)cb);
    }
    for (cb = w->gterm.resizeCallback;  cb;  cb = cb_next) {
	cb_next = cb->next;
	XtFree ((char *)cb);
    }
    for (cb = w->gterm.inputCallback;  cb;  cb = cb_next) {
	cb_next = cb->next;
	XtFree ((char *)cb);
    }
    w->gterm.resetCallback = NULL;
    w->gterm.resizeCallback = NULL;
    w->gterm.inputCallback = NULL;

    XtFree (w->gterm.ginmodeCursor);
}

static void
Resize (gw)
    Widget gw;
{
    GtermWidget w = (GtermWidget) gw;
    register GtCallback *cb;
    int char_width, char_height, char_base;
    int bestfont, fonterr, dx, dy, i;
    unsigned int width, height, u_junk;
    GtCallback cbl[128];
    XFontStruct *fp;
    int ncb, junk;
    Pixmap pixmap;
    Window root;

    if (!XtIsRealized(gw))
	return;

    /* Create new pixmap. */
    pixmap = XCreatePixmap (w->gterm.display, w->gterm.window,
	w->core.width + 1, w->core.height + 1, w->core.depth);
    if (pixmap)
	XFillRectangle (w->gterm.display, pixmap,
	    w->gterm.clearGC, 0, 0, w->core.width, w->core.height);

    /* Copy old pixmap into new and free old pixmap. */
    if (w->gterm.pixmap) {
	XGetGeometry (w->gterm.display, w->gterm.pixmap,
	    &root, &junk, &junk, &width, &height, &u_junk, &u_junk);
	XSetClipMask (w->gterm.display, w->gterm.exposeGC, None);
	if (w->gterm.copyOnResize)
	    XCopyArea (w->gterm.display, w->gterm.pixmap, pixmap,
		w->gterm.exposeGC, 0, 0, width-1, height-1, 0, 0);
	XFreePixmap (w->gterm.display, w->gterm.pixmap);
    }

    /* Install new pixmap. */
    w->gterm.pixmap = pixmap;
    w->gterm.preserve_valid = 0;

    /* Redraw window. */
    if (w->gterm.pixmap) {
	XCopyArea (w->gterm.display, w->gterm.pixmap, w->gterm.window,
	    w->gterm.exposeGC, 0, 0, w->core.width, w->core.height, 0, 0);
    }

    /* Pick best alpha font. */
    bestfont = 0;  fonterr = 9999;
    for (i=0;  i < NAlphaFonts;  i++) {
	fp = w->gterm.alpha_fonts[i];
	char_width = fp->max_bounds.width;
	char_height = fp->max_bounds.ascent + fp->max_bounds.descent;
	dx = (((int)w->core.width / char_width) - w->gterm.optcols) * 2;
	dy = ((int)w->core.height / char_height) - w->gterm.optrows;
	if (abs(dx) + abs(dy) < fonterr) {
	    bestfont = i;
	    fonterr = abs(dx) + abs(dy);
	}
    }

    w->gterm.alpha_font = bestfont;
    fp = w->gterm.alpha_fonts[w->gterm.alpha_font];
    XSetFont (w->gterm.display, w->gterm.drawGC, fp->fid);

    /* Pick best dialog font. */
    bestfont = 0;  fonterr = 9999;
    for (i=0;  i < NDialogFonts;  i++) {
	fp = w->gterm.dialog_fonts[i];
	char_width = fp->max_bounds.width;
	dx = ((int)w->core.width / char_width) - 80;
	if (abs(dx) < fonterr) {
	    bestfont = i;
	    fonterr = abs(dx);
	}
    }

    w->gterm.dialog_font = bestfont;
    fp = w->gterm.dialog_fonts[w->gterm.dialog_font];
    char_height = fp->max_bounds.ascent + fp->max_bounds.descent;
    char_base = fp->max_bounds.ascent;

    w->gterm.d_xoff = 2;
    w->gterm.d_yoff = w->core.height - char_height - 2;
    w->gterm.d_height = char_height;
    XSetFont (w->gterm.display, w->gterm.dialogGC, fp->fid);

    /* Create dialog save area pixmap. */
    if (w->gterm.d_pixmap)
	XFreePixmap (w->gterm.display, w->gterm.d_pixmap);
    w->gterm.d_pixmap = XCreatePixmap (w->gterm.display, w->gterm.window,
	w->core.width, char_height, w->core.depth);
    w->gterm.d_saved = 0;

    /* Adjust cursor position to allow for change in window size. */
    w->gterm.cur_x = w->gterm.cur_x * (int)w->core.width / w->gterm.old_width;
    w->gterm.cur_y = w->gterm.cur_y * (int)w->core.height / w->gterm.old_height;
    w->gterm.old_width = w->core.width;
    w->gterm.old_height = w->core.height;
    if (w->gterm.cursor_type == GtGinmodeCursor) {
	XWarpPointer (w->gterm.display, w->gterm.window, w->gterm.window,
	    0,0,0,0, w->gterm.cur_x, w->gterm.cur_y);
	update_cursor (w);
    }

    /* Raster descriptor 0 must track the window size. */
    if (w->gterm.rasters) {
	Raster rp = &w->gterm.rasters[0];
	rp->width = w->core.width;
	rp->height = w->core.height;
    }

    /* Mark gterm widget ready for further client input. */
    w->gterm.delay = 0;

    /* Call any resize callbacks.  Callbacks can delete or add callbacks,
     * so make a copy of the callback list first.
     */
    for (cb = w->gterm.resizeCallback, ncb=0;  cb;  cb = cb->next)
	cbl[ncb++] = *cb;
    for (i=0;  i < ncb;  i++)
	(*cbl[i].proc) (cbl[i].client_data, w);
}

/* ARGSUSED */
static void
Redisplay (gw, event, region)
    Widget gw;
    XEvent *event;
    Region region;
{
    register GtermWidget w = (GtermWidget) gw;
    register XExposeEvent *ev = (XExposeEvent *)event;
    int x, y, width, height;

    if (!XtIsRealized (gw))
	return;

    if (event) {
	x = ev->x;
	y = ev->y;
	width = ev->width;
	height = ev->height;
    } else {
	x = 0;
	y = 0;
	width = w->core.width;
	height = w->core.height;
    }

    if (w->gterm.pixmap) {
	/* Clipping with the region argument does not work properly with
	 * the OpenLook server for some reason - the clip region is one
	 * pixel too small on the right and bottom.  Until the reason for
	 * this becomes clear, we use the bounding box provided in the
	 * Expose event to roughly clip the refresh.
	 *
	XSetClipOrigin (w->gterm.display, w->gterm.exposeGC, 0, 0);
	XSetRegion (w->gterm.display, w->gterm.exposeGC, region);
	 */

	XSetClipMask (w->gterm.display, w->gterm.exposeGC, None);
	XCopyArea (w->gterm.display, w->gterm.pixmap, w->gterm.window,
	    w->gterm.exposeGC, x, y, width, height, x, y);
    }

    update_transients (w, region);

    /* A dummy expose event is used to ensure that the resize delay is
     * cleared, in the event that the resize request is not granted.
     */
    if (ev && ev->send_event)
	w->gterm.delay = 0;
}

/* ARGSUSED */
static Boolean
SetValues (current, request, set)
    Widget current, request, set;
{
    GtermWidget old = (GtermWidget) current;
    GtermWidget req = (GtermWidget) request;
    register GtermWidget w = (GtermWidget) set;
    Display *display = w->gterm.display;
    Boolean redisplay = False;
    register GC gc;

    if (old->gterm.dialogBgColor != req->gterm.dialogBgColor) {
	gc = w->gterm.dialogGC;
	XSetBackground (display, gc, w->gterm.dialogBgColor);
    }
    if (old->gterm.dialogFgColor != req->gterm.dialogFgColor) {
	gc = w->gterm.dialogGC;
	XSetForeground (display, gc, w->gterm.dialogFgColor);
    }

    if (old->gterm.ginmodeCursor != req->gterm.ginmodeCursor) {
	static char *full_crosshair = "full_crosshair";

	XtFree (old->gterm.ginmodeCursor);
	w->gterm.ginmodeCursor = XtNewString (w->gterm.ginmodeCursor);

	erase_crosshair (w);
	w->gterm.full_crosshair = 
	    (strcmp (w->gterm.ginmodeCursor, full_crosshair) == 0);

	if (w->gterm.full_crosshair) {
	    w->gterm.ginmode_cursor = w->gterm.crosshair_cursor;
	    color_crosshair (w);
	} else {
	    w->gterm.ginmode_cursor = get_cursor (w, w->gterm.ginmodeCursor);
	    color_ginmodeCursor (w);
	}

	if (w->gterm.cursor_type == GtGinmodeCursor && w->core.visible)
	    XDefineCursor (display, w->gterm.window,
		w->gterm.cursor = w->gterm.ginmode_cursor);
    }

    if (old->gterm.crosshairCursorColor != req->gterm.crosshairCursorColor) {
	color_crosshair (w);
    }

    if (old->gterm.ginmodeCursorBgColor != req->gterm.ginmodeCursorBgColor ||
	old->gterm.ginmodeCursorFgColor != req->gterm.ginmodeCursorFgColor) {
	color_ginmodeCursor (w);
    }

    return (XtIsRealized(current) ? redisplay : False);
}

static void
color_crosshair (w)
    register GtermWidget w;
{
    register Display *display = w->gterm.display;
    XColor fg_color, bg_color;
    Colormap defcmap;
    register GC gc;

    erase_crosshair (w);
    defcmap = DefaultColormapOfScreen (w->gterm.screen);
    bg_color.pixel = w->gterm.color0;
    XQueryColor (display, defcmap, &bg_color);
    fg_color.pixel = w->gterm.crosshairCursorColor;
    XQueryColor (display, defcmap, &fg_color);

    gc = w->gterm.cursorGC;
    XSetForeground (display, gc, w->gterm.crosshairCursorColor);
    XRecolorCursor (display, w->gterm.crosshair_cursor, &fg_color, &bg_color);

    w->gterm.ginmodeColors[0] = bg_color;
    w->gterm.ginmodeColors[1] = fg_color;

    update_cursor (w);
}

static void
color_ginmodeCursor (w)
    register GtermWidget w;
{
    register Display *display = w->gterm.display;
    XColor fg_color, bg_color;
    Colormap defcmap;
 
    defcmap = DefaultColormapOfScreen (w->gterm.screen);
    bg_color.pixel = w->gterm.ginmodeCursorBgColor;
    XQueryColor (display, defcmap, &bg_color);
    fg_color.pixel = w->gterm.ginmodeCursorFgColor;
    XQueryColor (display, defcmap, &fg_color);

    XRecolorCursor (display, w->gterm.ginmode_cursor, &fg_color, &bg_color);
    w->gterm.ginmodeColors[0] = bg_color;
    w->gterm.ginmodeColors[1] = fg_color;
}

/*
 * Action procedures.
 * -----------------------
 */

/* ARGSUSED */
static void HandleIgnore (widget, event, params, param_count)
    Widget widget;
    XEvent *event;              /* unused */
    String *params;             /* unused */
    Cardinal *param_count;      /* unused */
{
    /* ignore an event */
}

/* ARGSUSED */
static void HandleGraphicsInput (widget, event, params, param_count)
    Widget widget;
    XEvent *event;              /* unused */
    String *params;             /* unused */
    Cardinal *param_count;      /* unused */
{
    register GtermWidget w = (GtermWidget)widget;
    register XKeyEvent *ev = (XKeyEvent *) event;
    register GtCallback *cb;
    GtCallback cbl[128];
    int ncb, i;

    /* Call any resize callbacks.  Callbacks can delete or add callbacks,
     * so make a copy of the callback list first.
     */
    for (cb = w->gterm.inputCallback, ncb=0;  cb;  cb = cb->next)
	cbl[ncb++] = *cb;
    for (i=0;  i < ncb;  i++)
	(*cbl[i].proc) (cbl[i].client_data, w, event);
}

/* ARGSUSED */
static void HandleDisplayCrosshair (widget, event, params, nparams)
    Widget widget;
    XEvent *event;              /* unused */
    String *params;             /* unused */
    Cardinal *nparams;      /* unused */
{
    register GtermWidget w = (GtermWidget)widget;
    XButtonEvent *ev = &event->xbutton;

    /* Ignore if cursor is in a marker. */
    if (w->gterm.gm_active)
	return;

    if (*nparams && strcmp (params[0], "on") == 0) {
	erase_crosshair (w);
	XDefineCursor (w->gterm.display, w->gterm.window,
	    w->gterm.crosshair_cursor);
	draw_crosshair (w, ev->x, ev->y);
    } else if (*nparams && strcmp (params[0], "off") == 0) {
	erase_crosshair (w);
	XDefineCursor (w->gterm.display, w->gterm.window, w->gterm.cursor);
    }
}

/* ARGSUSED */
static void HandleTrackCursor (widget, event, params, param_count)
    Widget widget;
    XEvent *event;              /* unused */
    String *params;             /* unused */
    Cardinal *param_count;      /* unused */
{
    register GtermWidget w = (GtermWidget)widget;
    XMotionEvent *ev = &event->xmotion;
    gmSelection what;
    Marker gm;

    savepos (w, (XEvent *)ev);

    if ((gm = GmSelect (w, ev->x, ev->y, &what)))
	gm_focusin (w, gm, &what);
    else if (w->gterm.gm_active)
	gm_focusout (w, 1);

    if (w->gterm.cursor_type == GtGinmodeCursor) 
	if (w->gterm.full_crosshair) {
	    erase_crosshair (w);
	    draw_crosshair (w, ev->x, ev->y);

	    /* Flushing here keeps cursor tracking synchronous and tends
	     * to aid motion compression, by preventing crosshair draw
	     * requests from being queued up for transmission to the
	     * server.
	     */
	    XFlush (w->gterm.display);

	} else {
	    w->gterm.cur_x = ev->x;
	    w->gterm.cur_y = ev->y;
	}
}

/* ARGSUSED */
static void HandleEnterWindow (widget, event, params, param_count)
    Widget widget;
    XEvent *event;              /* unused */
    String *params;             /* unused */
    Cardinal *param_count;      /* unused */
{
    register GtermWidget w = (GtermWidget)widget;
    register XEnterWindowEvent *ev = (XEnterWindowEvent *) event;

/*printf ("HandleEnterWindow....");*/
    if (!w->gterm.useDefaultCM && w->gterm.haveColormap) {
	int update = w->gterm.cmapUpdate;

	/* To avoid excessive server queries the colormap is only updated
	 * every so often.  Updating is disabled if cmapUpdate is set to zero.
	if (update && ev->time - w->gterm.cmapLastUpdate > update * 1000) {
	 */
	if (update) {
	    inherit_default_colormap (w);
	    w->gterm.cmapLastUpdate = ev->time;
	}

	/* Advise the window manager to load our colormap. */
/*printf ("requesting focus....");*/
	request_colormap_focus (w);
    }
/*printf ("\n");*/

    w->gterm.in_window++;
}

/* ARGSUSED */
static void HandleLeaveWindow (widget, event, params, param_count)
    Widget widget;
    XEvent *event;              /* unused */
    String *params;             /* unused */
    Cardinal *param_count;      /* unused */
{
    register GtermWidget w = (GtermWidget)widget;
    register XLeaveWindowEvent *ev = (XLeaveWindowEvent *) event;

/*printf ("HandleLeaveWindow....");*/
    if (!w->gterm.useDefaultCM && w->gterm.haveColormap) {
	int shadow = w->gterm.cmapShadow;

	/* The shadow option matches unused cells in the default colormap
	 * with the colors in our custom colormap.
	if (shadow && ev->time - w->gterm.cmapLastShadow > shadow * 1000) {
	 */
	if (shadow) {
	    update_default_colormap (w);
	    w->gterm.cmapLastShadow = ev->time;
	}

/*printf ("restoring focus....");*/
	restore_colormap_focus (w);
    }
/*printf ("\n");*/

    w->gterm.in_window = 0;
}

/* ARGSUSED */
static void HandleSoftReset (widget, event, params, param_count)
    Widget widget;
    XEvent *event;              /* unused */
    String *params;             /* unused */
    Cardinal *param_count;      /* unused */
{
    register GtermWidget w = (GtermWidget)widget;
    register GtCallback *cb;
    GtCallback cbl[128];
    int ncb, i;

    GtReset (w);

    /* Call any resize callbacks.  Callbacks can delete or add callbacks,
     * so make a copy of the callback list first.
     */
    for (cb = w->gterm.resetCallback, ncb=0;  cb;  cb = cb->next)
	cbl[ncb++] = *cb;
    for (i=0;  i < ncb;  i++)
	(*cbl[i].proc) (cbl[i].client_data, w, NULL);
}


/*
 * GRAPHICS routines (public functions).
 * --------------------------------------
 */

GtActivate (w)
    GtermWidget w;
{
    w->gterm.interactive = 0;
    w->gterm.save_x = w->gterm.save_y = 0;
}

GtDeactivate (w)
    GtermWidget w;
{
    Display *display = w->gterm.display;
    Window window = w->gterm.window;

    if (w->gterm.interactive) {
	if (w->gterm.save_x > 0 && w->gterm.save_y > 0) {
	    if (w->gterm.warpCursor) {
		/* Workaround X server bug. */
		if (w->gterm.root != w->gterm.save_root)
		    XWarpPointer (display,None,w->gterm.root, 0,0,0,0,
			WidthOfScreen(w->gterm.screen) - 1,
			HeightOfScreen(w->gterm.screen) - 1);

		/* Move pointer to saved position. */
		XWarpPointer (display, None, w->gterm.save_root,
		    0,0,0,0, w->gterm.save_x, w->gterm.save_y);
	    }
	    w->gterm.save_x = 0;
	    w->gterm.save_y = 0;
	}
	w->gterm.interactive = 0;
    }
}

GtReady (w)
    GtermWidget w;
{
    return (w->gterm.delay == 0);
}

GtReset (w)
    GtermWidget w;
{
    invalidate_draw_context (w);
    set_default_color_index (w);
    XSetFunction (w->gterm.display, w->gterm.drawGC, GXcopy);
    XSetLineAttributes (w->gterm.display,
	w->gterm.drawGC, 1, LineSolid, CapButt, JoinMiter);

    /* Set defaults. */
    w->gterm.line_width = 1;
    w->gterm.data_level = GtSet;
    w->gterm.line_style = GtSolid;
    w->gterm.fill_type  = GtSolid;
    w->gterm.raster     = 0;
}

GtTimerInhibit (w, state)
    GtermWidget w;
    Boolean state;
{
    /* This is a kludge to allow a client (xgterm) to disable use of timers
     * if they don't work in a given implementation.
     */
    w->gterm.useTimers = !state;
}

GtAugmentTranslations (w, translations)
    register GtermWidget w;
    char *translations;
{
    register int i;

    if ((i = w->gterm.nauxTrans) < MAX_AUXTRANS) {
	w->gterm.auxTrans[i] =
	    XtParseTranslationTable (translations);
	w->gterm.auxTType[i] = T_augment;
	w->gterm.nauxTrans++;
	XtAugmentTranslations ((Widget)w, w->gterm.auxTrans[i]);
    }
}

GtOverrideTranslations (w, translations)
    register GtermWidget w;
    char *translations;
{
    register int i;

    if ((i = w->gterm.nauxTrans) < MAX_AUXTRANS) {
	w->gterm.auxTrans[i] =
	    XtParseTranslationTable (translations);
	w->gterm.auxTType[i] = T_override;
	w->gterm.nauxTrans++;
	XtOverrideTranslations ((Widget)w, w->gterm.auxTrans[i]);
    }
}

GtFlush (w)
    GtermWidget w;
{
    XFlush (w->gterm.display);
}

GtSetLogRes (w, width, height)
    GtermWidget w;
    int width, height;
{
    w->gterm.xres = width;
    w->gterm.yres = height;
}

GtGetLogRes (w, width, height)
    GtermWidget w;
    int *width, *height;
{
    *width = w->gterm.xres;
    *height = w->gterm.yres;
}

GtGetPhysRes (w, raster, width, height)
    GtermWidget w;
    int raster;				/* zero for screen size */
    int *width, *height;
{
    if (raster) {
	register Raster rp = &w->gterm.rasters[raster];
	*width = rp->width;
	*height = rp->height;
    } else {
	*width = w->core.width;
	*height = w->core.height;
    }
}

GtSetPhysRes (w, raster, width, height)
    GtermWidget w;
    int raster;
    int width, height;
{
    GtCreateRaster (w, raster, GtServer, width, height, RasterDepth);
}

GtSetRaster (w, raster)
    GtermWidget w;
    int raster;
{
    if (raster >= 0 && raster < w->gterm.maxRasters) {
	w->gterm.raster = raster;
	invalidate_draw_context (w);
    }
}

GtGetRaster (w)
    GtermWidget w;
{
    return (w->gterm.raster);
}

/* ARGSUSED */
GtSetTextRes (w, optrows, optcols)
    GtermWidget w;
    int optrows, optcols;
{
    w->gterm.optrows = optrows;
    w->gterm.optcols = optcols;
}

/* ARGSUSED */
GtSetCharSize (w, ival)
    GtermWidget w;
    int ival;
{
}

GtSetDataLevel (w, ival)
    GtermWidget w;
    int ival;
{
    invalidate_draw_context (w);

    switch (ival) {
    case GtSet:
	XSetFunction (w->gterm.display, w->gterm.drawGC, GXcopy);
	XSetForeground (w->gterm.display, w->gterm.drawGC,
	    w->gterm.cmap[w->gterm.color_index]);
	w->gterm.data_level = ival;
	break;
    case GtClear:
	XSetFunction (w->gterm.display, w->gterm.drawGC, GXcopy);
	XSetForeground (w->gterm.display, w->gterm.drawGC, w->gterm.color0);
	w->gterm.data_level = ival;
	break;
    case GtInvert:
	/* This probably won't work correctly but leave it for now... */
	XSetFunction (w->gterm.display, w->gterm.drawGC, GXxor);
	w->gterm.data_level = ival;
	break;
    }
}


GtSetLineWidth (w, ival)
    GtermWidget w;
    int ival;
{
    w->gterm.line_width = ival;
    GtSetLineStyle (w, w->gterm.line_style);
}

#define	Dashed		"\010\003"
#define	Dotted		"\002\003"
#define	DashDot		"\016\003\001\003"
#define	Dash3Dot	"\024\003\001\003\001\003\001\003"

GtSetLineStyle (w, ival)
    GtermWidget w;
    int ival;
{
    int line_width = w->gterm.line_width;
    int line_style = LineSolid;
    int cap_style = CapButt;
    int join_style = JoinMiter;
    int dash_offset = 0;
    char *dash_list = NULL;
    int dash_list_length = 0;

    switch (ival) {
    case GtSolid:
	w->gterm.line_style = ival;
	break;
    case GtDashed:
	line_style = LineOnOffDash;
	dash_list = (char *)Dashed;
	dash_list_length = strlen(Dashed);
	w->gterm.line_style = ival;
	break;
    case GtDotted:
	line_style = LineOnOffDash;
	dash_list = (char *)Dotted;
	dash_list_length = strlen(Dotted);
	w->gterm.line_style = ival;
	break;
    case GtDashDot:
	line_style = LineOnOffDash;
	dash_list = (char *)DashDot;
	dash_list_length = strlen(DashDot);
	w->gterm.line_style = ival;
	break;
    case GtDash3Dot:
	line_style = LineOnOffDash;
	dash_list = (char *)Dash3Dot;
	dash_list_length = strlen(Dash3Dot);
	w->gterm.line_style = ival;
	break;
    }

    if (dash_list_length)
	XSetDashes (w->gterm.display, w->gterm.drawGC, dash_offset, dash_list,
	    dash_list_length);

    XSetLineAttributes (w->gterm.display,
	w->gterm.drawGC, line_width, line_style, cap_style, join_style);

    invalidate_draw_context (w);
}

GtSetColorIndex (w, ival)
    GtermWidget w;
    int ival;
{
    register int color = w->gterm.iomap[ival];

    if (color >= 0 && color < w->gterm.ncolors) {
	XSetForeground (w->gterm.display, w->gterm.drawGC,
	    w->gterm.cmap[color]);
	w->gterm.color_index = color;
	invalidate_draw_context (w);
    }
}

GtSetFillType (w, ival)
    GtermWidget w;
    int ival;
{
    switch (ival) {
    case GtSolid:
    case GtOutline:
	w->gterm.fill_type = ival;
	break;
    }
}

GtClearScreen (w)
GtermWidget w;
{
    register Mapping mp;

    if (!XtIsRealized ((Widget)w))
	return;

    if (w->gterm.pixmap)
	XFillRectangle (w->gterm.display, w->gterm.pixmap,
	    w->gterm.clearGC, 0, 0, w->core.width, w->core.height);
    XClearWindow (w->gterm.display, w->gterm.window);

    set_default_color_index (w);
    XSetFunction (w->gterm.display, w->gterm.drawGC, GXcopy);
    XSetForeground (w->gterm.display, w->gterm.drawGC, w->gterm.color1);
    XSetLineAttributes (w->gterm.display,
	w->gterm.drawGC, 1, LineSolid, CapRound, JoinRound);

    w->gterm.line_width = 1;
    w->gterm.line_style = GtSolid;
    w->gterm.fill_type = GtSolid;
    w->gterm.data_level = GtSet;
    w->gterm.preserve_valid = 0;
    w->gterm.gm_redisplay = 1;
    w->gterm.d_saved = 0;

    /* Mark any screen mappings to be unconditionally refreshed. */
    for (mp = w->gterm.mp_head;  mp;  mp = mp->next)
	if (mp->enabled && mp->dst == 0)
	    mp->refresh++;

    invalidate_draw_context (w);
    update_transients (w, NULL);
}

GtDrawPolyline (w, pv, npts)
    GtermWidget w;
    XPoint *pv;
    int npts;
{
    XPoint *points, o_pv[MAX_POINTS];
    DrawContext dx = get_draw_context (w);
    register MappingContext mx;
    register int i;

    for (i=0;  i < dx->nmappings;  i++) {
	mx = &dx->mapContext[i];
	points = mx->scale ? mapVector(mx,pv,o_pv,npts) : pv;

	/* Add code to support max display request size. */
	if (mx->use_backing_store)
	    XDrawLines (w->gterm.display, w->gterm.pixmap,
		mx->drawGC, points, npts, CoordModeOrigin);
	XDrawLines (w->gterm.display, mx->pixmap,
	    mx->drawGC, points, npts, CoordModeOrigin);
    }

    update_transients (w, (Region)NULL);
}

GtDrawPolymarker (w, pv, npts)
    GtermWidget w;
    XPoint *pv;
    int npts;
{
    XPoint *points, o_pv[MAX_POINTS];
    DrawContext dx = get_draw_context (w);
    register MappingContext mx;
    register int i;

    for (i=0;  i < dx->nmappings;  i++) {
	mx = &dx->mapContext[i];
	points = mx->scale ? mapVector(mx,pv,o_pv,npts) : pv;

	/* Add code to support max display request size. */
	if (mx->use_backing_store)
	    XDrawPoints (w->gterm.display, w->gterm.pixmap,
		mx->drawGC, points, npts, CoordModeOrigin);
	XDrawPoints (w->gterm.display, mx->pixmap,
	    mx->drawGC, points, npts, CoordModeOrigin);
    }

    update_transients (w, (Region)NULL);
}

GtDrawPolygon (w, pv, npts)
    GtermWidget w;
    XPoint *pv;
    int npts;
{
    XPoint *points, o_pv[MAX_POINTS];
    DrawContext dx = get_draw_context (w);
    register MappingContext mx;
    register int i;

    for (i=0;  i < dx->nmappings;  i++) {
	mx = &dx->mapContext[i];
	points = mx->scale ? mapVector(mx,pv,o_pv,npts) : pv;

	if (w->gterm.fill_type == GtOutline) {
	    /* Draw outline of region.
	     */
	    int first = 0;
	    int last = npts - 1;

	    if (mx->use_backing_store)
		XDrawLines (w->gterm.display, w->gterm.pixmap, mx->drawGC,
		    points, npts, CoordModeOrigin);
	    XDrawLines (w->gterm.display, mx->pixmap, mx->drawGC,
		points, npts, CoordModeOrigin);

	    if (points[last].x != points[first].x ||
		points[last].y != points[first].y) {

		if (mx->use_backing_store)
		    XDrawLine (w->gterm.display, w->gterm.pixmap, mx->drawGC,
			points[last].x, points[last].y,
			points[first].x, points[first].y);
		XDrawLine (w->gterm.display, mx->pixmap, mx->drawGC,
		    points[last].x, points[last].y,
		    points[first].x, points[first].y);
	    }
	} else {
	    /* Fill the outlined area.
	     */
	    if (mx->use_backing_store) {
		XFillPolygon (w->gterm.display, w->gterm.pixmap, mx->drawGC,
		    points, npts, Nonconvex, CoordModeOrigin);
		XDrawLines (w->gterm.display, w->gterm.pixmap, mx->drawGC,
		    points, npts, CoordModeOrigin);
	    }
	    XFillPolygon (w->gterm.display, mx->pixmap, mx->drawGC,
		points, npts, Nonconvex, CoordModeOrigin);
	    XDrawLines (w->gterm.display, mx->pixmap, mx->drawGC,
		points, npts, CoordModeOrigin);
	}
    }

    update_transients (w, (Region)NULL);
}

GtDrawMarker (w, x, y, xsize, ysize, type)
    GtermWidget w;
    int x, y;
    int xsize, ysize;
    int type;
{
}

GtBell (w)
    GtermWidget w;
{
    XBell (w->gterm.display, 0);
}


/* GtSetCursorPos -- Set the cursor position to the given coordinates X,Y.
 * Coordinates are specified in the current graphics coordinate system,
 * defined by the current raster and logical resolution.
 *
 * This routine is a little more complex than one might think due to the
 * complication of mappings.  Screen coordinates are required to set the
 * cursor, but the graphics drawing context may be defined relative to
 * any raster.  In the general case a graphics pipeline defines the series
 * of coordinate transformations required to transform from graphics
 * coordinates to screen coordinates.  Things are further complicated since
 * the pipeline or desired position may not map to the screen, or there
 * may be multiple mappings to the screen.  The first case (no mapping to
 * the screen) is dealt with by ignoring the request to warp the cursor.
 * The second case (one-to-many mapping) is dealt with by a heuristic:
 * the most recent screen coordinates are unmapped back to the raster we
 * are "drawing" into, defining a unique path through the mappings which
 * we can use to map back to the screen.
 *
 * The simplest case occurs when we are drawing directly into the screen.
 * In this case (raster=0) there may still be a logical to physical
 * coordinate transformation, but there are no mappings to complicate things.
 */
GtSetCursorPos (w, x, y)
    GtermWidget w;
    int x, y;
{
    register MappingContext mx;
    register DrawContext dx;
    register Mapping mp;

    Window window = w->gterm.window;
    int sv_raster = w->gterm.raster;
    int sv_xres = w->gterm.xres, sv_yres = w->gterm.yres;
    int rasters[256], mappings[256], nmap=0, ntrans=0;
    int rx, ry, src, dst, map, i, npts = 1;
    int raster = w->gterm.raster;
    XPoint pv1[1], pv2[2];
    XPoint *points, pv[1];
    Raster rp;

    if (!XtIsRealized ((Widget)w))
	return;

    /* Follow the current cursor position back to the source raster if
     * possible.  This gives us a default pipeline to follow in the reverse
     * direction to map raster coordinates to screen coordinates, and is
     * necessary to find the right mappings when multiple mappings are
     * defined on a single source.
     */
    rx = w->gterm.last_x;
    ry = w->gterm.last_y;
    src = 0;
    do {
	src = GtSelectRaster (w, dst=src, GtPixel,rx,ry, GtPixel,&rx,&ry,&map);
	if (src != dst) {
	    rasters[nmap] = src;
	    mappings[nmap++] = map;
	}
    } while (src != dst && src != raster);

    /* Ray trace the point through all of the mappings to the screen.
     * This isn't fully general, but gives us the capability to follow
     * most graphics pipelines to a point on the screen.
     */
    do {
	GtSetRaster (w, raster);
	if (ntrans++ || raster == 0) {				/* MF041 */
	    /* After the first transformation we have raster coordinates,
	     * so set the logical resolution to the raster dimensions.
	     */
	    rp = &w->gterm.rasters[raster];
	    GtSetLogRes (w, rp->width, rp->height);
	}

	dx = get_draw_context (w);
	if (!dx->nmappings)
	    return;

	/* Try to find the next mapping. */
	if (nmap && rasters[nmap-1] == raster)
	    for (i=0;  i < dx->nmappings;  i++) {
		mx = &dx->mapContext[i];
		if (mx->mapping == mappings[nmap-1]) {
		    mp = mx->mp;
		    nmap--;
		    goto havemap;
		}
	    }
	for (i=0;  i < dx->nmappings;  i++) {
	    mx = &dx->mapContext[i];
	    mp = mx->mp;
	    if (mp && mp->dst == 0)
		break;
	}
	if (i >= dx->nmappings) {
	    mx = &dx->mapContext[0];
	    mp = mx->mp;
	}

havemap:
	rp = &w->gterm.rasters[mp ? mp->dst : raster];

	/* Compute the coordinates points[0].{x,y} of the point x,y in the
	 * destination raster.
	 */
	if (mx->scale) {
	    /* Scaling is in effect.  The following subterfuge is used to
	     * compute the coordinates of the center of the raster pixel (x,y)
	     * when the image is zoomed.  We want to set the cursor to the
	     * center of the selected pixel, not the edge.
	     */
	    pv[0].x = x;  pv[0].y = y;
	    mapVector (mx, pv, pv1, npts);
	    pv[0].x = x + 1;  pv[0].y = y + 1;
	    mapVector (mx, pv, pv2, npts);

	    pv[0].x = (pv1[0].x + pv2[0].x) / 2.0;
	    pv[0].y = (pv1[0].y + pv2[0].y) / 2.0;
	    points = pv;

	} else {
	    /* No scaling. */
	    pv[0].x = x;  pv[0].y = y;
	    points = pv;
	}

	/* Clip to the bounds of the destination raster and generate the
	 * new x,y.
	 */
	x = max(0, min(rp->width-1,  points[0].x));
	y = max(0, min(rp->height-1, points[0].y));

    } while (mp && (raster = mp->dst));

    XWarpPointer (w->gterm.display, window, window, 0,0,0,0, x,y);

    w->gterm.last_x = w->gterm.cur_x = x;
    w->gterm.last_y = w->gterm.cur_y = y;

    GtSetRaster (w, sv_raster);
    GtSetLogRes (w, sv_xres, sv_yres);
}


GtGetCursorPos (w, x, y)
    GtermWidget w;
    int *x, *y;
{
    *x = w->gterm.last_x;
    *y = w->gterm.last_y;
}

GtSetCursorType (w, type)
    GtermWidget w;
    int type;
{
    static XtIntervalId id = (XtIntervalId) NULL;
    Display *display = w->gterm.display;
    Cursor cursor;
    int interval;
    Widget pw;

    if (!XtIsRealized ((Widget)w))
	return;
    if (w->gterm.cursor_type == type)
	return;

    switch (w->gterm.cursor_type = type) {
    case GtNoCursor:
    case GtIdleCursor:
	erase_crosshair (w);
	cursor = w->gterm.idle_cursor;
	break;

    case GtGinmodeCursor:
	/* Begin graphics cursor mode.
	 */

	/* If a screen clear or drawing operation has caused the redisplay
	 * flag to be set, redisplay any marker overlays.
	 */
	if (w->gterm.gm_redisplay) {
	    GmRedisplay (w, (Region)NULL);
	    w->gterm.gm_redisplay = False;
	}

	/* Make sure the window is visible.
	 */
	if (w->gterm.raiseWindow || w->gterm.deiconifyWindow)
	    for (pw = (Widget)w;  pw;  pw = XtParent(pw))
		if (XtIsShell(pw)) {
		    if (w->gterm.deiconifyWindow)
			XMapWindow (display, XtWindow(pw));
		    if (w->gterm.raiseWindow)
			XRaiseWindow (display, XtWindow(pw));
		    XSync (display, False);
		}

	/* The first time this is done after a GtActivate causes the cursor
	 * to be warped into the graphics window.  The interactive flag is set
	 * to cause GtDeactivate to restore the cursor to its original position
	 * after the graphics interaction finishes.
	 */
	if (w->gterm.warpCursor) {
	    int root_x, root_y, win_x, win_y;
	    int xoff, yoff, width, height, x, y;
	    Window gtermwin, root, child;
	    unsigned int keys;
	    int in_window = 0;

	    width = w->core.width;
	    height = w->core.height;
	    gtermwin = w->gterm.window;
	    XTranslateCoordinates (display, gtermwin, w->gterm.root,
		w->core.x, w->core.y, &xoff, &yoff, &child);

	    if (XQueryPointer (display, w->gterm.root, &root, &child,
		&root_x, &root_y, &win_x, &win_y, &keys)) {

		/* Already in gterm window? */
		if ((root_x >= xoff && root_x < xoff+width) &&
		    (root_y >= yoff && root_y < yoff+height)) {

		    if (!w->gterm.interactive) {
			w->gterm.save_x = 0;
			w->gterm.save_y = 0;
			w->gterm.interactive++;
		    }
		    x = root_x - xoff;   y = root_y - yoff;
		    in_window++;

		} else {
		    if (!w->gterm.interactive) {
			w->gterm.save_x = root_x;
			w->gterm.save_y = root_y;
			w->gterm.save_root = root;
			w->gterm.interactive++;
		    }
		    x = w->gterm.cur_x;  y = w->gterm.cur_y;
		}
	    } else {
		/* Pointer not on the current screen.
		 */
		if (!w->gterm.interactive) {
		    w->gterm.save_x = root_x;
		    w->gterm.save_y = root_y;
		    w->gterm.save_root = root;
		    w->gterm.interactive++;
		}
		x = w->gterm.cur_x;  y = w->gterm.cur_y;
	    }

	    if ((x < 5 || x > width-5) || (y < 5 || y > height-5)) {
		x = width / 2;
		y = height / 2;
	    }

	    if (!in_window) {
		erase_crosshair (w);
		if (w->gterm.warpCursor) {
		    XWindowAttributes wa;
		    if (XGetWindowAttributes (display, gtermwin, &wa) &&
			    wa.map_state == IsViewable) {

			/* The following should not be necessary but is needed
			 * to workaround an X server bug.  When warping to a
			 * different screen the pointer is not erased on the
			 * old screen.  It is hard to erase it, but we can
			 * at least move it to the corner of the screen.
			 */
			if (root != w->gterm.root) {
			    Screen *screen = w->gterm.screen;
			    if (XGetWindowAttributes (display, root, &wa))
				screen = wa.screen;
			    XWarpPointer (display,None,root, 0,0,0,0,
				WidthOfScreen(screen) - 1,
				HeightOfScreen(screen) - 1);
			}

			/* Now warp into the gterm window. */
			XWarpPointer (display, None, gtermwin, 0,0,0,0, x,y);
		    }
		    if (w->gterm.full_crosshair)
			draw_crosshair (w, x, y);
		}

	    } else if (w->gterm.full_crosshair) {
		erase_crosshair (w);
		draw_crosshair (w, x, y);
	    }
	} else
	    update_cursor (w);

	cursor = w->gterm.ginmode_cursor;
	if (interval = w->gterm.ginmodeBlinkInterval) {
	    XtAppContext appcon = XtWidgetToApplicationContext ((Widget) w);
	    id = XtAppAddTimeOut (appcon,
		interval, blink_cursor, (XtPointer)w);
	} else
	    id = (XtIntervalId) NULL;
	break;

    case GtBusyCursor:
	/* Exit graphics cursor mode.
	 */
	erase_crosshair (w);
	cursor = w->gterm.busy_cursor;
	break;
    }

    if (w->core.visible)
	XDefineCursor (w->gterm.display, w->gterm.window,
	    w->gterm.cursor = cursor);
    if (id && w->gterm.cursor_type != GtGinmodeCursor) {
	XtRemoveTimeOut (id);
	id = (XtIntervalId) NULL;
    }
}

static void
blink_cursor (w, id)
    GtermWidget w;
    XtIntervalId *id;
{
    XtAppContext app_context;
    XColor bg, fg;
    int interval;

    if (w->gterm.cursor_type != GtGinmodeCursor) 		/* MF032 */
	return;

    bg = w->gterm.ginmodeColors[1];
    fg = w->gterm.ginmodeColors[0];

    app_context = XtWidgetToApplicationContext ((Widget) w);
    XRecolorCursor (w->gterm.display, w->gterm.ginmode_cursor, &fg, &bg);
    XFlush (w->gterm.display);

    w->gterm.ginmodeColors[0] = bg;
    w->gterm.ginmodeColors[1] = fg;

    if (interval = w->gterm.ginmodeBlinkInterval)
	XtAppAddTimeOut (app_context,
	    interval, (XtTimerCallbackProc) blink_cursor, (XtPointer)w);
}

GtPostInputProc (w, userfcn, client_data)
    GtermWidget w;
    GtCallbackProc userfcn;
    XtPointer client_data;
{
    register GtCallback *cb, *new;

    new = (GtCallback *) XtMalloc (sizeof (GtCallback));
    new->proc = userfcn;
    new->client_data = client_data;
    new->next = NULL;

    for (cb = w->gterm.inputCallback;  cb && cb->next;  cb = cb->next)
	;
    if (cb)
	cb->next = new;
    else
	w->gterm.inputCallback = new;
}

GtDeleteInputProc (w, userfcn, client_data)
    GtermWidget w;
    GtCallbackProc userfcn;
    XtPointer client_data;
{
    register GtCallback *cb, *prev;

    for (prev=NULL, cb = w->gterm.inputCallback;  cb;  cb = cb->next)
	if (cb->proc == userfcn && cb->client_data == client_data) {
	    if (prev)
		prev->next = cb->next;
	    else
		w->gterm.inputCallback = cb->next;
	    XtFree ((char *)cb);
	    break;
	} else
	    prev = cb;
}

GtPostResetProc (w, userfcn, client_data)
    GtermWidget w;
    GtCallbackProc userfcn;
    XtPointer client_data;
{
    register GtCallback *cb, *new;

    new = (GtCallback *) XtMalloc (sizeof (GtCallback));
    new->proc = userfcn;
    new->client_data = client_data;
    new->next = NULL;

    for (cb = w->gterm.resetCallback;  cb && cb->next;  cb = cb->next)
	;
    if (cb)
	cb->next = new;
    else
	w->gterm.resetCallback = new;
}

GtDeleteResetProc (w, userfcn, client_data)
    GtermWidget w;
    GtCallbackProc userfcn;
    XtPointer client_data;
{
    register GtCallback *cb, *prev;

    for (prev=NULL, cb = w->gterm.resetCallback;  cb;  cb = cb->next)
	if (cb->proc == userfcn && cb->client_data == client_data) {
	    if (prev)
		prev->next = cb->next;
	    else
		w->gterm.resetCallback = cb->next;
	    XtFree ((char *)cb);
	    break;
	} else
	    prev = cb;
}

GtPostResizeProc (w, userfcn, client_data)
    GtermWidget w;
    GtCallbackProc userfcn;
    XtPointer client_data;
{
    register GtCallback *cb, *new;

    new = (GtCallback *) XtMalloc (sizeof (GtCallback));
    new->proc = userfcn;
    new->client_data = client_data;
    new->next = NULL;

    for (cb = w->gterm.resizeCallback;  cb && cb->next;  cb = cb->next)
	;
    if (cb)
	cb->next = new;
    else
	w->gterm.resizeCallback = new;
}

GtDeleteResizeProc (w, userfcn, client_data)
    GtermWidget w;
    GtCallbackProc userfcn;
    XtPointer client_data;
{
    register GtCallback *cb, *prev;

    for (prev=NULL, cb = w->gterm.resizeCallback;  cb;  cb = cb->next)
	if (cb->proc == userfcn && cb->client_data == client_data) {
	    if (prev)
		prev->next = cb->next;
	    else
		w->gterm.resizeCallback = cb->next;
	    XtFree ((char *)cb);
	    break;
	} else
	    prev = cb;
}

GtDrawAlphaText (w, x, y, text)
    GtermWidget w;
    int x, y;
    char *text;
{
    XPoint *points, pv[1], o_pv[1];
    DrawContext dx = get_draw_context (w);
    register MappingContext mx;
    register int npts, i;

    pv[0].x = x;
    pv[0].y = y;
    npts = 1;

    for (i=0;  i < dx->nmappings;  i++) {
	mx = &dx->mapContext[i];
	points = mx->scale ? mapVector(mx,pv,o_pv,npts) : pv;
	x = points[0].x;  y = points[0].y;

	if (mx->use_backing_store)
	    XDrawString (w->gterm.display, w->gterm.pixmap,
		mx->drawGC, x, y, text, strlen(text));
	XDrawString (w->gterm.display, mx->pixmap,
	    mx->drawGC, x, y, text, strlen(text));
    }

    update_transients (w, (Region)NULL);
}

GtGetAlphaTextSize (w, string, width, height, base)
    GtermWidget w;
    char *string;
    int *width, *height, *base;
{
    XFontStruct *fp;

    fp = w->gterm.alpha_fonts[w->gterm.alpha_font];
    if (string)
	*width = XTextWidth (fp, string, strlen(string));
    else
	*width = fp->max_bounds.width;

    *height = fp->max_bounds.ascent + fp->max_bounds.descent;
    *base = fp->max_bounds.ascent;
}

GtWriteAlphaCursor (w, x, y)
    GtermWidget w;
    int x, y;
{
}

GtEraseAlphaCursor (w)
    GtermWidget w;
{
}

GtStartDialog (w)
    GtermWidget w;
{
    if (w->gterm.d_pixmap)
	if (w->gterm.d_saved) {
	    GtEraseDialog (w);
	} else {
	    XSetClipMask (w->gterm.display, w->gterm.exposeGC, None);
	    XCopyArea (w->gterm.display,
		w->gterm.pixmap ? w->gterm.pixmap : w->gterm.window,
		w->gterm.d_pixmap, w->gterm.exposeGC,
		0, w->gterm.d_yoff, w->core.width, w->gterm.d_height, 0, 0);
	    w->gterm.d_saved = 1;
	}
}

GtEndDialog (w)
    GtermWidget w;
{
    GtEraseDialog (w);
    w->gterm.d_saved = 0;
}

GtEraseDialog (w)
    GtermWidget w;
{
    if (w->gterm.d_pixmap && w->gterm.d_saved) {
	XSetClipMask (w->gterm.display, w->gterm.exposeGC, None);
	XCopyArea (w->gterm.display,
	    w->gterm.d_pixmap, w->gterm.window, w->gterm.exposeGC,
	    0, 0, w->core.width, w->gterm.d_height, 0, w->gterm.d_yoff);
	if (w->gterm.pixmap)
	    XCopyArea (w->gterm.display,
		w->gterm.d_pixmap, w->gterm.pixmap, w->gterm.exposeGC,
		0, 0, w->core.width, w->gterm.d_height, 0, w->gterm.d_yoff);
	update_transients (w, (Region)NULL);
    }
}

GtDrawDialogText (w, x, y, text)
    GtermWidget w;
    int x, y;
    char *text;
{
    int xpos = w->gterm.d_xoff + x;
    int ypos = w->gterm.d_yoff + y;

    if (w->gterm.pixmap)
	XDrawImageString (w->gterm.display, w->gterm.pixmap,
	    w->gterm.dialogGC, xpos, ypos, text, strlen(text));
    XDrawImageString (w->gterm.display, w->gterm.window,
	w->gterm.dialogGC, xpos, ypos, text, strlen(text));
}

GtGetDialogTextSize (w, string, width, height, base)
    GtermWidget w;
    char *string;
    int *width, *height, *base;
{
    XFontStruct *fp;

    fp = w->gterm.dialog_fonts[w->gterm.dialog_font];
    if (string)
	*width = XTextWidth (fp, string, strlen(string));
    else
	*width = fp->max_bounds.width;

    *height = fp->max_bounds.ascent + fp->max_bounds.descent;
    *base = fp->max_bounds.ascent;
}


/*
 * Internal functions for above code.
 * ----------------------------------------
 */

static
set_default_color_index (w)
    GtermWidget w;
{
    /* The default color index is 1, corresponding to the foreground
     * drawing color color1.  Index zero is the background drawing color
     * color0.  The remaining NColors color table entries are the optional
     * drawing colors corresponding to resources "color2" through "colorN".
     * These are used only if explicitly selected by the client application.
     */
    XSetForeground (w->gterm.display, w->gterm.drawGC, w->gterm.cmap[1]);
    w->gterm.color_index = 1;
    invalidate_draw_context (w);
}


static
draw_crosshair (w, x, y)
    GtermWidget w;
    int x, y;
{
    if (!XtIsRealized ((Widget)w))
	return;

    if (w->gterm.pixmap) {
	/* The preserve_screen flag is set if we need to preserve the
	 * exact display window contents, rather than merely refresh from
	 * the backing store pixmap.
	 */
	if (w->gterm.preserve_screen) {
	    if (!w->gterm.preserve_valid || y != w->gterm.cur_y)
		XCopyArea (w->gterm.display,
		    w->gterm.window, w->gterm.pixmap, w->gterm.exposeGC,
		    0, y, w->core.width, 1, 0, w->core.height);
	    if (!w->gterm.preserve_valid || x != w->gterm.cur_x)
		XCopyArea (w->gterm.display,
		    w->gterm.window, w->gterm.pixmap, w->gterm.exposeGC,
		    x, 0, 1, w->core.height, w->core.width, 0);
	    w->gterm.preserve_valid = 1;
	}

	XDrawLine (w->gterm.display, w->gterm.window, w->gterm.cursorGC,
	    0, y, w->core.width, y);
	XDrawLine (w->gterm.display, w->gterm.window, w->gterm.cursorGC,
	    x, 0, x, w->core.height);

	XFlush (w->gterm.display);
	w->gterm.cursor_drawn++;
    }

    w->gterm.cur_x = x;
    w->gterm.cur_y = y;
}


static
erase_crosshair (w)
    GtermWidget w;
{
    if (!XtIsRealized ((Widget)w))
	return;

    if (w->gterm.cursor_drawn) {
	register int x = w->gterm.cur_x;
	register int y = w->gterm.cur_y;

	XSetClipMask (w->gterm.display, w->gterm.exposeGC, None);
	if (w->gterm.pixmap) {
	    if (w->gterm.preserve_screen && w->gterm.preserve_valid) {
		XCopyArea (w->gterm.display,
		    w->gterm.pixmap, w->gterm.window, w->gterm.exposeGC,
		    0, w->core.height, w->core.width, 1, 0, y);
		XCopyArea (w->gterm.display,
		    w->gterm.pixmap, w->gterm.window, w->gterm.exposeGC,
		    w->core.width, 0, 1, w->core.height, x, 0);
	    } else {
		XCopyArea (w->gterm.display,
		    w->gterm.pixmap, w->gterm.window, w->gterm.exposeGC,
		    0, y, w->core.width, 1, 0, y);
		XCopyArea (w->gterm.display,
		    w->gterm.pixmap, w->gterm.window, w->gterm.exposeGC,
		    x, 0, 1, w->core.height, x, 0);
	    }
	}

	w->gterm.cursor_drawn = 0;
	w->gterm.preserve_valid = 0;
    }
}


static
update_transients (w, region)
    GtermWidget w;
    Region region;
{
    /* If an explicit region is given redisplay any markers in it immediately,
     * otherwise set the redisplay flag to cause a full screen redisplay when
     * drawing finishes and the widget is ready for input.
     */
    if ((char *)region)
	GmRedisplay (w, region);
    else
	w->gterm.gm_redisplay = True;

    /* Update the crosshair cursor if GIN mode is in effect. */
    update_cursor (w);
}


static
update_cursor (w)
    GtermWidget w;
{
    if (w->gterm.cursor_type == GtGinmodeCursor && w->gterm.full_crosshair) {
	register int x = w->gterm.cur_x;
	register int y = w->gterm.cur_y;

	if (x || y)
	    draw_crosshair (w, x, y);
    }
}

static Cursor
get_cursor (w, cursor_name)
    GtermWidget w;
    String cursor_name;
{
    XrmValue from, to;
    Cursor cursor;

    from.size = strlen (cursor_name) + 1;
    from.addr = cursor_name;

    to.addr = (caddr_t) &cursor;
    to.size = sizeof(cursor);

    if (!XtConvertAndStore ((Widget)w, XtRString, &from, XtRCursor, &to))
	cursor = XCreateFontCursor (w->gterm.display, XC_crosshair);

    return (cursor);
}


static DrawContext
get_draw_context (w)
    GtermWidget w;
{
    DrawContext dx = &w->gterm.draw;

    if (!dx->valid) {
	int raster = w->gterm.raster;
	Raster rp = &w->gterm.rasters[raster];
	register MappingContext mx = &dx->mapContext[0];
	Region clip_region, mask_region;
	struct mapping *map, *mp, *np, p_mp;
	int xres = w->gterm.xres;
	int yres = w->gterm.yres;
	float xscale, yscale;
	XRectangle r;
	int i, j;

	dx->raster = w->gterm.raster;
	dx->rp = rp;

	if (raster == 0) {
	    dx->nmappings = 1;
	    mx->mapping = 0;
	    mx->mp = NULL;
	    mx->use_backing_store = (w->gterm.pixmap != (Pixmap)NULL);
	    mx->pixmap = w->gterm.window;
	    mx->drawGC = w->gterm.drawGC;
	    mx->GC_private = 0;

/* (7/16/97) MJF - we don't scale raster 0 since it's already in screen coords.
   Otherwise the cursor movement keystrokes scale incorrectly and quickly move
   to (0,0).
	    mx->xoffset = mx->yoffset = mx->scale = 0;
   [DCT] This doesn't look entirely right as it disables logical coords for
   the screen.  Leave as is until this can be studied more carefully.
 */

	    mx->xoffset = mx->yoffset = 0;
	    if (xres == rp->width && yres == rp->height)
		mx->scale = 0;
	    else {
		mx->xscale = (float)rp->width / (float)xres;
		mx->yscale = (float)rp->height / (float)yres;
		mx->scale = 1;
	    }

	} else {
	    dx->nmappings = 0;
	    for (mp = w->gterm.mp_head;  mp;  mp = mp->next) {
		if (!mp->enabled || mp->src != raster ||
			w->gterm.rasters[mp->dst].type != GtServer)
		    continue;
		if (!valid_mapping (w, mp))
		    continue;

		mx->mp = mp;
		mx->mapping = mp->mapping;
		mx->pixmap = w->gterm.rasters[mp->dst].r.pixmap;
		mx->use_backing_store = (mp->dst == 0 &&
		    w->gterm.pixmap && !(mp->rop & R_Transient));

		/* Determine if any scaling is necessary. */
		get_pixel_mapping (w, mp, &p_mp, 0);
		map = &p_mp;

		/* Compute logical-to-raster scaling. */
		mx->xoffset = mx->yoffset = 0;
		if (xres == rp->width && yres == rp->height) {
		    mx->xscale = mx->yscale = 1.0;
		    mx->scale = 0;
		} else {
		    mx->xscale = (float)rp->width / (float)xres;
		    mx->yscale = (float)rp->height / (float)yres;
		    mx->scale = 1;
		}

		/* Compute overall scale factors by combining logical-to-
		 * raster and raster-to-screen mappings.
		 */
		if (map->snx != map->dnx || map->sny != map->dny ||
		    map->sx  != map->dx  || map->sy  != map->dy) {

		    xscale = (float)map->dnx / (float)map->snx;
		    mx->xscale *= xscale;
		    if (xscale < 0)
			mx->xoffset = map->dx + abs(map->dnx) - 1;
		    else
			mx->xoffset = map->dx;
		    mx->xoffset -= (map->sx * xscale);

		    yscale = (float)map->dny / (float)map->sny;
		    mx->yscale *= yscale;
		    if (yscale < 0)
			mx->yoffset = map->dy + abs(map->dny) - 1;
		    else
			mx->yoffset = map->dy;
		    mx->yoffset -= (map->sy * yscale);

		    mx->scale = 1;
		}

		/* Compute the clip mask which will clip graphics to the
		 * destination rect of the mapping, minus any regions of
		 * this rect covered by other mappings.
		 */
		clip_region = XCreateRegion();
		r.x = map->dx;  r.y = map->dy;
		r.width = abs(map->dnx);
		r.height = abs(map->dny);
		XUnionRectWithRegion (&r, clip_region, clip_region);

		for (np = mp->next;  np;  np = np->next) {
		    struct mapping p_np;

		    if (!np->enabled || np->dst != mp->dst)
			continue;
		    get_pixel_mapping (w, np, &p_np, 0);

		    mask_region = XCreateRegion();
		    r.x = p_np.dx;  r.y = p_np.dy;
		    r.width = abs(p_np.dnx);
		    r.height = abs(p_np.dny);
		    XUnionRectWithRegion (&r, mask_region, mask_region);

		    XSubtractRegion (clip_region, mask_region, clip_region);
		    XDestroyRegion (mask_region);
		}

		/* Create a drawing GC which is a copy of the global drawGC
		 * but using the clip mask computed above.
		 */
		mx->drawGC = XCreateGC (w->gterm.display, w->gterm.root,
		    0, NULL);
		XCopyGC (w->gterm.display, w->gterm.drawGC, ~0, mx->drawGC);
		XSetRegion (w->gterm.display, mx->drawGC, clip_region);
		XDestroyRegion (clip_region);
		mx->GC_private = 1;

		if (++dx->nmappings >= MAX_DRAW)
		    break;
		else
		    mx++;
	    }
	}

	dx->valid = 1;
    }

    return (dx);
}


static
invalidate_draw_context (w)
    GtermWidget w;
{
    register DrawContext dx = &w->gterm.draw;
    register MappingContext mx;
    register int i;

    if (dx->valid) {
	for (i=0;  i < dx->nmappings;  i++) {
	    mx = &dx->mapContext[i];
	    if (mx->GC_private)
		XFreeGC (w->gterm.display, mx->drawGC);
	}
	dx->valid = 0;
    }
}

static XPoint *
mapVector (mx, pv1, pv2, npts)
    register MappingContext mx;
    XPoint *pv1;
    XPoint *pv2;
    int npts;
{
    register XPoint *ip = pv1;
    register XPoint *op = pv2;
    register int n;

    for (n=npts;  --n >= 0;  ip++, op++) {
	op->x = ip->x * mx->xscale + mx->xoffset;
	op->y = ip->y * mx->yscale + mx->yoffset;
    }

    return (pv2);
}


static void
savepos (w, event)
    GtermWidget w;
    XEvent *event;
{
    if (event == NULL)
	return;

    switch (event->type) {
    case KeyPress:
    case KeyRelease:
	w->gterm.last_x = event->xkey.x;
	w->gterm.last_y = event->xkey.y;
	break;
    case ButtonPress:
    case ButtonRelease:
	w->gterm.last_x = event->xbutton.x;
	w->gterm.last_y = event->xbutton.y;
	break;
    case MotionNotify:
	w->gterm.last_x = event->xmotion.x;
	w->gterm.last_y = event->xmotion.y;
	break;
    }
}


/*
 * IMAGING routines.
 * -----------------------
 * Our strategy here is to support a range of visuals with pseudocolor being
 * preferred if available.  All imaging is done internally using 8 bit images
 * and a max 256 element colormap.  If the display hardware has a depth less
 * than 8 bits, e.g. for a monochrome display, the image is reduced to the
 * screen depth by some technique before being output to the display.
 * 
 * Images (rasters) are implemented internally in Gterm using either ximages or
 * off screen pixmaps.  Which format is used is decided at raster create time
 * and is controlled by a Gterm resource.  This is transparent to the client
 * application.  Currently only 8 bit rasters are supported.
 *
 *	        GtRasterInit (gt)
 *	      GtAssignRaster (gt, raster, drawable)
 *	      GtCreateRaster (gt, raster, type, width, height, depth)
 *	     GtDestroyRaster (gt, raster)
 *    exists = GtQueryRaster (gt, raster, &type, &width, &height, &depth)
 *     raster = GtNextRaster (gt)
 *	         GtSetRaster (gt, raster)
 *      raster = GtGetRaster (gt)
 *	      n = GtNRasters (gt)
 *
 *	       GtWritePixels (gt, raster, pixels, nbits, x1, y1, nx, ny)
 *	        GtReadPixels (gt, raster, pixels, nbits, x1, y1, nx, ny)
 *		 GtSetPixels (gt, raster, ct, x1, y1, nx, ny, color, rop)
 *	     GtRefreshPixels (gt, raster, ct, x1, y1, nx, ny)
 *  pixmap = GtExtractPixmap (gt, src, ct, x, y, width, height)
 *	      GtInsertPixmap (gt, pixmap, dst, ct, x, y, width, height)
 *
 * colormap = GtNextColormap (gt)
 *            GtFreeColormap (gt, colormap)
 *	     GtWriteColormap (gt, colormap, first, nelem, r, g, b)
 *	      GtReadColormap (gt, colormap, first, nelem, r, g, b)
 *	      GtLoadColormap (gt, colormap, offset, scale)
 *  exists = GtQueryColormap (gt, colormap, &first, &nelem, &maxelem)
 *	        GtWriteIomap (gt, iomap, first, nelem)
 *	         GtReadIomap (gt, iomap, first, nelem)
 *  pixel = GtGetClientPixel (gt, gterm_pixel)
 *
 *	      GtInitMappings (gt)
 *   mapping = GtNextMapping (gt)
 *             GtFreeMapping (gt, mapping)
 *	      GtRaiseMapping (gt, mapping, ref|NULL)
 *	      GtLowerMapping (gt, mapping, ref|NULL)
 *   int = GtCompareMappings (gt, map1, map2)
 *	     GtEnableMapping (gt, mapping, refresh)
 *          GtDisableMapping (gt, mapping, erase)
 *  active = GtActiveMapping (gt, mapping)
 *	    GtRefreshMapping (gt, mapping)
 *   raster = GtSelectRaster (gt, dras, dt, dx, dy, rt, &rx, &ry, &mapping)
 *
 *	        GtCopyRaster (gt, rop,
 *				  src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny)
 *	        GtSetMapping (gt, mapping, rop,
 *				  src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny)
 *	        GtGetMapping (gt, mapping, rop,
 *				  src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny)
 *
 *               GtMapVector (gt, mapping, dir, pv1, pv2, npts)
 *              GtPixelToNDC (gt, raster, pv1, pv2, npts)
 *              GtNDCToPixel (gt, raster, pv1, pv2, npts)
 *
 *		     GtDebug (gt, fp, what)
 *
 * In the case of CopyRaster or {Set|Get}Mapping, raster coordinates may be
 * specified in either raster pixel (GtPixel) units or in normalized device
 * coordinates (GtNDC) in the range 0-32767.
 * ---------------------------------------------------------------------------
 */

GtRasterInit (w)
    GtermWidget w;
{
    register int i;
    register Raster rp;
    register struct colormap *cm;
    struct colormap *next_cm;

    invalidate_draw_context (w);

    /* Destroy any existing rasters. */
    if (w->gterm.rasters)
	for (i=1;  i < w->gterm.maxRasters;  i++)
	    if (w->gterm.rasters[i].type)
		GtDestroyRaster (w, i);

    /* Allocate the initially empty raster descriptors. */
    XtFree ((char *)w->gterm.rasters);
    w->gterm.rasters = rp =
	(Raster) XtCalloc (w->gterm.maxRasters, sizeof (struct raster));
    w->gterm.nrasters = 0;
    w->gterm.raster = 0;

    /* Raster 0 is the display window. */
    rp->type = PixmapRaster;
    rp->width = w->core.width;
    rp->height = w->core.height;
    rp->r.pixmap = w->gterm.window;
    rp->delete = 0;
    w->gterm.nrasters++;

    /* Free any previously allocated colormap cells. */
    if (w->gterm.ncolors > SZ_STATIC_CMAP && w->gterm.useDefaultCM) {
	XFreeColors (w->gterm.display, w->core.colormap,
	    &w->gterm.cmap[SZ_STATIC_CMAP], w->gterm.ncolors - SZ_STATIC_CMAP,
	    0);
	w->gterm.ncolors = SZ_STATIC_CMAP;
	invalidate_cmap (w);
    }

    /* Free any client defined colormaps. */
    for (cm = w->gterm.colormaps;  cm;  cm = next_cm) {
	next_cm = cm->next;
	XtFree ((char *)cm);
    }
    w->gterm.colormaps = NULL;

    /* Initialize the mappings. */
    GtInitMappings (w);
}


/* GtNextRaster -- Return the index of the next unused raster.
 */
GtNextRaster (w)
    register GtermWidget w;
{
    register int i;

    if (w->gterm.rasters)
        for (i=1;  i < w->gterm.maxRasters;  i++)
            if (!w->gterm.rasters[i].type)
                return (i);

    return (-1);
}


/* GtNRasters -- Return the number of currently defined rasters.
 */
GtNRasters (w)
    GtermWidget w;
{
    return (w->gterm.nrasters);
}


/* GtAssignRaster -- Assign a raster descriptor to an externally created
 * drawable (window or pixmap).  The raster thus created may be mapped or
 * drawn into like the rasters created privately by the imaging code, but
 * this allows use of this code to access other windows, or shared pixmaps.
 */
GtAssignRaster (w, raster, drawable, type)
    GtermWidget w;
    int raster;			/* one-indexed */
    XtPointer drawable;		/* object containing pixel array */
    int type;			/* type of drawable [not used] */
{
    register Raster rp;
    XWindowAttributes wa;

    if (raster <= 0 || raster >= w->gterm.maxRasters)
	return (ERR);
    else
	rp = &w->gterm.rasters[raster];

    if (!XGetWindowAttributes (w->gterm.display, (Window)drawable, &wa))
	return (ERR);

    rp->type = PixmapRaster;
    rp->width = wa.width;
    rp->height = wa.height;
    rp->r.pixmap = (Pixmap) drawable;
    rp->delete = 0;

    return (OK);
}


/* GtCreateRaster -- Create a new raster of the given size.  A server pixmap
 * (GtServer) or ximage (GtClient) raster will be created depending upon the
 * current value of the cacheRasters resource.
 */
GtCreateRaster (w, raster, type, width, height, depth)
    GtermWidget w;
    int raster;			/* one-indexed */
    int type;
    int width, height;
    int depth;
{
    register uchar *op;
    register int npix, pixel;
    uchar *data;
    XImage *xp;
    Raster rp;
    int cache;

    if (!XtIsRealized ((Widget)w))
	return (ERR);

    /* Only rasters of depth 8 bits are currently supported. */
    if (depth && depth != 8)
	return (ERR);

    /* Check for a raster number in bounds. */
    if (raster < 0 || raster >= w->gterm.maxRasters)
	return (ERR);
    else
	rp = &w->gterm.rasters[raster];

    /* A create on raster 0 (the display window) is treated as an attempt
     * to resize the window.
     */
    if (raster == 0) {
	XWindowAttributes wa;

	invalidate_draw_context (w);

	/* Issue the resize request. */
	XtVaSetValues ((Widget)w,
	    XtNwidth, (XtArgVal)width,
	    XtNheight, (XtArgVal)height,
	    NULL);
	XFlush (w->gterm.display);

	/* The following generates a round trip request to the server and
	 * is an attempt to allow the window system time to process the
	 * resize request before the client can issue a GtQueryRaster to
	 * see if the request has succeeded (hence causing a race condition).
	 * If the window is not the requested size the delay flag is set
	 * to cause graphics input processing to be suspended until the
	 * window is resized or redisplayed.  A dummy expose event is
	 * generated to clear the delay condition in case the resize request
	 * is not granted.
	 */
	if (XGetWindowAttributes (w->gterm.display, w->gterm.window, &wa)) {
	    rp->width = wa.width;
	    rp->height = wa.height;

	    if (rp->width != width || rp->height != height) {
		XExposeEvent ev;
		ev.type = Expose;
		ev.send_event = True;
		ev.display = w->gterm.display;
		ev.window = w->gterm.window;
		ev.x = ev.y = 0;
		ev.width = ev.height = 1;
		ev.count = 0;

		XSendEvent (w->gterm.display, w->gterm.window, False,
		    NoEventMask, (XEvent *)&ev);
		w->gterm.delay = 1;
	    }
	}
	return (OK);
    }

    /* Get rid of any old raster. */
    GtDestroyRaster (w, raster);

    rp->width = width;
    rp->height = height;
    rp->delete = 1;

    /* Cache the raster? */
    if (strcmp (w->gterm.cacheRasters, "always") == 0)
	cache = 1;
    else if (strcmp (w->gterm.cacheRasters, "never") == 0)
	cache = 0;
    else
	cache = (type == GtServer);

    /* Create new raster. */
    if (cache) {
	/* Create a pixmap.  */
	rp->type = PixmapRaster;
	rp->r.pixmap = XCreatePixmap (w->gterm.display, w->gterm.window,
	    width, height, RasterDepth);
	if (rp->r.pixmap == (Pixmap)NULL)
	    goto ximage;
	XFillRectangle (w->gterm.display, rp->r.pixmap, w->gterm.clearGC,
	    0, 0, width, height);

    } else {
	/* Create an XImage. */
ximage:
	rp->type = ImageRaster;

	/* Get pixel storage. */
	npix = width * height;
	if ((data = (uchar *) XtMalloc (npix)) == NULL)
	    return (ERR);
	else {
	    for (op=data, pixel=w->gterm.color0;  --npix >= 0;  )
		*op++ = pixel;
	}

	/* The following doesn't yet deal properly with byte and bit ordering
	 * differences between the server and client.
	 */
	rp->r.ximage = xp = XCreateImage (w->gterm.display, NULL, RasterDepth,
	    ZPixmap, 0, (char *)data, width, height, 8, 0);
	if (xp == NULL) {
	    rp->type = 0;
	    return (ERR);
	}
    }

    w->gterm.nrasters++;
    return (OK);
}


/* GtDestroyRaster -- Destroy a raster.  Any mappings which reference the
 * raster are deactivated, and all storage associated with the raster is freed.
 */
GtDestroyRaster (w, raster)
    GtermWidget w;
    int raster;
{
    register Raster rp;
    register Mapping mp, next;

    if (raster <= 0)
	return;

    invalidate_draw_context (w);

    /* Disable any mappings that reference this raster. */
    for (mp = w->gterm.mp_head;  mp;  mp = next) {
	next = mp->next;
	if (mp->src == raster || mp->dst == raster)
	    free_mapping (w, mp);
    }

    /* Destroy the raster. */
    rp = &w->gterm.rasters[raster];
    if (rp->type) {
	if (rp->delete) {
	    if (rp->type == ImageRaster)
		XDestroyImage (rp->r.ximage);
	    else if (rp->type == PixmapRaster)
		XFreePixmap (w->gterm.display, rp->r.pixmap);
	}
	w->gterm.nrasters--;
	rp->type = 0;
	rp->delete = 0;
    }
}


/* GtQueryRaster -- Determine whether a raster exists and if so return its
 * size.
 */
GtQueryRaster (w, raster, type, width, height, depth)
    GtermWidget w;
    int raster;			/* one-indexed */
    int *type;
    int *width, *height;
    int *depth;
{
    register Raster rp;

    if (raster < 0 || raster > w->gterm.maxRasters)
	return (0);

    rp = &w->gterm.rasters[raster];
    if (rp->type) {
	if (type) {
	    if (rp->type == PixmapRaster)
		*type = GtServer;
	    else
		*type = GtClient;
	}
	if (width)
	    *width = rp->width;
	if (height)
	    *height = rp->height;
	if (depth)
	    *depth = RasterDepth;
	return (1);
    } else
	return (0);
}


/* GtWritePixels -- Write to a rectangular region of a raster.  If any
 * mappings are currently defined which reference this raster as the source,
 * and a mapped region is being rewritten, the affected pixels will be
 * refreshed by the mapping.
 */
GtWritePixels (w, raster, pixels, nbits, x1, y1, nx, ny)
    GtermWidget w;
    int raster;
    uchar *pixels;
    int nbits;			/* not used */
    int x1, y1;
    int nx, ny;
{
    register uchar *ip, *op;
    register Pixel *cmap;
    register int n;
    int bytes_per_line, i;
    Mapping mp;
    Raster rp;
    uchar *lp;

    rp = &w->gterm.rasters[raster];
    if (rp->type == 0)
	return (ERR);

    /* Perform some range checks. */
    if (x1 < 0 || x1 > rp->width || nx <= 0 || x1+nx > rp->width)
	return (ERR);
    if (y1 < 0 || y1 > rp->height || ny <= 0 || y1+ny > rp->height)
	return (ERR);

    if (rp->type == PixmapRaster) {
	Display *display = w->gterm.display;
	XImage *ximage;
	uchar *data;
	int npix;

	/* Get a data buffer. */
	if ((data = (uchar *)XtMalloc (npix = nx * ny)) == NULL)
	    return (ERR);

	/* Convert the pixel values to colormap indices. */
	cmap = get_cmap_in (w);
	for (ip=pixels, op=data, n=npix;  --n >= 0;  )
	    *op++ = (cmap[*ip++] & 0377);

	ximage = XCreateImage (w->gterm.display, NULL, RasterDepth,
	    ZPixmap, 0, (char *)data, nx, ny, 8, 0);

	if (raster == 0 && w->gterm.pixmap) {

	    /* ### Someone (Mike?) added this code for some reason, but it
	     * does not appear to be valid.  This code already writes to the
	     * backing store (gterm.pixmap) so use_backing_store should not
	     * be required.  Also blindly using only the first mapping context
	     * and ignoring any others cannot be correct.  There is code
	     * below which executes any mappings defined on the raster.
	     * In general this requires scaling, not a simple XCopyArea.
	     *
             * DrawContext dx = get_draw_context (w);
             * register MappingContext mx;
             * mx = &dx->mapContext[0];
	     */

	    XPutImage (display, w->gterm.pixmap, w->gterm.exposeGC, ximage,
		0, 0, x1, y1, nx, ny);

	    /* ### (cont'd)
             * if (mx->use_backing_store)
	     *    XCopyArea (display, w->gterm.pixmap, mx->pixmap,
	     *	     w->gterm.exposeGC, x1, y1, nx, ny, x1, y1);
	     */

	    XCopyArea (display, w->gterm.pixmap, rp->r.pixmap,
		w->gterm.exposeGC, x1, y1, nx, ny, x1, y1);
	} else
	    XPutImage (display, rp->r.pixmap, w->gterm.exposeGC, ximage,
		0, 0, x1, y1, nx, ny);

	XtFree ((char *)data);
	ximage->data = NULL;
	XDestroyImage (ximage);

    } else if (rp->type == ImageRaster) {
	cmap = get_cmap_in (w);
	bytes_per_line = rp->r.ximage->bytes_per_line;
	lp = (uchar *)rp->r.ximage->data + y1 * bytes_per_line + x1;
	ip = pixels;

	/* Copy the data into the ximage data raster, converting input
	 * pixels to Xlib pixel values in the process.
	 *
	 * Possibly this should be done at Pixmap write time rather than
	 * during raster i/o so that the image pixel values are preserved.
	 * Otherwise reading back pixels is difficult and if the color map is
	 * dynamically modified the original pixel values may be lost.
	 * Postponing display pixel value generation woudl also make it easy
	 * to add support later for image depths other than 8 bit.  Doing the
	 * conversion to display pixels here is however simpler and more
	 * efficient so that is how we do it for now.
	 */
	for (i=0;  i < ny;  i++) {
	    for (n=nx, op=lp;  --n >= 0;  )
		*op++ = (cmap[*ip++] & 0377);
	    lp += bytes_per_line;
	}
    }

    /* Execute any mappings that reference this raster. */
    for (mp = w->gterm.mp_head;  mp;  mp = mp->next) {
	if (mp->enabled && mp->src == raster) {
	    struct mapping *map=mp, p_mp;
	    if (map->st != GtPixel || map->dt != GtPixel) {
		initialize_mapping (&p_mp);
		get_pixel_mapping (w, map, &p_mp, 1);
		update_mapping (w, map = &p_mp);
	    } else
		update_mapping (w, map);
	    refresh_source (w, map, x1, y1, nx, ny);
	    if (map == &p_mp)
		free_mapping (w, map);
	}
    }

    return (OK);
}


/* GtReadPixels -- Read a rectangular region of a raster.
 */
GtReadPixels (w, raster, pixels, nbits, x1, y1, nx, ny)
    GtermWidget w;
    int raster;
    uchar *pixels;
    int nbits;			/* not used */
    int x1, y1;
    int nx, ny;
{
    register uchar *ip, *op;
    register Pixel *cmap;
    register int n;

    int bytes_per_line, i;
    int x, y, delxin = 0;
    XImage *xin;
    Raster rp;
    uchar *lp;


    rp = &w->gterm.rasters[raster];
    if (rp->type == 0)
	return (ERR);

    /* Perform some range checks. */
    if (x1 < 0 || x1 > rp->width || nx <= 0 || x1+nx > rp->width)
	return (ERR);
    if (y1 < 0 || y1 > rp->height || ny <= 0 || y1+ny > rp->height)
	return (ERR);

    /* Get the input ximage. */
    if (rp->type == PixmapRaster) {
	Display *display = w->gterm.display;

	/* Read the pixmap subraster into an ximage.  If we are reading the
	 * screen (raster == 0) and we have an off-screen backing store pixmap,
	 * use that instead of the screen.
	 */
        xin = XGetImage (display,
	    (raster == 0 && w->gterm.pixmap) ? w->gterm.pixmap : rp->r.pixmap,
            x1, y1, nx, ny, 0xff, ZPixmap);

	if (xin == NULL)
	    return (ERR);

	delxin++;
	x = y = 0;

    } else {
	xin = rp->r.ximage;
	x = x1;
	y = y1;
    }

    cmap = get_cmap_out (w);
    bytes_per_line = xin->bytes_per_line;
    lp = (uchar *)xin->data + y * bytes_per_line + x;
    op = pixels;

    /* Copy the data to the output buffer, converting display pixels to
     * client pixels in the process.
     */
    for (i=0;  i < ny;  i++) {
	for (n=nx, ip=lp;  --n >= 0;  ) {
	    *op++ = cmap[*ip++];
	}
	lp += bytes_per_line;
    }

    if (delxin)
	XDestroyImage (xin);
    return (OK);
}


/* GtSetPixels -- Set all the raster pixels in a region to a single color.
 * If nx=ny=0 the entire raster will be written.
 */
GtSetPixels (w, raster, ct, x1, y1, nx, ny, color, rop)
    GtermWidget w;
    int raster;
    int ct;
    int x1, y1;
    int nx, ny;
    int color;
    int rop;
{
    register Raster rp;
    Mapping mp;

    /* Get raster pointer. */
    rp = &w->gterm.rasters[raster];
    if (rp->type == 0)
	return (ERR);

    /* Get pixel coordinates. */
    if (ct != GtPixel) {
	struct mapping sv_mp, p_mp;
	initialize_mapping (&sv_mp);				/* MF035 */
	save_mapping (&sv_mp, 0, 0,
	    0, GtPixel, 0,0,0,0,
	    raster, ct, x1,y1,nx,ny);
	get_pixel_mapping (w, &sv_mp, &p_mp, 0);

	x1 = p_mp.dx;
	y1 = p_mp.dy;
	nx = p_mp.dnx;
	ny = p_mp.dny;
    }

    /* Perform some range checks. */
    if (x1 == 0 && y1 == 0 && nx == 0 && ny == 0) {
	nx = rp->width;
	ny = rp->height;
    } else {
	if (x1 < 0 || x1 > rp->width || nx <= 0 || x1+nx > rp->width)
	    return (ERR);
	if (y1 < 0 || y1 > rp->height || ny <= 0 || y1+ny > rp->height)
	    return (ERR);
    }

    /* Set the pixels.
     */
    if (rp->type == PixmapRaster) {
	Display *display = w->gterm.display;
	GC gc = w->gterm.clearGC;
	int use_backing_store;

	use_backing_store =
	    (raster == 0 && w->gterm.pixmap && !(rop & R_Transient));

	XSetForeground (display, gc, get_pixel(w,color));
	XFillRectangle (display, rp->r.pixmap, gc, x1, y1, nx, ny);
	if (use_backing_store)
	    XFillRectangle (display, w->gterm.pixmap, gc, x1, y1, nx, ny);
	XSetForeground (display, gc, w->gterm.color0);

    } else {
	register int n, i;
	register uchar *op;
	register Pixel pixel;
	int bytes_per_line;
	uchar *lp;

	pixel = get_pixel (w, color);
	bytes_per_line = rp->r.ximage->bytes_per_line;
	lp = (uchar *)rp->r.ximage->data + y1 * bytes_per_line + x1;

	/* Set all pixels in the indicated region.  */
	for (i=0;  i < ny;  i++) {
	    for (n=nx, op=lp;  --n >= 0;  )
		*op++ = pixel;
	    lp += bytes_per_line;
	}
    }

    /* Execute any mappings that reference this raster. */
    for (mp = w->gterm.mp_head;  mp;  mp = mp->next) {
	if (mp->enabled && mp->src == raster) {
	    struct mapping *map=mp, p_mp;
	    if (map->st != GtPixel || map->dt != GtPixel) {
		initialize_mapping (&p_mp);
		get_pixel_mapping (w, map, &p_mp, 1);
		update_mapping (w, map = &p_mp);
	    } else
		update_mapping (w, map);
	    refresh_source (w, map, x1, y1, nx, ny);
	    if (map == &p_mp)
		free_mapping (w, map);
	}
    }

    return (OK);
}


/* GtRefreshPixels -- Update any mappings defined upon the given region of
 * the given source raster, as if the pixel values had been set with a
 * write pixels call.
 */
GtRefreshPixels (w, raster, ct, x1, y1, nx, ny)
    GtermWidget w;
    int raster;
    int ct;
    int x1, y1;
    int nx, ny;
{
    register Raster rp = &w->gterm.rasters[raster];
    register Mapping mp;

    if (!XtIsRealized ((Widget)w))
	return;

    /* Get pixel coordinates. */
    if (ct != GtPixel) {
	struct mapping sv_mp, p_mp;
	initialize_mapping (&sv_mp);
	save_mapping (&sv_mp, 0, 0,
	    raster, ct, x1,y1,nx,ny,
	    0, GtPixel, 0,0,0,0);
	get_pixel_mapping (w, &sv_mp, &p_mp, 0);

	x1 = p_mp.sx;
	y1 = p_mp.sy;
	nx = p_mp.snx;
	ny = p_mp.sny;
    }

    /* Execute any mappings that reference this raster. */
    for (mp = w->gterm.mp_head;  mp;  mp = mp->next) {
	if (mp->enabled && mp->src == raster) {
	    struct mapping *map=mp, p_mp;
	    if (map->st != GtPixel || map->dt != GtPixel) {
		initialize_mapping (&p_mp);
		get_pixel_mapping (w, map, &p_mp, 1);
		update_mapping (w, map = &p_mp);
	    } else
		update_mapping (w, map);
	    refresh_source (w, map, x1, y1, nx, ny);
	    if (map == &p_mp)
		free_mapping (w, map);
	}
    }
}


/* GtExtractPixmap -- Extract a rectangular region of a raster and return
 * as a pixmap.  The caller is responsible for later deleting this pixmap.
 */
Pixmap
GtExtractPixmap (w, src, ctype, x, y, width, height)
    GtermWidget w;
    int src;
    int ctype;
    int x, y;
    int width, height;
{
    register Raster rp;
    int x1, y1, nx, ny;
    String cache;
    int i;

    rp = &w->gterm.rasters[src];
    if (!rp->type)
	return ((Pixmap)NULL);

    /* If width and height are zero, return the full raster. */
    if (width <= 0)
	width = rp->width;
    if (height <= 0)
	height = rp->height;

    /* Get pixel coordinates. */
    if (ctype != GtPixel) {
	struct mapping sv_mp, p_mp;
	initialize_mapping (&sv_mp);				/* MF035 */
	save_mapping (&sv_mp, 0, 0,
	    0, GtPixel, 0,0,0,0,
	    src, ctype, x,y,width,height);
	get_pixel_mapping (w, &sv_mp, &p_mp, 0);

	x1 = p_mp.dx;
	y1 = p_mp.dy;
	nx = p_mp.dnx;
	ny = p_mp.dny;

    } else {
	x1 = x;
	y1 = y;
	nx = width;
	ny = height;
    }
 
    /* Find any empty raster slot and use it to generate the output pixmap.
     */
    for (i=0;  i < w->gterm.maxRasters;  i++) {
	rp = &w->gterm.rasters[i];
	if (!rp->type) {
	    cache = w->gterm.cacheRasters;
	    w->gterm.cacheRasters = "always";

	    if (GtCreateRaster (w, i, GtServer, nx, ny, 	/* MF006 */
		RasterDepth) == ERR) {
		    w->gterm.cacheRasters = cache;
		    return ((Pixmap)NULL);
	    } else if (rp->type != PixmapRaster)
		goto err;

	    if (GtCopyRaster (w, 0,
		src,0, x1,y1,nx,ny, i,0, 0,0,nx,ny) == ERR) {
err:
		GtDestroyRaster (w, i);				/* MF005 */
		w->gterm.cacheRasters = cache;
		return ((Pixmap)NULL);
	    }

	    rp->type = 0;
	    w->gterm.nrasters--;
	    w->gterm.cacheRasters = cache;

	    return (rp->r.pixmap);
	}
    }

    return ((Pixmap)NULL);
}


/* GtInsertPixmap -- Insert the contents of the given pixmap into a raster
 * at the indicated coordinates.
 */
GtInsertPixmap (w, pixmap, dst, ctype, x, y, width, height)
    GtermWidget w;
    Pixmap pixmap;
    int dst;
    int ctype;
    int x, y;
    int width, height;
{
    register Raster rp;
    XWindowAttributes wa;
    int x1, y1, nx, ny;
    int i;

    /* Check that the pixmap exists. */
    if (!XGetWindowAttributes (w->gterm.display, pixmap, &wa))
	return (ERR);

    /* Default to full dimensions of pixmap if no dimensions given. */
    if (width <= 0)
	width = wa.width;
    if (height <= 0)
	height = wa.height;

    /* Get pixel coordinates. */
    if (ctype != GtPixel) {
	struct mapping sv_mp, p_mp;
	initialize_mapping (&sv_mp);				/* MF035 */
	save_mapping (&sv_mp, 0, 0,
	    0, GtPixel, 0,0,0,0,
	    dst, ctype, x,y,width,height);
	get_pixel_mapping (w, &sv_mp, &p_mp, 0);

	x1 = p_mp.dx;
	y1 = p_mp.dy;
	nx = p_mp.dnx;
	ny = p_mp.dny;

    } else {
	x1 = x;
	y1 = y;
	nx = width;
	ny = height;
    }

    /* Create the destination raster if none exists. */
    if (!w->gterm.rasters[dst].type)
	if (GtCreateRaster (w, dst, GtDefault, nx, ny,		/* MF006 */
	    RasterDepth) == ERR)
	        return (ERR);
    
    /* Find an empty raster slot and use it to build a fake source raster
     * using the supplied pixmap.
     */
    for (i=0;  i < w->gterm.maxRasters;  i++) {
	rp = &w->gterm.rasters[i];
	if (!rp->type) {
	    rp->type = PixmapRaster;
	    rp->width = nx;
	    rp->height = ny;
	    rp->r.pixmap = pixmap;

	    if (GtCopyRaster (w, 0,
		    i,0, 0,0,nx,ny, dst,0, x1,y1,nx,ny) < 0)
		return (ERR);

	    rp->type = 0;
	    return (OK);
	}
    }

    return (ERR);
}


/* GtWriteColormap -- Allocate or modify colormap cells.  The Gterm widget
 * colormap consists of a fixed number of preassigned, read-only color cells,
 * plus a variable number of dynamically allocated application defined color
 * cells.  The application sees only the preassigned pixel space 0-N where
 * N is the maximum pixel value.  These values are mapped to Xlib pixel values
 * by the CMAP vector in the main Gterm widget descriptor.  The server
 * colormap does the final mapping to RGB triplets.
 *
 * Our strategy here is as follows.  If we have a monochrome screen set up a
 * one-to-one fake colormap so that we preserve the input pixel values and
 * render a one-bit image later.  If we have a StaticGray or StaticColor
 * visual allocate read-only color cells to allow X to choose the closest
 * colors to what we request.  If we have a GrayScale or PseudoColor visual
 * allocate private read-write colors.  The TrueColor and DirectColor
 * visuals should not be seen here as we should have been able to set up the
 * PseudoColor visual on screens that support these visuals, but if they are
 * seen use a one-to-one mapping to preserve the 8 bit pixel values.
 */
GtWriteColormap (w, map, first, nelem, r, g, b)
    GtermWidget w;
    int map;
    int first;
    int nelem;
    ushort *r, *g, *b;
{
    XWindowAttributes wa;
    register XColor *cp;
    register int i, j, v, n;
    unsigned long plane_masks[1];
    int req, need;

    if (!XtIsRealized ((Widget)w))
	return (ERR);

    if (map > 0) {
	/* Create or modify a colormap descriptor.  The display colormap
	 * is not affected.
	 */
	register struct colormap *cm;
	struct colormap *last_cm;
	register XColor *cp;
	register int i;

	/* See if the colormap already exists. */
	for (cm = w->gterm.colormaps, last_cm = NULL;  cm;  cm = cm->next) {
	    last_cm = cm;
	    if (cm->map == map)
		break;
	}

	/* If not, create it. */
	if (!cm) {
	    if (!(cm = (struct colormap *)XtCalloc(1,sizeof(struct colormap))))
		return (ERR);
	    if (last_cm)
		last_cm->next = cm;
	    else
		w->gterm.colormaps = cm;

	    /* Initialize static part of colormap. */
	    for (i=0;  i < SZ_STATIC_CMAP;  i++) {
		cp = &w->gterm.color[i];
		cm->r[i] = cp->red;
		cm->g[i] = cp->green;
		cm->b[i] = cp->blue;
	    }
	}

	cm->map = map;
	cm->ncells = max (cm->ncells, first + nelem);

	/* Ignore attempts to overwrite static part of colormap. */
	for (  ;  first < SZ_STATIC_CMAP;  first++, nelem--) {
	    r++;  g++;  b++;
	}

        if (nelem >= 0) {
	    memmove (&cm->r[first], r, nelem * sizeof (ushort));
	    memmove (&cm->g[first], g, nelem * sizeof (ushort));
	    memmove (&cm->b[first], b, nelem * sizeof (ushort));
        }

	return (OK);
    }

    /* Write to the display colormap.
     */
    if (first < SZ_STATIC_CMAP || first + nelem > MAX_SZCMAP)
	return (ERR);

    /* Invalidate the cmap cache. */
    invalidate_cmap (w);

    /* Get the window attributes.  We need to do this to determine the type
     * of visual used for the window, which determines our color allocation
     * strategy.  This is only done once since presumably the visual and
     * window depth will not change after the widget has been around long
     * enough to receive a GtWriteColormap call.
     */
    if (!w->gterm.wa_defined) {
	if (!XGetWindowAttributes (w->gterm.display, w->gterm.window, &wa))
	    return (ERR);
	w->gterm.wa = wa;
	w->gterm.wa_defined++;
    } else
	wa = w->gterm.wa;
    
    if (wa.depth == 1)
	goto unitary;

    switch (wa.visual->class) {
    case StaticGray:
    case StaticColor:
	/* Allocate "best-match" colors. */
	for (i=first;  i < first+nelem;  i++) {
	    cp = &w->gterm.color[i];
	    cp->red = r[i-first];
	    cp->green = g[i-first];
	    cp->blue = b[i-first];
	    cp->flags = (DoRed | DoGreen | DoBlue);
	    if (XAllocColor (w->gterm.display, wa.colormap, cp))
		w->gterm.cmap[i] = cp->pixel;
	    else {
		w->gterm.cmap[i] = cp->pixel =
		    BlackPixelOfScreen (w->gterm.screen);
	    }
	}
	break;

    case GrayScale:
    case PseudoColor:
	if (w->gterm.useDefaultCM) {
usedef:	    /* Allocate private r/w colors from default colormap. */
	    need = first + nelem - w->gterm.ncolors;

	    /* Allocate new color cells if needed.  If we can't allocate all
	     * the requested cells the unallocated pixel values are set to
	     * black.
	     */
	    if ((n = need) > 0) {
		/* Get the colormap cells. */
		req = min(w->gterm.maxColors, first + nelem - SZ_STATIC_CMAP) -
		    (w->gterm.ncolors - SZ_STATIC_CMAP);
		for (n=0;  req > 0 && n < need;  )
		    if (XAllocColorCells (w->gterm.display, wa.colormap,
			    False, plane_masks, 0,
			    &w->gterm.cmap[w->gterm.ncolors+n], req)) {
			n += req;
		    } else
			req /= 2;

		/* Initialize the color descriptors. */
		for (i = w->gterm.ncolors;  i < first+nelem;  i++) {
		    cp = &w->gterm.color[i];
		    if (i < w->gterm.ncolors + n) {
			cp->pixel = w->gterm.cmap[i];
			cp->flags = (DoRed | DoGreen | DoBlue);
		    } else {
			w->gterm.cmap[i] = cp->pixel =
			    BlackPixelOfScreen (w->gterm.screen);
			cp->flags = 0;
		    }
		}
		w->gterm.ncolors += n;
	    }

	    /* Assign RGB colors to the referenced cells. */
	    for (i=0;  i < nelem;  i++) {
		cp = &w->gterm.color[first+i];
		cp->red = r[i];
		cp->green = g[i];
		cp->blue = b[i];
	    }

	    n = w->gterm.ncolors - first;
	    if (n > 0)
		XStoreColors (w->gterm.display, wa.colormap,
		    &w->gterm.color[first], n);

	} else {
	    /* Allocate colors in a custom colormap.  If the named colormap
	     * does not yet exist we create one.  Multiple gterm widget
	     * instances may share the same colormap.
	     */
	    Colormap colormap;
	    long timeval, time();
	    int ncolors, shadow;

	    /* get_colormap will direct us to the default colormap if the
	     * custom colormap cannot be accessed or created.
	     */
	    colormap = get_colormap (w);
	    if (w->gterm.useDefaultCM)
		goto usedef;

	    /* Assign RGB colors to the referenced cells. */
	    ncolors = min (w->gterm.maxColors, nelem);
	    cp = &w->gterm.color[first];

	    for (i=0;  i < ncolors;  i++, cp++) {
		cp->flags = (DoRed | DoGreen | DoBlue);
		cp->red = r[i];
		cp->green = g[i];
		cp->blue = b[i];
	    }

	    /* Store the new colors. */
	    if (ncolors > 0)
		XStoreColors (w->gterm.display,
		    colormap, &w->gterm.color[first], ncolors);

	    /* Also attempt to store the colors in the default colortable,
	     * by allocating, writing, and then deallocating cells.  This
	     * helps keeps the gterm window visible when the default
	     * colormap is loaded.  To avoid excessive server queries the
	     * default colormap is only updated every so often.  Updating is
	     * disabled if cmapShadow is set to zero.  If shadowing is
	     * enabled the update is always performed if the WriteColormap
	     * occurs when the pointer is not in the window (e.g., when a
	     * button elsewhere in the GUI is pressed) as otherwise the
	     * change will not be visible as the private colormap will not
	     * be loaded by the window manager.
	     */
	    shadow = w->gterm.cmapShadow;
	    timeval = time((long *)NULL);

	    if (shadow && (!w->gterm.in_window ||
		    (timeval - w->gterm.cmapLastShadow > shadow * 1000))) {
		update_default_colormap (w);
		w->gterm.cmapLastShadow = timeval;
	    }
	}
	break;

    default:
	/* Set up a unitary, or one-to-one mapping, to preserve the input
	 * pixel values so that we can render them later.
	 */
unitary:
	for (i = first;  i < first+nelem;  i++) {
	    w->gterm.cmap[i] = i;
	    cp = &w->gterm.color[i];
	    cp->pixel = i;
	    cp->red = r[i+first];
	    cp->green = g[i+first];
	    cp->blue = b[i+first];
	    cp->flags = (DoRed | DoGreen | DoBlue);
	    if (i+1 > w->gterm.ncolors)
		w->gterm.ncolors = i + 1;
	}
	break;
    }

    return (OK);
}


/* GtReadColormap -- Return the color assignments for a region of the named
 * colormap.
 */
GtReadColormap (w, map, first, nelem, r, g, b)
    GtermWidget w;
    int map;
    int first;
    int nelem;
    ushort *r, *g, *b;
{
    if (map > 0) {
	/* Read from a colormap descriptor.
	 */
	register struct colormap *cm;
	register int i, j;

	/* Locate colormap. */
	for (cm = w->gterm.colormaps;  cm;  cm = cm->next)
	    if (cm->map == map)
		break;
	if (!cm)
	    return (0);

	/* Return RGB values. */
	for (i=0;  i < nelem;  i++) {
	    j = first + i;
	    if (j < cm->ncells) {
		r[i] = cm->r[j];
		g[i] = cm->g[j];
		b[i] = cm->b[j];
	    } else
		break;
	}

	return (i);

    } else {
	/* Read the display colormap.
	 */
	register XColor *cp;
	register int i;

	/* Return RGB values. */
	for (i=0;  i < nelem;  i++)
	    if (first+i < w->gterm.ncolors) {
		cp = &w->gterm.color[first+i];
		r[i] = cp->red;
		g[i] = cp->green;
		b[i] = (ushort)cp->blue;
	    } else
		break;

	return (i);
    }
}


/* GtLoadColormap -- Load a colormap into the display, optionally scaling
 * the colormap via a linear transformation in the process.  The colormap is
 * unaffected if offset=0.5, scale=1.0.  A negative scale inverts the image.
 * If map=0 the linear transformation is applied directly to the display
 * colormap.
 *
 * The offset refers to the center of the mapped region of the transfer
 * function, which is why the center value is at 0.5.  For example, if the
 * range of raster pixel intensities is normalized to the range 0.0 to 1.0,
 * then a transfer function of [offset=0.3,slope=3.0] will display the region
 * of intenstities centered around the normalized intenstity of 0.3, with a
 * contrast of 3.0 (the screen intensity changes 3 units for a unit change in
 * raster pixel intensity).  The transfer function [offset=0.3,slope=-3.0]
 * will display the same range of pixel intensitites, but with a negative
 * contrast.  The transfer function [offset=0.5,slope=1.0] has intercepts
 * of [0,0] and [1,1] hence it displays the full range of raster pixel
 * intensities - the input colormap is used as is, without resampling.
 */
GtLoadColormap (w, map, offset, slope)
    GtermWidget w;
    int map;
    float offset, slope;
{
    register int i;
    register XColor *cp;
    register struct colormap *cm;
    struct colormap d_cmap, o_cmap;
    int noscale, nelem, c1, c2;
    float x, y, z, frac;
    ushort r, g, b;

    /* Get the colormap to be loaded.
     */
    if (map == 0) {
	/* Create a dummy colormap struct from the screen colormap. */
	cm = &d_cmap;
	cm->map = 0;
	cm->next = NULL;
	cm->ncells = w->gterm.ncolors;
	for (i=0;  i < cm->ncells;  i++) {
	    cp = &w->gterm.color[i];
	    cm->r[i] = cp->red;
	    cm->g[i] = cp->green;
	    cm->b[i] = cp->blue;
	}
    } else {
	/* Locate colormap. */
	for (cm = w->gterm.colormaps;  cm;  cm = cm->next)
	    if (cm->map == map)
		break;
	if (!cm)
	    return (ERR);
    }

    /* Compute the scaled colormap.  Only the dynamic part of the colormap
     * is scaled, the static cells are not affected.
     */
    o_cmap.map = 0;
    o_cmap.ncells = cm->ncells;
    nelem = cm->ncells - SZ_STATIC_CMAP;
    noscale = (abs(offset - 0.5) < 0.0001 && abs(slope - 1.0) < 0.0001);

    if (noscale) {
	for (i=0;  i < nelem;  i++) {
	    o_cmap.r[i] = cm->r[SZ_STATIC_CMAP+i];
	    o_cmap.g[i] = cm->g[SZ_STATIC_CMAP+i];
	    o_cmap.b[i] = cm->b[SZ_STATIC_CMAP+i];
	}
    } else {
	for (i=0;  i < nelem;  i++) {
	    x = (float)i / (float)(nelem - 1);
	    y = (x - offset) * slope + 0.5;

	    if (y <= 0.0) {
		r = cm->r[SZ_STATIC_CMAP];
		g = cm->g[SZ_STATIC_CMAP];
		b = cm->b[SZ_STATIC_CMAP];
	    } else if (y >= 1.0) {
		r = cm->r[cm->ncells-1];
		g = cm->g[cm->ncells-1];
		b = cm->b[cm->ncells-1];
	    } else {
		z = y * (nelem - 1) + SZ_STATIC_CMAP;
		if (w->gterm.cmapInterpolate) {
		    c1 = (int)z;
		    c2 = min (cm->ncells-1, c1 + 1);
		    frac = z - c1;
		    r = cm->r[c1] * (1.0 - frac) + cm->r[c2] * frac;
		    g = cm->g[c1] * (1.0 - frac) + cm->g[c2] * frac;
		    b = cm->b[c1] * (1.0 - frac) + cm->b[c2] * frac;
		} else {
		    c1 = (int)z;
		    r = cm->r[c1];
		    g = cm->g[c1];
		    b = cm->b[c1];
		}
	    }

	    o_cmap.r[i] = r;
	    o_cmap.g[i] = g;
	    o_cmap.b[i] = b;
	}
    }

    /* Load the colormap into the display. */
    GtWriteColormap (w, 0, SZ_STATIC_CMAP, nelem,
	o_cmap.r, o_cmap.g, o_cmap.b);

    /* If the colormap we loaded to the display was the display colormap,
     * restore the original unscaled colors in the gterm descriptor so that
     * we won't be scaling a scaled colormap in the next operation.
     */
    if (map == 0)
	for (i=SZ_STATIC_CMAP;  i < cm->ncells;  i++) {
	    cp = &w->gterm.color[i];
	    cp->red   = cm->r[i];
	    cp->green = cm->g[i];
	    cp->blue  = cm->b[i];
	}

    return (OK);
}


/* GtQueryColormap -- Return information on the size and state of a colormap.
 */
GtQueryColormap (w, map, first, nelem, maxelem)
    register GtermWidget w;
    int map;
    int *first, *nelem, *maxelem;
{
    register struct colormap *cm;
    int nitems;

    if (first)
	*first = SZ_STATIC_CMAP;
    if (nelem)
	*nelem = 0;
    if (maxelem)
	*maxelem = min (w->gterm.maxColors, MAX_SZCMAP) - SZ_STATIC_CMAP;

    if (map > 0) {
	for (cm = w->gterm.colormaps;  cm;  cm = cm->next)
	    if (cm->map == map)
		break;
	if (!cm)
	    return (0);

	if (nelem)
	    *nelem = cm->ncells - SZ_STATIC_CMAP;

    } else {
	if (nelem)
	    *nelem = w->gterm.ncolors - SZ_STATIC_CMAP;
	if (maxelem) {
	    nitems = min (MAX_SZCMAP, CellsOfScreen(w->gterm.screen));
	    *maxelem = min (nitems,
		min (w->gterm.maxColors, MAX_SZCMAP - w->gterm.base_pixel));
	}
    }

    return (1);
}


/* GtNextColormap -- Return a unique colormap number.
 */
GtNextColormap (w)
    register GtermWidget w;
{
    register struct colormap *cm;
    register int mapno = 0;

    /* Get the next map number. */
    for (cm = w->gterm.colormaps;  cm;  cm = cm->next)
	if (cm->map > mapno)
	    mapno = cm->map;

    return (mapno + 1);
}


/* GtFreeColormap -- Free a colormap descriptor.
 */
GtFreeColormap (w, colormap)
    register GtermWidget w;
    int colormap;
{
    register struct colormap *p_cm, *cm;

    /* Find the colormap and free it. */
    for (p_cm = NULL, cm = w->gterm.colormaps;  cm;  p_cm = cm, cm = cm->next)
	if (cm->map == colormap) {
	    if (p_cm)
		p_cm->next = cm->next;
	    else
		w->gterm.colormaps = cm->next;
	    XtFree ((char *)cm);
	    return;
	}
}


/* GtWriteIomap -- An iomap is an optional lookup table used to isolate the
 * client application from the color model used within the Gterm widget.
 * To simplify color allocation the Gterm widget defines a logical color
 * space where color 0 is the background, 1 the foreground, 2-N are statically
 * allocated standard colors, and colors N+1 and above are dynamically
 * allocated by the graphics application.  Less-demanding applications use
 * only the statically allocated, shared colors.  The widget internally maps
 * these logical colors to whatever the window system requires, but providing
 * a well-defined logical color space isolates the client from the details of
 * color allocation in the underlying window system.
 *
 * An iomap can be used to define a mapping between the color model of the
 * client application and the Gterm color model (when we say color model here
 * we mean color allocation schemes for 8 bit pseudocolor).  By default the
 * iomap is one-to-one.  The use of an iomap frees the client from having to
 * worry about color index translations, and allows color tables to be
 * combined in the widget for greater efficiency when color tables are serially
 * applied.  The iomap applies to all color indices or pixel values passed
 * in i/o operations between the client and the Gterm widget.
 */
GtWriteIomap (w, iomap, first, nelem)
    register GtermWidget w;
    ushort *iomap;
    int first, nelem;
{
    register int c1, c2;

    c1 = max(0, min(MAX_SZCMAP-1, first));
    c2 = max(0, min(MAX_SZCMAP-1, first + nelem - 1));
    nelem = c2 - c1 + 1;

    memmove (&w->gterm.iomap[c1], iomap, nelem * sizeof(ushort));
    invalidate_cmap (w);
}


/* GtReadIomap -- Read back the contents of the iomap.
 */
GtReadIomap (w, iomap, first, nelem)
    register GtermWidget w;
    uchar *iomap;
    int first, nelem;
{
    register int c1, c2;

    c1 = max(0, min(MAX_SZCMAP-1, first));
    c2 = max(0, min(MAX_SZCMAP-1, first + nelem - 1));
    nelem = c2 - c1 + 1;

    memmove (iomap, &w->gterm.iomap[c1], nelem * sizeof(ushort));
}


/* init_iomap -- Initialize the iomap and the cmap cache.
 */
static void
init_iomap (w)
    GtermWidget w;
{
    register ushort *iomap = w->gterm.iomap;
    register int i;

    for (i=0;  i < MAX_SZCMAP;  i++)
	iomap[i] = i;
    invalidate_cmap (w);
}


/* invalidate_cmap -- Invalidate the cmap cache.
 */
static void
invalidate_cmap (w)
    register GtermWidget w;
{
    w->gterm.cmap_in_valid = w->gterm.cmap_out_valid = 0;
}


/* get_cmap_in -- Get the combined input colormap, used to transform color
 * values received from the client to window system color indices.
 */
static Pixel *
get_cmap_in (w)
    register GtermWidget w;
{
    register Pixel *cmap, *cmap_in = w->gterm.cmap_in;
    register ushort *iomap;
    register int i, j;
    int ncolors;

    if (w->gterm.cmap_in_valid)
	return (cmap_in);

    cmap = w->gterm.cmap;
    iomap = w->gterm.iomap;
    ncolors = w->gterm.ncolors - SZ_STATIC_CMAP;

    /* If ncolors is small wrap around so that pixel values stay within
     * the mapped range of output pixels.
     */
    for (i=0;  i < MAX_SZCMAP;  i++) {
	j = iomap[i];
	if (j > SZ_STATIC_CMAP && ncolors)
	    j = ((j - SZ_STATIC_CMAP) % ncolors) + SZ_STATIC_CMAP;
	cmap_in[i] = cmap[j];
    }

    w->gterm.cmap_in_valid++;
    return (cmap_in);
}


/* get_cmap_out -- Get the combined output colormap, used to transform window
 * system color indices to the color system of the client.  Note that this is
 * not necessarily a uniquely defined invertible transformation.
 */
static Pixel *
get_cmap_out (w)
    GtermWidget w;
{
    register Pixel *cmap;
    register ushort *iomap;
    Pixel *cmap_out = w->gterm.cmap_out;
    register int pixel, i;
    int j;

    if (w->gterm.cmap_out_valid)
	return (cmap_out);

    cmap = w->gterm.cmap;
    iomap = w->gterm.iomap;

    /* Invert the two colormaps.  This is not very efficient, but we don't
     * have to do this very often (a GtReadPixels call is about the only
     * case, and even then the map is cached).
     */
    for (j=0;  j < MAX_SZCMAP;  j++) {
	pixel = j;

	/* Lookup display pixel in cmap. */
	for (i=0;  i < MAX_SZCMAP;  i++)
	    if (cmap[i] == pixel) {
		pixel = i;
		break;
	    }
	if (i >= MAX_SZCMAP) {
	    cmap_out[j] = 0;
	    continue;
	}

	/* Lookup cmap pixel in iomap. */
	if (iomap[pixel] != pixel) {
	    for (i=0;  i < MAX_SZCMAP;  i++)
		if (iomap[i] == pixel) {
		    pixel = i;
		    break;
		}
	    if (i >= MAX_SZCMAP) {
		cmap_out[j] = 0;
		continue;
	    }
	}

	cmap_out[j] = pixel;
    }

    w->gterm.cmap_out_valid++;
    return (cmap_out);
}


/* get_pixel -- Convert a client color index into a display pixel.
 */
static Pixel
get_pixel (w, client_pixel)
    register GtermWidget w;
    register int client_pixel;
{
    register Pixel *cmap = get_cmap_in (w);

    if (client_pixel < 0 || client_pixel >= MAX_SZCMAP)
	return (w->gterm.cmap[1]);
    else
	return (cmap[client_pixel]);
}


/* GtGetClientPixel -- Convert a gterm pixel into a client pixel.
 */
GtGetClientPixel (w, pixel)
    GtermWidget w;
    register int pixel;
{
    register int i;
    register ushort *iomap;
    int client_pixel = 0;

    get_cmap_in (w);
    iomap = w->gterm.iomap;

    for (i=0;  i < MAX_SZCMAP;  i++)
	if (iomap[i] == pixel) {
	    client_pixel = i;
	    break;
	}

    return (client_pixel);
}


/* GtInitMappings -- Delete all mappings and initialize the mapping subsystem.
 */
GtInitMappings (w)
    register GtermWidget w;
{
    register Mapping mp;
    register int i;

    invalidate_draw_context (w);

    /* Free any mapping storage. */
    if (w->gterm.mappings) {
	for (i=0;  i < w->gterm.maxMappings;  i++) {
	    mp = &w->gterm.mappings[i];
	    if (mp->defined)
		free_mapping (w, mp);
	}
	XtFree ((char *)w->gterm.mappings);
	w->gterm.mp_head = NULL;
	w->gterm.mp_tail = NULL;
    }

    /* Allocate the initially empty mapping descriptors. */
    w->gterm.mappings =
	(Mapping) XtCalloc (w->gterm.maxMappings, sizeof (struct mapping));

    for (i=0;  i < w->gterm.maxMappings;  i++) {
	mp = &w->gterm.mappings[i];
	mp->mapping = i;
    }

    w->gterm.nmappings = 0;
}


/* GtNextMapping -- Return the index of the next available mapping descriptor.
 * This routine always returns a mapping index of 1 or higher.
 */
GtNextMapping (w)
    register GtermWidget w;
{
    register Mapping mp;
    register int i;

    for (i=1;  i < w->gterm.maxMappings;  i++) {
	mp = &w->gterm.mappings[i];
	if (!mp->defined)
	    return (i);
    }

    return (-1);
}


/* GtFreeMapping -- Free a mapping descriptor.
 */
GtFreeMapping (w, mapping)
    register GtermWidget w;
    int mapping;
{
    free_mapping (w, &w->gterm.mappings[mapping]);
}


/* GtRaiseMapping -- Set the stacking order of a mapping to one level
 * higher than the reference mapping.  If no reference mapping is given
 * the mapping is raised to the top of the stacking order.
 */
GtRaiseMapping (w, mapping, reference)
    register GtermWidget w;
    int mapping, reference;
{
    register Mapping mp, ref_mp;

    mp = &w->gterm.mappings[mapping];
    if (!mp->defined)
	return;

    if (reference <= 0 || reference >= w->gterm.maxMappings)
	ref_mp = w->gterm.mp_tail;
    else
	ref_mp = &w->gterm.mappings[reference];

    /* Already on top? */
    if (mp == w->gterm.mp_tail)
	return;

    mp_unlink (w, mp);
    mp_linkafter (w, mp, ref_mp);
}


/* GtLowerMapping -- Change the stacking order of a mapping relative to another
 * mapping, causing the first mapping to be drawn below the second.
 */
GtLowerMapping (w, mapping, reference)
    register GtermWidget w;
    int mapping, reference;
{
    register Mapping mp, ref_mp;

    mp = &w->gterm.mappings[mapping];
    if (!mp->defined)
	return;

    if (reference <= 0 || reference >= w->gterm.maxMappings)
	ref_mp = NULL;
    else
	ref_mp = &w->gterm.mappings[reference];

    /* Already lowered? */
    if (mp == w->gterm.mp_head)
	return;

    /* Lower it. */
    mp_unlink (w, mp);
    if (ref_mp && ref_mp->prev)
	mp_linkafter (w, mp, ref_mp->prev);
    else {
	mp->next = w->gterm.mp_head;
	w->gterm.mp_head = mp;
	if (mp->next)
	    mp->next->prev = mp;
	if (!w->gterm.mp_tail)
	    w->gterm.mp_tail = mp;
    }
}


/* GtCompareMappings -- Compare the stacking order of two mappings.  A
 * negative value is returned if the m1 < m2, zero is returned if the
 * mappings are the same, and a positive value is returned if m1 > m2.
 */
GtCompareMappings (w, map1, map2)
    register GtermWidget w;
    int map1, map2;
{
    register Mapping mp, mp1, mp2;

    if (map1 == map2)
	return (0);

    mp1 = &w->gterm.mappings[map1];
    mp2 = &w->gterm.mappings[map2];

    for (mp = w->gterm.mp_head;  mp;  mp = mp->next)
	if (mp == mp1)
	    return (-1);
	else if (mp == mp2)
	    return (1);

    return (0);
}


/* GtSelectRaster -- Select the raster which maps to the given raster pixel,
 * and transform the coordinates back to the source raster.  The raster number
 * and the raster coordinates of the source raster are returned.  If no raster
 * maps to the given pixel, raster=src and source raster coordinates are
 * returned.
 *
 * The raster pixel coordinate system is best explained by an example.
 * Suppose we have a 10x10 raster mapped into a 500x500 window.  The
 * window pixel 0 on an axis has raster pixel coordinate 0.0; pixel 500
 * (which is outside the window) has raster pixel coordinate 10.0.  The
 * coordinates correspond to the edge of the pixel as displayed in the
 * window, i.e., the left edge of the (nonflipped) window is at x=0.0, and
 * the right edge at x=10.0.  Due to the pixelization of the screen, the
 * maximum value is a limit which is only approached as the magnification
 * increases.
 *
 * The client application may have a different coordinate system than the
 * above.  For example, if the client wants an integer pixel value to be
 * at the center of a pixel, the first pixel has the coordinate [1,1], and
 * the raster is 10 pixels wide, the client coordinate system would range
 * from 0.5 to 10.5 at the edges of the NDC space.
 */
GtSelectRaster (w, dras, dt, dx, dy, rt, rx, ry, rmap)
    GtermWidget w;
    int dras;		/* display raster */
    int dt;		/* coordinate type of input coords */
    int dx, dy;		/* display raster coordinates */
    int rt;		/* coordinate type for output */
    int *rx, *ry;	/* raster coordinates (output) */
    int *rmap;		/* mapping selected */
{
    register Mapping mp;
    float x, y, x2, y2;
    int raster, mapping;

    /* Get display raster pixel coordinates. */
    if (dt != GtPixel) {
	struct mapping sv_mp, p_mp;
	initialize_mapping (&sv_mp);				/* MF035 */
	save_mapping (&sv_mp, 0, 0,
	    0,  0, 0,0,0,0,
	    0, dt, dx,dy,0,0);
	get_pixel_mapping (w, &sv_mp, &p_mp, 0);

	dx = p_mp.dx;
	dy = p_mp.dy;
    }

    /* Search for a mapping which maps to this pixel.  The mapping we are
     * looking for is the mapping closest to the tail of the mapping list
     * (highest stacking order) which is defined and enabled and which
     * includes the given display raster pixel in its destination rect.
     */
    for (mp = w->gterm.mp_tail, mapping = -1;  mp;  mp = mp->prev) {
	if (mp->defined && mp->enabled && mp->dst == dras) {
	    struct mapping *map, p_mp;
	    int dnx, dny;

	    get_pixel_mapping (w, mp, &p_mp, 0);
	    map = &p_mp;

	    if ((dnx = map->dnx) < 0)
		dnx = -dnx;
	    if ((dny = map->dny) < 0)
		dny = -dny;

	    /* Is display raster pixel in destination rect for this mapping?
	     */
	    if (dnx > 0 && dx >= map->dx && dx < map->dx + dnx &&
		dny > 0 && dy >= map->dy && dy < map->dy + dny) {

		/* Compute offset into destination rect and apply axis flip
		 * if any from mapping.
		 */
		x = dx - map->dx + 0.5;
		if (map->dnx < 0)
		    x = dnx - x;
		y = dy - map->dy + 0.5;
		if (map->dny < 0)
		    y = dny - y;

		/* Compute the source raster coordinates corresponding to
		 * the given display pixel.  This is done in floating point
		 * to permit fractional pixel resolution if the mapping
		 * zooms the raster.
		 */
		x = x * (float)map->snx / (float)dnx + map->sx;
		y = y * (float)map->sny / (float)dny + map->sy;

		mapping = map->mapping;
		raster = map->src;
		x2 = w->gterm.rasters[raster].width;
		y2 = w->gterm.rasters[raster].height;

		break;
	    }
	}
    }

    /* Return display raster coordinates if no mapped raster was found.
     */
    if (mapping < 0) {
	x = dx;  y = dy;
	x2 = (int)w->core.width - 1;
	y2 = (int)w->core.height - 1;
	raster = dras;
    }

    /* Output coordinates of the requested type.  The increased resolution
     * of NDC coordinates allows fractional pixel coordinates to be returned
     * (e.g. 1/32 of a pixel for a 1K raster).
     */
    if (rt == GtPixel) {
	*rx = x;
	*ry = y;
    } else {
	*rx = (     x) / x2 * MAXNDC;
	*ry = (y2 - y) / y2 * MAXNDC;	/* NDC is flipped in Y */
    }

    *rmap = mapping;
    return (raster);
}


/* GtCopyRaster -- Copy a region of the source raster to a region of the
 * destination raster.  If the input and output regions are not the same
 * size the subimage is automatically scaled to fit the destination region.
 * If the destination extent DNX or DNY is negative, the image is flipped in
 * that axis.  The type of spatial scaling performed is determined by the
 * scale factors (zoom, dezoom, or no scaling).  The rasterop argument is
 * used to exercise fine control over how the mapping is performed, e.g., to
 * force a refresh, implement a transient mapping, or in the case of a dezoom
 * (many-to-one) mapping, select the antialiasing technique to be used.
 */
GtCopyRaster (w, rop, src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny)
    GtermWidget w;
    int rop;			/* rasterop */
    int src;			/* 0=window, >0 = raster number */
    int st;			/* coordinate type for source raster */
    int sx,sy,snx,sny;		/* source raster */
    int dst;			/* 0=window, >0 = raster number */
    int dt;			/* coordinate type for destination raster */
    int dx,dy,dnx,dny;		/* destination raster */
{
    struct mapping sv_mp, p_mp;				/* MF007 */
    int status;

    if (!XtIsRealized ((Widget)w))
	return (OK);

    /* Construct a temporary mapping describing the desired raster copy. */
    initialize_mapping (&sv_mp);				/* MF035 */
    save_mapping (&sv_mp, w->gterm.maxMappings, 0,
	src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny);
    initialize_mapping (&p_mp);
    get_pixel_mapping (w, &sv_mp, &p_mp, 1);
    update_mapping (w, &p_mp);

    /* Refresh the destination pixels. */
    status = refresh_destination (w, &p_mp, dx, dy, abs(dnx), abs(dny));

    /* Discard the temporary mapping. */
    free_mapping (w, &p_mp);

    return (status);
}


/* GtSetMapping -- Define a new mapping function, or modify an old one.
 * If a new mapping is defined it is merely enabled, and no refreshing
 * of the screen takes place until either some mapped data is written
 * to or the mapping is explicitly refreshed.  If an existing mapping is
 * modified the old and new mappings are examined and only those portions
 * of the destination rect for which the mapping changed are updated.
 * This permits minor changes to a mapping (e.g. moving an edge) without
 * having to redraw the entire region.  Regions of the destination drawable
 * which were previously covered by the mapping but which were exposed by
 * modifying the mapping are redrawn.
 */
GtSetMapping (w, mapping, rop, src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny)
    GtermWidget w;
    int mapping;		/* mapping number */
    int rop;			/* rasterop */
    int src;			/* 0=window, >0 = raster number */
    int st;			/* coordinate type for source raster */
    int sx,sy,snx,sny;		/* source raster */
    int dst;			/* 0=window, >0 = raster number */
    int dt;			/* coordinate type for source raster */
    int dx,dy,dnx,dny;		/* destination raster */
{
    register int i, j;
    register Mapping mp, o_mp, n_mp;
    struct mapping pix_mp, new_mp;
    int defined, scale_changed, offset, current, state, old_i;
    int nx, xs[MAX_REGIONS], xe[MAX_REGIONS], xv[MAX_REGIONS];
    int ny, ys[MAX_REGIONS], ye[MAX_REGIONS], yv[MAX_REGIONS];
    int n_dnx, n_dny, n_xflip=0, n_yflip=0, i1, i2;
    int o_dnx, o_dny, o_xflip=0, o_yflip=0;
    int *o_xymap, *o_xmap, *o_ymap;
    int *n_xymap, *n_xmap, *n_ymap;
    int dummy_rop;						/* MF011 */
    XRectangle rl[MAX_REGIONS];
    int nrect, buflen, refresh;

    /* Check mapping number in range. */
    if (mapping < 0 || mapping >= w->gterm.maxMappings)
	return (ERR);
    else
	mp = &w->gterm.mappings[mapping];

    invalidate_draw_context (w);
    initialize_mapping (&pix_mp);
    initialize_mapping (&new_mp);

    /* Get local pixel space copy of old mapping, store new mapping. */
    get_pixel_mapping (w, mp, &pix_mp, 1);
    mp->src = src;  mp->st = st;
	mp->sx = sx;  mp->sy = sy;  mp->snx = snx;  mp->sny = sny;
    mp->dst = dst;  mp->dt = dt;
	mp->dx = dx;  mp->dy = dy;  mp->dnx = dnx;  mp->dny = dny;
    mp->rop = (rop & ~(R_RefreshNone|R_RefreshAll));
    mp->updated = 0;

    /* Newly defined mappings are linked at the tail of the mapping list,
     * i.e. they stack (display) on top of any other mappings.  If the
     * mapping is already defined the stacking order is not changed.
     */
    if (!(defined = mp->defined)) {
	mp_linkafter (w, mp, w->gterm.mp_tail);
	mp->defined = 1;
    }

    if (!valid_mapping (w, mp)) {
	mp_unlink (w, mp);
	mp->defined = 0;
	return (ERR);
    }
    update_mapping (w, mp);

    /* If we are defining a new mapping just define it and quit, without
     * refreshing the window, unless R_RefreshAll is explicitly set in the
     * mapping.  If the mapping is not enabled merely store the new mapping.
     * If the mapping is a null mapping (no pixels) do nothing.  If refresh
     * is disabled in the rasterop merely store the new mapping.  If we are
     * editing an existing mapping which is enabled with the default rasterop,
     * we continue on to compare the old and new mappings and refresh any
     * changed pixels in the destination rect.
     */
    if (!defined || src != mp->src || dst != mp->dst) {
	mp->enabled = mp->defined = 1;
	mp->refresh = 0;
	return (OK);
    } else if (!mp->enabled) {
	return (OK);
    } else if (snx == 0 || sny == 0 || dnx == 0 || dny == 0)
	return (OK);

    if (rop & R_RefreshNone)
	return (OK);

    /* Convert input mappings to pixel coordinates, we deal with only pixel
     * coordinates from here on.
     */
    get_pixel_mapping (w, mp, &new_mp, 1);
    load_mapping (&new_mp, &mapping, &dummy_rop,		/* MF011 */
	&src,&st,&sx,&sy,&snx,&sny, &dst,&dt,&dx,&dy,&dnx,&dny);
    update_mapping (w, n_mp = &new_mp);
    update_mapping (w, o_mp = &pix_mp);

    /*
     * We are editing an existing mapping.  Determine what has changed in
     * the mapping and refresh the changed regions.
     */

    /* Get old XY scaling maps.
     */
    o_xmap = o_mp->x_srcpix;
    o_ymap = o_mp->y_srcpix;

    if ((o_dnx = o_mp->dnx) < 0) {
	o_dnx = -o_dnx;
	o_xflip = 1;
    }
    if ((o_dny = o_mp->dny) < 0) {
	o_dny = -o_dny;
	o_yflip = 1;
    }

    /* Get new XY scaling maps.
     */
    n_xmap = n_mp->x_srcpix;
    n_ymap = n_mp->y_srcpix;

    if (dnx < 0) {
	dnx = -dnx;
	n_xflip = 1;
    }
    if (dny < 0) {
	dny = -dny;
	n_yflip = 1;
    }

    /* Refresh the entire region if the refresh flag is set, a flip has
     * occurred, or we are doing a complex dezoom mapping.
     */
    refresh = (o_mp->refresh || (rop & R_RefreshAll));
    if (n_xflip != o_xflip || n_yflip != o_yflip)
	refresh = 1;
    if (n_mp->scaling == M_DEZOOM)
	refresh = 1;

    /* Has the spatial scale changed? */
    scale_changed =
	abs (o_mp->xscale - n_mp->xscale) > 1.0E-4 ||
	abs (o_mp->yscale - n_mp->yscale) > 1.0E-4;

    /* If any of the above conditions are true refresh the entire mapping,
     * otherwise compare the old and new mappings and refresh any subregions
     * which have changed.
     */
    if (refresh || scale_changed || n_mp->scaling == M_DEZOOM) {
	refresh_destination (w, n_mp, dx, dy, dnx, dny);

    } else {
	/* Compare the old and new mappings to see what needs to be
	 * refreshed.  For speed reasons we only want to refresh the pixels
	 * which have been remapped.  Any destination pixel in the new mapping
	 * which does not map to the same source pixel as in the old mapping
	 * must be refreshed.  We examine each X and Y coordinate in the new
	 * destination rect and see if the source coordinate this maps to is
	 * the same as in the old mapping.  If a given destination pixel [i,j]
	 * maps to the same pixel [i,j] in the source as it did in the
	 * previous mapping, we do not need to refresh that pixel.  We examine
	 * the X and Y axis in turn and build up a list of regions which do or
	 * do not need to be refreshed.
	 */

	/* Get a list of ranges {XS,XE,XV} in X. */
	nx = get_regions (xs,xe,xv, MAX_REGIONS,
	    dx, dnx, n_xmap, o_mp->dx, o_dnx, o_xmap);

	/* Get a list of ranges {YS,YE,YV} in Y. */
	ny = get_regions (ys,ye,yv, MAX_REGIONS,
	    dy, dny, n_ymap, o_mp->dy, o_dny, o_ymap);

	/* The list of ranges in X and Y together define a raster of arbitary
	 * sized subrectangles filling the destination rect.  If the state
	 * value is nonzero (bit 1 set) in either X or Y, the subrectangle
	 * must be refreshed.  The get_rects routine returns a list of the
	 * rectangular subregions matching the given condition (bit 1 set in
	 * either axis).  Adjacent rectangles are merged to minimize the
	 * number of calls to refresh_destination.
	 */
	nrect = get_rects (rl, MAX_REGIONS, xs,xe,xv,nx, ys,ye,yv,ny, 1,1);
	for (i=0;  i < nrect;  i++)
	    refresh_destination (w, n_mp,
		rl[i].x, rl[i].y, rl[i].width, rl[i].height);
    }

    /* Refresh any lower level mappings exposed when the current mapping was
     * modified.  These will be regions of the old rect not present in the
     * new, modified rect for the current mapping.
     */
    nx = get_regions (xs,xe,xv, MAX_REGIONS,
	o_mp->dx, o_dnx, o_xmap, dx, dnx, n_xmap);
    ny = get_regions (ys,ye,yv, MAX_REGIONS,
	o_mp->dy, o_dny, o_ymap, dy, dny, n_ymap);
    nrect = get_rects (rl, MAX_REGIONS, xs,xe,xv,nx, ys,ye,yv,ny, 2,2);

    for (i=0;  i < nrect;  i++) {
	XRectangle r, in;
	Mapping mp;

	/* Clear the uncovered region. */
	GtSetPixels (w, dst, GtPixel, rl[i].x, rl[i].y, rl[i].width,
	    rl[i].height, GtGetClientPixel(w,0), 0);

	/* Refresh any lower level mappings the destination rects of
	 * which intersect the uncovered region.
	 */
	for (mp = w->gterm.mp_head;  mp && mp->mapping != mapping;
	    mp = mp->next) {

	    if (mp->enabled && mp->dst == dst) {
		r.x = mp->dx;
		r.y = mp->dy;
		r.width = mp->dnx;
		r.height = mp->dny;

		if (rect_intersect (&in, &r, &rl[i]))
		    refresh_destination (w, mp,
			in.x, in.y, in.width, in.height);
	    }
	}
    }

    free_mapping (w, n_mp);
    free_mapping (w, o_mp);
    mp = &w->gterm.mappings[mapping];
    mp->refresh = 0;

    return (OK);
}


/* GtGetMapping -- Return the external parameters of a mapping.  If the
 * numberd mapping is undefined -1 is returned; 0 is returned if the
 * mapping is defined but not enabled, and 1 is returned if the mapping
 * is active.
 */
GtGetMapping (w, mapping, rop, src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny)
    GtermWidget w;
    int mapping;		/* mapping number */
    int *rop;			/* rasterop */
    int *src;			/* 0=window, >0 = raster number */
    int *st;			/* coordinate type for source raster */
    int *sx,*sy,*snx,*sny;	/* source raster */
    int *dst;			/* 0=window, >0 = raster number */
    int *dt;			/* coordinate type for source raster */
    int *dx,*dy,*dnx,*dny;	/* destination raster */
{
    register Mapping mp;

    if (mapping < 0 || mapping >= w->gterm.maxMappings)
	return (-1);
    else if (!(mp = &w->gterm.mappings[mapping])->defined)
	return (-1);

    *rop = mp->rop;
    *src = mp->src;  *st = mp->st;
	*sx = mp->sx;  *sy = mp->sy;  *snx = mp->snx;  *sny = mp->sny;
    *dst = mp->dst;  *dt = mp->dt;
	*dx = mp->dx;  *dy = mp->dy;  *dnx = mp->dnx;  *dny = mp->dny;

    return (mp->enabled != 0);
}


/* GtActiveMapping -- Query whether a mapping is active.
 */
GtActiveMapping (w, mapping)
    register GtermWidget w;
    int mapping;		/* mapping number */
{
    register Mapping mp;

    if (mapping < 0 || mapping >= w->gterm.maxMappings)
	return (0);
    else if (!(mp = &w->gterm.mappings[mapping])->defined)
	return (0);

    return (mp->enabled != 0);
}


/* GtEnableMapping -- Enable a mapping.  Enabling a mapping does not
 * update the destination unless the refresh flag is set.  Enabling a
 * mapping activates the mapping so that any changes to the source will
 * be mapped to the destination.
 */
GtEnableMapping (w, mapping, refresh)
    GtermWidget w;
    int mapping;		/* mapping number */
    int refresh;		/* refresh destination */
{
    register Mapping mp;

    invalidate_draw_context (w);
    if (mapping >= 0 && mapping < w->gterm.maxMappings) {
	mp = &w->gterm.mappings[mapping];
	if (mp->defined) {
	    if (!mp->enabled) {
		mp->enabled = True;
		if (refresh)
		    GtRefreshMapping (w, mapping);
	    }
	    return (OK);
	}
    }
    return (ERR);
}


/* GtDisableMapping -- Disable a mapping.  Disabling a mapping does not
 * affect the mapping definition, hence a disabled mapping may later be
 * reenabled.  If the ERASE flag is set the destination region is redrawn
 * with the mapping disabled.
 */
GtDisableMapping (w, mapping, erase)
    GtermWidget w;
    int mapping;		/* mapping number */
    int erase;			/* erase the destination */
{
    register int i;
    register Mapping mp, dmp;
    XRectangle r, d, in;

    invalidate_draw_context (w);
    if (mapping >= 0 && mapping < w->gterm.maxMappings) {
	mp = &w->gterm.mappings[mapping];
	if (mp->defined) {
	    if (mp->enabled) {
		mp->enabled = False;

		if (erase) {
		    d.x = mp->dx;
		    d.y = mp->dy;
		    d.width = abs(mp->dnx);
		    d.height = abs(mp->dny);

		    /* Clear the uncovered region. */
		    GtSetPixels (w, mp->dst, GtPixel,
			d.x, d.y, d.width, d.height,
			GtGetClientPixel(w,0), 0);

		    /* Refresh any lower level mappings the destination rects of
		     * which intersect the uncovered region.
		     */
		    for (dmp = w->gterm.mp_head;
			dmp && dmp->mapping != mapping;  dmp = dmp->next) {

			if (dmp->enabled && dmp->dst == mp->dst) {
			    r.x = dmp->dx;
			    r.y = dmp->dy;
			    r.width = dmp->dnx;
			    r.height = dmp->dny;

			    if (rect_intersect (&in, &r, &d))
				refresh_destination (w, dmp,
				    in.x, in.y, in.width, in.height);
			}
		    }
		}
	    }
	    return (OK);
	}
    }

    return (ERR);
}


/* GtRefreshMapping -- Refresh the destination region defined by a mapping.
 */
GtRefreshMapping (w, mapping)
    GtermWidget w;
    int mapping;		/* mapping number */
{
    register Mapping mp;
    struct mapping p_mp;

    if (mapping >= 0 && mapping < w->gterm.maxMappings) {
	mp = &w->gterm.mappings[mapping];
	if (mp->defined) {
	    if (mp->st != GtPixel || mp->dt != GtPixel) {
		initialize_mapping (&p_mp);
		get_pixel_mapping (w, mp, &p_mp, 1);
		update_mapping (w, mp = &p_mp);
	    } else
		update_mapping (w, mp);

	    if (CacheXImage)					/* MF004 */
	        DestroyCachedXImage();				/* MF004 */

	    refresh_destination (w, mp,
		mp->dx, mp->dy, abs(mp->dnx), abs(mp->dny));
	    if (mp == &p_mp)
		free_mapping (w, mp);
	}
    }
}


/* GtMapVector -- Map a point vector from the coordinate system of one raster
 * to another as defined by the given mapping.  The mapping may be either in
 * the forward direction (dir = GtMap) or the reverse (dir = GtUnmap).  The
 * point vector is maintained in floating point for this operation to avoid
 * loss of precision.  The input and output vectors may be the same vector.
 */
GtMapVector (w, mapping, dir, pv1, pv2, npts)
    GtermWidget w;
    int mapping;
    int dir;			/* GtMap, GtUnmap */
    DPoint *pv1;
    DPoint *pv2;
    int npts;
{
    register DPoint *ip = pv1;
    register DPoint *op = pv2;
    register Mapping mp;
    register int n;

    struct mapping p_mp;
    double xscale, xoffset;
    double yscale, yoffset;
    int sx, sy;

    xscale = yscale = 1.0;
    xoffset = yoffset = 0.0;
    sx = sy = 0;

    if (mapping >= 0 && mapping < w->gterm.maxMappings) {
	mp = &w->gterm.mappings[mapping];
	if (valid_mapping (w, mp)) {
	    /* Determine the transformation coefficients. */
	    get_pixel_mapping (w, mp, &p_mp, 0);
	    mp = &p_mp;

	    xscale = (float)mp->dnx / (float)mp->snx;
	    if (xscale < 0)
		xoffset = mp->dx + abs(mp->dnx) - 1;
	    else
		xoffset = mp->dx;

	    yscale = (float)mp->dny / (float)mp->sny;
	    if (yscale < 0)
		yoffset = mp->dy + abs(mp->dny) - 1;
	    else
		yoffset = mp->dy;

	    sx = mp->sx;
	    sy = mp->sy;
	}
    }

    /* Map the vector. */
    if (dir == GtMap) {
	for (n=npts;  --n >= 0;  ip++, op++) {
	    op->x = (ip->x - sx) * xscale + xoffset;
	    op->y = (ip->y - sy) * yscale + yoffset;
	}
    } else {
	for (n=npts;  --n >= 0;  ip++, op++) {
	    op->x = (ip->x - xoffset) / xscale + sx;
	    op->y = (ip->y - yoffset) / yscale + sy;
	}
    }
}


/* GtPixelToNDC -- Transform a vector from pixel to NDC coordinates in the
 * coordinate system of the given reference raster.  The input and output
 * vectors may be the same vector.
 */
GtPixelToNDC (w, raster, pv1, pv2, npts)
    GtermWidget w;
    int raster;
    DPoint *pv1;
    DPoint *pv2;
    int npts;
{
    register Raster rp = &w->gterm.rasters[raster];
    register DPoint *ip = pv1;
    register DPoint *op = pv2;
    register int n;

    for (n=npts;  --n >= 0;  ip++, op++) {
	op->x = (             ip->x) / rp->width * MAXNDC;
	op->y = (rp->height - ip->y) / rp->height * MAXNDC;
    }
}


/* GtNDCToPixel -- Transform a vector from NDC to pixel coordinates in the
 * coordinate system of the given reference raster.  The input and output
 * vectors may be the same vector.
 */
GtNDCToPixel (w, raster, pv1, pv2, npts)
    GtermWidget w;
    int raster;
    DPoint *pv1;
    DPoint *pv2;
    int npts;
{
    register Raster rp = &w->gterm.rasters[raster];
    register DPoint *ip = pv1;
    register DPoint *op = pv2;
    register int n;

    for (n=npts;  --n >= 0;  ip++, op++) {
	op->x = ip->x / MAXNDC * rp->width;
	op->y = rp->height - (ip->y / MAXNDC * rp->height);
    }
}


/* GtDebug -- Print debug info.  If the file descriptor is NULL output is
 * to the process stdout.  The "what" argument may be used to select the
 * type of output desired.  If what=0 the full output is generated,
 * otherwise bits are used to select what output is to be generated.
 *
 * "what" bitflags:
 *
 *	001	Widget information
 *	002	List rasters
 *	004	List mappings
 *	010	List colormaps
 *	020	List markers
 *
 * This routine is intended only for use during debugging.
 */
GtDebug (w, fp, what)
    GtermWidget w;
    FILE *fp;
    int what;
{
    /* Default is to write everything to the stdout. */
    what = what ? what : 0777;
    fp = fp ? fp : stdout;

    /* Print widget header. */
    if (what & 001) {
	fprintf (fp, "Widget 0x%x (%s) %dx%d raster=%d\n",
	    w, w->core.name, w->core.width, w->core.height, w->gterm.raster);
	fprintf (fp,
	    "--------------------------------------------------------------\n");
    }

    /* Print raster information. */
    if (what & 002) {
	register int i;
	register Raster rp;

	if (w->gterm.rasters) {
	    for (i=0;  i < w->gterm.maxRasters;  i++) {
		rp = &w->gterm.rasters[i];
		if (!rp->type)
		    continue;
		fprintf (fp, "raster %4d type=%s delete=%d size=%dx%d\n",
		    i, rp->type == ImageRaster ? "client" : "server",
		    rp->delete, rp->width, rp->height);
	    }
	} else
	    fprintf (fp, "no rasters\n");
    }

    /* Print mapping information. */
    if (what & 004) {
	register int i;
	register Mapping mp;
	char flags[32];

	if (w->gterm.mappings) {
	    for (i=0;  i < w->gterm.maxMappings;  i++) {
		mp = &w->gterm.mappings[i];
		if (!mp->defined)
		    continue;

		flags[0] = mp->enabled ? 'E' : 'D';
		flags[1] = mp->updated ? 'U' : ' ';
		flags[2] = mp->refresh ? 'R' : ' ';
		flags[3] = '\0';

		fprintf (fp, "mapping %3d %s %8o", i, flags, mp->rop);
		fprintf (fp, "  %2d %s %3d %3d %3d %3d",
		    mp->src, mp->st == GtPixel ? "pix" : "ndc",
		    mp->sx, mp->sy, mp->snx, mp->sny);
		fprintf (fp, "  %2d %s %3d %3d %3d %3d\n",
		    mp->dst, mp->dt == GtPixel ? "pix" : "ndc",
		    mp->dx, mp->dy, mp->dnx, mp->dny);
	    }
	} else
	    fprintf (fp, "no mappings\n");

	fprintf (fp, "mappings from head: ");
	for (mp = w->gterm.mp_head;  mp;  mp = mp->next)
	    fprintf (fp, " %d", mp->mapping);
	fprintf (fp, "\n");

	fprintf (fp, "mappings from tail: ");
	for (mp = w->gterm.mp_tail;  mp;  mp = mp->prev)
	    fprintf (fp, " %d", mp->mapping);
	fprintf (fp, "\n");
    }

    /* Print colormap information. */
    if (what & 010) {
	register struct colormap *cm;

	fprintf (fp, "cmapName=%s ncolors=%d basePixel=%d\n",
	    w->gterm.cmapName, w->gterm.ncolors, w->gterm.base_pixel);
	for (cm = w->gterm.colormaps;  cm;  cm = cm->next)
	    fprintf (fp, "colormap %2d ncells=%d\n", cm->map, cm->ncells);
    }

    /* Print marker information. */
    if (what & 020) {
	register Marker mm;
	char value[256];

	for (mm = w->gterm.gm_head;  mm;  mm = mm->next) {
	    GmGetAttribute (mm, GmType, (XtArgVal)value, XtRString);
	    fprintf (fp, "marker 0x%x: %10s flags=0x%x [%d %d %d %d] %0.5g\n",
		mm, value, mm->flags, mm->x, mm->y, mm->width, mm->height,
		mm->rotangle);
	}
    }
}


/*
 * Internal procedures for the above code.
 * ----------------------------------------
 */

/* get_colormap -- Get a private colormap.  On all calls after the first
 * this just returns the existing gterm widget colormap.  On the first call
 * we query the server for the named custom colormap, and if the colormap
 * exists we modify the gterm widget to use it.  If the custom colormap has
 * not yet been created by some other client, we create it.
 *
 * This code creates a custom colormap using the "standard colormap"
 * facilities provided by XLIB.  Although we do not use any of the predefined
 * standard colormaps, use of the standard colormap facilities allows any
 * number of clients to share the same custom colormap.  Use of a custom
 * colormap helps avoid running out of space in the default colormap, ensures
 * that the gterm widget will get the color cells it needs, and  makes it
 * easier for several imaging clients which share the same colormap to
 * simultaneously display their windows.
 *
 * To minimize colormap flashing we try to avoid using the full colormap,
 * setting the unused cells to the colors set in the default colormap.  In
 * most cases this will prevent the rest of the screen from changing color
 * when the custom colormap is installed.
 */
static Colormap
get_colormap (w)
    GtermWidget w;
{
    register int i, j;
    Display *display = w->gterm.display;
    Screen *screen = w->gterm.screen;
    XColor def_colors[SZ_STATIC_CMAP], *cp, *c1, *c2;
    XStandardColormap cm, *cm_p;
    XColor colors[MAX_SZCMAP];
    int base_pixel, p1, p2;
    Colormap colormap;
    char property[128];
    int ncmap, nitems;
    Pixel pixel;
    Atom atom;

    if (w->gterm.haveColormap)
	return (w->core.colormap);

    /* Map custom colormap name to atom. */
    sprintf (property, "GT_%s", w->gterm.cmapName);
    atom = XInternAtom (display, property, False);
    w->gterm.cmapAtom = atom;

    /* Get custom colormap.
     */
    if (!w->gterm.cmapInitialize &&
	    XGetRGBColormaps (display, w->gterm.root, &cm_p, &ncmap, atom)) {

	/* Colormap aleady exists, just use it.
	 */
	cm = *cm_p;
	colormap = cm.colormap;
	w->gterm.base_pixel = cm.base_pixel;

    } else {
	/* Create or reinitialize a global colormap.
	 */
	XVisualInfo template, *vi;
	Display *d;
	Screen *s;
	Window root;
	long mask;

	if (!(d = XOpenDisplay (DisplayString(display))))
	    goto use_default;
	s = DefaultScreenOfDisplay (d);
	root = DefaultRootWindow (d);

	/* Try to get a pseudocolor visual. */
	mask = 0;
	template.screen = DefaultScreen(d);	mask |= VisualScreenMask;
	template.depth  = RasterDepth;		mask |= VisualDepthMask;
	template.class  = PseudoColor;		mask |= VisualClassMask;

	if (!(vi = XGetVisualInfo (d, mask, &template, &nitems))) {
	    XCloseDisplay (d);
	    goto use_default;
	}

	/* Create custom colormap with all cells allocated read/write */
	colormap = XCreateColormap (d, root, vi->visual, AllocAll);

	/* Initialize colormap to be same as default colormap. */
	nitems = min (MAX_SZCMAP, CellsOfScreen(s));
	for (i=0;  i < nitems;  i++)
	    colors[i].pixel = i;
	XQueryColors (d, DefaultColormapOfScreen(s), colors, nitems);
	XStoreColors (d, colormap, colors, nitems);

	/* Globally define permanent server custom colormap. */
	memset ((char *)&cm, 0, sizeof(cm));
	cm.colormap = colormap;
	cm.base_pixel = w->gterm.base_pixel;
	cm.red_max = 0;
	cm.visualid = vi->visualid;
	cm.killid = 1;
	XSetRGBColormaps (d, root, &cm, 1, atom);

	XSetCloseDownMode (d, RetainPermanent);
	XCloseDisplay (d);
	w->gterm.cmapInitialize = False;

	/* Free the XVisualInfo struct. */
	if (vi)
	    XFree ((void *)vi);					/* MF040 */
    }

    /* Save default color assignments for static colors. */
    for (i=0;  i < SZ_STATIC_CMAP;  i++)
	def_colors[i] = w->gterm.color[i];

    nitems = min (MAX_SZCMAP, CellsOfScreen(screen));
    w->gterm.ncolors = SZ_STATIC_CMAP + w->gterm.maxColors;
    base_pixel = w->gterm.base_pixel;

    /* Get the private colormap. */
    for (i=0;  i < nitems;  i++)
	colors[i].pixel = i;
    XQueryColors (display, colormap, colors, nitems);

    /* Initialize the raster pixel to display pixel mapping and set the
     * color assigned to each pixel value in the private colormap.
     */
    for (i = SZ_STATIC_CMAP;  i < w->gterm.ncolors;  i++) {
	w->gterm.color[i].pixel = w->gterm.cmap[i] = pixel =
	    min (nitems - 1, base_pixel + i - SZ_STATIC_CMAP);
	w->gterm.color[i] = colors[pixel];
    }

    /* Check the static part of the cmap to make sure that the pixel numbers
     * aren't aliased to pixels in the dynamic part of the custom colormap.
     * If this happens, reassign these color numbers to the pixels just
     * preceeding the dynamic part of the custom colormap.  The red_max
     * field of the colormap descriptor is used to keep track of the number
     * of static colors allocated by different clients.  These static colors
     * are shared, hence the same color will not be stored twice.
     */
    p1 = p2 = base_pixel - cm.red_max;
    for (i=0;  i < SZ_STATIC_CMAP;  i++) {
	pixel = w->gterm.cmap[i];
	if (pixel >= base_pixel && pixel < base_pixel+DEF_MAXCOLORS && p1 > 2) {
	    /* First check to see if we already have a static entry reserved
	     * for this color.
	     */
	    c1 = &def_colors[i];
	    for (j=p1, cp=NULL;  j < base_pixel;  j++) {
		c2 = &colors[j];
		if (c1->red == c2->red && c1->green == c2->green &&
			c1->blue == c2->blue) {
		    cp = c2;
		    break;
		}
	    }

	    /* Assign a new screen pixel value. */
	    if (cp)
		w->gterm.cmap[i] = cp->pixel;
	    else {
		cp = &colors[--p1];
		*cp = def_colors[i];
		cp->flags = (DoRed | DoGreen | DoBlue);
		cp->pixel = w->gterm.cmap[i] = p1;
		cm.red_max++;
	    }
	    w->gterm.color[i].pixel = w->gterm.cmap[i];
	}
    }
    if (p1 < p2) {
	XStoreColors (display, colormap, &colors[p1], p2 - p1);
	XSetRGBColormaps (display, w->gterm.root, &cm, 1, atom);
    }

    /* Assign the new colormap to the gterm widget's window. */
    XtVaSetValues ((Widget)w, XtNcolormap, (XtArgVal)colormap, NULL);
    w->gterm.haveColormap++;

    /* If the pointer is in the window, advise window manager to load the
     * colortable for the window.
     */
    if (w->gterm.in_window)
{ printf ("get_colormap ... requesting focus...\n");
	request_colormap_focus (w);
}

    return (colormap);

use_default:
    /* Unable to create custom colormap. */
    w->gterm.useDefaultCM++;
    w->gterm.haveColormap++;
    return (w->core.colormap);
}


/* request_colormap_focus -- Modify the WM_COLORMAP_WINDOWS property on a
 * widget's top level shell window to advise the window manager that the
 * widget's window should have its colormap loaded.  This should only be
 * used for windows that have a colormap different than that of the top
 * level window.
 */
static
request_colormap_focus (w)
    GtermWidget w;
{
    Widget p;

    if (!XtIsRealized ((Widget)w))
	return;

    /* Find the top level window. */
    for (p = XtParent(w);  !XtIsShell(p);  p = XtParent(p))
	;

    /* Modify WM_COLORMAP_WINDOWS to give the current window priority.
     */
    if (p) {
	Window window = XtWindow (p);
	Window *wl = NULL, n_wl[MAX_WMWIN+1];
	register int n_nw, i;
	int nw;

	/* If WM_COLORMAP_WINDOWS is already set save its value, otherwise
	 * start a list initially containing only the top level window.
	 */
	w->gterm.wmTop = window;
	if (XGetWMColormapWindows (w->gterm.display, window, &wl, &nw)) {
	    memmove (w->gterm.wmWindows, (char *)wl, nw * sizeof(int));
	    w->gterm.n_wmWindows = nw = min (nw, MAX_WMWIN);
	    free ((char *)wl);
	} else {
	    w->gterm.wmWindows[0] = window;
	    w->gterm.n_wmWindows = nw = 1;
	}

	n_nw = 0;
	wl = w->gterm.wmWindows;
	n_wl[n_nw++] = XtWindow(w);

	for (i=0;  i < nw;  i++)
	    if (wl[i] != XtWindow(w))
		n_wl[n_nw++] = wl[i];

	XSetWMColormapWindows (w->gterm.display, window, n_wl, n_nw);
    }
}


/* restore_colormap_focus -- Reset WM_COLORMAP_WINDOWS.  Retain the window
 * that had the focus in the list, but drop its priority one notch.  This
 * should follow a prior call to request_colormap_focus.
 */
static
restore_colormap_focus (w)
    GtermWidget w;
{
    register int nw, n_nw, i;
    Window *wl, n_wl[MAX_WMWIN+1], old;

    if (!XtIsRealized ((Widget)w))
	return;

    old = XtWindow(w);
    wl = w->gterm.wmWindows;
    if ((nw = w->gterm.n_wmWindows) == 0 || (nw == 1 && wl[0] == old))
	return;

    n_nw = 0;
    if (wl[0] != old)
	n_wl[n_nw++] = wl[0];
    n_wl[n_nw++] = old;

    for (i=1;  i < nw;  i++)
	if (wl[i] != old)
	    n_wl[n_nw++] = wl[i];

    XSetWMColormapWindows (w->gterm.display, w->gterm.wmTop, n_wl, n_nw);
}


/* inherit_default_colormap -- Set any unused cells of the custom colormap
 * to the colors defined for the corresponding cells of the default colormap.
 * This minimizes colormap flashing when using a custom colormap, but only
 * works if a few unused cells can be reserved, e.g., at the beginning of
 * the colormap (which is usually where X allocates its colors).
 */
static
inherit_default_colormap (w)
    GtermWidget w;
{
    register XColor *cp, *ap;
    register int ncolors, i;
    Display *display = w->gterm.display;
    Screen *screen = w->gterm.screen;
    Window root = w->gterm.root;
    Atom atom = w->gterm.cmapAtom;
    XColor colors[MAX_SZCMAP];
    XStandardColormap *cm;
    int first, nitems, ncmap;

    if (!XtIsRealized ((Widget)w))
	return;
    if (w->gterm.useDefaultCM || !w->gterm.haveColormap)
	return;
    if (w->gterm.base_pixel <= 0)
	return;				/* fully allocated colormap */

    /* We have to read the colormap property again as another client could
     * have reserved more static colors (i.e.,changed red_max), and we don't
     * want to clobber these colors.
     */
    if (XGetRGBColormaps (display, root, &cm, &ncmap, atom)) {
	/* Make sure we have the right colormap. */
	if (w->core.colormap != cm->colormap)
	    XtVaSetValues ((Widget)w,XtNcolormap,(XtArgVal)cm->colormap,NULL);

	/* Get lower part of default colormap. */
	ncolors = cm->base_pixel - cm->red_max;
	for (cp=colors, i=0;  i < ncolors;  i++, cp++) {
	    cp->flags = (DoRed | DoGreen | DoBlue);
	    cp->pixel = i;
	}

	/* Get upper part of default colormap. */
	first = cm->base_pixel + w->gterm.ncolors - SZ_STATIC_CMAP;
	ncolors = min (MAX_SZCMAP, CellsOfScreen(screen)) - first;
	for (i=0;  i < ncolors;  i++, cp++) {
	    cp->flags = (DoRed | DoGreen | DoBlue);
	    cp->pixel = first + i;
	}

	/* Inherit values from default colormap. */
	ncolors = cp - colors;
	XQueryColors (display, DefaultColormapOfScreen(screen),
	    colors, ncolors);
	XStoreColors (display, w->core.colormap, colors, ncolors);

	/* The global gterm colormap may have changed.  Compare widget's
	 * version of color table with the global colortable and update
	 * the widget's state if the global colortable has changed.
	 */
	ncolors = w->gterm.ncolors;
	memmove (colors, w->gterm.color, ncolors * sizeof(*cp));
	XQueryColors (display, w->core.colormap, colors, ncolors);
	for (i=ncolors, cp=colors, ap=w->gterm.color;  --i >= 0;  cp++, ap++)
	    if (cp->red != ap->red || cp->green != ap->green ||
		    cp->blue != ap->blue) {
		memmove (w->gterm.color, colors, ncolors * sizeof(*cp));
		invalidate_cmap (w);
	    }
    }
}


/* update_default_colormap -- Update the default colormap so that any
 * unallocated cells mirror the widget's custom colormap.  This increases
 * the chance that the widget's contents will be visible when the window
 * does not have the colormap focus, and minimizes flashing when the
 * colormap focus changes.
 */
static
update_default_colormap (w)
    GtermWidget w;
{
    register XColor *ip, *op;
    register int j, n;
    register Pixel v;

    XColor colors[MAX_SZCMAP];
    Pixel pixels[MAX_SZCMAP];
    char allocated[MAX_SZCMAP];
    int overflow, req, need, first, nelem, i;
    unsigned long plane_masks[1];
    Colormap defcmap;

    if (!XtIsRealized ((Widget)w))
	return;
    if (w->gterm.useDefaultCM || !w->gterm.haveColormap)
	return;

    first = SZ_STATIC_CMAP;
    nelem = w->gterm.ncolors;

    defcmap = DefaultColormapOfScreen (w->gterm.screen);
    /* need = min (MAX_SZCMAP, first + nelem - SZ_STATIC_CMAP); */
    need = MAX_SZCMAP;

    /* Get the colormap cells. */
    for (req=need, n=0;  req > 0 && n < need;  )
	if (XAllocColorCells (w->gterm.display, defcmap, False,
		plane_masks, 0, &pixels[n], req)) {
	    n += req;
	} else
	    req /= 2;

    /* Perform the color matching.  This is awkward as the pixel value
     * assignments may be different in the two colormaps.  We have to look
     * up each pixel before attempting to assign a color, or XStoreColors
     * below will result in a server error.
     */
    memset (allocated, 0, sizeof(allocated));
    overflow = 0;

    for (i=0;  i < n;  i++) {
	v = pixels[i];
	if (v < MAX_SZCMAP)
	    allocated[v] = 1;
	else {
	    overflow++;
	    break;
	}
    }

    ip = &w->gterm.color[first];
    op = colors;
    if (overflow) {
	for (i=0;  i < nelem;  i++, ip++)
	    for (j=0, v = ip->pixel;  j < n;  j++)
		if (pixels[j] == v) {
		    *op++ = *ip;
		    break;
		}
    } else {
	for (j=0;  j < nelem;  j++, ip++)
	    if (allocated[ip->pixel]) {
		allocated[ip->pixel] = 0;
		*op++ = *ip;
	    }
    }

    if (op > colors)
	XStoreColors (w->gterm.display, defcmap,
	    colors, op - colors);

    XFreeColors (w->gterm.display, defcmap, pixels, n, 0);
}


/* refresh_source -- Refresh a portion of a mapping given the source rect
 * to be refreshed.  If the given source rect is not within the mapping,
 * this is a no-op.
 */
static
refresh_source (w, mp, x1, y1, nx, ny)
    GtermWidget w;
    register Mapping mp;	/* mapping defining refresh operation */
    int x1, y1, nx, ny;		/* region of source to be refreshed */
{
    int sx1, sx2, sy1, sy2, snx, sny;
    int dx1, dx2, dy1, dy2, dnx, dny;

    /* Do nothing if mapping not defined and enabled. */
    if (!valid_mapping (w, mp))
	return (ERR);
    if (!mp->enabled)
	return (OK);

    /* Compute the intersection of the modified region of the source raster
     * with the rectangular region of the source raster affected by the given
     * mapping.
     */
    sx1 = max (x1, mp->sx);
    sy1 = max (y1, mp->sy);
    sx2 = min(x1 + nx, mp->sx + mp->snx) - 1;
    sy2 = min(y1 + ny, mp->sy + mp->sny) - 1;
    snx = sx2 - sx1 + 1;
    sny = sy2 - sy1 + 1;

    /* Do nothing if the rectangles do not intersect. */
    if (snx <= 0 || sny <= 0)
	return (OK);

    /* Compute the destination rect affected by the mapped source rect.
     */
    dx1 = mp->x_extent[sx1 - mp->sx].lo;
    dx2 = mp->x_extent[sx2 - mp->sx].hi;
    if (dx1 > dx2) {
	dx1 = mp->x_extent[sx2 - mp->sx].lo;
	dx2 = mp->x_extent[sx1 - mp->sx].hi;
    }

    dy1 = mp->y_extent[sy1 - mp->sy].lo;
    dy2 = mp->y_extent[sy2 - mp->sy].hi;
    if (dy1 > dy2) {
	dy1 = mp->y_extent[sy2 - mp->sy].lo;
	dy2 = mp->y_extent[sy1 - mp->sy].hi;
    }

    dnx = dx2 - dx1 + 1;
    dny = dy2 - dy1 + 1;

    if (CacheXImage)						/* MF004 */
        DestroyCachedXImage();					/* MF004 */

    /* Perform the refresh operation. */
    return (refresh_destination (w, mp, dx1, dy1, dnx, dny));
}


/* refresh_destination -- Refresh (redraw) the pixels in the given destination
 * rect.  The mapping operand defines how to generate the value of each output
 * pixel.  This is the routine which does all the real work of a mapping,
 * computing the values of the output pixels and writing to the destination
 * drawable.  Note: the destination rect must be specified in raster pixel
 * coordinates (no NDC).
 */
static
refresh_destination (w, mp, x1, y1, nx, ny)
    GtermWidget w;
    Mapping mp;			/* mapping defining refresh operation */
    int x1, y1, nx, ny;		/* region of destination to be refreshed */
{
    Raster sr, dr;
    Display *display = w->gterm.display;
    int scaling, xflip, yflip, delxin=0, delxout=0;
    int ox, oy, rop, clip, mapping, i, j;
    int src, st, sx, sy, snx, sny;
    int dst, dt, dx, dy, dnx, dny;
    int xoff, yoff, p1, p2, q1, q2;
    float xscale, yscale;
    struct mapping *np, p_mp;
    XImage *xin, *xout;
    int status = OK;
    Pixmap pixmap;						/* MF004 */

    Region clip_region, mask_region;
    uchar *old_xin_lp, *old_xout_lp;
    uchar *xin_lp, *xout_lp, *op;
    int xin_bpl, xout_bpl;
    int *xmap, *ymap;
    XRectangle r;

    if (!XtIsRealized ((Widget)w))
	return (OK);

    /* Do nothing if mapping not defined and enabled. */
    if (!valid_mapping (w, mp))
	return (ERR);
    if (!mp->enabled)
	return (OK);

    /* Offsets into the x_*,y_* mapping lookup tables. */
    xoff = x1 - mp->dx;
    yoff = y1 - mp->dy;

    /* Get source and destination rects. */
    dst = mp->dst;
    dx = x1;	dy = y1;
    dnx = nx;	dny = ny;

    src = mp->src;
    p1 = mp->x_srcpix[xoff];
    q1 = mp->y_srcpix[yoff];
    p2 = mp->x_srcpix[xoff + nx - 1];
    q2 = mp->y_srcpix[yoff + ny - 1];
    sx = min (p1, p2);
    sy = min (q1, q2);
    snx = abs (p2 - p1) + 1;
    sny = abs (q2 - q1) + 1;

    /* Define some local variables. */
    sr = &w->gterm.rasters[src];
    dr = &w->gterm.rasters[dst];
    mapping = mp->mapping;
    scaling = mp->scaling;
    xscale = mp->xscale;
    yscale = mp->yscale;
    rop = mp->rop;

    if (!sr->type || !dr->type)
	return (ERR);
    if (snx <= 0 || sny <= 0 || dnx == 0 || dny == 0)
	return (ERR);
    if (src < 0 || src > w->gterm.maxRasters ||
	dst < 0 || dst > w->gterm.maxRasters)
	return (ERR);

    /* Do we have a flip in X or Y? */
    xflip = mp->dnx < 0;
    yflip = mp->dny < 0;

    /* Any higher numbered mappings which map to the same destination as the
     * mapping MP will obscure the current mapping.  Construct a clip mask
     * defining the region of the destination we can write to.  This will be
     * the region not obscured by any higher numbered, active mappings.
     */
    clip = False;
    clip_region = XCreateRegion();
    r.x = dx;  r.y = dy;
    r.width = dnx;  r.height = dny;
#ifdef sun
    /* As far as I can tell the Sun (probably in the OW X server) has an
     * off by one bug affecting clipping in the server.  A clip region is
     * too small by one pixel at the right and bottom, causing these pixels
     * to not be written when refreshing the screen (usually this shows up
     * as black lines on the screen when a region is refreshed).  So far
     * I haven't seen this on any other platform.  The problem is imperfectly
     * understood and the following workaround may not completely workaround
     * the problem.
     */
    r.width++;  r.height++;
#endif
    XUnionRectWithRegion (&r, clip_region, clip_region);

    for (np = mp->next;  np;  np = np->next) {
	struct mapping p_np;

	if (!np->enabled || np->dst != dst)
	    continue;
	get_pixel_mapping (w, np, &p_np, 0);

	r.x = p_np.dx;  r.y = p_np.dy;
	r.width = abs(p_np.dnx);
	r.height = abs(p_np.dny);

	if (XRectInRegion (clip_region,
	    r.x, r.y, r.width, r.height) != RectangleOut) {

	    mask_region = XCreateRegion();
	    XUnionRectWithRegion (&r, mask_region, mask_region);
	    XSubtractRegion (clip_region, mask_region, clip_region);
	    XDestroyRegion (mask_region);
	    clip++;
	}
    }

    if (clip && dr->type == PixmapRaster)
	XSetRegion (w->gterm.display, w->gterm.exposeGC, clip_region);

    /* Handle the special case of a pixmap to pixmap (or window) copy in
     * the server with no scaling.
     */
    if (!scaling && sr->type == PixmapRaster && dr->type == PixmapRaster) {
	if (src == 0 && dst == 0 && w->gterm.pixmap && !(rop & R_Transient)) {
	    XCopyArea (display, w->gterm.pixmap, w->gterm.pixmap,
		w->gterm.exposeGC, sx, sy, snx, sny, dx, dy);
	    XCopyArea (display, w->gterm.pixmap, dr->r.pixmap,
		w->gterm.exposeGC, dx, dy, dnx, dny, dx, dy);
	} else {
	    XCopyArea (display, sr->r.pixmap, dr->r.pixmap, w->gterm.exposeGC,
		sx, sy, snx, sny, dx, dy);
	    if (dst == 0 && w->gterm.pixmap && !(rop & R_Transient))
		XCopyArea (display, sr->r.pixmap, w->gterm.pixmap,
		    w->gterm.exposeGC, sx, sy, snx, sny, dx, dy);
	}
	goto done;
    }

    /* Any other case requires working on ximages in the client.  The first
     * step is to get the source ximage.
     */
    if (sr->type == PixmapRaster) {
	/* Source is a pixmap but we need a local copy as either the
	 * destination is not a pixmap, or we need to do some scaling.
	 */
	if (CacheXImage) {					/* MF004 */
            pixmap = (src || !w->gterm.pixmap) ? sr->r.pixmap : w->gterm.pixmap;
            xin = GetCachedXImage (w, pixmap, sr->width, sr->height);
            if (xin == NULL) {
                xin = XGetImage (display, pixmap,
                    0, 0, sr->width, sr->height, 0xff, ZPixmap);
                if (xin == NULL) {
                    status = ERR;
                    goto done;
                } else
                    NewCachedXImage (w, xin, pixmap, sr->width, sr->height);
            }
	} else {						/* MF004 */
	    xin = XGetImage (display,
	        (src || !w->gterm.pixmap) ? sr->r.pixmap : w->gterm.pixmap,
	        0, 0, sr->width, sr->height, 0xff, ZPixmap);
	    if (xin == NULL) {
	        status = ERR;
	        goto done;
	    }
   	    delxin++;
	}							/* MF004 */

    } else {
	/* Source is an ximage. */
	xin = sr->r.ximage;
    }

    /* Handle the special case of a copy of an ximage to an output pixmap
     * with no scaling.
     */
    if (!scaling && dr->type == PixmapRaster) {
	if (dst == 0 && w->gterm.pixmap && !(rop & R_Transient)) {
	    XPutImage (display, w->gterm.pixmap, w->gterm.exposeGC, xin,
		sx, sy, dx, dy, dnx, dny);
	    XCopyArea (display, w->gterm.pixmap, dr->r.pixmap,
		w->gterm.exposeGC, dx, dy, dnx, dny, dx, dy);
	} else {
	    XPutImage (display, dr->r.pixmap, w->gterm.exposeGC, xin,
		sx, sy, dx, dy, dnx, dny);
	}
	goto done;
    }

    /* Get output ximage. */
    if (dr->type == ImageRaster) {
	xout = dr->r.ximage;
	ox = dx;  oy = dy;
    } else {
	uchar *data = (uchar *) XtMalloc (dnx * dny);
	if (data == NULL) {
	    status = ERR;
	    goto done;
	}
	xout = XCreateImage (w->gterm.display, NULL, RasterDepth,
	    ZPixmap, 0, (char *)data, dnx, dny, 8, 0);
	if (xout == NULL) {
	    XtFree ((char *)data);
	    status = ERR;
	    goto done;
	}
	ox = 0;  oy = 0;
	delxout++;
    }

    xin_lp = (uchar *)xin->data;
    xout_lp = (uchar *)xout->data;
    xin_bpl = xin->bytes_per_line;
    xout_bpl = xout->bytes_per_line;

    /* Map a region of the input ximage XIN to the output ximage XOUT.  Various
     * approaches are used to generate the output data, depending upon what
     * type of scaling we are doing.
     */
    if (!scaling) {
	/* No scaling.  Copy a region of the ximage xin to xout without
	 * spatially scaling the image data.
	 */
	if (clip && dr->type == ImageRaster)
	    goto zoom;

	xin_lp = (uchar *)xin->data + sy * xin_bpl + sx;
	xout_lp = (uchar *)xout->data + oy * xout_bpl + ox;

	for (j=0;  j < dny;  j++) {
	    memmove (xout_lp, xin_lp, dnx);
	    xin_lp += xin_bpl;
	    xout_lp += xout_bpl;
	}

    } else if (scaling == M_INTZOOM) {
	/* Integer zoom.  The zoom factor is an integer, allowing the zoomed
	 * image to be calculated without using the xmap,ymap lookup tables.
	 */
	if (clip && dr->type == ImageRaster)
	    goto zoom;

	scale_intzoom (xin_lp,xin_bpl, xout_lp,xout_bpl, sx,sy, ox,oy,dnx,dny,
	    xflip,yflip, (int)(xscale + 0.5), (int)(yscale + 0.5));

    } else if (scaling == M_ZOOM) {
	/* We have a zoom, or one-to-many, scaling.  Zoom scaling is always
	 * done with pixel replication.  The [xy]_srcpix arrays in the mapping
	 * descriptor give the source pixel corresponding to each mapped pixel
	 * in the destination raster.
	 */
zoom:
	xmap = &mp->x_srcpix[xoff];
	ymap = &mp->y_srcpix[yoff];

	scale_zoom (xin_lp, xin_bpl, xout_lp, xout_bpl,
	    xmap, ymap, ox, oy, dnx, dny,
	    (clip && dr->type == ImageRaster) ? clip_region : (Region)NULL);

    } else if (scaling == M_DEZOOM) {
	/* We have a dezoom, or many-to-one, scaling.  A block of pixels in
	 * the input raster are combined to generate the value of each output
	 * pixel, using one of several antialias algorithms to compute the
	 * output pixel value.
	 */
	float *x_src = &mp->x_src[xoff];
	float *y_src = &mp->y_src[yoff];
	int near_unitary = (xscale > 0.5 && yscale > 0.5);
	int function;

	/* Get the antialising function to be applied. */
	if (!(function = (mp->rop & R_MFMask)))
	    function = MF_NEAREST;

	/* If the dezoom factor is small and either MF_BILINEAR or
	 * MF_NEAREST is enabled, use the indicated method to sample the
	 * input data.  This uses all the data but minimizes smoothing.
	 */
	if ((function & (MF_BILINEAR|MF_NEAREST)) && near_unitary)
	    function = (function & MF_BILINEAR) ? MF_BILINEAR : MF_NEAREST;
	else if (function != (function & (MF_BILINEAR|MF_NEAREST)))
	    function &= ~(MF_BILINEAR|MF_NEAREST);

filter:
	/* This can take a while so update the display. */
	XFlush (w->gterm.display);

	/* If the filtering operation involves any arithmetic combinations
	 * of pixels we must convert pixel numbers to pixel intensity values
	 * before performing the filtering operation.  This is a case where
	 * we would be better off if frame buffers were maintained using
	 * pixel intensities rather than hardware pixel numbers.
	 */
	if (function != MF_NEAREST) {
            uchar *data = (uchar *) XtMalloc (xin->width * xin->height);
            if (data == NULL) {
                status = ERR;
                goto done;
            }

            mf_getinten (w,
		xin_lp, xin->width, xin->height, xin_bpl, sx,sy,
	        data, xin->width, xin->height, xin_bpl, sx,sy, snx,sny);

            if (!delxin) {
                xin = XCreateImage (w->gterm.display, NULL, RasterDepth,
                    ZPixmap, 0, (char *)data, xin->width, xin->height, 8, 0);
                if (xin == NULL) {
                    XtFree ((char *)data);
                    status = ERR;
                    goto done;
                }
                delxin++;
            } else {
                XtFree ((char *)xin->data);
                xin->data = (char *) data;
            }
            xin_lp = (uchar *)xin->data;
	}

	/* Filter the source rect to the destination. */
	switch (function) {
	case MF_NEAREST:
	    scale_nearest (
		xin_lp, xin->width, xin->height, xin_bpl,
		xout_lp, xout->width, xout->height, xout_bpl,
		x_src, y_src, ox, oy, dnx, dny,
		(clip && dr->type == ImageRaster) ? clip_region : (Region)NULL
	    );
	    break;
	case MF_BILINEAR:
	    scale_bilinear (
		xin_lp, xin->width, xin->height, xin_bpl,
		xout_lp, xout->width, xout->height, xout_bpl,
		x_src, y_src, ox, oy, dnx, dny,
		(clip && dr->type == ImageRaster) ? clip_region : (Region)NULL
	    );
	    break;
	case MF_BLKAVG:
	    scale_boxcar (
		xin_lp, xin->width, xin->height, xin_bpl,
		xout_lp, xout->width, xout->height, xout_bpl,
		x_src,y_src, sx,sy,snx,sny, ox,oy,dnx,dny,
		xscale,yscale, 0, clip ? clip_region : (Region)NULL
	    );
	    break;
	case MF_BOXCAR:
	    scale_boxcar (
		xin_lp, xin->width, xin->height, xin_bpl,
		xout_lp, xout->width, xout->height, xout_bpl,
		x_src,y_src, sx,sy,snx,sny, ox,oy,dnx,dny,
		xscale,yscale, 1, clip ? clip_region : (Region)NULL
	    );
	    break;
	case MF_LOWPASS:
	    scale_lowpass (
		xin_lp, xin->width, xin->height, xin_bpl,
		xout_lp, xout->width, xout->height, xout_bpl,
		x_src,y_src, sx,sy,snx,sny, ox,oy,dnx,dny,
		xscale,yscale, clip ? clip_region : (Region)NULL
	    );
	    break;
	default:
	    function = MF_BILINEAR;
	    goto filter;
	}

	/* If the operation was performed in intensity space convert back
	 * to pixel number.
	 */
	if (function != MF_NEAREST)
            mf_getpixel (w,
		xout_lp, xout->width, xout->height, xout_bpl,  ox,oy,
	        xout_lp, xout->width, xout->height, xout_bpl,  ox,oy, dnx,dny);

    } else {
	status = ERR;
	goto done;
    }

    /* Copy the scaled ximage to the output pixmap, if any.
     */
    if (dr->type == PixmapRaster) {
	if (dst == 0 && w->gterm.pixmap && !(rop & R_Transient)) {
	    XPutImage (display, w->gterm.pixmap, w->gterm.exposeGC, xout,
		ox, oy, dx, dy, dnx, dny);
	    XCopyArea (display, w->gterm.pixmap, dr->r.pixmap,
		w->gterm.exposeGC, dx, dy, dnx, dny, dx, dy);
	} else {
	    XPutImage (display, dr->r.pixmap, w->gterm.exposeGC, xout,
		ox, oy, dx, dy, dnx, dny);
	}
    }
    
done:
    /* Clean up.
     */
    if (delxin)
	XDestroyImage (xin);
    if (delxout)
	XDestroyImage (xout);

    XDestroyRegion (clip_region);
    if (clip && dr->type == PixmapRaster)
	XSetClipMask (w->gterm.display, w->gterm.exposeGC, None);

    /* Execute any mappings defined on the raster just updated. */
    if (status == OK) {
	GtRefreshPixels (w, dst, GtPixel, dx, dy, dnx, dny);

	if (dst == 0) {
	    Region clip_region = (Region)NULL;
	    XRectangle r;

	    clip_region = XCreateRegion();
	    r.x = dx;  r.y = dy;
	    r.width = dnx;  r.height = dny;
	    XUnionRectWithRegion (&r, clip_region, clip_region);

	    update_transients (w, clip_region);
	    XDestroyRegion (clip_region);
	}
    }

    return (status);
}


/* scale_zoom -- Compute the given destination rect from the input image,
 * using pixel replication and the given x and y dst->scr pixels maps.
 */
static
scale_zoom (idata,ibpl, odata,obpl, xmap,ymap, dx,dy,dnx,dny, clip_region)

    uchar *idata, *odata;		/* input, output data */
    int ibpl, obpl;			/* bytes per line */
    register int *xmap;			/* src coords of each dst pixel */
    int *ymap;				/* src coords of each dst pixel */
    int dx, dy, dnx, dny;		/* destination rect */
    Region clip_region;			/* clip Region or null */
{
    register int i, j;
    register uchar *ip, *op;
    uchar *last_ip = NULL;
    uchar *last_op = NULL;

    for (j=0;  j < dny;  j++) {
	ip = idata + ymap[j] * ibpl;
	op = odata + (j+dy) * obpl + dx;

	if (!clip_region) {
	    if (ip == last_ip)
		memmove (op, last_op, dnx);
	    else {
		for (i=0;  i < dnx;  i++)
		    op[i] = ip[xmap[i]];
	    }
	    last_ip = ip;
	    last_op = op;
	} else {
	    for (i=0;  i < dnx;  i++)
		if (XPointInRegion (clip_region, i + dx, j + dy))
		    op[i] = ip[xmap[i]];
	}
    }
}


/* scale_intzoom -- Compute the given destination rect from the input image,
 * using pixel replication and integer scaling.  This is functionally
 * equivalent to scale_zoom using the lookup tables, but optimized for the
 * case of integer scaling.
 */
static
scale_intzoom (idata,ibpl,odata,obpl, sx,sy,dx,dy,dnx,dny, xflip,yflip, nx,ny)

    uchar *idata, *odata;		/* input, output data */
    int ibpl, obpl;			/* bytes per line */
    int sx, sy;				/* start coords of src rect */
    int dx, dy, dnx, dny;		/* destination rect */
    int xflip, yflip;			/* set if x or y is flipped */
    int nx, ny;				/* replication factors */
{
    register int n;
    register int pix;
    register uchar *ip, *op;
    uchar *otop, *olast, *lp;
    int i, j, k;

    olast = odata + (dy + dny) * obpl - dnx + dx;

    if (xflip) {
	for (j=0, k=0;  j < dny;  j += ny, k++) {
	    ip = idata + (sy + k) * ibpl + sx;

	    op = odata + (dy + (yflip ? (dny-ny-j) : j)) * obpl + dx + dnx;
	    otop = lp = op - dnx;


	    /* Why are the case statements below necessary, doesn't the
	     * default case do the same thing regardless of what nx is?  MJF
	     */

	    /* Replicate a block of pixels. */
	    switch (nx) {
	    case 2:
		for (n = (dnx/2);  --n >= 0;  ) {
		    pix = *ip++;
		    *--op = pix;  *--op = pix;
		}
		break;
	    case 3:
		for (n = (dnx/3);  --n >= 0;  ) {
		    pix = *ip++;
		    *--op = pix;  *--op = pix;  *--op = pix;
		}
		break;
	    case 4:
		for (n = (dnx/4);  --n >= 0;  ) {
		    pix = *ip++;
		    *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;
		}
		break;
	    case 5:
		for (n = (dnx/5);  --n >= 0;  ) {
		    pix = *ip++;
		    *--op = pix;  *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;
		}
		break;
	    case 6:
		for (n = (dnx/6);  --n >= 0;  ) {
		    pix = *ip++;
		    *--op = pix;  *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;  *--op = pix;
		}
		break;
	    case 7:
		for (n = (dnx/7);  --n >= 0;  ) {
		    pix = *ip++;
		    *--op = pix;  *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;
		}
		break;
	    case 8:
		for (n = (dnx/8);  --n >= 0;  ) {
		    pix = *ip++;
		    *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;
		}
		break;
	    default:
		for (n = (dnx/nx);  --n >= 0;  ) {
		    pix = *ip++;
		    for (i=nx;  --i >= 0;  )
			*--op = pix;
		}
		break;
	    }

	    /* Fill the last partial pixel. */
	    pix = *ip++;
	    while (op > otop)
		*--op = pix;

	    /* Replicate in Y. */
	    for (n=ny, op=lp;  --n > 0;  ) {
		op += obpl;
		if (op <= olast)
		    memmove (op, lp, dnx);
	    }
	}
    } else {
	for (j=0, k=0;  j < dny;  j += ny, k++) {
	    ip = idata + (sy + k) * ibpl + sx;
	    op = lp = odata + (dy + (yflip ? (dny-ny-j) : j)) * obpl + dx;
	    otop = op + dnx;

	    /* Replicate a block of pixels. */
	    switch (nx) {
	    case 2:
		for (n = (dnx/2);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    case 3:
		for (n = (dnx/3);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		}
		break;
	    case 4:
		for (n = (dnx/4);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    case 5:
		for (n = (dnx/5);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    case 6:
		for (n = (dnx/6);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		}
		break;
	    case 7:
		for (n = (dnx/7);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    case 8:
		for (n = (dnx/8);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    default:
		for (n = (dnx/nx);  --n >= 0;  ) {
		    pix = *ip++;
		    for (i=nx;  --i >= 0;  )
			*op++ = pix;
		}
		break;
	    }

	    /* Fill the last partial pixel. */
	    pix = *ip++;
	    while (op < otop)
		*op++ = pix;

	    /* Replicate in Y. */
	    for (n=ny, op=lp;  --n > 0;  ) {
		op += obpl;
		if (op <= olast)
		    memmove (op, lp, dnx);
	    }
	}
    }
}


/* scale_nearest -- Compute the given destination rect from the input image,
 * using the nearest neighbor technique.
 */
static
scale_nearest (idata,inx,iny,ibpl, odata,onx,ony,obpl,
    x_src,y_src, dx,dy,dnx,dny, clip_region)

    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* dimensions of input array */
    int onx, ony, obpl;			/* dimensions of output array */
    float *x_src, *y_src;		/* src coords of each dst pixel */
    int dx, dy, dnx, dny;		/* destination rect */
    Region clip_region;			/* clip Region or null */
{
    register int m, n, i, j;
    register uchar *op;

    for (j=0;  j < dny;  j++) {
	op = odata + (j+dy) * obpl + dx;
	n = y_src[j];

	if (!clip_region) {
	    for (i=0;  i < dnx;  i++) {
		m = x_src[i];
		op[i] = idata[n * ibpl + m];
	    }
	} else {
	    for (i=0;  i < dnx;  i++)
		if (XPointInRegion (clip_region, i + dx, j + dy)) {
		    m = x_src[i];
		    op[i] = idata[n * ibpl + m];
		}
	}
    }
}


/* scale_bilinear -- Compute the given destination rect from the input image,
 * using bilinear interpolation.
 */
static
scale_bilinear (idata,inx,iny,ibpl, odata,onx,ony,obpl,
    x_src,y_src, dx,dy,dnx,dny, clip_region)

    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* dimensions of input array */
    int onx, ony, obpl;			/* dimensions of output array */
    float *x_src, *y_src;		/* src coords of each dst pixel */
    int dx, dy, dnx, dny;		/* destination rect */
    Region clip_region;			/* clip Region or null */
{
    register int i;
    register uchar *op;
    register float *lp, *w1, *w2;
    int buflen, line, *px, pixel, j;
    float lo_w, hi_w, x, y;
    uchar *lo, *hi;
    float *buf;

    buflen = (3 * dnx + 2) * sizeof(float) + dnx * sizeof(int);
    if ((buf = (float *) XtMalloc (buflen)) == NULL)
	return;

    lp = buf + 1;
    w1 = lp + dnx + 1;
    w2 = w1 + dnx;
    px = (int *)(w2 + dnx);

    /* Compute the weight vectors at each point in X. */
    for (i=0;  i < dnx;  i++) {
	x = x_src[i] - 0.5;
	px[i] = (int) x;
	w1[i] = 1.0 - (x - (int)x);
	w2[i] = 1.0 - w1[i];
    }

    /* For each line of the destination rect first interpolate in Y to the
     * y_src coordinate of the output line, then interpolate in X to compute
     * the final output pixels.
     */
    for (j=0;  j < dny;  j++) {
	op = odata + (j+dy) * obpl + dx;
	y = y_src[j] - 0.5;
	line = (int) y;
	lo = idata + line * ibpl;
	hi = idata + min (iny - 1, line + 1) * ibpl;
	lo_w = 1.0 - (y - line);
	hi_w = 1.0 - lo_w;

	/* Interpolate in Y to the line at y_src[j]. */
	for (i=0;  i < dnx;  i++) {
	    pixel = px[i];
	    lp[i] = (float)lo[pixel] * lo_w + (float)hi[pixel] * hi_w;
	}
	lp[-1]  = lp[0];
	lp[dnx] = lp[dnx-1];

	/* Interpolate in X to the final output pixels. */
	if (!clip_region) {
	    for (i=0;  i < dnx;  i++)
		op[i] = lp[i] * w1[i] + lp[i+1] * w2[i];
	} else {
	    for (i=0;  i < dnx;  i++)
		if (XPointInRegion (clip_region, i + dx, j + dy))
		    op[i] = lp[i] * w1[i] + lp[i+1] * w2[i];
	}
    }

    XtFree ((char *)buf);
}


/* scale_lowpass -- Apply a lowpass filter to a region of a 2D data array.
 * The region ox,oy,nx,ny of the output data array is calculated by running
 * a convolution kernel over the region of the input data array at ix,iy.
 * The size of the convolution kernel is adjusted to match the scale factors
 * xscale, yscale.
 */
static
scale_lowpass (idata,inx,iny,ibpl, odata,onx,ony,obpl, x_src,y_src,
    sx,sy,snx,sny, dx,dy,dnx,dny, xscale,yscale, clip_region)

    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* full input array */
    int onx, ony, obpl;			/* full input array */
    float *x_src, *y_src;		/* src coords of each dst pixel */
    int sx, sy, snx, sny;		/* source rect */
    int dx, dy, dnx, dny;		/* destination rect */
    float xscale, yscale;		/* scale factors */
    Region clip_region;			/* clip Region or null */
{
    uchar *data;
    
    if ((data = (uchar *) XtMalloc (inx * iny)) == NULL)
	return;

    /* Run a lowpass filter over the input rect. */
    lw_convolve (idata,inx,iny,ibpl, sx,sy, data,inx,iny,ibpl, sx,sy,
	snx,sny, xscale,yscale);

    /* Sample the filtered data to generate the output rect. */
    scale_nearest (data,inx,iny,ibpl, odata,onx,ony,obpl, x_src,y_src,
	dx,dy,dnx,dny, clip_region);

    XtFree ((char *)data);
}


/* lw_convolve -- Convolution primitive for scale_lowpass.
 */
static
lw_convolve (idata,inx,iny,ibpl,ix,iy, odata,onx,ony,obpl,ox,oy,
    nx, ny, xscale, yscale)

    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ix, iy;		/* size of input array, start pos */
    int onx, ony, ox, oy;		/* size of output array, start pos */
    int ibpl, obpl;			/* bytes per line */
    int nx, ny;				/* size of output region */
    float xscale, yscale;		/* determines amount of smoothing */
{
    register uchar *ip;
    register int l, m, x, hx, pixval;
    int kx, ky, hy, i, j, y;
    uchar *lp[11], *op;

    /* Determine kernel size (min 3x3, max 7x7). */
    if (xscale < 0.1)
	hx = 3;
    else if (xscale >= 0.5)
	hx = 1;
    else
	hx = ((int)(1.0 / xscale)) / 2;

    if (yscale < 0.1)
	hy = 3;
    else if (yscale >= 0.5)
	hy = 1;
    else
	hy = ((int)(1.0 / yscale)) / 2;

    kx = hx * 2 + 1;
    ky = hy * 2 + 1;

    /* Compute the output data.
     */
    for (j=0;  j < ny;  j++) {
	/* Get line pointers. */
	op = odata + (j+oy) * obpl + ox;
	for (i=0;  i < ky;  i++) {
	    y = iy + j - hy + i;
	    if (y < 0)
		y = 0;
	    else if (y >= iny)
		y = iny - 1;
	    lp[i] = y * ibpl + idata;
	}

	/* Compute a line of output pixels */
	for (i=0;  i < nx;  i++) {
	    x = ix + i;
	    pixval = 0;

	    if (x < hx) {
		/* Near left edge. */
		for (m=0;  m < ky;  m++)
		    for (l=0;  l < kx;  l++)
			pixval += lp[m][max(0,x-hx+l)];
	    } else if (x >= inx - hx) {
		/* Near right edge. */
		for (m=0;  m < ky;  m++)
		    for (l=0;  l < kx;  l++)
			pixval += lp[m][min(inx-1,x-hx+l)];
	    } else {
		/* In central region. */
		for (m=0;  m < ky;  m++) {
		    ip = lp[m] + x - hx;
		    for (l=0;  l < kx;  l++)
			pixval += ip[l];
		}
	    }

	    op[i] = (float)pixval / (float)(kx * ky) + 0.5;
	}
    }
}


/* scale_boxcar -- Apply a boxcar filter to a region of a 2D data array
 * and interpolate the result to the output grid.  The region ox,oy,nx,ny of
 * the output data array is calculated by block averaging the corresponding
 * source rect and then sampling the reduced image using bilinear interpolation
 * to compute the output pixel raster.  This antialiasing technique aims to
 * be as fast as possible but still does a reasonable job of reducing the
 * image.
 */
static
scale_boxcar (idata,inx,iny,ibpl, odata,onx,ony,obpl, x_src,y_src,
    sx,sy,snx,sny, dx,dy,dnx,dny, xscale,yscale, interp, clip_region)

    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* full input array */
    int onx, ony, obpl;			/* full input array */
    float *x_src, *y_src;		/* src coords of each dst pixel */
    int sx, sy, snx, sny;		/* source rect */
    int dx, dy, dnx, dny;		/* destination rect */
    float xscale, yscale;		/* scale factors */
    int interp;				/* set if interpolation is desired */
    Region clip_region;			/* clip Region or null */
{
    int xblock, yblock;
    int x1, x2, y1, y2, nx, ny;
    float xstep, ystep;
    int xoff, yoff;
    uchar *bp;

    /* Determine blocking factors.   If interpolation is disabled we need
     * to block one step more than for the linear interpolate case in order
     * to ensure that all the data is used.
     */
    xblock = max(1, (int) (1.0 / xscale));
    if (!interp && (1.0 / xscale) - xblock > ZOOM_TOL)
	xblock++;
    yblock = max(1, (int) (1.0 / yscale));
    if (!interp && (1.0 / yscale) - yblock > ZOOM_TOL)
	yblock++;

    /* Align the input region for the given blocking factors. */
    x1 = sx / xblock * xblock;  x2 = (sx + snx - 1) / xblock * xblock;
    y1 = sy / yblock * yblock;  y2 = (sy + sny - 1) / yblock * yblock;
    nx = (x2 - x1) / xblock + 1;  ny = (y2 - y1) / yblock + 1;

    /* Compute the block averaged input rect.  */
    if (xblock > 1 || yblock > 1) {
	if ((bp = (uchar *) XtMalloc (nx * ny)) == NULL)
	    return;
	bx_boxcar (idata,inx,iny,ibpl, x1,y1,x2,y2, bp, xblock, yblock);
	idata = bp;
	inx = ibpl = nx;
	iny = ny;
	xoff = x1;  yoff = y1;
	xstep = 1.0 / xblock;  ystep = 1.0 / yblock;
    } else {
	bp = NULL;
	xoff = yoff = 0;
	xstep = ystep = 1.0;
    }

    /* Interpolate the input rect to the output grid. */
    if (interp &&
	((1.0 / xscale) - xblock) > ZOOM_TOL ||
	((1.0 / yscale) - yblock) > ZOOM_TOL) {

	/* Use bilinear interpolation to compute the output grid. */
	bx_interp (idata,inx,iny,ibpl, odata,onx,ony,obpl,
	    x_src,y_src, xoff,yoff,xstep,ystep, dx,dy,dnx,dny, clip_region);

    } else {
	/* Extract pixels from block averaged input data. */
	bx_extract (idata,inx,iny,ibpl, odata,onx,ony,obpl,
	    x_src,y_src, xoff,yoff,xstep,ystep, dx,dy,dnx,dny, clip_region);
    }

    if (bp)
	XtFree ((char *)bp);
}


/* bx_boxcar -- Block average primitive for scale_boxcar.
 */
static
bx_boxcar (idata,inx,iny,ibpl, x1,y1,x2,y2, obuf, xblock, yblock)
    uchar *idata;			/* input data array */
    int inx, iny, ibpl;			/* array dimensions */
    int x1,y1,x2,y2;			/* region to be block averaged */
    uchar *obuf;			/* output array */
    int xblock, yblock;			/* blocking factors */
{
    register uchar *ip, *op;
    register int count, i, *sp;
    int obpl, block, nxblocks, nyblocks, j, k;
    uchar *lp, *bp;
    int *sb;

    nxblocks = obpl = (x2 - x1) / xblock + 1;
    nyblocks = (y2 - y1) / yblock + 1;
    count = xblock * yblock;

    if ((sb = (int *) XtMalloc (obpl * sizeof(int))) == NULL)
	return;

    /* I don't think the following works for pixel values allocated from the
     * default colormap, as the pixel values are not sequentially allocated.
     */
    for (block = 0;  block < nyblocks;  block++) {
	lp = idata + ibpl * (block * yblock + y1) + x1;
	bp = obuf + block * obpl;

	memset (sb, 0, obpl * sizeof(int));
	for (k=yblock;  --k >= 0;  lp += ibpl)
	    for (j=nxblocks, ip=lp, sp=sb;  --j >= 0;  sp++)
		for (i=xblock;  --i >= 0;  )
		    *sp += *ip++;

	for (i=obpl, sp=sb, op=bp;  --i >= 0;  )
	    *op++ = *sp++ / count;
    }

    XtFree ((char *)sb);
}


/* bx_extract -- Block extract primitive for scale_boxcar.
 */
static
bx_extract (idata,inx,iny,ibpl, odata,onx,ony,obpl,
    x_src,y_src, xoff,yoff,xstep,ystep, dx,dy,dnx,dny, clip_region)

    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* full input array */
    int onx, ony, obpl;			/* full input array */
    float *x_src, *y_src;		/* src coords of each dst pixel */
    int dx, dy, dnx, dny;		/* destination rect */
    int xoff, yoff;			/* offset of input region */
    float xstep, ystep;			/* scale of input region */
    Region clip_region;			/* clip Region or null */
{
    register int m, n, i;
    register uchar *op;
    int j;

    for (j=0;  j < dny;  j++) {
	op = odata + (j+dy) * obpl + dx;
	n = (y_src[j] - yoff) * ystep;

	if (!clip_region) {
	    for (i=0;  i < dnx;  i++) {
		m = (x_src[i] - xoff) * xstep;
		op[i] = idata[n * ibpl + m];
	    }
	} else {
	    for (i=0;  i < dnx;  i++)
		if (XPointInRegion (clip_region, i + dx, j + dy)) {
		    m = (x_src[i] - xoff) * xstep;
		    op[i] = idata[n * ibpl + m];
		}
	}
    }
}


/* bx_interp -- Bilinear interpolation primitive for scale_boxcar.
 */
static
bx_interp (idata,inx,iny,ibpl, odata,onx,ony,obpl,
    x_src,y_src, xoff,yoff,xstep,ystep, dx,dy,dnx,dny, clip_region)

    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* dimensions of input array */
    int onx, ony, obpl;			/* dimensions of output array */
    float *x_src, *y_src;		/* src coords of each dst pixel */
    int xoff, yoff;			/* offset of input region */
    float xstep, ystep;			/* scale of input region */
    int dx, dy, dnx, dny;		/* destination rect */
    Region clip_region;			/* clip Region or null */
{
    register int i;
    register uchar *op;
    register float *lp, *w1, *w2;
    int buflen, line, *px, pixel, j;
    float lo_w, hi_w, x, y;
    uchar *lo, *hi;
    float *buf;

    buflen = (3 * dnx + 2) * sizeof(float) + dnx * sizeof(int);
    if ((buf = (float *) XtMalloc (buflen)) == NULL)
	return;

    lp = buf + 1;
    w1 = lp + dnx + 1;
    w2 = w1 + dnx;
    px = (int *)(w2 + dnx);

    /* Compute the weight vectors at each point in X. */
    for (i=0;  i < dnx;  i++) {
	x = ((x_src[i] - xoff) * xstep) - 0.5;
	px[i] = (int) x;
	w1[i] = 1.0 - (x - (int)x);
	w2[i] = 1.0 - w1[i];
    }

    /* For each line of the destination rect first interpolate in Y to the
     * y_src coordinate of the output line, then interpolate in X to compute
     * the final output pixels.
     */
    for (j=0;  j < dny;  j++) {
	op = odata + (j+dy) * obpl + dx;
	y = ((y_src[j] - yoff) * ystep) - 0.5;
	line = (int) y;
	lo = idata + line * ibpl;
	hi = idata + min (iny - 1, line + 1) * ibpl;
	lo_w = 1.0 - (y - line);
	hi_w = 1.0 - lo_w;

	/* Interpolate in Y to the line at y_src[j]. */
	for (i=0;  i < dnx;  i++) {
	    pixel = px[i];
	    lp[i] = (float)lo[pixel] * lo_w + (float)hi[pixel] * hi_w;
	}
	lp[-1]  = lp[0];
	lp[dnx] = lp[dnx-1];

	/* Interpolate in X to the final output pixels. */
	if (!clip_region) {
	    for (i=0;  i < dnx;  i++)
		op[i] = lp[i] * w1[i] + lp[i+1] * w2[i];
	} else {
	    for (i=0;  i < dnx;  i++)
		if (XPointInRegion (clip_region, i + dx, j + dy))
		    op[i] = lp[i] * w1[i] + lp[i+1] * w2[i];
	}
    }

    XtFree ((char *)buf);
}


/* mf_getinten -- Copy the source rect to the destination rect, converting
 * pixel numbers to pixel intensities.
 */
static
mf_getinten (w, idata,inx,iny,ibpl, sx,sy, odata,onx,ony,obpl, dx,dy, nx,ny)

    GtermWidget w;
    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* dimensions of input array */
    int onx, ony, obpl;			/* dimensions of output array */
    int sx, sy;				/* source offset */
    int dx, dy;				/* destination offset */
    int nx, ny;				/* size of region */
{
    register Pixel *cmap;
    register uchar *ip, *op;
    register int n;
    int j;

    cmap = get_cmap_out (w);
    for (j=0;  j < ny;  j++) {
	ip = idata + ibpl * (sy + j) + sx;
	op = odata + obpl * (dy + j) + dx;
	for (n = nx;  --n >= 0;  )
	    *op++ = (cmap[*ip++]);
    }
}


/* mf_getpixel -- Copy the source rect to the destination rect, converting
 * pixel intensities to pixel numbers.
 */
static
mf_getpixel (w, idata,inx,iny,ibpl, sx,sy, odata,onx,ony,obpl, dx,dy, nx,ny)

    GtermWidget w;
    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* dimensions of input array */
    int onx, ony, obpl;			/* dimensions of output array */
    int sx, sy;				/* source offset */
    int dx, dy;				/* destination offset */
    int nx, ny;				/* size of region */
{
    register Pixel *cmap;
    register uchar *ip, *op;
    register int n;
    int j;

    cmap = get_cmap_in (w);
    for (j=0;  j < ny;  j++) {
	ip = idata + ibpl * (sy + j) + sx;
	op = odata + obpl * (dy + j) + dx;
	for (n = nx;  --n >= 0;  )
	    *op++ = (cmap[*ip++]);
    }
}


/* get_regions -- For each pixel I in the sequence of DNX pixels starting at DX
 * there is an associated value XMAP[I].  Compare this sequence to an alternate
 * sequence and return a list of subregions {XS,XE,XV} for which the XMAP
 * values are equal (XV=0), not equal (XV=1), or not common to (XV=2) the two
 * regions.  The number of regions output is returned as the function value.
 */
static
get_regions (xs,xe,xv, max_regions, dx, dnx, xmap, alt_dx, alt_dnx, alt_xmap)
    int *xs, *xe, *xv, max_regions;
    int dx, dnx, *xmap;
    int alt_dx, alt_dnx, *alt_xmap;
{
    register int state, current;
    register int nx, i;
    int offset, old_i;

    offset = dx - alt_dx;
    nx = 0;

    for (i=0;  i < dnx;  i++) {
	if (nx >= max_regions-1)
	    return (0);

	/* Determine whether or not this pixel is mapped the same in both
	 * sequences.
	 */
	old_i = i + offset;
	if (alt_dnx <= 0 || old_i < 0 || old_i >= alt_dnx)
	    state = 2;
	else
	    state = (xmap[i] != alt_xmap[old_i]);

	/* When the state boundary is crossed add a range. */
	if (i == 0) {
	    xs[nx] = dx;
	    xv[nx] = current = state;
	}
	if (state != current) {
	    xe[nx] = dx + i - 1;
	    xs[++nx] = dx + i;
	    xv[nx] = current = state;
	}
	if (i == dnx-1)
	    xe[nx++] = dx + i;
    }

    return (nx);
}


/* get_rects -- Combine two lists of regions, one in X and the other in Y,
 * to produce a list of 2D rectangles defining the regions outlined by the
 * region lists.  Only rects for which the given condition is true in either
 * X or Y are selected.  Adjacent rects are combined.
 */
static
get_rects (o_rl, max_rects, xs,xe,xv,nx, ys,ye,yv,ny, xcond,ycond)
    XRectangle *o_rl;		/* receives list of rectangles */
    int max_rects;		/* max rectangles out */
    int *xs, *xe, *xv, nx;	/* X list of regions */
    int *ys, *ye, *yv, ny;	/* Y list of regions */
    int xcond, ycond;		/* X,Y condition bitflags */
{
    register int i, j;
    XRectangle rl[MAX_REGIONS];
    int limit = min (max_rects, MAX_REGIONS);
    int o_nrects=0, nrects=0;
    int i1, i2, j1, j2;

    /* Get initial list of rects matching the given X and Y conditions.
     * Rects which are adjacent in X are combined to form one larger rect.
     */
    for (j=0;  j < ny;  j++) {
	for (i=0;  i < nx;  i++) {
	    if ((xv[i] & xcond) || (yv[j] & ycond)) {
		/* Collapse rects adjacent in X. */
		for (i1 = i2 = i++;  i < nx;  i++) {
		    if ((xv[i] & xcond) || (yv[j] & ycond))
			i2 = i;
		    else
			break;
		}

		rl[nrects].x = xs[i1];
		rl[nrects].y = ys[j];
		rl[nrects].width = xe[i2] - xs[i1] + 1;
		rl[nrects].height = ye[j] - ys[j] + 1;

		if (++nrects >= limit)
		    return (nrects);
	    }
	}
    }

    /* Now scan the rect list and collapse rects which are adjacent in Y
     * into one larger rect.  Find all the rects that are at the same X
     * and write them to the output list, collapsing rects that are adjacent
     * in Y in the process.
     */
    for (i=0;  i < nx;  i++)
	for (j=0;  j < nrects;  ) {
	    /* Find first rect if any. */
	    for (j1=0, j2 = -1;  j < nrects;  j++)
		if (rl[j].x == xs[i]) {
		    j1 = j2 = j++;
		    break;
		}
	    
	    /* Collapse rects adjacent in Y. */
	    for (   ;  j < nrects;  j++)
		if (rl[j].x == xs[i])
		    if (rl[j].y == rl[j2].y + rl[j2].height &&
			    rl[j].width == rl[j1].width)
			j2 = j;
		    else
			break;
	    
	    /* Output the rect. */
	    if (j2 >= j1) {
		o_rl[o_nrects].x = rl[j1].x;
		o_rl[o_nrects].y = rl[j1].y;
		o_rl[o_nrects].width = rl[j1].width;
		o_rl[o_nrects].height = rl[j2].y + rl[j2].height - rl[j1].y;

		if (++o_nrects >= max_rects)
		    return (o_nrects);
	    }
	}

    return (o_nrects);
}


/* rect_intersect -- Compute the intersection of two rects.  The area of
 * the intersection is returned as the function value, i.e., zero is
 * returned if the rects do not intersect.
 */
static
rect_intersect (in, r1, r2)
    register XRectangle *in;
    register XRectangle *r1, *r2;
{
    int x1, y1, x2, y2;

    x1 = max (r1->x, r2->x);
    y1 = max (r1->y, r2->y);
    x2 = min ((int)r1->x + (int)r1->width, (int)r2->x + (int)r2->width) - 1;
    y2 = min ((int)r1->y + (int)r1->height, (int)r2->y + (int)r2->height) - 1;

    in->x = x1;
    in->y = y1;
    in->width = max (0, x2 - x1 + 1);
    in->height = max (0, y2 - y1 + 1);

    return (in->width * in->height);
}


/* save_mapping -- Store a mapping in a mapping descriptor.
 */
static
save_mapping (mp, mapping, rop, src, st, sx,sy,sw,sh, dst, dt, dx,dy,dw,dh)
    register Mapping mp;
    int mapping, rop;
    int src, st, sx,sy,sw,sh;
    int dst, dt, dx,dy,dw,dh;
{
    mp->src = src;  mp->st = st;
	mp->sx = sx; mp->sy = sy; mp->snx = sw; mp->sny = sh;
    mp->dst = dst;  mp->dt = dt;
	mp->dx = dx; mp->dy = dy; mp->dnx = dw; mp->dny = dh;
    mp->defined = mp->enabled = mp->refresh = 1;
    mp->mapping = mapping;
    mp->rop = rop;
}

/* load_mapping -- Load a mapping from a mapping descriptor.
 */
static
load_mapping (mp, mapping, rop, src, st, sx,sy,sw,sh, dst, dt, dx,dy,dw,dh)
    register Mapping mp;
    int *mapping, *rop;
    int *src, *st, *sx,*sy,*sw,*sh;
    int *dst, *dt, *dx,*dy,*dw,*dh;
{
    *src = mp->src;  *st = mp->st;
	*sx = mp->sx; *sy = mp->sy; *sw = mp->snx; *sh = mp->sny;
    *dst = mp->dst;  *dt = mp->dt;
	*dx = mp->dx; *dy = mp->dy; *dw = mp->dnx; *dh = mp->dny;
    *mapping = mp->mapping;
    *rop = mp->rop;
}


/* get_pixel_mapping -- Copy a mapping, converting to pixel coordinates in
 * the process if the mapping is not already in pixel coordinates.
 */
static
get_pixel_mapping (w, mp1, mp2, update)
    GtermWidget w;
    register Mapping mp1;		/* input mapping */
    register Mapping mp2;		/* output mapping */
    int update;				/* update mapping */
{
    float maxndc = (float)MAXNDC;

    mp2->mapping = mp1->mapping;
    mp2->refresh = mp1->refresh;
    mp2->enabled = mp1->enabled;
    mp2->rop = mp1->rop;

    if (!(mp2->defined = mp1->defined))
	return;

    mp2->src = mp1->src;
    if (mp1->st == GtPixel) {
	mp2->st = mp1->st;
	mp2->sx = mp1->sx;	mp2->sy = mp1->sy;
	mp2->snx = mp1->snx;	mp2->sny = mp1->sny;
    } else {
	float width  = w->gterm.rasters[mp1->src].width;
	float height = w->gterm.rasters[mp1->src].height;
	mp2->sx = mp1->sx * width / maxndc;
	mp2->sy = (MAXNDC - (mp1->sy + abs(mp1->sny))) * height / maxndc;
	mp2->snx = mp1->snx * width / maxndc;
	mp2->sny = mp1->sny * height / maxndc;		/* NDC flipped in Y */
	mp2->st = GtPixel;
    }

    mp2->dst = mp1->dst;
    if (mp1->dt == GtPixel) {
	mp2->dt = mp1->dt;
	mp2->dx = mp1->dx;	mp2->dy = mp1->dy;
	mp2->dnx = mp1->dnx;	mp2->dny = mp1->dny;
    } else {
	float width  = w->gterm.rasters[mp1->dst].width;
	float height = w->gterm.rasters[mp1->dst].height;
	mp2->dx = mp1->dx * width / maxndc;
	mp2->dy = (MAXNDC - (mp1->dy + abs(mp1->dny))) * height / maxndc;
	mp2->dnx = mp1->dnx * width / maxndc;
	mp2->dny = mp1->dny * -height / maxndc;		/* NDC flipped in Y */
	mp2->dt = GtPixel;
    }

    /* The lookup tables are already in pixel space, so we can propagate
     * these to the new mapping if the old mapping was updated.
     */
    if (update && mp1->updated) {
	if (mp2->mapdata = (uchar *) XtMalloc (mp1->datalen)) {

	    memmove (mp2->mapdata, mp1->mapdata, mp1->datalen);
	    mp2->datalen = mp1->datalen;
	    mp2->scaling = mp1->scaling;
	    mp2->xscale = mp1->xscale;
	    mp2->yscale = mp1->yscale;

	    mp2->x_extent = (mapExtent *)
		((uchar *)mp1->x_extent - mp1->mapdata + mp2->mapdata);
	    mp2->y_extent = (mapExtent *)
		((uchar *)mp1->y_extent - mp1->mapdata + mp2->mapdata);
	    mp2->x_srcpix = (int *)
		((uchar *)mp1->x_srcpix - mp1->mapdata + mp2->mapdata);
	    mp2->y_srcpix = (int *)
		((uchar *)mp1->y_srcpix - mp1->mapdata + mp2->mapdata);
	    mp2->x_src = (float *)
		((uchar *)mp1->x_src - mp1->mapdata + mp2->mapdata);
	    mp2->y_src = (float *)
		((uchar *)mp1->y_src - mp1->mapdata + mp2->mapdata);

	    mp2->updated = 1;
	}
    } else
	mp2->updated = 0;
}

/* valid_mapping -- Perform some sanity checks on a mapping to verify that
 * it contains something meaningful.
 */
static
valid_mapping (w, mp)
    GtermWidget w;
    register Mapping mp;
{
    register int x, y;
    int snx, sny, dnx, dny;
    int s_width, s_height, d_width, d_height;
    Raster sr, dr;

    if (!mp || !mp->defined)
	return (False);

    if (mp->src < 0 || mp->src >= w->gterm.maxRasters)
	return (False);
    if (mp->dst < 0 || mp->dst >= w->gterm.maxRasters)
	return (False);

    sr = &w->gterm.rasters[mp->src];
    dr = &w->gterm.rasters[mp->dst];
    if (!sr->type || !dr->type)
	return (False);

    switch (mp->st) {
    case GtPixel:
	s_width = sr->width;  s_height = sr->height;
	break;
    case GtNDC:
	s_width = MAXNDC + 1;  s_height = MAXNDC + 1;
	break;
    default:
	return (False);
    }

    switch (mp->dt) {
    case GtPixel:
	d_width = dr->width;  d_height = dr->height;
	break;
    case GtNDC:
	d_width = MAXNDC + 1;  d_height = MAXNDC + 1;
	break;
    default:
	return (False);
    }

    snx = mp->snx;  dnx = abs(mp->dnx);
    sny = mp->sny;  dny = abs(mp->dny);
    if (snx <= 0 || dnx <= 0 || sny <= 0 || dny <= 0)
	return (False);

    x = mp->sx;  y = mp->sy;
    if (x < 0 || x >= s_width || y < 0 || y >= s_height)
	return (False);
    x = mp->sx + snx - 1;  y = mp->sy + sny - 1;
    if (x < 0 || x >= s_width || y < 0 || y >= s_height)
	return (False);

    x = mp->dx;  y = mp->dy;
    if (x < 0 || x >= d_width || y < 0 || y >= d_height)
	return (False);
    x = mp->dx + dnx - 1;  y = mp->dy + dny - 1;
    if (x < 0 || x >= d_width || y < 0 || y >= d_height)
	return (False);

    return (True);
}


/* initialize_mapping -- Initialize the contents of a mapping descriptor.
 */
static
initialize_mapping (mp)
    register Mapping mp;
{
    memset ((char *)mp, 0, sizeof(struct mapping));
}


/* update_mapping -- Update the portion of a mapping descriptor used at
 * runtime to execute the mapping.  This information consists of several
 * lookup tables and other parameters describing how a destination pixel
 * maps back to a source pixel and vice versa.
 */
static
update_mapping (w, mp)
    GtermWidget w;
    register Mapping mp;
{
    register uchar *op;
    register int i, j, k;
    int snx, sny, dnx, dny, sx, sy, dx, dy;
    int xmax, ymax, lo, hi, edge1, edge2;
    int temp, xflip=0, yflip=0;
    struct mapping p_mp;
    float pixwidth, *fp;
    int *ip;

    if (mp->updated)
	return;

    /* The internal lookup tables are in pixel units. */
    initialize_mapping (&p_mp);
    get_pixel_mapping (w, mp, &p_mp, 0);

    if ((snx = p_mp.snx) <= 0 || (sny = p_mp.sny) <= 0)
	return;

    if ((dnx = p_mp.dnx) < 0) {
	dnx = -dnx;
	xflip++;
    }
    if ((dny = p_mp.dny) < 0) {
	dny = -dny;
	yflip++;
    }

    sx = p_mp.sx;
    sy = p_mp.sy;
    dx = p_mp.dx;
    dy = p_mp.dy;
    xmax = dnx - 1;
    ymax = dny - 1;

    /* Discard the temporary mapping.
    free_mapping (w, &p_mp);
     */

    /* Get scale factors. */
    mp->xscale = (float)dnx / (float)snx;
    mp->yscale = (float)dny / (float)sny;

    /* Determine type of scaling to be used. */
    if (mp->xscale < 1.0 || mp->yscale < 1.0) {
	mp->scaling = M_DEZOOM;
    } else if (mp->xscale > 1.0 || mp->yscale > 1.0) {
	mp->scaling = M_ZOOM;
	if (abs(mp->xscale - (int)(mp->xscale+0.5)) < ZOOM_TOL &&
	    abs(mp->yscale - (int)(mp->yscale+0.5)) < ZOOM_TOL)
	    mp->scaling = M_INTZOOM;
    } else
	mp->scaling = (xflip || yflip) ? M_ZOOM : M_NOSCALING;

    /* Get a data buffer for the lookup tables. */
    mp->datalen =
	snx * sizeof(mapExtent) +		/* xy, extent */
	sny * sizeof(mapExtent) +
	dnx * sizeof(int) +			/* xy, srcpix */
	dny * sizeof(int) +
	dnx * sizeof(float) +			/* xy, src */
	dny * sizeof(float);

    if (mp->mapdata)
	mp->mapdata = (uchar *) XtRealloc ((char *)mp->mapdata, mp->datalen);
    else
	mp->mapdata = (uchar *) XtMalloc (mp->datalen);
    if (mp->mapdata == NULL)
	return;

    /* Set the table pointers. */
    op = mp->mapdata;
    mp->x_extent = (mapExtent *) op;	op += snx * sizeof(mapExtent);
    mp->y_extent = (mapExtent *) op;	op += sny * sizeof(mapExtent);
    mp->x_srcpix = (int *) op;		op += dnx * sizeof(int);
    mp->y_srcpix = (int *) op;		op += dny * sizeof(int);
    mp->x_src    = (float *) op;	op += dnx * sizeof(float);
    mp->y_src    = (float *) op;	op += dny * sizeof(float);

    /* Compute the backpointers to the source raster for each destination
     * pixel center.
     */
    for (i=0, ip = mp->x_srcpix, fp = mp->x_src;  i < dnx;  i++) {
	fp[i] = ((xflip ? xmax - i : i) + 0.5) / mp->xscale + sx;
	ip[i] = fp[i];
    }
    for (i=0, ip = mp->y_srcpix, fp = mp->y_src;  i < dny;  i++) {
	fp[i] = ((yflip ? ymax - i : i) + 0.5) / mp->yscale + sy;
	ip[i] = fp[i];
    }

    /* Compute the extent arrays.  These define the range of destination
     * pixels affected by each source pixel.
     */
    lo = dnx - 1 + dx;
    hi = dx;
    for (i=0;  i < snx;  i++) {
	mp->x_extent[i].lo = lo;
	mp->x_extent[i].hi = hi;
    }
    lo = dny - 1 + dy;
    hi = dy;
    for (i=0;  i < sny;  i++) {
	mp->y_extent[i].lo = lo;
	mp->y_extent[i].hi = hi;
    }

    /* Map the left and right or top and bottom edges of each destination
     * pixel back into the source rect and update the corresponding extent
     * entries to indicate that these source pixels are used to compute the
     * destination pixel.
     */
    pixwidth = 1.0 - ZOOM_TOL;

    for (i=0;  i < dnx;  i++) {
	edge1 = (xflip ? xmax - i : i) / mp->xscale;
	edge2 = (xflip ? xmax - (i-pixwidth) : (i+pixwidth)) / mp->xscale;
	if (edge1 > edge2) {
	    temp = edge1;  edge1 = edge2;  edge2 = temp;
	}
	edge1 = max (0, edge1);
	edge2 = min (snx - 1, edge2);

	for (j=edge1, k = dx + i;  j <= edge2;  j++) {
	    if (mp->x_extent[j].lo > k)
		mp->x_extent[j].lo = k;
	    if (mp->x_extent[j].hi < k)
		mp->x_extent[j].hi = k;
	}
    }

    for (i=0;  i < dny;  i++) {
	edge1 = (yflip ? ymax - i : i) / mp->yscale;
	edge2 = (yflip ? ymax - (i-pixwidth) : (i+pixwidth)) / mp->yscale;
	if (edge1 > edge2) {
	    temp = edge1;  edge1 = edge2;  edge2 = temp;
	}
	edge1 = max (0, edge1);
	edge2 = min (sny - 1, edge2);

	for (j=edge1, k = dy + i;  j <= edge2;  j++) {
	    if (mp->y_extent[j].lo > k)
		mp->y_extent[j].lo = k;
	    if (mp->y_extent[j].hi < k)
		mp->y_extent[j].hi = k;
	}
    }

    mp->updated = 1;
}


/* free_mapping -- Free any storage used internally by a mapping descriptor,
 * and deactivate the mapping.
 */
static
free_mapping (w, mp)
    GtermWidget w;
    register Mapping mp;
{
    mp_unlink (w, mp);
    mp->defined = mp->enabled = mp->updated = 0;
    if (mp->mapdata) {
	XtFree ((char *) mp->mapdata);
	mp->mapdata = NULL;
	mp->datalen = 0;
	mp->x_extent = mp->y_extent = NULL;
	mp->x_srcpix = mp->y_srcpix = NULL;
	mp->x_src = mp->y_src = NULL;
	mp->updated = 0;
    }
}

static void
mp_linkafter (w, mp, ref_mp)
    register GtermWidget w;
    register Mapping mp;
    register Mapping ref_mp;
{
    register Mapping map;

    /* Don't use the reference mapping unless it is already linked or
     * the list is empty.
     */
    if (w->gterm.mp_head) {
	for (map = w->gterm.mp_head;  map && map != ref_mp;  map = map->next)
	    ;
	if (map != ref_mp)
	    ref_mp = NULL;
    }

    mp->prev = ref_mp;
    mp->next = ref_mp ? ref_mp->next : NULL;
    if (ref_mp && ref_mp->next)
	ref_mp->next->prev = mp;
    if (ref_mp)
	ref_mp->next = mp;

    if (!w->gterm.mp_tail || ref_mp == w->gterm.mp_tail)
	w->gterm.mp_tail = mp;
    if (!w->gterm.mp_head)
	w->gterm.mp_head = mp;
}


static void
mp_unlink (w, mp)
    register GtermWidget w;
    register Mapping mp;
{
    if (mp->prev)
	mp->prev->next = mp->next;
    if (mp->next)
	mp->next->prev = mp->prev;
    if (w->gterm.mp_head == mp)
	w->gterm.mp_head = mp->next;
    if (w->gterm.mp_tail == mp)
	w->gterm.mp_tail = mp->prev;

    mp->prev = mp->next = NULL;
}


/*
 * Graphics MARKERS.
 * --------------------
 * A marker is an active graphics object displayed on top of a drawing to
 * mark, annotate, or outline a region.  Markers can respond to events and
 * move, resize, or modify themselves, optionally executing callback
 * procedures when the marker changes state.  Markers are used to
 * interactively define regions with the mouse, to provide a dynamic graphical
 * display which doesn't interfere with the underlying graphics frame, or as a
 * graphical means of command input, using callbacks to perform some operation
 * when the marker is moved or resized by the user.
 * 
 *	       GtMarkerInit (w)
 *	       GtMarkerFree (w)
 *
 *	      gm = GmCreate (w, type, interactive)
 *		GmRedisplay (w, region|NULL)
 *		gm = GmCopy (gm)
 *		  GmDestroy (gm)
 *	      GmAddCallback (gm, events, func, client_data)
 *	   GmDeleteCallback (gm, func, client_data)
 *	      gm = GmSelect (gt, x, y, &what)
 *
 *		  GmMarkpos (gm)
 *		   GmRedraw (gm, func, erase)
 *		    GmRaise (gm, ref_gm|NULL)
 *		    GmLower (gm, ref_gm|NULL)
 *		   GmNotify (gm, events, event, param, nparams)
 *
 *		    GmAddPt (gm, x, y)
 *		 GmDeletePt (gm, x, y)
 *		   GmMovePt (gm, x, y)
 *		     GmMove (gm, x, y)
 *		   GmResize (gm, x, y)
 *		   GmRotate (gm, x, y)
 *
 *          GmSetAttributes (gm, args, nargs, type)
 *          GmGetAttributes (gm, args, nargs, type)
 *	     GmSetAttribute (gm, attribute, value, type)
 *	     GmGetAttribute (gm, attribute, value, type)
 *	      GmSetVertices (gm, points, first, npts)
 *     npts = GmGetVertices (gm, points, first, maxpts)
 *	   GmGetBoundingBox (gm, x, y, width, height)
 *
 *	 type = GmStrToType (marker_type)
 *     event = GmStrToEvent (event_type)
 *   func = GmStrToFunction (drawing_function)
 *
 * Markers operate in screen coordinates (raster 0).  The SelectRaster
 * and MapVector routines may be used to convert to and from raster
 * coordinates if desired.
 *
 *  raster = GtSelectRaster (gt, dras, dt, dx, dy, rt, &rx, &ry, &mp)
 *              GtMapVector (gt, mp, dir, st, sv, dt, dv, npts, clip)
 *
 * The Gm procedures above implement the main functionality of markers.  User
 * interaction is provided at a higher level using action procedures which
 * are bound to pointer and keyboard events via translations (or by the GUI
 * itself directly calling the above procedures).
 */

static void gm_text_init(), gm_line_init(), gm_plin_init(), gm_rect_init();
static void gm_boxx_init(), gm_circ_init(), gm_elip_init(), gm_pgon_init();
static int gm_putint(), gm_putfloat(), gm_do_callbacks(), gm_constraint();
static int gm_getint(), gm_getattribute(), gm_gettype();
static double gm_getfloat();
static char *gm_getstring();

static void gm_markpos(), gm_erase(), gm_redraw(), gm_setCurRect();
static void gm_linkafter(), gm_unlink();
static double gm_niceAngle();
static Pixel gm_getpixel();
static int gm_select();
static int gm_getfillstyle();

static GmVMethod gm_classinit[] = {
    gm_text_init, gm_line_init, gm_plin_init, gm_rect_init,
    gm_boxx_init, gm_circ_init, gm_elip_init, gm_pgon_init
};

static Region null_region;
static XRectangle null_rect = { 0, 0, 0, 0 };
#define NullRect(r)	(!(r)->width || !(r)->height)

#define PI_2		1.57079632679489661923
#define PI_4		0.78539816339744830962
#define BORDER		5

static void M_create(), M_destroy(), M_destroyNull(), M_set(), M_raise();
static void M_lower(), M_notify(), M_markpos(), M_markposAdd(), M_redraw();
static void M_addPt(), M_deletePt(), M_movePt(), M_deleteDestroy();
static void M_move(), M_resize(), M_moveResize(), M_rotate();
static void M_rotateResize(), M_input();
static void gm_focusin(), gm_focusout();

static XtActionsRec markerActionsList[] = {
	{ "m_create",		M_create },
	{ "m_destroy",		M_destroy },
	{ "m_destroyNull",	M_destroyNull },
	{ "m_set",		M_set },
	{ "m_raise",		M_raise },
	{ "m_lower",		M_lower },
	{ "m_notify",		M_notify },
	{ "m_input",		M_input },
	{ "m_markpos",		M_markpos },
	{ "m_markposAdd",	M_markposAdd },
	{ "m_redraw",		M_redraw },
	{ "m_addPt",		M_addPt },
	{ "m_deletePt",		M_deletePt },
	{ "m_movePt",		M_movePt },
	{ "m_deleteDestroy",	M_deleteDestroy },
	{ "m_move",		M_move },
	{ "m_resize",		M_resize },
	{ "m_moveResize",	M_moveResize },
	{ "m_rotate",		M_rotate },
	{ "m_rotateResize",	M_rotateResize },
};


/* GtMarkerInit -- Initialize the marker subsystem.
 */
GtMarkerInit (w)
    GtermWidget w;
{
    register Marker gm, prev;
    XColor fg_color, bg_color;
    Display *display = w->gterm.display;
    int type, i;
    GC gc;

    for (gm = w->gterm.gm_tail;  gm;  gm = prev) {
	prev = gm->prev;
        GmDestroy (gm);
    }

    if (!w->gterm.gm_initialized) {
	/* Register some additional actions for markers. */
	XtAppAddActions (XtWidgetToApplicationContext((Widget)w),
	    markerActionsList, XtNumber(markerActionsList));

	/* Get the gterm widget translations. */
	if ((char *)w->gterm.defTranslations == NULL) {
	    char *translations = NULL;
	    XtTranslations tt;
	    XtResource r;
	    int ttype, i;

	    r.resource_name   = XtNtranslations;
	    r.resource_class  = XtCTranslations;
	    r.resource_type   = XtRString;
	    r.resource_size   = sizeof (char *);
	    r.resource_offset = 0;
	    r.default_type    = XtRString;
	    r.default_addr    = (caddr_t) NULL;

	    XtGetApplicationResources ((Widget)w, &translations, &r, 1,NULL,0);

	    if (translations) {
		if (strncmp (translations, "#augment", 8) == 0)
		    ttype = T_augment;
		else if (strncmp (translations, "#override", 9) == 0)
		    ttype = T_override;
		else
		    ttype = T_replace;

		if (ttype == T_replace) {
		    w->gterm.defTranslations =
			XtParseTranslationTable (translations);
		} else if ((i = w->gterm.nauxTrans) < MAX_AUXTRANS) {
		    w->gterm.defTranslations =
			XtParseTranslationTable (defaultGtermTranslations);
		    w->gterm.auxTrans[i] =
			XtParseTranslationTable (translations);
		    w->gterm.auxTType[i] = ttype;
		    w->gterm.nauxTrans++;
		}

	    } else {
		w->gterm.defTranslations =
		    XtParseTranslationTable (defaultGtermTranslations);
	    }

	    /* Get the marker translations. */
	    if ((char *)w->gterm.gm_defTranslations == NULL)
		w->gterm.gm_defTranslations =
		    XtParseTranslationTable (w->gterm.gm_translations);
	}

	/* Cancel any load translation table interval timer. */
	if (w->gterm.gm_timer_id) {
	    XtRemoveTimeOut (w->gterm.gm_timer_id);
	    w->gterm.gm_timer_id = (XtIntervalId) NULL;
	}

	/* Set the default gterm window translations. */
	gm_load_translations (w, NULL);

	/* Get graphics drawing GC. */
	gc = XCreateGC (display, w->gterm.root, 0, NULL);
	XSetBackground (display, gc, w->gterm.color0);
	XSetForeground (display, gc, w->gterm.color1);
	XSetLineAttributes (display, gc,
	    w->gterm.gm_lineWidth, w->gterm.gm_lineStyle, CapButt, JoinMiter);
	w->gterm.gm_drawGC = gc;

	/* Get graphics rubber-band GC. */
	gc = XCreateGC (display, w->gterm.root, 0, NULL);
	XSetFunction (display, gc, GXxor);
	XSetFillStyle (display, gc, FillSolid);
	XSetForeground (display, gc, w->gterm.gm_xorFillColor);
	XSetBackground (display, gc, w->gterm.gm_xorFillBgColor);
	XSetLineAttributes (display, gc,
	    0, LineDoubleDash, CapButt, JoinMiter);
	w->gterm.gm_rubberGC = gc;

	fg_color.pixel = w->gterm.gm_cursorFgColor;
	bg_color.pixel = w->gterm.gm_cursorBgColor;
	XQueryColor (display, w->core.colormap, &fg_color);
	XQueryColor (display, w->core.colormap, &bg_color);

	w->gterm.gm_markerCursor = XCreateFontCursor (display, XC_fleur);
	XRecolorCursor (display, w->gterm.gm_markerCursor, &fg_color,&bg_color);
	w->gterm.gm_edgeCursor = XCreateFontCursor (display, XC_dotbox);
	XRecolorCursor (display, w->gterm.gm_edgeCursor, &fg_color,&bg_color);
	w->gterm.gm_pointCursor = XCreateFontCursor (display, XC_sizing);
	XRecolorCursor (display, w->gterm.gm_pointCursor, &fg_color,&bg_color);

	if (!(type = GmStrToType (w->gterm.gm_defaultMarker)))
	    type = Gm_Rectangle;
	w->gterm.gm_defaultType = type;

	if (!null_region)
	    null_region = XCreateRegion();
	w->gterm.gm_initialized++;
    }

    w->gterm.gm_create = NULL;
    w->gterm.gm_active = NULL;
    w->gterm.gm_redisplay = False;
    w->gterm.preserve_screen = 0;
}


/* GtMarkerFree -- Free any marker subsystem resources.
 */
static void
GtMarkerFree (w)
    register GtermWidget w;
{
    register Display *display = w->gterm.display;
    register Marker gm;

    /* Cancel any load translation table interval timer. */
    if (w->gterm.gm_timer_id) {
	XtRemoveTimeOut (w->gterm.gm_timer_id);
	w->gterm.gm_timer_id = (XtIntervalId) NULL;
    }

    /* Set the default gterm window translations. */
    gm_load_translations (w, NULL);

    for (gm = w->gterm.gm_tail;  gm;  gm = gm->prev)
        GmDestroy (gm);

    if (!w->gterm.gm_initialized)
	return;

    XFreeGC (display, w->gterm.gm_drawGC);
    XFreeGC (display, w->gterm.gm_rubberGC);

    /* This call can fail - see comments elsewhere in this file about
     * XFreeCursor.
     *
    XFreeCursor (display, w->gterm.gm_markerCursor);
    XFreeCursor (display, w->gterm.gm_edgeCursor);
    XFreeCursor (display, w->gterm.gm_pointCursor);
     */

    w->gterm.gm_initialized = 0;
}


/* gm_focusin -- Called when gterm window input is directed to a marker.
 */
static void
gm_focusin (w, gm, what)
    register GtermWidget w;
    register Marker gm;
    GmSelection what;
{
    Cursor cursor;
    int erase;
    Marker am;

    if (!XtIsRealized ((Widget)w))
	return;

    if (am = w->gterm.gm_active) {
	if (am != gm)
	    gm_focusout (w, 0);
	else if (what && what->type == w->gterm.gm_selection.type) {
	    /* no change */
	    return;
	}
    }

    if (what) {
	switch (what->type) {
	case Ge_Point:
	    cursor = w->gterm.gm_pointCursor;
	    break;
	case Ge_Edge:
	    cursor = w->gterm.gm_edgeCursor;
	    break;
	default:
	    cursor = w->gterm.gm_markerCursor;
	    break;
	}
    } else
	cursor = w->gterm.gm_markerCursor;

    erase_crosshair (w);
    XDefineCursor (w->gterm.display, w->gterm.window, cursor);
    w->gterm.gm_active = gm;
    w->gterm.gm_selection = *what;

    if (gm && gm != am) {
	gm_request_translations (w, gm);
	GmMarkpos (gm);
	GmRedraw (gm, GXcopy, erase=True);
    }

    gm_do_callbacks (gm, GmEvFocusIn, NULL, NULL, 0);
}


/* gm_focusout -- Called to restore the normal gterm window input when the
 * pointer moves off a marker.
 */
static void
gm_focusout (w, enableSetTrans)
    register GtermWidget w;
    int enableSetTrans;			/* replace translations */
{
    register Display *display = w->gterm.display;
    register Marker gm = w->gterm.gm_active;
    int erase, i;

    if (!XtIsRealized ((Widget)w))
	return;

    /* Restore the default gterm window translations. */
    if (enableSetTrans)
	gm_request_translations (w, NULL);

    XDefineCursor (display, w->gterm.window, w->gterm.cursor);
    w->gterm.gm_active = NULL;

    if (gm) {
	GmMarkpos (gm);
	GmRedraw (gm, GXcopy, erase=True);
    }

    gm_do_callbacks (gm, GmEvFocusOut, NULL, NULL, 0);
}


/* gm_refocus -- Simulate a pointer event to recompute the marker pointer
 * focus.  Called when a software event changes the marker stacking order
 * in some way.
 */
static void 
gm_refocus (w)
    GtermWidget w;
{
    XMotionEvent event;
    int nparams = 0;

    event.type = MotionNotify;					/* MF009 */
    event.x = w->gterm.last_x;
    event.y = w->gterm.last_y;
    HandleTrackCursor ((Widget)w, &event, NULL, &nparams);
}


/* 
 * Translation tables.  The widget's translation table must not be replaced
 * while a translation is executing.  This can be a problem as it is often
 * events and their translations which lead to the translation table getting
 * replaced.  To avoid this problem we merely queue a timer event to load
 * the desired translation table, allowing any existing translation to
 * finish executing before the translation table is swapped out.  If multiple
 * translation table load requests are issued only the final one has any
 * effect.
 */

/* gm_request_translations -- Queue a request to load the translations for the
 * specified marker (or NULL to load the default gterm translations).  If this
 * is the first request and timers are enabled a timer is posted to load the
 * translations when any current event processing is complete.  If a request
 * is already active then the most recent request supercedes any previous one.
 */
static void
gm_request_translations (w, gm)
    register GtermWidget w;
    Marker gm;
{
    w->gterm.gm_reqTranslations = gm;

    if (!w->gterm.useTimers)
	gm_load_translations (w, NULL);
    else if (!w->gterm.gm_timer_id) {
	w->gterm.gm_timer_id =
	    XtAppAddTimeOut (XtWidgetToApplicationContext((Widget)w), 0,
		gm_load_translations, (XtPointer)w);
    }
}


/* gm_load_translations -- Swap out the widget's translation table.  This is
 * a no-op if the requested translation table is already loaded.
 */
static void
gm_load_translations (w, id)
    register GtermWidget w;
    XtIntervalId id;
{
    register Marker am, gm;
    register int i;

    w->gterm.gm_timer_id = (XtIntervalId) NULL;

    am = w->gterm.gm_curTranslations;
    gm = w->gterm.gm_reqTranslations;
    if (am == gm && w->gterm.gm_initialized)
	return;

    if (gm) {
	/* Set the translations for the indicated marker. */
	if (!am || am->translations != gm->translations)
	    XtOverrideTranslations ((Widget)w, gm->translations);
    } else {
	/* Restore the default gterm window translations. */
	XtVaSetValues ((Widget)w,
	    XtNtranslations, (XtArgVal)w->gterm.defTranslations, NULL);
	for (i=0;  i < w->gterm.nauxTrans;  i++) {
	    switch (w->gterm.auxTType[i]) {
	    case T_augment:
		XtAugmentTranslations ((Widget)w, w->gterm.auxTrans[i]);
		break;
	    case T_override:
		XtOverrideTranslations ((Widget)w, w->gterm.auxTrans[i]);
		break;
	    }
	}
    }

    w->gterm.gm_curTranslations = w->gterm.gm_reqTranslations;
}


/* Public marker functions.
 * --------------------------
 */

/* GmCreate -- Create a new marker.
 */
Marker
GmCreate (w, type, interactive)
    GtermWidget w;
    int type;			/* marker type */
    int interactive;		/* use pointer to set position */
{
    register Marker gm;

    /* Allocate descriptor. */
    if (type < 1 || type > Gm_NTypes)
	return (NULL);
    if (!(gm = (Marker) XtCalloc (1, sizeof (struct marker))))
	return (NULL);

    /* Initialize descriptor. */
    gm->w = w;
    gm->type = type;
    gm->flags = interactive ? (Gm_Visible|Gm_Sensitive) : 0;
    gm->translations = w->gterm.gm_defTranslations;
    gm->old_region = XCreateRegion();
    gm->cur_region = XCreateRegion();
    (gm_classinit[type-1]) (gm, interactive);

    /* Link marker to tail of marker list. */
    gm_linkafter (gm, w->gterm.gm_tail);

    /* If marker is being created interactive, set flag to indicate that the
     * next create marker event should finish creating this marker.
     */
    if (w->gterm.gm_create)
	GmDestroy (w->gterm.gm_create);
    w->gterm.gm_create = interactive ? gm : NULL;

    return (gm);
}


/* GmDestroy -- Destroy a marker.
 */
GmDestroy (gm)
    register Marker gm;
{
    register GtermWidget w = gm->w;
    Region old_region, cur_region;

    /* GmDestroy can be called recursively during a destroy operation as a
     * side effect of the destroy callback.  Set the Gm_BeingDestroyed flag
     * to cause these redundant destroy requests to be ignored.
     */
    if (gm->flags & Gm_BeingDestroyed)
	return (OK);
    gm->flags |= Gm_BeingDestroyed;

    /* Release the focus if active marker.   This should be done before
     * proceeding to destroy the marker, i.e. before calling the destroy
     * callbacks.
     */
    if (w->gterm.gm_active == gm) {
	gm_focusout (w, 1);
	w->gterm.gm_active = NULL;
    }

    /* Inform any clients that have registered a callback for this marker
     * that we are about to destroy the marker.
     */
    gm_do_callbacks (gm, GmEvDestroy, NULL, NULL, 0);

    /* Erase the marker from the screen. */
    GmMarkpos (gm);
    gm_erase (gm);

    /* Note marker position. */
    old_region = gm->old_region;
    cur_region = gm->cur_region;

    /* Free all storage and unlink the marker. */
    if (gm->npoints > GM_MAXVERTICES)
	XtFree ((char *)gm->points);
    if (gm->text)
	XtFree ((char *)gm->text);
    if (gm->pgon)
	XtFree ((char *)gm->pgon);

    gm_unlink (gm);
    XtFree ((char *)gm);

    /* Redraw any markers that were obscured by the deleted marker. */
    update_transients (w, old_region);

    XDestroyRegion (old_region);
    XDestroyRegion (cur_region);

    /* Recompute the marker focus. */
    gm_refocus (w);

    return (OK);
}


/* GmCopy -- Copy a marker.
 */
Marker
GmCopy (gm)
    register Marker gm;
{
    register GtermWidget w = gm->w;
    register Marker nm;

    if (!(nm = (Marker) XtCalloc (1, sizeof (struct marker))))
	return (NULL);

    *nm = *gm;
    nm->parent = gm;
    nm->old_region = NULL;
    nm->cur_region = NULL;
    nm->points = NULL;
    nm->pgon = NULL;
    nm->text = NULL;

    /* Copy region descriptors. */
    if ((char *)(nm->old_region = XCreateRegion()) == NULL)
	goto fail;
    if ((char *)(nm->cur_region = XCreateRegion()) == NULL)
	goto fail;
    XUnionRegion (nm->old_region, gm->cur_region, nm->cur_region);

    /* Copy any polypoint data. */
    if (gm->pgon) {
	nm->pgon = (DPoint *) XtMalloc (gm->npoints * sizeof(DPoint));
	if (nm->pgon == NULL)
	    goto fail;
	memmove (nm->pgon, gm->pgon, gm->npoints * sizeof(DPoint));
    }

    /* Copy region polygon. */
    if (gm->npoints > GM_MAXVERTICES) {
	if (!(nm->points = (XPoint *) XtMalloc (gm->npoints * sizeof(XPoint))))
	    goto fail;
	memmove (nm->points, gm->points, gm->npoints * sizeof(XPoint));
    }

    /* Copy any text data. */
    if (gm->text) {
	int nchars = strlen (gm->text);
	if (!(nm->text = XtMalloc (nchars + 1)))
	    goto fail;
	memmove (nm->text, gm->text, nchars + 1);
    }

    gm_linkafter (nm, w->gterm.gm_tail);
    return (nm);

fail:
    if (nm->text)
	XtFree (nm->text);
    if (nm->pgon)
	XtFree ((char *)nm->pgon);
    if (nm->points && nm->points != nm->point_data)
	XtFree ((char *)nm->points);
    if ((char *)nm->cur_region)
	XDestroyRegion (nm->cur_region);
    if ((char *)nm->old_region)
	XDestroyRegion (nm->old_region);

    XtFree ((char *)nm);
    return (NULL);
}


/* GmAddCallback -- Add a callback to a marker.
 */
GmAddCallback (gm, events, func, client_data)
    register Marker gm;
    int events;			/* events callback is to receive */
    GmIMethod func;		/* function to be called */
    XtPointer client_data;	/* client data for above */
{
    register struct markerCallback *cb;
    register int i;

    /* Find an empty callback slot. */
    for (i=0;  i < GM_MAXCALLBACKS;  i++)
	if (!gm->callback[i].events)
	    break;

    /* Register the callback. */
    if (i < GM_MAXCALLBACKS) {
	cb = &gm->callback[i];
	cb->events = events;
	cb->func = func;
	cb->client_data = client_data;
	gm->ncallbacks = max (gm->ncallbacks, i + 1);
    }

    if (events & GmEvConstraint)
	gm->constraints++;
}


/* GmDeleteCallback -- Delete a previously posted callback given the
 * function pointer and client data passed when the callback was registered.
 */
GmDeleteCallback (gm, func, client_data)
    register Marker gm;
    GmIMethod func;		/* callback function */
    XtPointer client_data;	/* client data for above */
{
    register struct markerCallback *cb;
    register int i, n;

    for (i=n=0;  i < GM_MAXCALLBACKS;  i++) {
	cb = &gm->callback[i];

	if (cb->func == func && cb->client_data == client_data) {
	    if (cb->events & GmEvConstraint)
		gm->constraints--;
	    cb->events = (int)NULL;
	    cb->func = (GmIMethod)NULL;
	    cb->client_data = (XtPointer)NULL;
	} else if (cb->events)
	    n = i;
    }

    gm->ncallbacks = n + 1;
}


/* GmSelect -- Scan the marker list to see if the given pointer coordinates
 * are within an active marker.  If so, the marker descriptor is returned as
 * the function value, and the "what" argument is set to indicate what part
 * of the marker was selected.
 */
Marker
GmSelect (w, x, y, what)
    GtermWidget w;
    int x, y;
    GmSelection what;
{
    register int flags = (Gm_Activated|Gm_Visible|Gm_Sensitive);
    register XRectangle *r;
    register Marker gm;

    for (gm = w->gterm.gm_tail;  gm;  gm = gm->prev) {
	if (!((gm->flags & (flags|Gm_BeingDestroyed)) == flags))
	    continue;
	r = &gm->cur_rect;
	if (x < (int)r->x || x >= (int)(r->x + r->width) ||
	    y < (int)r->y || y >= (int)(r->y + r->height))
	    continue;
	if (gm->select (gm, x, y, what))
	    return (gm);
    }

    return (NULL);
}


/* GmMarkpos -- Save the current marker position, e.g., prior to modifying
 * the marker.  This is used to erase the old marker when the modified
 * marker is later redrawn.
 */
GmMarkpos (gm)
    register Marker gm;
{
    gm->markpos (gm);
}


/* GmRedraw -- Redraw a marker using the given drawing function.  If the erase
 * flag is not set (as when in rubber-band mode) the marker is merely drawn
 * to the screen.  Otherwise if the old marker position has been saved the
 * old marker is first erased, then any markers affected by the erase are
 * redrawn, and finally the current marker is redrawn at the new location.
 */
GmRedraw (gm, func, erase)
    Marker gm;
    int func;
    int erase;
{
    register Marker mm;
    register XRectangle *o, *n, *r;
    int flags = (Gm_Activated|Gm_Visible);
    Region clip_region, temp_region, temp;
    GtermWidget w = gm->w;
    int outside;

    /* Recompute marker polygon if any attributes have changed. */
    gm->update (gm);

    clip_region = XCreateRegion();
    temp_region = XCreateRegion();

    /* Erase the previously marked region (old position). */
    if (erase) {
	XUnionRegion (gm->old_region, clip_region, temp_region);
	temp = clip_region; clip_region = temp_region; temp_region = temp;
	XSetRegion (w->gterm.display, w->gterm.exposeGC, clip_region);
	gm_erase (gm);
    }

    if (!erase && func == GXxor)
	gm->redraw (gm, func);
    else {
	/* Draw the marker and any markers it intersects, clipping to the
	 * new marker region.
	 */
	o = &gm->old_rect;
	n = &gm->cur_rect;

	XUnionRegion (gm->cur_region, clip_region, temp_region);
	temp = clip_region; clip_region = temp_region; temp_region = temp;
	XSetRegion (w->gterm.display, w->gterm.gm_drawGC, clip_region);

	for (mm = gm->w->gterm.gm_head;  mm;  mm = mm->next) {
	    if (!((mm->flags & flags) == flags))
		continue;

	    /* Redraw a marker if it intersects either the old rect or the
	     * new rect.
	     */
	    if (mm != gm) {
		r = &mm->cur_rect;
		outside = 0;
		if ((int)r->x >= (int)o->x + (int)o->width  ||
			(int)r->x + (int)r->width <= (int)o->x ||
		    (int)r->y >= (int)o->y + (int)o->height ||
			(int)r->y + (int)r->height <= (int)o->y)
		    outside++;
		if ((int)r->x >= (int)n->x + (int)n->width  ||
			(int)r->x + (int)r->width  <= (int)n->x ||
		    (int)r->y >= (int)n->y + (int)n->height ||
			(int)r->y + (int)r->height <= (int)n->y)
		    outside++;
		if (outside == 2)
		    continue;
	    }
	    mm->redraw (mm, func);
	}

	XSetClipMask (w->gterm.display, w->gterm.gm_drawGC, None);
    }

    if (erase)
	XSetClipMask (w->gterm.display, w->gterm.exposeGC, None);
    XDestroyRegion (clip_region);
    XDestroyRegion (temp_region);

    if (func != GXxor && gm->width > 0 && gm->height > 0) {
	/* Redraw callback. */
	gm_do_callbacks (gm, GmEvRedraw, NULL, NULL, 0);

	/* Generate moveResize callback, if marker was moved or resized.
	 */
	if (gm->old_rect.x != gm->cur_rect.x ||
	    gm->old_rect.y != gm->cur_rect.y ||
	    gm->old_rect.width != gm->cur_rect.width ||
	    gm->old_rect.height != gm->cur_rect.height) {

	    char x[32], y[32];
	    char width[32], height[32];
	    char *argv[5];
	    int argc;

	    /* If the marker was just created (old_rect null) or the marker
	     * moved and we did a full erase and redraw, any old markpos is
	     * obsolete so we may as well update the saved position.
	     */
	    if (erase || !gm->old_rect.width || !gm->old_rect.height)
		GmMarkpos (gm);

	    sprintf (x, "%d", gm->x);
	    sprintf (y, "%d", gm->y);
	    sprintf (width, "%d", gm->width);
	    sprintf (height, "%d", gm->height);
	    argv[0] = x;
	    argv[1] = y;
	    argv[2] = width;
	    argv[3] = height;
	    argv[4] = NULL;
	    argc = 4;

	    gm_do_callbacks (gm, GmEvMoveResize, NULL, argv, argc);
	}
    }
}


/* GmRedisplay -- Redisplay the markers in the given region, or redisplay
 * the entire window if the region is given as (char *)NULL.
 */
GmRedisplay (w, region)
    GtermWidget w;
    Region region;
{
    register int flags = (Gm_Activated|Gm_Visible);
    register XRectangle *r;
    register Marker gm;

    if (!XtIsRealized ((Widget)w))
	return;

    /* Set the clip mask to only draw in the affected region. */
    if (region)
	XSetRegion (w->gterm.display, w->gterm.gm_drawGC, region);

    /* Draw all markers that intersect the target region. */
    for (gm = w->gterm.gm_head;  gm;  gm = gm->next) {
	if (!((gm->flags & flags) == flags))
	    continue;

	if ((char *)region) {
	    gm->update (gm);
	    r = &gm->cur_rect;
	    if (XRectInRegion (region,
		r->x, r->y, r->width, r->height) == RectangleOut)
		continue;
	}

	gm->redraw (gm, GXcopy);
    }

    XSetClipMask (w->gterm.display, w->gterm.gm_drawGC, None);
    w->gterm.gm_redisplay = False;
}


/* GmRaise -- Change the stacking order of a marker relative to another
 * marker, causing the first marker to be drawn above the second.
 */
GmRaise (gm, ref_gm)
    register Marker gm, ref_gm;
{
    register GtermWidget w = gm->w;
    int erase;

    /* Already on top? */
    if (gm == w->gterm.gm_tail || ref_gm && ref_gm->next == gm)
	return;

    /* Raise it. */
    gm_unlink (gm);
    gm_linkafter (gm, ref_gm ? ref_gm : w->gterm.gm_tail);

    GmMarkpos (gm);
    GmRedraw (gm, GXcopy, erase=True);
    gm_refocus (w);
}


/* GmLower -- Change the stacking order of a marker relative to another
 * marker, causing the first marker to be drawn below the second.
 */
GmLower (gm, ref_gm)
    register Marker gm, ref_gm;
{
    register GtermWidget w = gm->w;
    int erase;

    /* Already lowered? */
    if (gm == w->gterm.gm_head || ref_gm && ref_gm->prev == gm)
	return;

    /* Lower it. */
    gm_unlink (gm);
    if (ref_gm && ref_gm->prev)
	gm_linkafter (gm, ref_gm->prev);
    else {
	gm->next = w->gterm.gm_head;
	w->gterm.gm_head = gm;
	if (gm->next)
	    gm->next->prev = gm;
	if (!w->gterm.gm_tail)
	    w->gterm.gm_tail = gm;
    }

    GmMarkpos (gm);
    GmRedraw (gm, GXcopy, erase=True);
    gm_refocus (w);
}


/* GmNotify -- Notify any clients that have registered callbacks that the
 * given marker events have occurred.
 */
GmNotify (gm, events, event, params, nparams)
    register Marker gm;
    int events;
    XEvent *event;
    String *params;
    Cardinal nparams;
{
    gm_do_callbacks (gm, events, event, params, nparams);
}


/* GmAddPt -- Add a point to a marker.
 */
GmAddPt (gm, x, y)
    register Marker gm;
    int x, y;
{
    int erase;
    if (gm->addPt) {
	GmRedraw (gm, GXxor, erase=False);
	gm->addPt (gm, x, y);
	GmRedraw (gm, GXxor, erase=False);
	gm_refocus (gm->w);
    }
}


/* GmDeletePt -- Delete a point from a marker.
 */
GmDeletePt (gm, x, y)
    register Marker gm;
    int x, y;
{
    int erase;
    if (gm->deletePt) {
	GmMarkpos (gm);
	gm->deletePt (gm, x, y);
	GmRedraw (gm, GXcopy, erase=True);
	gm_refocus (gm->w);
    }
}


/* GmMovePt -- Move a point within a marker.
 */
GmMovePt (gm, x, y)
    register Marker gm;
    int x, y;
{
    int erase;
    if (gm->movePt) {
	GmRedraw (gm, GXxor, erase=False);
	gm->movePt (gm, x, y);
	GmRedraw (gm, GXxor, erase=False);
    }
}


/* GmMove -- Move a marker.
 */
GmMove (gm, x, y)
    register Marker gm;
    int x, y;
{
    int erase;
    if (gm->move) {
	GmRedraw (gm, GXxor, erase=False);
	gm->move (gm, x, y);
	GmRedraw (gm, GXxor, erase=False);
    }
}


/* GmResize -- Resize a marker.
 */
GmResize (gm, x, y)
    register Marker gm;
    int x, y;
{
    int erase;
    if (gm->resize) {
	GmRedraw (gm, GXxor, erase=False);
	gm->resize (gm, x, y);
	GmRedraw (gm, GXxor, erase=False);
    }
}


/* GmRotate -- Rotate a marker.
 */
GmRotate (gm, x, y)
    register Marker gm;
    int x, y;
{
    int erase;
    if (gm->rotate) {
	GmRedraw (gm, GXxor, erase=False);
	gm->rotate (gm, x, y);
	GmRedraw (gm, GXxor, erase=False);
    }
}


/* GmSetAttributes -- Set a list of attributes.  Requires that all attribute
 * values be specified in the same type.  Autoredraw, if enabled, is suspended
 * until all attributes have been changed.
 */
GmSetAttributes (gm, args, nargs, argtype)
    register Marker gm;
    ArgList args;
    int nargs;
    char *argtype;
{
    register int i;
    int autoredraw, erase;
    int status = OK;

    if (autoredraw = (gm->flags & Gm_AutoRedraw)) {
	gm->flags &= ~Gm_AutoRedraw;
	GmMarkpos (gm);
    }
    
    for (i=0;  i < nargs;  i++) {
	status |= GmSetAttribute (gm, args[i].name, args[i].value, argtype);
	if (strcmp (args[i].name, GmAutoRedraw) == 0)
	    autoredraw = gm_getint (args[i].value, argtype);
    }

    if (autoredraw) {
	gm->flags |= Gm_AutoRedraw;
	GmRedraw (gm, GXcopy, erase=True);
    }

    return (status ? ERR : OK);
}


/* GmSetAttribute -- Set the value of a marker attribute.
 */
GmSetAttribute (gm, attribute, value, type)
    register Marker gm;
    char *attribute;
    XtArgVal value;
    char *type;
{
    GtermWidget w = gm->w;
    int marker_type, atType;
    int erase, n, i;

    if (gm->flags & Gm_AutoRedraw)
	GmMarkpos (gm);

    switch (atType = gm_getattribute (attribute)) {
    case Ga_Type:
	switch (gm_gettype (type)) {
	case Gt_String:
	    marker_type = GmStrToType ((char *)value);
	    break;
	case Gt_Int:
	    marker_type = gm_getint (value, type);
	    break;
	default:
	    return (ERR);
	}

	marker_type = max(1, min(Gm_NTypes, marker_type));
	(gm_classinit[marker_type-1]) (gm, False);
	gm->flags |= Gm_Modified;
	break;

    case Ga_Activated:
	if (gm_getint (value, type)) {
	    if (!(gm->flags & Gm_Activated)) {
		gm->flags |= Gm_Activated;
		GmRedraw (gm, GXcopy, erase=False);
	    }
	} else {
	    GmMarkpos (gm);
	    gm_erase (gm);
	    gm->flags &= ~Gm_Activated;
	}
	return (OK);

    case Ga_Visible:
	if (gm_getint (value, type)) {
	    if (!(gm->flags & Gm_Visible)) {
		gm->flags |= Gm_Visible;
 		GmRedraw (gm, GXcopy, erase=False); 
	    }
	} else if (gm->flags & Gm_Visible) {
	    GmMarkpos (gm);
	    gm_erase (gm);
	    gm->flags &= ~Gm_Visible;
	}
	return (OK);

    case Ga_Sensitive:
	if (gm_getint (value, type))
	    gm->flags |= Gm_Sensitive;
	else
	    gm->flags &= ~Gm_Sensitive;
	return (OK);

    case Ga_AutoRedraw:
	if (gm_getint (value, type))
	    gm->flags |= Gm_AutoRedraw;
	else
	    gm->flags &= ~Gm_AutoRedraw;
	return (OK);

    case Ga_Translations:
	switch (gm_gettype (type)) {
	case Gt_String:
	    gm->translations = XtParseTranslationTable ((char *)value);
	    break;
	default:
	    return (ERR);
	}
	return (OK);

    case Ga_X:
	gm->x = gm_getint (value, type);
	break;
    case Ga_Y:
	gm->y = gm_getint (value, type);
	break;

    case Ga_Width:
    case Ga_Height:
	/* For a text marker a size can be specified either in integer
	 * pixels or in characters, e.g., "40ch" or "40 chars".
	 */
	if (gm->type == Gm_Text && type == XtRString) {
	    XFontStruct *fp = gm->font;
	    int char_width, char_height;
	    int l_pix, r_pix;
	    char *ip;

	    for (n=0, ip=(char *)value;  *ip && isdigit(*ip);  ip++)
		n = n * 10 + (*ip - '0');

	    while (isspace (*ip))
		ip++;
	    if (ip[0] == 'c' && ip[1] == 'h') {
		char_width  = fp->max_bounds.width;
		char_height = fp->max_bounds.ascent + fp->max_bounds.descent;
		l_pix = (gm->lineWidth + 1) / 2 + gm->textBorder - 1;
		r_pix = (gm->lineWidth + 1) / 2 + gm->textBorder - 1 -
		    (fp->max_bounds.width - fp->max_bounds.rbearing);

		if (atType == Ga_Width)
		    n = n * char_width + l_pix + r_pix;
		else
		    n = n * char_height + l_pix * 2;
	    }
	} else
	    n = gm_getint (value, type);

	if (atType == Ga_Width)
	    gm->width = n;
	else
	    gm->height = n;
	break;

    case Ga_Rotangle:						/* MF022 */
 	gm->rotangle = gm_getfloat (value, type) * (M_PI / (double) 180.0);
	break;

    case Ga_HighlightColor:
	gm->highlightColor = gm_getpixel (w, value, type);
	break;
    case Ga_LineColor:
	gm->lineColor = gm_getpixel (w, value, type);
	break;
    case Ga_LineWidth:
	gm->lineWidth = gm_getint (value, type);
	break;
    case Ga_LineStyle:
	gm->lineStyle = gm_getint (value, type);
	break;

    case Ga_KnotColor:
	gm->knotColor = gm_getpixel (w, value, type);
	break;
    case Ga_KnotSize:
	gm->knotSize = gm_getint (value, type);
	break;

    case Ga_Fill:
	gm->fill = gm_getint (value, type);
	break;
    case Ga_FillColor:
	gm->fillColor = gm_getpixel (w, value, type);
	break;
    case Ga_FillBgColor:
	gm->fillBgColor = gm_getpixel (w, value, type);
	break;
    case Ga_FillStyle:
	switch (gm_gettype (type)) {
	case Gt_String:
	    gm->fillStyle = gm_getfillstyle (w, value, type);
	    break;
	default:
	    break;
	}
	break;
    case Ga_FillPattern:
	switch (gm_gettype (type)) {
	case Gt_Pointer:
	    gm->fillPattern = (Pixmap) (value);
	    break;
	default:
	    return (ERR);
	}
	break;

    case Ga_TextColor:
	gm->textColor = gm_getpixel (w, value, type);
	break;
    case Ga_TextBgColor:
	gm->textBgColor = gm_getpixel (w, value, type);
	break;
    case Ga_TextBorder:
	gm->textBorder = gm_getint (value, type);
	break;
    case Ga_ImageText:
	gm->imageText = gm_getint (value, type);
	break;
    case Ga_Font:
	switch (gm_gettype (type)) {
	case Gt_Int:
	    i = gm_getint (value, type);
	    if (i >= 0 && i < NDialogFonts)
		gm->font = w->gterm.dialog_fonts[i];
	    break;
	case Gt_Pointer:
	    gm->font = (XFontStruct *) (value);
	    break;
	default:
	    return (ERR);
	}
	break;
    case Ga_Text:
	switch (gm_gettype (type)) {
	case Gt_Pointer:
	case Gt_String:
	    if (gm->text)
		XtFree (gm->text);
	    if (!(gm->text = XtMalloc (strlen((char *)value) + 1)))
		return (ERR);
	    strcpy (gm->text, (char *)value);
	    break;
	default:
	    return (ERR);
	}
	break;
    case Ga_RotIndicator:					/* MF020 */
	gm->rotIndicator = gm_getint (value, type);
	break;

    default:
	return (ERR);
    }

    gm->flags |= Gm_Modified;

    if (gm->flags & Gm_AutoRedraw)
	GmRedraw (gm, GXcopy, erase=True);

    /* Notify client that a marker attribute has changed. */
    {   char *argv[2];
	int argc;

	argv[0] = attribute;
	argv[1] = NULL;
	argc = 1;

	gm_do_callbacks (gm, GmEvModify, NULL, argv, argc);
    }

    return (OK);
}


/* GmGetAttributes -- Get a list of attributes.  Requires that all attribute
 * values be specified in the same type.
 */
GmGetAttributes (gm, args, nargs, argtype)
    register Marker gm;
    ArgList args;
    int nargs;
    char *argtype;
{
    register int i;

    for (i=0;  i < nargs;  i++)
	GmGetAttribute (gm, args[i].name, args[i].value, argtype);
}


/* GmGetAttribute -- Get the value of a marker attribute.
 */
GmGetAttribute (gm, attribute, value, type)
    register Marker gm;
    char *attribute;
    XtArgVal value;
    char *type;
{
    GtermWidget w = gm->w;
    int i;

    switch (gm_getattribute (attribute)) {
    case Ga_Type:
	switch (gm_gettype (type)) {
	case Gt_String:
	    switch (gm->type) {
	    case Gm_Text:
		strcpy ((char *)value, GmText);
		break;
	    case Gm_Line:
		strcpy ((char *)value, GmLine);
		break;
	    case Gm_Polyline:
		strcpy ((char *)value, GmPolyline);
		break;
	    case Gm_Rectangle:
		strcpy ((char *)value, GmRectangle);
		break;
	    case Gm_Box:
		strcpy ((char *)value, GmBox);
		break;
	    case Gm_Circle:
		strcpy ((char *)value, GmCircle);
		break;
	    case Gm_Ellipse:
		strcpy ((char *)value, GmEllipse);
		break;
	    case Gm_Polygon:
		strcpy ((char *)value, GmPolygon);
		break;
	    default:
		return (ERR);
	    }
	    break;
	case Gt_Int:
	    if (gm_putint (gm->type, value, type) == ERR)
		return (ERR);
	    break;
	default:
	    return (ERR);
	}
	break;

    case Ga_Activated:
	if (gm_putint ((gm->flags & Gm_Activated) != 0, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_Visible:
	if (gm_putint ((gm->flags & Gm_Visible) != 0, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_Sensitive:
	if (gm_putint ((gm->flags & Gm_Sensitive) != 0, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_AutoRedraw:
	if (gm_putint ((gm->flags & Gm_AutoRedraw) != 0, value, type) == ERR)
	    return (ERR);
	break;

    case Ga_X:
	if (gm_putint (gm->x, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_Y:
	if (gm_putint (gm->y, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_Width:
	if (gm_putint (gm->width, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_Height:
	if (gm_putint (gm->height, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_Rotangle:						/* MF022 */
	if (gm_putfloat(((double)180.0/M_PI)*(gm->rotangle),value,type) == ERR)
	    return (ERR);
	break;

    case Ga_HighlightColor:
	if (gm_putint ((int)gm->highlightColor, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_LineColor:
	if (gm_putint ((int)gm->lineColor, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_LineWidth:
	if (gm_putint (gm->lineWidth, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_LineStyle:
	if (gm_putint (gm->lineStyle, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_KnotColor:
	if (gm_putint ((int)gm->knotColor, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_KnotSize:
	if (gm_putint (gm->knotSize, value, type) == ERR)
	    return (ERR);
	break;

    case Ga_Fill:
	if (gm_putint (gm->fill, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_FillColor:
	if (gm_putint ((int)gm->fillColor, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_FillBgColor:
	if (gm_putint ((int)gm->fillBgColor, value, type) == ERR)
	    return (ERR);
	break;

    case Ga_FillStyle:
	switch (gm_gettype (type)) {
	case Gt_String:
	    switch (gm->fillStyle) {
	    case FillSolid:
		strcpy ((char *)value, "FillSolid");
		break;
	    case FillTiled:
		strcpy ((char *)value, "FillTiled");
		break;
	    case FillStippled:
		strcpy ((char *)value, "FillStippled");
		break;
	    case FillOpaqueStippled:
		strcpy ((char *)value, "FillOpaqueStippled");
		break;
	    default:
		strcpy ((char *)value, "FillSolid");
		break;
	    }
	    break;
	case Gt_Int:
	    if (gm_putint (gm->fillStyle, value, type) == ERR)
		return (ERR);
	    break;
	default:
	    return (ERR);
	}
	break;

    case Ga_FillPattern:
	switch (gm_gettype (type)) {
	case Gt_Pointer:
	    *(Pixmap *)value = gm->fillPattern;
	    break;
	default:
	    return (ERR);
	}
	break;

    case Ga_TextColor:
	if (gm_putint ((int)gm->textColor, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_TextBorder:
	if (gm_putint (gm->textBorder, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_ImageText:
	if (gm_putint (gm->imageText, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_Font:
	switch (gm_gettype (type)) {
	case Gt_Int:
	    for (i=0;  i < NDialogFonts;  i++)
		if (gm->font == w->gterm.dialog_fonts[i]) {
		    if (gm_putint (i, value, type) == ERR)
			return (ERR);
		    break;
		}
	    break;
	case Gt_Pointer:
	    *(XFontStruct **)value = gm->font;
	    break;
	default:
	    return (ERR);
	}
	break;
    case Ga_Text:
	switch (gm_gettype (type)) {
	case Gt_Pointer:
	    *((char **)value) = gm->text;
	    break;
	case Gt_String:
	    strcpy ((char *)value, gm->text);
	    break;
	default:
	    return (ERR);
	}
	break;
    case Ga_RotIndicator:					/* MF020 */
	if (gm_putint (gm->rotIndicator, value, type) == ERR)
	    return (ERR);
	break;


    default:
	return (ERR);
    }

    return (OK);
}


/* GmSetVertices -- Set the vertices of a "poly" type object.
 */
GmSetVertices (gm, points, first, npts)
    Marker gm;
    DPoint *points;		/* input array of points */
    int first;			/* first point to be set */
    int npts;			/* number of points to set */
{
    register DPoint *ip, *pp;
    register XPoint *op;
    register int i;
    int erase;

    /* The point vector is automatically extended if more space is needed.
     * Small vectors are stored directly in the marker descriptor in the
     * point_data array.
     */
    if (first + npts != gm->npoints) {				/* MF013 */
	if (gm->npoints > GM_MAXVERTICES) {
	    if ((gm->points = (XPoint *) XtRealloc ((char *)gm->points,
		    first + npts)) == (XPoint *)NULL)
		return;
	} else if (first + npts > GM_MAXVERTICES) {
	    if ((gm->points = (XPoint *)
		    XtMalloc (first + npts)) == (XPoint *)NULL)
		return;
	} else if (!gm->points)
	    gm->points = gm->point_data;

	gm->npoints = first + npts;
    }

    /* Copy the point data. */
    ip = points;
    op = &gm->points[first];
    for (i=0;  i < npts;  i++) {
	op->x = (int) ip->x + 0.5;
	op->y = (int) ip->y + 0.5;
	ip++, op++;
    }

    /* If we're defining the vertices of a 'poly' marker update the
     * pgon[] array with the new set of points.  Polygons are initialized
     * with a unit rectangle and since vertices can't be set as an attribute
     * the must be set with a setVertices call so we need to update the
     * structure here.
     */
    if (gm->type == Gm_Polygon) {				/* MF018 */

        if (gm->pgon)						/* MF018 */
            XtFree ((char *)gm->pgon);
        gm->pgon = (DPoint *) XtCalloc (first+npts+1, sizeof(DPoint));

        /* Copy the point data to the polygon array. */
        op = &gm->points[0];
        pp = &gm->pgon[0];
        for (i=0; i< gm->npoints; i++, pp++, op++) {
            pp->x = (double)op->x - gm->x;
            pp->y = (double)op->y - gm->y;
        }
        gm->points[first+npts] = gm->points[0];   /* Close the polygon.       */

        gm->npoints = gm->pgon_npts = first + npts + 1;
        gm->rotangle = 0.0;             	  /* reset rotation angle     */
        gm->flags |= Gm_Modified; 		  /* marker has been modified */
    }

    /* Redraw the marker if autoredraw is enabled. */
    if (gm->flags & Gm_AutoRedraw) {
	GmMarkpos (gm);
	GmRedraw (gm, GXcopy, erase=True);
    }
}


/* GmGetVertices -- Get the vertices of a "poly" type object.  The actual
 * number of points output is returned as the function value.
 */
GmGetVertices (gm, points, first, maxpts)
    register Marker gm;
    register DPoint *points;	/* output array of points */
    int first;			/* first point to be returned */
    int maxpts;			/* max number of points to return */
{
    register XPoint *ip;
    register DPoint *op;
    register int i;
    int top, nout;

    if (first >= gm->npoints)
	return (0);
    top = min (first + maxpts, gm->npoints);
    nout = top - first;

    /* In the case of a poly object don't return the closing segment. */
    if (gm->type == Gm_Polygon) 				/* MF027 */
        --nout;

    if (points) {
	ip = &gm->points[first];
	op = points;
	for (i=0;  i < nout;  i++) {
	    op->x = ip->x;
	    op->y = ip->y;
	    ip++, op++;
	}
    }

    return (nout);
}


/* GmGetBoundingBox -- Returns a rect large enough to completely enclose a
 * marker, regardless of its type or orientation.
 */
GmGetBoundingBox (gm, x, y, width, height)
    register Marker gm;
    int *x, *y;
    int *width, *height;
{
    register XRectangle *r = &gm->cur_rect;

    *x = r->x;
    *y = r->y;
    *width = r->width;
    *height = r->height;
}


/* GmStrToType -- Convert a marker type string to a marker type code.
 */
GmStrToType (marker_type)
register char *marker_type;
{
    register int type;

    if (strcmp (marker_type, GmText) == 0)
	type = Gm_Text;
    else if (strcmp (marker_type, GmLine) == 0)
	type = Gm_Line;
    else if (strcmp (marker_type, GmPolyline) == 0)
	type = Gm_Polyline;
    else if (strcmp (marker_type, GmRectangle) == 0)
	type = Gm_Rectangle;
    else if (strcmp (marker_type, GmBox) == 0)
	type = Gm_Box;
    else if (strcmp (marker_type, GmCircle) == 0)
	type = Gm_Circle;
    else if (strcmp (marker_type, GmEllipse) == 0)
	type = Gm_Ellipse;
    else if (strcmp (marker_type, GmPolygon) == 0)
	type = Gm_Polygon;
    else
	type = 0;

    return (type);
}


/* GmStrToEvent -- Convert a marker event type string to a marker event code.
 */
GmStrToEvent (event_type)
register char *event_type;
{
    register int type;

    if (strcmp (event_type, "notify") == 0)
	type = GmEvNotify;
    else if (strcmp (event_type, "moveResize") == 0)
	type = GmEvMoveResize;
    else if (strcmp (event_type, "modify") == 0)
	type = GmEvModify;
    else if (strcmp (event_type, "redraw") == 0)
	type = GmEvRedraw;
    else if (strcmp (event_type, "destroy") == 0)
	type = GmEvDestroy ;
    else if (strcmp (event_type, "input") == 0)
	type = GmEvInput;
    else if (strcmp (event_type, "focusIn") == 0)
	type = GmEvFocusIn;
    else if (strcmp (event_type, "focusOut") == 0)
	type = GmEvFocusOut;
    else if (strcmp (event_type, "constraint") == 0)
	type = GmEvConstraint;
    else
	type = 0;

    return (type);
}


/* GmStrToFunction -- Convert a drawing function string to the corresponding
 * XLIB function code.
 */
GmStrToFunction (function)
register char *function;
{
    register int code;

    if (strcmp (function, "clear") == 0)
	code = GXclear;
    else if (strcmp (function, "and") == 0)
	code = GXand;
    else if (strcmp (function, "andReverse") == 0)
	code = GXandReverse;
    else if (strcmp (function, "copy") == 0)
	code = GXcopy;
    else if (strcmp (function, "andInverted") == 0)
	code = GXandInverted;
    else if (strcmp (function, "noop") == 0)
	code = GXnoop;
    else if (strcmp (function, "xor") == 0)
	code = GXxor;
    else if (strcmp (function, "or") == 0)
	code = GXor;
    else if (strcmp (function, "nor") == 0)
	code = GXnor;
    else if (strcmp (function, "equiv") == 0)
	code = GXequiv;
    else if (strcmp (function, "invert") == 0)
	code = GXinvert;
    else if (strcmp (function, "orReverse") == 0)
	code = GXorReverse;
    else if (strcmp (function, "copyInverted") == 0)
	code = GXcopyInverted;
    else if (strcmp (function, "orInverted") == 0)
	code = GXorInverted;
    else if (strcmp (function, "nand") == 0)
	code = GXnand;
    else if (strcmp (function, "set") == 0)
	code = GXset;
    else
	code = -1;

    return (code);
}


/* Internal procedures for above code.
 * ------------------------------------
 */

static int
gm_getint (value, type)
    XtArgVal value;
    char *type;
{
    register int ch;

    switch (gm_gettype (type)) {
    case Gt_Bool:
    case Gt_Int:
	return ((int)value);
    case Gt_DFloatP:
	return (*(double *)value);
    case Gt_String:
	ch = *((char *)value);
	if (ch == 'T' || ch == 't')
	    return (1);
	else if (ch == 'F' || ch == 'f')
	    return (0);
	else
	    return (atoi((char *)value));
    default:
	return (0);
    }
}


static Pixel
gm_getpixel (w, value, type)
    GtermWidget w;
    XtArgVal value;
    char *type;
{
    XrmValue from, to;
    Pixel pixel;
    char *str;

    switch (gm_gettype (type)) {
    case Gt_Int:
	/* Pixel value (colormap index). */
	return ((Pixel)value);

    case Gt_String:
	/* The pixel is expressed either as a pixel number input as a string,
	 * or as a color name.  The latter case requires a type conversion.
	 */
	str = (char *)value;
	if (isdigit(str[0]) && (int)strlen(str) <= 3) {
	    int index = atoi (str);
	    pixel = w->gterm.cmap[index];
	    return (pixel);
	}

	if (w->gterm.useDefaultCM || !w->gterm.haveColormap) {
	    /* Allocate color from default colormap.
	     */
	    from.size = strlen ((char *)value) + 1;
	    from.addr = (char *)value;
	    to.addr = (caddr_t) &pixel;
	    to.size = sizeof(pixel);

	    if (!XtConvertAndStore ((Widget)w, XtRString, &from, XtRPixel, &to))
		pixel = w->gterm.cmap[1];

	} else {
	    /* Allocate closest match from custom colormap.  This is crude,
	     * but for the standard colors this will return an exact match.
	     */
	    int index, min_dist, dist, i;
	    XColor exact, best, *cp;

	    pixel = w->gterm.cmap[1];
	    if (XLookupColor (w->gterm.display,
		    get_colormap(w), str, &exact, &best)) {
		min_dist = 9999;
		index = 1;

		for (i=0;  i < w->gterm.ncolors;  i++) {
		    cp = &w->gterm.color[i];
		    dist = abs((int)exact.red - (int)cp->red) +
			   abs((int)exact.green - (int)cp->green) +
			   abs((int)exact.blue - (int)cp->blue);
		    if (dist == 0) {
			index = i;
			break;
		    } else if (dist < min_dist) {
			index = i;
			min_dist = dist;
		    }
		}

		pixel = w->gterm.color[index].pixel;
	    }
	}
	return (pixel);

    default:
	return (w->gterm.cmap[1]);
    }
}


static int
gm_getfillstyle (w, value, type)
    GtermWidget w;
    XtArgVal value;
    char *type;
{
    switch (gm_gettype (type)) {
    case Gt_String:
	if (strcmp ((char *)value, "FillSolid") == 0)
	    return (FillSolid);
	else if (strcmp ((char *)value, "FillTiled") == 0)
	    return (FillTiled);
	else if (strcmp ((char *)value, "FillStippled") == 0)
	    return (FillStippled);
	else if (strcmp ((char *)value, "FillOpaqueStippled") == 0)
	    return (FillOpaqueStippled);
	break;
    default:
	break;
    }

    return (FillSolid);
}


static double
gm_getfloat (value, type)
    XtArgVal value;
    char *type;
{
    switch (gm_gettype (type)) {
    case Gt_Bool:
    case Gt_Int:
	return ((int)value);
    case Gt_DFloatP:
	return (*(double *)value);
    case Gt_String:
	return (atof((char *)value));
    default:
	return (0);
    }
}


static char *
gm_getstring (value, type)
    XtArgVal value;
    char *type;
{
    if (strcmp (type, XtRString) == 0)
	return ((char *)value);
    else
	return ("");
}


static int
gm_putint (ival, value, type)
    int ival;
    XtArgVal value;
    char *type;
{
    switch (gm_gettype (type)) {
    case Gt_Bool:
    case Gt_Int:
	*(int *)value = ival;
	break;
    case Gt_DFloatP:
	*(double *)value = (double) ival;
	break;
    case Gt_String:
	sprintf ((char *)value, "%d", ival);
	break;
    default:
	return (ERR);
    }
    return (OK);
}


static int
gm_putfloat (fval, value, type)
    double fval;
    XtArgVal value;
    char *type;
{
    switch (gm_gettype (type)) {
    case Gt_Bool:
    case Gt_Int:
	*(int *)value = (int) fval;
	break;
    case Gt_DFloatP:
	*(double *)value = fval;
	break;
    case Gt_String:
	sprintf ((char *)value, "%g", fval);
	break;
    default:
	return (ERR);
    }
    return (OK);
}


static int
gm_gettype (type)
    char *type;
{
    if (strcmp (type, XtRBool) == 0)
	return (Gt_Int);
    else if (strcmp (type, XtRInt) == 0)
	return (Gt_Int);
    else if (strcmp (type, XtRFloat) == 0)
	return (Gt_DFloatP);
    else if (strcmp (type, XtRPointer) == 0)
	return (Gt_Pointer);
    else if (strcmp (type, XtRString) == 0)
	return (Gt_String);
    else
	return (ERR);
}


static int
gm_getattribute (attribute)
    char *attribute;
{
    if (strcmp (attribute, GmType) == 0)
	return (Ga_Type);
    else if (strcmp (attribute, GmActivated) == 0)
	return (Ga_Activated);
    else if (strcmp (attribute, GmVisible) == 0)
	return (Ga_Visible);
    else if (strcmp (attribute, GmSensitive) == 0)
	return (Ga_Sensitive);
    else if (strcmp (attribute, GmAutoRedraw) == 0)
	return (Ga_AutoRedraw);
    else if (strcmp (attribute, GmTranslations) == 0)
	return (Ga_Translations);
    else if (strcmp (attribute, GmX) == 0)
	return (Ga_X);
    else if (strcmp (attribute, GmY) == 0)
	return (Ga_Y);
    else if (strcmp (attribute, GmWidth) == 0)
	return (Ga_Width);
    else if (strcmp (attribute, GmHeight) == 0)
	return (Ga_Height);
    else if (strcmp (attribute, GmRotangle) == 0)
	return (Ga_Rotangle);
    else if (strcmp (attribute, GmHighlightColor) == 0)
	return (Ga_HighlightColor);
    else if (strcmp (attribute, GmLineColor) == 0)
	return (Ga_LineColor);
    else if (strcmp (attribute, GmLineWidth) == 0)
	return (Ga_LineWidth);
    else if (strcmp (attribute, GmLineStyle) == 0)
	return (Ga_LineStyle);
    else if (strcmp (attribute, GmKnotColor) == 0)
	return (Ga_KnotColor);
    else if (strcmp (attribute, GmKnotSize) == 0)
	return (Ga_KnotSize);
    else if (strcmp (attribute, GmFill) == 0)
	return (Ga_Fill);
    else if (strcmp (attribute, GmFillColor) == 0)
	return (Ga_FillColor);
    else if (strcmp (attribute, GmFillBgColor) == 0)
	return (Ga_FillBgColor);
    else if (strcmp (attribute, GmFillPattern) == 0)
	return (Ga_FillPattern);
    else if (strcmp (attribute, GmFillStyle) == 0)
	return (Ga_FillStyle);
    else if (strcmp (attribute, GmTextColor) == 0)
	return (Ga_TextColor);
    else if (strcmp (attribute, GmTextBgColor) == 0)
	return (Ga_TextBgColor);
    else if (strcmp (attribute, GmTextBorder) == 0)
	return (Ga_TextBorder);
    else if (strcmp (attribute, GmImageText) == 0)
	return (Ga_ImageText);
    else if (strcmp (attribute, GmFont) == 0)
	return (Ga_Font);
    else if (strcmp (attribute, GmText) == 0)
	return (Ga_Text);
    else if (strcmp (attribute, GmRotIndicator) == 0)		/* MF020 */
	return (Ga_RotIndicator);
    else
	return (ERR);
}

static void
gm_linkafter (gm, prev)
    register Marker gm;
    register Marker prev;
{
    register GtermWidget w = gm->w;

    gm->prev = prev;
    gm->next = prev ? prev->next : NULL;
    if (prev)
	prev->next = gm;

    if (!w->gterm.gm_tail || prev == w->gterm.gm_tail)
	w->gterm.gm_tail = gm;
    if (!w->gterm.gm_head)
	w->gterm.gm_head = gm;

    w->gterm.preserve_screen++;
}


static void
gm_unlink (gm)
    register Marker gm;
{
    register GtermWidget w = gm->w;

    if (gm->prev)
	gm->prev->next = gm->next;
    if (gm->next)
	gm->next->prev = gm->prev;
    if (w->gterm.gm_head == gm)
	w->gterm.gm_head = gm->next;
    if (w->gterm.gm_tail == gm)
	w->gterm.gm_tail = gm->prev;

    gm->prev = gm->next = NULL;
    if (!w->gterm.gm_head)
	w->gterm.preserve_screen = 0;
}


/* gm_do_callbacks -- Call any client callbacks registered for the given
 * event type.
 */
static int
gm_do_callbacks (gm, events, event, params, nparams)
    Marker gm;
    register int events;
    XEvent *event;
    String *params;
    Cardinal nparams;
{
    register int n;
    register struct markerCallback *cb;
    struct markerCallback callback[GM_MAXCALLBACKS];
    int ncallbacks, status;

    /* Copy the callbacks list into local memory to ensure that it is not
     * changed by executing a callback.
     */
    ncallbacks = gm->ncallbacks;
    memmove ((char *)callback, (char *)gm->callback,
	sizeof (struct markerCallback) * GM_MAXCALLBACKS);

    for (n = ncallbacks, cb = callback;  --n >= 0;  cb++)
	if (cb->events & events) {
	    status = cb->func (cb->client_data,
		gm, events, event, params, nparams);
	    if (status)
		return (status);
	}

    return (0);
}


/* gm_constraint -- Handle the constraint callback.  This is a client
 * callback called when a marker position or size attribute is changed
 * interactively at runtime.  The purpose of the callback is to allow the
 * client to apply any constraints, e.g. to keep the marker within a
 * certain area or range of sizes, to forbid rotation, and so on.
 */
static int
gm_constraint (gm, new_gm, what)
    register Marker gm, new_gm;
    register int what;
{
    register char *ip, *op;
    char argbuf[2048];
    char *argv[30];
    int argc = 0;

    /* Return immediately if there are no constraint callbacks. */
    if (!gm->constraints)
	return;

    /* Prepare an argument list listing the marker attributes being changed
     * and their old and new values.  Each attribute is passed as three
     * arg strings: name old-value new-value.  Each argument string is
     * allocated a fixed amount of space of SZ_NUMBER characters.
     */
    op = argbuf;
    if (what & Gb_X) {
	strcpy (argv[argc++]=op, "x");			   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", gm->x);		   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", new_gm->x);	   op += SZ_NUMBER;
    }
    if (what & Gb_Y) {
	strcpy (argv[argc++]=op, "y");			   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", gm->y);		   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", new_gm->y);	   op += SZ_NUMBER;
    }
    if (what & Gb_Width) {
	strcpy (argv[argc++]=op, "width");		   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", gm->width);	   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", new_gm->width);	   op += SZ_NUMBER;
    }
    if (what & Gb_Height) {
	strcpy (argv[argc++]=op, "height");		   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", gm->height);	   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", new_gm->height);   op += SZ_NUMBER;
    }
    if (what & Gb_Rotangle) {					/* MF022 */
	double	rot = (gm->rotangle * ((double)180.0 / M_PI));
	double	new_rot = (new_gm->rotangle * ((double)180.0 / M_PI));
	strcpy (argv[argc++]=op, "rotangle");		   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%g", rot);	   	   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%g", new_rot); 	   op += SZ_NUMBER;
    }

    /* Call any constraint callbacks.  The argv value strings are modified
     * in place.
     */
    gm_do_callbacks (gm, GmEvConstraint, NULL, argv, argc);

    /* Copy the possibly edited values back into the new_gm struct.
     */
    ip = argbuf + SZ_NUMBER * 2;
    if (what & Gb_X) {
	new_gm->x = atoi (ip);			ip += SZ_NUMBER*3;
    }
    if (what & Gb_Y) {
	new_gm->y = atoi (ip);			ip += SZ_NUMBER*3;
    }
    if (what & Gb_Width) {
	new_gm->width = atoi (ip);		ip += SZ_NUMBER*3;
    }
    if (what & Gb_Height) {
	new_gm->height = atoi (ip);		ip += SZ_NUMBER*3;
    }
    if (what & Gb_Rotangle) {
	new_gm->rotangle = atof (ip);		ip += SZ_NUMBER*3;

	/* Convert back to radians.... */
	new_gm->rotangle *= (M_PI / (double)180.0);		/* MF022 */
    }
}


static void
gm_erase (gm)
    register Marker gm;
{
    register GtermWidget w = gm->w;
    register XRectangle *r = &gm->old_rect;

    if (!XtIsRealized ((Widget)w))
	return;

    /* Any clipping to the marker border is set outside this routine. */
    if ((gm->flags & Gm_Visible) && !NullRect(r))
	XCopyArea (w->gterm.display, w->gterm.pixmap, w->gterm.window,
	    w->gterm.exposeGC, r->x, r->y, r->width, r->height, r->x, r->y);
}


/* Marker actions.
 * -------------------------
 */


/* M_create -- Create a marker.
 */
static void
M_create (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;
    int interactive, type;
    gmSelection what;

    savepos (w, event);

    /* If the marker has already been created in interactive mode the event
     * merely initializes the marker, otherwise we create and initialize a
     * new marker.
     */
    if (!(gm = w->gterm.gm_create)) {
	type = w->gterm.gm_defaultType;
	if (*nparams == 1) {
	    if (!(type = GmStrToType (params[0])))
		type = w->gterm.gm_defaultType;
	}
	gm = GmCreate (w, type, interactive=True);
    }

    gm->x = ev->x;
    gm->y = ev->y;
    gm->flags |= Gm_Activated;
    w->gterm.gm_create = NULL;

    what.type = (gm->type == Gm_Polygon) ? Ge_Marker : Ge_Point;
    what.vertex = 0;
    gm_focusin (w, gm, &what);
}


/* M_destroy -- Destroy a marker.
 */
static void
M_destroy (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    GmDestroy (gm);
}


/* M_destroyNull -- Destroy a marker if it is null sized.
 */
static void
M_destroyNull (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    if (gm && gm->width <= 2 && gm->height <= 2)
	GmDestroy (gm);
}


/* M_set -- Set a marker attribute.
 */
static void
M_set (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;
    int i;

    savepos (w, event);

    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    for (i=0;  i < *nparams;  i += 2)
	GmSetAttribute (gm,
	    params[i], (XtArgVal)params[i+1], XtRString); 	/* MF010 */
}


/* M_raise -- Raise a marker to the top of the display list.
 */
static void
M_raise (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;
    int i;

    savepos (w, event);

    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    GmRaise (gm, NULL);
}


/* M_lower -- Lower a marker to the bottom of the display list.
 */
static void
M_lower (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;
    int i;

    savepos (w, event);

    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    GmLower (gm, NULL);
}


/* M_notify -- Notify any clients that have registered callbacks for the
 * specified type of events.
 */
static void
M_notify (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;
    int events, i;

    savepos (w, event);

    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    for (i=0, events=0;  i < *nparams;  i++)
	if (strcmp (params[i], "notify") == 0)
	    events |= GmEvNotify;
	else if (strcmp (params[i], "moveResize") == 0)
	    events |= GmEvMoveResize;
	else if (strcmp (params[i], "modify") == 0)
	    events |= GmEvModify;
	else if (strcmp (params[i], "redraw") == 0)
	    events |= GmEvRedraw;
	else if (strcmp (params[i], "destroy") == 0)
	    events |= GmEvDestroy;
	else if (strcmp (params[i], "input") == 0)
	    events |= GmEvInput;
	else if (strcmp (params[i], "focusIn") == 0)
	    events |= GmEvFocusIn;
	else if (strcmp (params[i], "focusOut") == 0)
	    events |= GmEvFocusOut;

    GmNotify (gm, events, event, params + 1, *nparams - 1);
}


/* M_input -- Notify any clients that have registered a input callback
 * that a input event has occurred.
 */
static void
M_input (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XKeyEvent *ev = (XKeyEvent *) event;
    register Marker gm;

    savepos (w, event);

    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    GmNotify (gm, GmEvInput, event, params, *nparams);
}


/* M_markpos -- Mark the current position of the marker, e.g., so that it
 * can later be erased.
 */
static void
M_markpos (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    GmMarkpos (gm);
}


/* M_markposAdd -- Execute either the markpos or add action, depending upon
 * the pointer location.  If the pointer is over an active marker at a
 * location where the add action can be executed this is done, otherwise the
 * markpos action is executed.
 */
static void
M_markposAdd (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    GmSelection what = &w->gterm.gm_selection;
    register Marker gm;

    savepos (w, event);

    /* Get marker and type of active portion of marker. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, what)))
	    return;

    /* Always do a markpos whether we Add or not. */
    GmMarkpos (gm);

    /* Add a point if possible for the given marker and pointer location. */
    if (what->type == Ge_Edge &&
	    (gm->type == Gm_Polyline || gm->type == Gm_Polygon))
	GmAddPt (gm, ev->x, ev->y);
}


/* M_redraw -- Redraw a marker.
 */
static void
M_redraw (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;
    int erase;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    /* This redraw undoes the final Xor draw. */
    GmRedraw (gm, GXxor, erase=False);

    /* Redraw the full marker. */
    GmRedraw (gm, GXcopy, erase=True);
}


/* M_addPt -- Add a point.
 */
static void
M_addPt (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    GmSelection what = &w->gterm.gm_selection;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, what)))
	    return;

    /* Add a point if possible for the given marker and pointer location. */
    if (what->type == Ge_Edge &&
	    (gm->type == Gm_Polyline || gm->type == Gm_Polygon))
	GmAddPt (gm, ev->x, ev->y);
}


/* M_deletePt -- Delete a point.
 */
static void
M_deletePt (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    GmSelection what = &w->gterm.gm_selection;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, what)))
	    return;

    if (what->type == Ge_Point)
	GmDeletePt (gm, ev->x, ev->y);
}


/* M_movePt -- Move a point.
 */
static void
M_movePt (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    GmSelection what = &w->gterm.gm_selection;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, what)))
	    return;

    /* Move a point (vertex) if supported by marker type. */
    if (what->type == Ge_Point &&
	    (gm->type == Gm_Polyline || gm->type == Gm_Polygon))
	GmMovePt (gm, ev->x, ev->y);
}


/* M_deleteDestroy -- Delete a point or destroy a marker, depending upon the
 * pointer position.
 */
static void
M_deleteDestroy (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    GmSelection what = &w->gterm.gm_selection;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, what)))
	    return;

    switch (what->type) {
    case Ge_Point:
	GmDeletePt (gm, ev->x, ev->y);
	break;
    case Ge_Marker:
	GmDestroy (gm);
	break;
    }
}


/* M_move -- Move a marker.
 */
static void
M_move (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    if (ev->time - gm->time > GM_UPDATE) {
	GmMove (gm, ev->x, ev->y);
	XFlush (w->gterm.display);
	gm->time = ev->time;
    }
}


/* M_resize -- Resize a marker.
 */
static void
M_resize (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    if (ev->time - gm->time > GM_UPDATE) {
	GmResize (gm, ev->x, ev->y);
	XFlush (w->gterm.display);
	gm->time = ev->time;
    }
}


/* M_moveResize -- Move a point or marker, or resize a marker, depending
 * upon the pointer position.
 */
static void
M_moveResize (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    GmSelection what = &w->gterm.gm_selection;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, what)))
	    return;

    if (ev->time - gm->time > GM_UPDATE) {
	switch (what->type) {
	case Ge_Marker:
	    GmMove (gm, ev->x, ev->y);
	    break;
	case Ge_Point:
	    if (gm->type == Gm_Polygon || gm->type == Gm_Polyline)
		GmMovePt (gm, ev->x, ev->y);
	    else
		goto resize;
	    break;
	case Ge_Edge:
resize:	    GmResize (gm, ev->x, ev->y);
	    break;
	}
	XFlush (w->gterm.display);
	gm->time = ev->time;
    }
}


/* M_rotate -- Rotate a marker.
 */
static void
M_rotate (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    if (ev->time - gm->time > GM_UPDATE) {
	GmRotate (gm, ev->x, ev->y);
	XFlush (w->gterm.display);
	gm->time = ev->time;
    }
}


/* M_rotateResize -- Rotate or resize a marker.
 */
static void
M_rotateResize (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    GmSelection what = &w->gterm.gm_selection;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    if (ev->time - gm->time > GM_UPDATE) {
	switch (what->type) {
	case Ge_Point:
	    GmRotate (gm, ev->x, ev->y);
	    break;
	case Ge_Edge:
	    if (gm->flags & Gm_Smooth)
		GmRotate (gm, ev->x, ev->y);
	    else
		GmResize (gm, ev->x, ev->y);
	    break;
	default:
	    GmResize (gm, ev->x, ev->y);
	    break;
	}

	XFlush (w->gterm.display);
	gm->time = ev->time;
    }
}


/*
 * Marker class code.
 * ---------------------
 * Each marker class implements a subset of the following procedures.  The
 * first set of procedures are required.  The second set are optional and
 * may be set to NULL in the marker descriptor if not implemented by the
 * marker class.
 *
 *	       gm_xxxx_init (gm, interactive)
 *    bool = gm_xxxx_select (gm, x, y, &what)
 *	    gm_xxxx_markpos (gm)
 *	     gm_xxxx_redraw (gm, func)
 *	     gm_xxxx_update (gm)
 *
 *	      gm_xxxx_addPt (gm, x, y)
 *	   gm_xxxx_deletePt (gm, x, y)
 *	     gm_xxxx_movePt (gm, x, y)
 *	       gm_xxxx_move (gm, x, y)
 *	     gm_xxxx_resize (gm, x, y)
 *	     gm_xxxx_rotate (gm, x, y)
 *
 * where xxxx is the 4 character marker class name.
 */

/* Marker class TEXT.
 */
static int gm_text_select();
static void gm_text_move(), gm_text_resize();
static void gm_text_markpos(), gm_text_redraw();
static void gm_text_update(), gm_text_updatePolygon();

static void
gm_text_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    register GtermWidget w = gm->w;

    gm->type = Gm_Text;
    if (!(gm->flags & Gm_Activated)) {
	gm->highlightColor = w->gterm.gm_highlightColor;
	gm->lineColor = w->gterm.gm_TextLineColor;
	gm->lineWidth = w->gterm.gm_lineWidth;
	gm->lineStyle = w->gterm.gm_lineStyle;
	gm->textColor = w->gterm.gm_TextColor;
	gm->textBgColor = w->gterm.gm_TextBgColor;
	gm->textBorder = w->gterm.gm_TextBorder;
	gm->fill = w->gterm.gm_fill;
	gm->fillStyle = w->gterm.gm_fillStyle;
	gm->fillColor = w->gterm.gm_fillColor;
	gm->fillBgColor = w->gterm.gm_fillBgColor;
	gm->fillStyle = w->gterm.gm_fillStyle;
	gm->font = w->gterm.gm_TextFont;
	gm->imageText = False;
    }

    if (gm->points && gm->npoints > GM_MAXVERTICES)
	XtFree ((char *)gm->points);
    gm->npoints = 4 + 1;
    gm->points = gm->point_data;

    gm->select   = gm_text_select;
    gm->markpos  = gm_text_markpos;
    gm->redraw   = gm_text_redraw;
    gm->update   = gm_text_update;
    gm->addPt    = NULL;
    gm->deletePt = NULL;
    gm->movePt   = NULL;
    gm->move     = gm_text_move;
    gm->resize   = gm_text_resize;
    gm->rotate   = NULL;

    if (w->gterm.gm_TextString) {
	if (gm->text)
	    XtFree (gm->text);
	gm->text = (char *) XtMalloc (strlen(w->gterm.gm_TextString)+1);
	strcpy (gm->text, w->gterm.gm_TextString);
    } else
	gm->text = NULL;
}

static int
gm_text_select (gm, x, y, what)
    register Marker gm;
    int x, y;
    GmSelection what;
{
    if (gm_select (gm, x, y, what)) {
	if (what && what->type == Ge_Edge)
	    what->type = Ge_Marker;
	return (1);
    } else
	return (0);
}


static void
gm_text_markpos (gm)
    register Marker gm;
{
    gm_markpos (gm);
}


static void
gm_text_redraw (gm, function)
    register Marker gm;
    int function;
{
    register GtermWidget w = gm->w;
    int flags = (Gm_Activated|Gm_Visible);
    int char_width, char_height, xsize, ysize;
    int breakline, l_pix, r_pix, maxch, x, y;
    XFontStruct *fp = gm->font;
    char *ip, *op, *otop;
    char *l_ip, *l_op;
    char line[1024];

    if (!((gm->flags & flags) == flags))
	return;
    if (gm->width <= 0 || gm->height <= 0)
	return;

    /* In rubber-band mode just draw the outline of the text region. */
    if (function == GXxor) {
	int save_lineWidth = gm->lineWidth;

	if (gm->lineWidth <= 0)
	    gm->lineWidth = 1;
	gm_redraw (gm, function);
	gm->lineWidth = save_lineWidth;
	return;
    }

    /* General case.  First draw the text box. */
    gm_redraw (gm, function);

    /* Now draw the text. */
    if (!gm->text)
	return;

    char_width  = fp->max_bounds.width;
    char_height = fp->max_bounds.ascent + fp->max_bounds.descent;
    xsize = gm->width;
    ysize = gm->height;

    l_pix = (gm->lineWidth + 1) / 2 + gm->textBorder - 1;
    r_pix = (gm->lineWidth + 1) / 2 + gm->textBorder - 1 -
	(fp->max_bounds.width - fp->max_bounds.rbearing);
    if ((maxch = (xsize - l_pix - r_pix) / char_width) < 1)
	return;

    x = gm->x + (gm->lineWidth + 1) / 2 + gm->textBorder + 1;
    y = gm->y + (gm->lineWidth + 1) / 2 + gm->textBorder +
	fp->max_bounds.ascent;

    XSetForeground (w->gterm.display, w->gterm.gm_drawGC, gm->textColor);
    XSetBackground (w->gterm.display, w->gterm.gm_drawGC, gm->textBgColor);
    XSetFont (w->gterm.display, w->gterm.gm_drawGC, fp->fid);

    /* Fill lines in a multiline text box.
     */
    l_ip = l_op = NULL;
    otop = line + maxch;
    breakline = 0;

    for (ip = gm->text, op=line;  *ip || op > line;  ) {
	if (! *ip) {
	    breakline++;
	} else if (*ip == ' ' || *ip == '\t') {
	    l_ip = ip;
	    l_op = op;
	    *op++ = ' ';
	    ip++;
	} else if (*ip == '\n') {
	    ip++;
	    breakline++;
	} else
	    *op++ = *ip++;

	if (breakline || op > otop) {
	    if (op > otop) {
		if (l_ip && l_op) {
		    ip = l_ip + 1;
		    *l_op = '\0';
		} else {
		    while (op > otop) {
			if (ip > gm->text && isprint (*(ip-1)))
			    --ip;
			--op;
		    }
		    *op = '\0';
		}
	    } else
		*op = '\0';

	    if (gm->imageText) {
		while (op < otop)
		    *op++ = ' ';
		*op = '\0';
		XDrawImageString (w->gterm.display, w->gterm.window,
		    w->gterm.gm_drawGC, x, y, line, strlen(line));
	    } else {
		XDrawString (w->gterm.display, w->gterm.window,
		    w->gterm.gm_drawGC, x, y, line, strlen(line));
	    }

	    y += char_height;
	    if (breakline)
		y += gm->textBorder;
	    if (y + fp->max_bounds.descent > gm->y + ysize)
		break;

	    op = line;
	    l_ip = l_op = NULL;
	    breakline = 0;
	}
    }
}


static void
gm_text_update (gm)
    register Marker gm;
{
    register GtermWidget w = gm->w;
    int flags = (Gm_Activated|Gm_Visible);

    if (!((gm->flags & flags) == flags))
	return;
    if (gm->width <= 0 || gm->height <= 0)
	return;

    if (gm->flags & Gm_Modified) {
	gm_text_updatePolygon (gm);
	gm_setCurRect (gm);
	gm->flags &= ~Gm_Modified;
    }
}

static void
gm_text_move (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    new_gm.x = max (0, x - gm->width / 2);
    new_gm.y = max (0, y - gm->height / 2);
    gm_constraint (gm, &new_gm, Gb_X|Gb_Y);	/* corner */

    gm->x = new_gm.x;
    gm->y = new_gm.y;
    gm_text_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_text_resize (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    new_gm.width = abs (x - gm->x);
    new_gm.height = abs (y - gm->y);
    gm_constraint (gm, &new_gm, Gb_Width|Gb_Height);

    gm->width = new_gm.width;
    gm->height = new_gm.height;
    gm_text_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_text_updatePolygon (gm)
    register Marker gm;
{
    register XPoint *p = gm->points;
    int xsize = gm->width;
    int ysize = gm->height;

    p[0].x = gm->x;		p[0].y = gm->y;
    p[1].x = gm->x;		p[1].y = gm->y + ysize;
    p[2].x = gm->x + xsize;	p[2].y = gm->y + ysize;
    p[3].x = gm->x + xsize;	p[3].y = gm->y;
    p[4].x = gm->x;		p[4].y = gm->y;
}


/* Marker class LINE.
 */
static void
gm_line_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    gm->type = Gm_Line;
    /* stub out for now */
}


/* Marker class POLYLINE.
 */
static void
gm_plin_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    gm->type = Gm_Polyline;
    /* stub out for now */
}


/* Marker class RECTANGLE.
 */
static int gm_rect_select();
static void gm_rect_move(), gm_rect_resize(), gm_rect_rotate();
static void gm_rect_update(), gm_rect_updatePolygon();

static void
gm_rect_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    register GtermWidget w = gm->w;

    gm->type = Gm_Rectangle;
    if (!(gm->flags & Gm_Activated)) {
	gm->lineWidth = w->gterm.gm_lineWidth;
	gm->lineStyle = w->gterm.gm_lineStyle;
	gm->highlightColor = w->gterm.gm_highlightColor;
	gm->lineColor = w->gterm.gm_RectLineColor;
	gm->knotColor = w->gterm.gm_RectKnotColor;
	gm->knotSize  = w->gterm.gm_RectKnotSize;
	gm->fill = w->gterm.gm_fill;
	gm->fillStyle = w->gterm.gm_fillStyle;
	gm->fillColor = w->gterm.gm_fillColor;
	gm->fillBgColor = w->gterm.gm_fillBgColor;
    }

    if (gm->points && gm->npoints > GM_MAXVERTICES)
	XtFree ((char *)gm->points);
    gm->points = gm->point_data;
    gm->npoints = 4 + 1;

    gm->select   = gm_rect_select;
    gm->markpos  = gm_markpos;
    gm->update   = gm_rect_update;
    gm->redraw   = gm_redraw;
    gm->addPt    = NULL;
    gm->deletePt = NULL;
    gm->movePt   = NULL;
    gm->move     = gm_rect_move;
    gm->resize   = gm_rect_resize;
    gm->rotate   = gm_rect_rotate;
}

static void
gm_rect_update (gm)
    register Marker gm;
{
    if (gm->flags & Gm_Modified) {
	gm_rect_updatePolygon (gm);
	gm_setCurRect (gm);
	gm->flags &= ~Gm_Modified;
    }
}

static int
gm_rect_select (gm, x, y, what)
    register Marker gm;
    int x, y;
    GmSelection what;
{
    if (gm_select (gm, x, y, what)) {
	if (what && what->type == Ge_Edge)
	    what->type = Ge_Marker;
	return (1);
    } else
	return (0);
}

static void
gm_rect_move (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    new_gm.x = x;  new_gm.y = y;
    gm_constraint (gm, &new_gm, Gb_X|Gb_Y);

    gm->x = new_gm.x;  gm->y = new_gm.y;
    gm_rect_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_rect_resize (gm, x, y)
    register Marker gm;
    int x, y;
{
/*  double cos_rotangle = cos (-(gm->rotangle));
    double sin_rotangle = sin (-(gm->rotangle)); */
    double cos_rotangle = cos ((gm->rotangle));			/* MF019 */
    double sin_rotangle = sin ((gm->rotangle));			/* MF019 */
    struct marker new_gm;
    int rx, ry;
    int ox = x, oy = y;

    /* Translate to the marker reference frame. */
    x = x - gm->x;
    y = y - gm->y;

    /* Rotate back to the unrotated reference frame. */
    rx = x * cos_rotangle - y * sin_rotangle;
    ry = x * sin_rotangle + y * cos_rotangle;

    /* Compute new width and height. */
    if (rx < 0)
	new_gm.x = gm->x - (-rx - gm->width) / 2;
    else
	new_gm.x = gm->x + (rx - gm->width) / 2;

    if (ry < 0)
	new_gm.y = gm->y - (-ry - gm->height) / 2;
    else
	new_gm.y = gm->y + (ry - gm->height) / 2;

    new_gm.width = gm->width + (abs(rx) - gm->width) / 2;
    new_gm.height = gm->height + (abs(ry) - gm->height) / 2;

    gm_constraint (gm, &new_gm, Gb_X|Gb_Y|Gb_Width|Gb_Height);
    gm->x = new_gm.x;
    gm->y = new_gm.y;
    gm->width = new_gm.width;
    gm->height = new_gm.height;

    gm_rect_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_rect_rotate (gm, x, y)
    register Marker gm;
    int x, y;
{
    double alpha, theta;
    struct marker new_gm;

    if (x == gm->x && y == gm->y)
	gm->rotangle = 0;
    else {

    /*  V1.1  These eqns have the effect of allowing a marker to be grabbed by
     *  any corner but doing so resets the rotation angle the first time the
     *  marker is rotated.

	theta = atan2 ((double)(y - gm->y), (double)(x - gm->x));
	alpha = atan2 ((double)gm->height, (double)gm->width);
 	new_gm.rotangle = gm_niceAngle (theta + alpha);
     */

	theta = atan2 ((double)(gm->y - y), (double)(x - gm->x)); /* MF019 */
	new_gm.rotangle = gm_niceAngle (theta);			  /* MF019 */
	gm_constraint (gm, &new_gm, Gb_Rotangle);
	gm_rotate_indicator (gm, GXxor);			  /* MF020 */
	gm->rotangle =  new_gm.rotangle;
	gm_rotate_indicator (gm, GXxor);			  /* MF020 */
	gm_rect_updatePolygon (gm);
	gm_setCurRect (gm);
    }
}

static void
gm_rect_updatePolygon (gm)
    Marker gm;
{
    register x, y;
    register XPoint *p = gm->points;
    double cos_rotangle, sin_rotangle;

/*  cos_rotangle = cos (gm->rotangle);
    sin_rotangle = sin (gm->rotangle);*/
    cos_rotangle = cos (-gm->rotangle);				/* MF019 */
    sin_rotangle = sin (-gm->rotangle);				/* MF019 */
    x = gm->width;
    y = gm->height;

    p[0].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[0].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    x = -x;
    p[1].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[1].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    y = -y;
    p[2].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[2].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    x = -x;
    p[3].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[3].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    p[4] = p[0];
}


/* Marker class BOX.  A box marker is like a rectangle except that it is
 * described and resized by the center and radius (width/height), like
 * the other "centered" marker types (circle, ellipse, etc.).
 */
static int gm_boxx_select();
static void gm_boxx_move(), gm_boxx_resize(), gm_boxx_rotate();
static void gm_boxx_update(), gm_boxx_updatePolygon();

static void
gm_boxx_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    register GtermWidget w = gm->w;

    gm->type = Gm_Box;
    if (!(gm->flags & Gm_Activated)) {
	gm->lineWidth = w->gterm.gm_lineWidth;
	gm->lineStyle = w->gterm.gm_lineStyle;
	gm->highlightColor = w->gterm.gm_highlightColor;
	gm->lineColor = w->gterm.gm_RectLineColor;
	gm->knotColor = w->gterm.gm_RectKnotColor;
	gm->knotSize  = w->gterm.gm_RectKnotSize;
	gm->fill = w->gterm.gm_fill;
	gm->fillStyle = w->gterm.gm_fillStyle;
	gm->fillColor = w->gterm.gm_fillColor;
	gm->fillBgColor = w->gterm.gm_fillBgColor;
    }

    if (gm->points && gm->npoints > GM_MAXVERTICES)
	XtFree ((char *)gm->points);
    gm->points = gm->point_data;
    gm->npoints = 4 + 1;

    gm->select   = gm_boxx_select;
    gm->markpos  = gm_markpos;
    gm->update   = gm_boxx_update;
    gm->redraw   = gm_redraw;
    gm->addPt    = NULL;
    gm->deletePt = NULL;
    gm->movePt   = NULL;
    gm->move     = gm_boxx_move;
    gm->resize   = gm_boxx_resize;
    gm->rotate   = gm_boxx_rotate;
}

static void
gm_boxx_update (gm)
    register Marker gm;
{
    if (gm->flags & Gm_Modified) {
	gm_boxx_updatePolygon (gm);
	gm_setCurRect (gm);
	gm->flags &= ~Gm_Modified;
    }
}

static int
gm_boxx_select (gm, x, y, what)
    register Marker gm;
    int x, y;
    GmSelection what;
{
    if (gm_select (gm, x, y, what)) {
	if (what && what->type == Ge_Edge)
	    what->type = Ge_Marker;
	return (1);
    } else
	return (0);
}

static void
gm_boxx_move (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    new_gm.x = x;  new_gm.y = y;
    gm_constraint (gm, &new_gm, Gb_X|Gb_Y);

    gm->x = new_gm.x;  gm->y = new_gm.y;
    gm_boxx_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_boxx_resize (gm, x, y)
    register Marker gm;
    int x, y;
{
/*  double cos_rotangle = cos (-(gm->rotangle));
    double sin_rotangle = sin (-(gm->rotangle)); */
    double cos_rotangle = cos ((gm->rotangle));			/* MF019 */
    double sin_rotangle = sin ((gm->rotangle));			/* MF019 */
    struct marker new_gm;
    int rx, ry;

    /* Translate to the marker reference frame. */
    x = x - gm->x;
    y = y - gm->y;

    /* Rotate back to the unrotated reference frame. */
    rx = x * cos_rotangle - y * sin_rotangle;
    ry = x * sin_rotangle + y * cos_rotangle;

    /* Compute new width and height. */
    new_gm.width = abs(rx);
    new_gm.height = abs(ry);
    gm_constraint (gm, &new_gm, Gb_Width|Gb_Height);
    gm->width  = new_gm.width;
    gm->height = new_gm.height;

    gm_boxx_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_boxx_rotate (gm, x, y)
    register Marker gm;
    int x, y;
{
    double alpha, theta;
    struct marker new_gm;

    if (x == gm->x && y == gm->y)
	gm->rotangle = 0;
    else {
    /* V1.1
	theta = atan2 ((double)(y - gm->y), (double)(x - gm->x));
	alpha = atan2 ((double)gm->height, (double)gm->width);
	new_gm.rotangle = gm_niceAngle (theta + alpha);
     */
	theta = atan2 ((double)(gm->y - y), (double)(x - gm->x)); /* MF019 */
	new_gm.rotangle = gm_niceAngle (theta);			  /* MF019 */
	gm_constraint (gm, &new_gm, Gb_Rotangle);
	gm_rotate_indicator (gm, GXxor);			  /* MF020 */
	gm->rotangle = new_gm.rotangle;
	gm_rotate_indicator (gm, GXxor);			  /* MF020 */
	gm_boxx_updatePolygon (gm);
	gm_setCurRect (gm);
    }
}

static void
gm_boxx_updatePolygon (gm)
    Marker gm;
{
    register x, y;
    register XPoint *p = gm->points;
    double cos_rotangle, sin_rotangle;
	
    double alpha = atan2 ((double)gm->height, (double)gm->width);

/*  cos_rotangle = cos (gm->rotangle);
    sin_rotangle = sin (gm->rotangle); */
    cos_rotangle = cos (-gm->rotangle);				/* MF019 */
    sin_rotangle = sin (-gm->rotangle);				/* MF019 */
    x = gm->width;
    y = gm->height;

    p[0].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[0].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    x = -x;
    p[1].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[1].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    y = -y;
    p[2].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[2].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    x = -x;
    p[3].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[3].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    p[4] = p[0];
}


/* Marker class CIRCLE.
 */
static int gm_circ_select();
static void gm_circ_move(), gm_circ_resize(), gm_circ_rotate();
static void gm_circ_update(), gm_circ_updatePolygon();

static void
gm_circ_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    register GtermWidget w = gm->w;

    gm->type = Gm_Circle;
    if (!(gm->flags & Gm_Activated)) {
	gm->lineWidth = w->gterm.gm_lineWidth;
	gm->lineStyle = w->gterm.gm_lineStyle;
	gm->highlightColor = w->gterm.gm_highlightColor;
	gm->lineColor = w->gterm.gm_CircleLineColor;
	gm->knotColor = w->gterm.gm_CircleKnotColor;
	gm->knotSize  = w->gterm.gm_CircleKnotSize;
	gm->fill = w->gterm.gm_fill;
	gm->fillStyle = w->gterm.gm_fillStyle;
	gm->fillColor = w->gterm.gm_fillColor;
	gm->fillBgColor = w->gterm.gm_fillBgColor;
	gm->flags |= Gm_Smooth;
    }

    gm->width = gm->height = (gm->width + gm->height) / 2.0;

    if (gm->points && gm->npoints > GM_MAXVERTICES)
	XtFree ((char *)gm->points);
    gm->points = gm->point_data;
    gm->npoints = GM_NPTSCIRCLE;				/* MF015 */

    gm->select   = gm_circ_select;
    gm->markpos  = gm_markpos;
    gm->update   = gm_circ_update;
    gm->redraw   = gm_redraw;
    gm->addPt    = NULL;
    gm->deletePt = NULL;
    gm->movePt   = NULL;
    gm->move     = gm_circ_move;
    gm->resize   = gm_circ_resize;
    gm->rotate   = NULL;
}

static void
gm_circ_update (gm)
    register Marker gm;
{
    if (gm->flags & Gm_Modified) {
	gm_circ_updatePolygon (gm);
	gm_setCurRect (gm);
	gm->flags &= ~Gm_Modified;
    }
}

static int
gm_circ_select (gm, x, y, what)
    register Marker gm;
    int x, y;
    GmSelection what;
{
    if (gm_select (gm, x, y, what)) {
	if (what && what->type == Ge_Point)
	    what->type = Ge_Edge;
	return (1);
    } else
	return (0);
}

static void
gm_circ_move (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    new_gm.x = x;  new_gm.y = y;
    gm_constraint (gm, &new_gm, Gb_X|Gb_Y);

    gm->x = new_gm.x;  gm->y = new_gm.y;
    gm_circ_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_circ_resize (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    new_gm.width = new_gm.height =
	sqrt ((double)(SQR(x - gm->x) + SQR(y - gm->y)));
    gm_constraint (gm, &new_gm, Gb_Width|Gb_Height);

    gm->width = gm->height = new_gm.width;
    gm_circ_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_circ_updatePolygon (gm)
    Marker gm;
{
    register XPoint *p = gm->points;
    register int npts, i, j;
    double theta, x, y;

    /*npts = (gm->npoints - 1) / 4;*/
    npts = gm->npoints / 4;					/* MF028 */

    for (i=0;  i < npts;  i++) {
	theta = PI_2 / npts * i;
	x = gm->width  * cos(theta);
	y = gm->height * sin(theta);
	
	j = i;
	p[npts*0+j].x = x + gm->x;
	p[npts*0+j].y = y + gm->y;
	
	x = -x;  j = npts-1 - i;
	p[npts*1+j].x = x + gm->x;
	p[npts*1+j].y = y + gm->y;
	
	y = -y;  j = i;
	p[npts*2+j].x = x + gm->x;
	p[npts*2+j].y = y + gm->y;
	
	x = -x;  j = npts-1 - i;
	p[npts*3+j].x = x + gm->x;
	p[npts*3+j].y = y + gm->y;
    }

    /*p[gm->npoints-1] = p[0];*/				/* MF015 */
}


/* Marker class ELLIPSE.
 */
static int gm_elip_select();
static void gm_elip_move(), gm_elip_resize(), gm_elip_rotate();
static void gm_elip_update(), gm_elip_updatePolygon();

static void
gm_elip_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    register GtermWidget w = gm->w;

    gm->type = Gm_Ellipse;
    if (!(gm->flags & Gm_Activated)) {
	gm->lineWidth = w->gterm.gm_lineWidth;
	gm->lineStyle = w->gterm.gm_lineStyle;
	gm->highlightColor = w->gterm.gm_highlightColor;
	gm->lineColor = w->gterm.gm_EllipseLineColor;
	gm->knotColor = w->gterm.gm_EllipseKnotColor;
	gm->knotSize  = w->gterm.gm_EllipseKnotSize;
	gm->fill = w->gterm.gm_fill;
	gm->fillStyle = w->gterm.gm_fillStyle;
	gm->fillColor = w->gterm.gm_fillColor;
	gm->fillBgColor = w->gterm.gm_fillBgColor;
	gm->flags |= Gm_Smooth;
    }

    if (gm->points && gm->npoints > GM_MAXVERTICES)
	XtFree ((char *)gm->points);
    gm->points = gm->point_data;
/*    gm->npoints = GM_NPTSCIRCLE + 1;*/
    gm->npoints = GM_NPTSCIRCLE;				/* MF015 */

    gm->select   = gm_elip_select;
    gm->markpos  = gm_markpos;
    gm->update   = gm_elip_update;
    gm->redraw   = gm_redraw;
    gm->addPt    = NULL;
    gm->deletePt = NULL;
    gm->movePt   = NULL;
    gm->move     = gm_elip_move;
    gm->resize   = gm_elip_resize;
    gm->rotate   = gm_elip_rotate;
}

static void
gm_elip_update (gm)
    register Marker gm;
{
    if (gm->flags & Gm_Modified) {
	gm_elip_updatePolygon (gm);
	gm_setCurRect (gm);
	gm->flags &= ~Gm_Modified;
    }
}

static int
gm_elip_select (gm, x, y, what)
    register Marker gm;
    int x, y;
    GmSelection what;
{
    if (gm_select (gm, x, y, what)) {
	if (what && what->type == Ge_Point)
	    what->type = Ge_Edge;
	return (1);
    } else
	return (0);
}

static void
gm_elip_move (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    new_gm.x = x;  new_gm.y = y;
    gm_constraint (gm, &new_gm, Gb_X|Gb_Y);

    gm->x = new_gm.x;  gm->y = new_gm.y;
    gm_elip_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_elip_resize (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
/*  double theta = -(gm->rotangle);*/
    double theta = (gm->rotangle);				/* MF019 */
    int rx, ry;

    /* Translate to the marker reference frame. */
    x = x - gm->x;
    y = y - gm->y;

    /* Rotate back to the unrotated reference frame. */
    rx = x * cos(theta) - y * sin(theta);
    ry = x * sin(theta) + y * cos(theta);

    /* Compute new width and height. */
    new_gm.width  = abs(rx);
    new_gm.height = abs(ry);

    gm_constraint (gm, &new_gm, Gb_Width|Gb_Height);
    gm->width  = new_gm.width;
    gm->height = new_gm.height;

    gm_elip_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_elip_rotate (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    double theta;

    if (x == gm->x && y == gm->y)
	gm->rotangle = 0;
    else {
/*	theta = atan2 ((double)(y - gm->y), (double)(x - gm->x));*/
	theta = atan2 ((double)(gm->y - y), (double)(x - gm->x)); /* MF019 */
	new_gm.rotangle = gm_niceAngle (theta);
	gm_constraint (gm, &new_gm, Gb_Rotangle);
	gm_rotate_indicator (gm, GXxor);			  /* MF020 */
	gm->rotangle = new_gm.rotangle;
	gm_rotate_indicator (gm, GXxor);			  /* MF020 */
	gm_elip_updatePolygon (gm);
	gm_setCurRect (gm);
    }
}

static void
gm_elip_updatePolygon (gm)
    Marker gm;
{
    register XPoint *p = gm->points;
    register int npts, i, j;
    double cos_rotangle, sin_rotangle;
    double theta, x, y;

    npts = (gm->npoints - 1) / 4 + 1;				/* MF017 */
/*  cos_rotangle = cos (gm->rotangle);
    sin_rotangle = sin (gm->rotangle); */
    cos_rotangle = cos (-gm->rotangle);				/* MF019 */
    sin_rotangle = sin (-gm->rotangle);				/* MF019 */

    for (i=0;  i < npts;  i++) {
	theta = PI_2 / npts * i;
	x = gm->width  * cos(theta);
	y = gm->height * sin(theta);
	
	j = i;
	p[npts*0+j].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
	p[npts*0+j].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;
	
	x = -x;  j = npts-1 - i;
	p[npts*1+j].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
	p[npts*1+j].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;
	
	y = -y;  j = i;
	p[npts*2+j].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
	p[npts*2+j].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;
	
	x = -x;  j = npts-1 - i;
	p[npts*3+j].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
	p[npts*3+j].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;
    }

    /*p[gm->npoints-1] = p[0];*/				/* MF015 */
}


/* Marker class POLYGON.
 */
static int gm_pgon_select();
static void gm_pgon_addPt(), gm_pgon_deletePt(), gm_pgon_movePt();
static void gm_pgon_move(), gm_pgon_resize(), gm_pgon_rotate();
static void gm_pgon_redraw(), gm_pgon_update(), gm_pgon_updatePolygon();

static void
gm_pgon_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    register GtermWidget w = gm->w;
    register DPoint *p;

    gm->type = Gm_Polygon;
    if (!(gm->flags & Gm_Activated)) {
	gm->lineWidth = w->gterm.gm_lineWidth;
	gm->lineStyle = w->gterm.gm_lineStyle;
	gm->highlightColor = w->gterm.gm_highlightColor;
	gm->lineColor = w->gterm.gm_PgonLineColor;
	gm->knotColor = w->gterm.gm_PgonKnotColor;
	gm->knotSize  = w->gterm.gm_PgonKnotSize;
	gm->fill = w->gterm.gm_fill;
	gm->fillStyle = w->gterm.gm_fillStyle;
	gm->fillColor = w->gterm.gm_fillColor;
	gm->fillBgColor = w->gterm.gm_fillBgColor;

	gm->npoints = gm->pgon_npts = 4 + 1;
	gm->points = gm->point_data;
	if (gm->pgon)
	    XtFree ((char *)gm->pgon);

	/* Start out with a small square polygon. */
	gm->pgon = p = (DPoint *) XtMalloc (5 * sizeof (DPoint));
	gm->x = w->gterm.last_x;
	gm->y = w->gterm.last_y;

	if (p) {
	    p[0].x = -1;  p[0].y = -1;
	    p[1].x = -1;  p[1].y =  1;
	    p[2].x =  1;  p[2].y =  1;
	    p[3].x =  1;  p[3].y = -1;
	    p[4].x = -1;  p[4].y = -1;

	    gm_pgon_updatePolygon (gm);
	    gm_setCurRect (gm);
	}

	if (interactive)
	    gm->flags |= Gm_PgonInit;
    }

    if (gm->points && gm->npoints > GM_MAXVERTICES)
	XtFree ((char *)gm->points);
    gm->points = gm->point_data;

    /* The following gets executed when an existing non-polygon marker is
     * turned into a polygon marker.
     */
    if (gm->pgon && gm->pgon_npts)
	gm->npoints = gm->pgon_npts;
    else {
	gm->npoints = gm->pgon_npts = 4 + 1;

	/* Start out with a small square polygon. */
	gm->pgon = p = (DPoint *) XtMalloc (5 * sizeof (DPoint));

	if (p) {
	    p[0].x = -gm->width;  p[0].y = -gm->height;
	    p[1].x = -gm->width;  p[1].y =  gm->height;
	    p[2].x =  gm->width;  p[2].y =  gm->height;
	    p[3].x =  gm->width;  p[3].y = -gm->height;
	    p[4].x = -gm->width;  p[4].y = -gm->height;

	    gm_pgon_updatePolygon (gm);
	    gm_setCurRect (gm);
	}
    }

    gm->select   = gm_select;
    gm->markpos  = gm_markpos;
    gm->update   = gm_pgon_update;
    gm->redraw   = gm_pgon_redraw;
    gm->addPt    = gm_pgon_addPt;
    gm->deletePt = gm_pgon_deletePt;
    gm->movePt   = gm_pgon_movePt;
    gm->move     = gm_pgon_move;
    gm->resize   = gm_pgon_resize;
    gm->rotate   = gm_pgon_rotate;
}

static void
gm_pgon_redraw (gm, function)
    register Marker gm;
    int function;
{
    /* The PgonInit flag is set when a polygon marker is interactively created
     * to cause any pointer motion event to resize the marker.  The first
     * pointer up causes a redraw which clears the flag.
     */
    if (function != GXxor && gm->width > 1 && gm->height > 1)
	gm->flags &= ~Gm_PgonInit;

    gm_redraw (gm, function);
}

static void
gm_pgon_update (gm)
    register Marker gm;
{
    if (gm->flags & Gm_Modified) {
	gm_pgon_updatePolygon (gm);
	gm_setCurRect (gm);
	gm->flags &= ~Gm_Modified;
    }
}

static void
gm_pgon_addPt (gm, x, y)
    register Marker gm;
    int x, y;
{
    register DPoint *pv;
    register GtermWidget w = gm->w;
/*  double cos_rotangle = cos (-(gm->rotangle));
    double sin_rotangle = sin (-(gm->rotangle)); */
    double cos_rotangle = cos ((gm->rotangle));			/* MF019 */
    double sin_rotangle = sin ((gm->rotangle));			/* MF019 */
    int vertex, nbytes;
    double rx, ry;

    if (gm->flags & Gm_PgonInit) {
	gm_pgon_resize (gm, x, y);
	return;
    }

    /* Translate to the marker reference frame. */
    x = x - gm->x;
    y = y - gm->y;

    /* Rotate back to the unrotated reference frame. */
    rx = x * cos_rotangle - y * sin_rotangle;
    ry = x * sin_rotangle + y * cos_rotangle;

    /* Add the point. */
    vertex = w->gterm.gm_selection.vertex;
    if (vertex < 0 || vertex >= gm->npoints)
	return;
    nbytes = (gm->npoints + 1) * sizeof (DPoint);
    if ((pv = (DPoint *)
	    XtRealloc ((char *)gm->pgon, nbytes)) == (DPoint *)NULL)
	return;

    gm->pgon = pv;
    memmove (&pv[vertex+2], &pv[vertex+1],
	(gm->npoints - vertex - 1) * sizeof(DPoint));
    pv[vertex+1].x = rx;
    pv[vertex+1].y = ry;
    gm->npoints++;

    nbytes = gm->npoints * sizeof (XPoint);
    if (gm->npoints > GM_MAXVERTICES) {
	if (gm->points != gm->point_data)
	    gm->points = (XPoint *) XtRealloc ((char *)gm->points, nbytes);
	else
	    gm->points = (XPoint *) XtMalloc (nbytes);
    } else
	gm->points = gm->point_data;

    gm_pgon_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_pgon_deletePt (gm, x, y)
    register Marker gm;
    int x, y;
{
    register DPoint *pv;
    register GtermWidget w = gm->w;
    int vertex, nbytes;

    if (gm->npoints <= 2)
	return;

    if (gm->flags & Gm_PgonInit) {
	gm_pgon_resize (gm, x, y);
	return;
    }

    /* Delete the point. */
    vertex = w->gterm.gm_selection.vertex;
    if (vertex < 0 || vertex >= gm->npoints)
	return;
    pv = gm->pgon;

    memmove (&pv[vertex], &pv[vertex+1],
	(gm->npoints - vertex - 1) * sizeof(DPoint));
    gm->npoints--;

    nbytes = gm->npoints * sizeof (DPoint);
    if ((pv = (DPoint *)
	    XtRealloc ((char *)gm->pgon, nbytes)) == (DPoint *)NULL)
	return;
    gm->pgon = pv;

    if (gm->npoints <= GM_MAXVERTICES && gm->points != gm->point_data)
	XtFree ((char *)gm->points);
    gm->points = gm->point_data;

    gm_pgon_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_pgon_movePt (gm, x, y)
    register Marker gm;
    int x, y;
{
    register DPoint *p;
    register GtermWidget w = gm->w;
/*  double cos_rotangle = cos (-(gm->rotangle));
    double sin_rotangle = sin (-(gm->rotangle)); */
    double cos_rotangle = cos ((gm->rotangle));			/* MF019 */
    double sin_rotangle = sin ((gm->rotangle));			/* MF019 */
    double rx, ry;
    int vertex;

    if (gm->flags & Gm_PgonInit) {
	gm_pgon_resize (gm, x, y);
	return;
    }

    /* Translate to the marker reference frame. */
    x = x - gm->x;
    y = y - gm->y;

    /* Rotate back to the unrotated reference frame. */
    rx = x * cos_rotangle - y * sin_rotangle;
    ry = x * sin_rotangle + y * cos_rotangle;

    /* Get vertex. */
    vertex = w->gterm.gm_selection.vertex;
    if (vertex < 0 || vertex >= gm->npoints)
	return;
    p = &gm->pgon[vertex];

    /* Edit point. */
    p->x = rx;
    p->y = ry;

    gm_pgon_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_pgon_move (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;

    if (gm->flags & Gm_PgonInit) {
	gm_pgon_resize (gm, x, y);
	return;
    }

    new_gm.x = x;  new_gm.y = y;
    gm_constraint (gm, &new_gm, Gb_X|Gb_Y);
    gm->x = new_gm.x;  gm->y = new_gm.y;
    gm_pgon_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_pgon_resize (gm, x, y)
    Marker gm;
    int x, y;
{
    register DPoint *p, *q;
    GtermWidget w = gm->w;
    double cos_rotangle = cos (-(gm->rotangle));
    double sin_rotangle = sin (-(gm->rotangle));
    double theta, scale, slope, rx, ry, x1, y1, x2, y2, xi;
    int vertex, i;

    /* Translate to the marker reference frame. */
    x = x - gm->x;
    y = y - gm->y;

    /* Rotate back to the unrotated reference frame. */
    rx = x * cos_rotangle - y * sin_rotangle;
    ry = x * sin_rotangle + y * cos_rotangle;

    /* Get first vertex of nearest edge. */
    vertex = w->gterm.gm_selection.vertex;
    if (vertex < 0 || vertex >= gm->npoints)
	return;
    p = &gm->pgon[vertex];
    q = p + 1;

    /* Rotate reference frame so that intercept is at y=0. */
    if (abs(rx) + abs(ry) < 1.0)
	scale = 1.0;
    else {
	theta = atan2 (ry, rx);
	cos_rotangle = cos (-theta);
	sin_rotangle = sin (-theta);

	x1 = p->x * cos_rotangle - p->y * sin_rotangle;
	y1 = p->x * sin_rotangle + p->y * cos_rotangle;
	x2 = q->x * cos_rotangle - q->y * sin_rotangle;
	y2 = q->x * sin_rotangle + q->y * cos_rotangle;

	/* Compute scale factor. */
	if (y1 == y2 || x1 == x2)
	    scale = 1.0;
	else {
	    slope = (y2 - y1) / (x2 - x1);
	    xi = x1 - y1 / slope;
	    scale = sqrt (SQR(rx) + SQR(ry)) / xi;
	}
    }

    /* Rescale the polygon. */
    for (i=0, p=gm->pgon;  i < gm->npoints;  i++, p++) {
	p->x *= scale;
	p->y *= scale;
    }

    gm_pgon_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_pgon_rotate (gm, x, y)
    register Marker gm;
    int x, y;
{
    register DPoint *p;
    register GtermWidget w = gm->w;
    double cos_rotangle = cos (-(gm->rotangle));
    double sin_rotangle = sin (-(gm->rotangle));
    double alpha, beta, rx, ry;
    double theta = atan2 ((double)(gm->y - y), (double)(x - gm->x));/* MF019 */
    struct marker new_gm;
    int vertex;

    if (gm->flags & Gm_PgonInit) {
	gm_pgon_resize (gm, x, y);
	return;
    }

    if (x == gm->x && y == gm->y)
	return;

    /* Translate to the marker reference frame. */
    x = x - gm->x;
    y = y - gm->y;

    /* Rotate back to the unrotated reference frame. */
    rx = x * cos_rotangle - y * sin_rotangle;
    ry = x * sin_rotangle + y * cos_rotangle;
    if (abs(rx) + abs(ry) < 1.0)
	return;

    vertex = w->gterm.gm_selection.vertex;
    if (vertex < 0 || vertex >= gm->npoints)
	return;

    p = &gm->pgon[vertex];
    alpha = atan2 (p->y, p->x);	/* angle btw origin & selected vertex */
    beta  = atan2 (ry, rx);	/* angle btw origin & cursor position */

    new_gm.rotangle = gm_niceAngle (gm->rotangle + (beta - alpha));

    new_gm.rotangle = gm_niceAngle (theta);			/* MF019 */

    gm_constraint (gm, &new_gm, Gb_Rotangle);
    gm_rotate_indicator (gm, GXxor);			  	/* MF020 */
    gm->rotangle = new_gm.rotangle;
    gm_rotate_indicator (gm, GXxor);			  	/* MF020 */

    gm_pgon_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_pgon_updatePolygon (gm)
    Marker gm;
{
    register npts, i;
    register DPoint *ip = gm->pgon;
    register XPoint *op = gm->points;
    double cos_rotangle, sin_rotangle;
    int width, height, xp, xn, yp, yn;

    npts = gm->npoints;
/*  cos_rotangle = cos (gm->rotangle);
    sin_rotangle = sin (gm->rotangle); */
    cos_rotangle = cos (-gm->rotangle);				/* MF019 */
    sin_rotangle = sin (-gm->rotangle);				/* MF019 */
    xp = xn = yp = yn = 0;

    for (i=0;  i < npts;  i++, ip++, op++) {
	/* Compute the rotated point. */
	op->x = ip->x * cos_rotangle - ip->y * sin_rotangle + gm->x + 0.5;
	op->y = ip->x * sin_rotangle + ip->y * cos_rotangle + gm->y + 0.5;

	/* Compute a width/height estimate for the polygon.
	 */
	if (ip->x > xp)
	    xp = ip->x;
	else if (ip->x < xn)
	    xn = ip->x;

	if (ip->y > yp)
	    yp = ip->y;
	else if (ip->y < yn)
	    yn = ip->y;

	gm->width  = (xp + -xn) / 2;
	gm->height = (yp + -yn) / 2;
    }

    gm->points[npts-1] = gm->points[0];
    gm->pgon_npts = gm->npoints;
}


/* Internal procedures for above code.
 * -----------------------------------
 */

/* gm_select -- Determine if a point is within or near a marker, and if so,
 * determine whether the point selects a vertex, edge, or the entire marker.
 */
static int
gm_select (gm, x, y, what)
    Marker gm;
    register int x, y;
    GmSelection what;
{
    register XPoint *p, *ptop;
    GtermWidget w = gm->w;
    int v_dist = w->gterm.gm_nearVertex;
    int e_dist = w->gterm.gm_nearEdge;
    double seglen, d1, d2, s, K, frac;
    int ncrossings, x0, y0;
    XPoint *q;
    int n;
    int use_old_method = 0;

    /* Determine if the point is near a vertex.  */
    for (p = gm->points, n = gm->npoints - 1;  --n >= 0;  p++)
	if (abs (x - p->x) < v_dist && abs (y - p->y) < v_dist) {
	    if (what) {
		what->type = Ge_Point;
		what->vertex = p - gm->points;
	    }
	    return (1);
	}

    /* Determine if the point is near an edge.  The test is based on the
     * observation that when a point is near a line segment, the sum of the
     * distances from the point to either end-point of the line segment is
     * nearly the same as the length of the line segment.
     */
    p = gm->points;

    ptop = p + (gm->npoints - 1);  				/* MF014 */
    x0 = p->x;  y0 = p->y;
    d1 = sqrt ((double)(SQR(x - x0) + SQR(y - y0)));

    for (p++;  p < ptop;  p++) {
	seglen = sqrt ((double)(SQR(p->x - x0) + SQR(p->y - y0)));
	d2 = sqrt ((double)(SQR(x - p->x) + SQR(y - p->y)));

	if (abs(d1 + d2 - seglen) < e_dist) {			/* MF028 */
	    if (what) {
		what->type = Ge_Edge;
		what->vertex = (p - 1) - gm->points;
	    }
	    return (1);
	}

	d1 = d2;
	x0 = p->x;  y0 = p->y;
    }

    /* If the marker is one of the closed polygon types, determine if the
     * point is inside the marker.
     */
    switch (gm->type) {
    case Gm_Line:
    case Gm_Polyline:
	return (0);
	break;
    case Gm_Circle:
        d1 = sqrt ((double)(SQR(x - gm->x) + SQR(y - gm->y)));
	if (d1 < gm->width) {
	    if (what) what->type = Ge_Marker;
	    return (1);
	} else
	    return (0);
	break;
    }

    if (use_old_method) {
        for (p = gm->points, ncrossings=0;  p < ptop;  p++) {
	    /* Scan forward until we find a line segment that crosses Y.
	     */
	    if (p->y > y) {
	        for (p++;  p < ptop && p->y >= y;  p++)
		    ;
	        --p;
	    } else if (p->y < y) {
	        for (p++;  p < ptop && p->y <= y;  p++)
		    ;
	        --p;
	    }

	    /* The line segment p[0]:p[1] crosses the Y plane.  If this lies
	     * entirely to the left of the X plane we can ignore it.  If any
	     * portion of the line segment lies to the right of X we compute
	     * the point where the line intersects the Y plane.  If this point
	     * is to the right of the X plane we have a crossing.
	     */
	    q = p + 1;
	    if (q < ptop && p->x > x || q->x > x) {
                if (q->y == p->y)
                     frac = (double) 0.0;
                else
                     frac = (double)(y - p->y) / (double)(q->y - p->y);
	        if ((frac * (q->x - p->x) + p->x) >= x)
		    ncrossings++;
	    }
        }

    } else {
        float xp[64], yp[64];
        int i;

        for (i=0, p=gm->points, ncrossings=0;  p <= ptop;  p++, i++) {
	    xp[i] = (float) p->x;
	    yp[i] = (float) p->y;
        }
        ncrossings = point_in_poly (gm->npoints, xp, yp, (float)x, (float)y);
    }

    if (ncrossings & 1) {
	if (what)
	    what->type = Ge_Marker;
	return (1);
    }

    return (0);
}

point_in_poly (npol, xp, yp, x, y)
int 	npol;
float 	*xp, *yp, x, y;
{
      int i, j, c = 0;

      for (i = 0, j = npol-1; i < npol; j = i++) {
        if ((((yp[i] <= y) && (y < yp[j])) ||
             ((yp[j] <= y) && (y < yp[i]))) &&
            (x < (xp[j] - xp[i]) * (y - yp[i]) / (yp[j] - yp[i]) + xp[i]))

          c = !c;
      }
      return c;
}




/* gm_markpos -- Mark the current position of a marker.
 */
static void
gm_markpos (gm)
    register Marker gm;
{
    gm->old_rect = gm->cur_rect;
    XUnionRegion (gm->cur_region, null_region, gm->old_region);
}


/* gm_redraw -- Redraw a marker expressed as a list of vertices.
 */
static void
gm_redraw (gm, function)
    register Marker gm;
    int function;
{
    GtermWidget w = gm->w;
    Display *display = w->gterm.display;
    Window window = w->gterm.window;
    int flags = (Gm_Activated|Gm_Visible);
    GC gc = (function == GXxor) ? w->gterm.gm_rubberGC : w->gterm.gm_drawGC;

    if (!XtIsRealized ((Widget)w))
	return;
    if (!((gm->flags & flags) == flags))
	return;

    /* Fill the polygon area if indicated. */
    if (gm->fill && function != GXxor) {
	if (gm->fillPattern) {
	    XSetStipple (display, gc, gm->fillPattern);
	    XSetForeground (display, gc, gm->fillColor);
	    XSetBackground (display, gc, gm->fillBgColor);
	    XSetFillStyle (display, gc, gm->fillStyle);
	} else {
	    XSetForeground (display, gc, gm->fillColor);
	    XSetFillStyle (display, gc, FillSolid);
	}

	XFillPolygon (display, window, gc,
	    gm->points, gm->npoints, Nonconvex, CoordModeOrigin);
    }

    /* Set up the drawing GC. */
    if (function != GXxor) {
	XSetFunction (display, gc, function);
	XSetFillStyle (display, gc, FillSolid);
	XSetForeground (display, gc, (gm == w->gterm.gm_active) ?
	    gm->highlightColor : gm->lineColor);

	XSetLineAttributes (display, gc,
	    gm->lineWidth +
		((gm == w->gterm.gm_active) ? w->gterm.gm_highlightWidth : 0),
	    gm->lineStyle,
	    CapButt,
	    (gm->type == Gm_Polygon || gm->type == Gm_Polyline) ?
		JoinBevel : JoinMiter);
    }

    /* Draw the marker outline. */
    if (gm->lineWidth > 0) {
	if (gm->type == Gm_Circle ||
		(gm->type == Gm_Ellipse && abs(gm->rotangle) < 0.01)) {

	    /* Special case - use X arc drawing primitive.  We could use the
	     * gm->points polygon instead, as this outline polygon is
	     * maintained for all classes of marker.
	     */
	    if (w->gterm.gm_xorFill && function == GXxor) {
		XFillArc (display, window, gc,
		    gm->x - gm->width, gm->y - gm->height,
		    gm->width * 2, gm->height * 2, 0, 360 * 64);
	    }
	    XDrawArc (display, window, gc,
		gm->x - gm->width, gm->y - gm->height,
		gm->width * 2, gm->height * 2, 0, 360 * 64);

	} else {
	    /* Draw marker expressed as a polygon. */
	    if (w->gterm.gm_xorFill && function == GXxor) {
		XFillPolygon (display, window, gc,
		    gm->points, gm->npoints, Convex, CoordModeOrigin);
	    }
	    XDrawLines (display, window, gc,
		gm->points, gm->npoints, CoordModeOrigin);
	}
    }

    /* Draw the knots if enabled. */
    if (function != GXxor && gm->knotSize > 0) {
	int knotsize = gm->knotSize;
	int halfsize = gm->knotSize / 2;
	int i;

	XSetForeground (display, gc, gm->knotColor);
	for (i=0;  i < gm->npoints;  i++) {
	    XFillRectangle (display, window, gc,
		gm->points[i].x - halfsize, gm->points[i].y - halfsize,
		gm->knotSize, gm->knotSize);
	}
    }
}


/* gm_rotate_indicator -- Draw a line indicating the rotation angle.
 */
static void
gm_rotate_indicator (gm, function)				/* MF020 */
Marker gm;
int    function;
{
    GtermWidget w = gm->w;
    Display *display = w->gterm.display;
    Window window = w->gterm.window;
    GC gc = (function == GXxor) ? w->gterm.gm_rubberGC : w->gterm.gm_drawGC;

    if (!gm->rotIndicator)
	return ;

    if (function == GXxor) {
        if (gm->type == Gm_Polygon ||
            gm->type == Gm_Ellipse ||
            gm->type == Gm_Box ||
            gm->type == Gm_Rectangle) {
    		int	x, y, x2, y2;
    		double  ar, cos_rotangle, sin_rotangle;
    		double  alpha = atan2 ((double)gm->height,(double)gm->width);

	        cos_rotangle = cos ((double)(-gm->rotangle - alpha));
    	        sin_rotangle = sin ((double)(-gm->rotangle - alpha));
		ar = (double) gm->height / (double) gm->width;
    	        x = (int) (ar * (gm->width / 2));
	        y = (int) (ar * (gm->height / 2));
    	        x2 = x * cos_rotangle - y * sin_rotangle + gm->x;
    	        y2 = x * sin_rotangle + y * cos_rotangle + gm->y;

  	        XDrawLine (display, window, gc, gm->x, gm->y, x2, y2); 
        }
    } else {
	; 	/* no-op at present */
    }
}


/* gm_setCurRect -- Compute a bounding rectangle which completely encloses
 * a marker (assumes that the marker is expressed as list of points).
 */
static void
gm_setCurRect (gm)
Marker gm;
{
    int border;

    XDestroyRegion (gm->cur_region);
    gm->cur_rect = null_rect;

    if (gm->npoints <= 0)
	gm->cur_region = XCreateRegion();
    else {
	gm->cur_region = XPolygonRegion (gm->points, gm->npoints, EvenOddRule);
	border = (max (gm->lineWidth, gm->knotSize) + 1) / 2;
	border = max (border, BORDER);
	XShrinkRegion (gm->cur_region, -border, -border);
	XClipBox (gm->cur_region, &gm->cur_rect);
    }
}


/* gm_niceAngle -- Round a rotation angle to a "nice" value.
 */
static double
gm_niceAngle (alpha)
    double alpha;
{
    double tol = 0.003;
    double beta;

    if (     abs (alpha - PI_2*0) < tol)
	beta = PI_2*0;
    else if (abs (alpha - PI_2*1) < tol)
	beta = PI_2*1;
    else if (abs (alpha - PI_2*2) < tol)
	beta = PI_2*2;
    else if (abs (alpha - PI_2*3) < tol)
	beta = PI_2*3;
    else if (abs (alpha - PI_2*4) < tol)
	beta = PI_2*0;
    else
	beta = alpha;

    return (beta);
}


static XImage *cached_ximage =	NULL;			/* MF004 BEGIN */

/* GetCachedXImage -- 
 */
static XImage *
GetCachedXImage (w, pixmap, width, height)
     GtermWidget w;
     Pixmap pixmap;
     int width;
     int height;
{
    if ((cached_ximage != NULL)) {
        if ((pixmap == w->gterm.pixmap) &&
            (width  == w->core.width)   &&
            (height == w->core.height)) {
                return (cached_ximage);
        }
    }
    return(NULL);
}


/* DestroyCachedXImage --
 */
static void 
DestroyCachedXImage ()
{
    if (cached_ximage != NULL) {
        XDestroyImage (cached_ximage);
      cached_ximage = NULL;
    }
}


/* NewCachedXImage --
 */
static void 
NewCachedXImage (w, xin, pixmap, width, height)
     GtermWidget w;
     XImage *xin;
     Pixmap pixmap;
     int width;
     int height;
{
    if ((pixmap ==  w->gterm.pixmap) &&
        (width  ==  w->core.width)   &&
        (height ==  w->core.height)) {
            DestroyCachedXImage();
            cached_ximage = xin;
    }
}							/* MF004 END   */

