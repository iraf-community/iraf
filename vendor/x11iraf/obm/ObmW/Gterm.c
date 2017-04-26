#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <time.h>

#include <X11/Xlib.h>
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/cursorfont.h>
#include <X11/Xaw/XawInit.h>
#include "GtermP.h"


#define	DBG_TRACE 	0
#define	DBG_CMAPS 	0
#define	DBG_IOMAP 	0
#define	DBG_VERBOSE 	0
#define	DBG_CM_VERB 	0



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

#define	CacheXImage		False				/* MF004 */

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

    {XtNdialogBgColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.dialogBgColorStr), 
	XtRImmediate,"yellow"},
    {XtNdialogFgColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.dialogFgColorStr), 
	XtRImmediate,"black"},
    {XtNidleCursorBgColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.idleCursorBgColorStr), 
	XtRImmediate,"white"},
    {XtNidleCursorFgColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.idleCursorFgColorStr), 
	XtRImmediate,"black"},
    {XtNbusyCursorBgColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.busyCursorBgColorStr), 
	XtRImmediate,"white"},
    {XtNbusyCursorFgColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.busyCursorFgColorStr), 
	XtRImmediate,"black"},
    {XtNginmodeCursorBgColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.ginmodeCursorBgColorStr), 
	XtRImmediate,"black"},
    {XtNginmodeCursorFgColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.ginmodeCursorFgColorStr), 
	XtRImmediate,"white"},
    {XtNcrosshairCursorColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.crosshairCursorColorStr), 
	XtRImmediate,"red"},

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

    {XtNcacheRasters, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.color0Str), XtRImmediate, 
	(caddr_t)"black"},
    {XtNcacheRasters, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.color1Str), XtRImmediate, 
	(caddr_t)"white"},
    {XtNcacheRasters, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.color2Str), XtRImmediate, 
	(caddr_t)"red"},
    {XtNcacheRasters, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.color3Str), XtRImmediate, 
	(caddr_t)"green"},
    {XtNcacheRasters, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.color4Str), XtRImmediate, 
	(caddr_t)"blue"},
    {XtNcacheRasters, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.color5Str), XtRImmediate, 
	(caddr_t)"cyan"},
    {XtNcacheRasters, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.color6Str), XtRImmediate, 
	(caddr_t)"yellow"},
    {XtNcacheRasters, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.color7Str), XtRImmediate, 
	(caddr_t)"magenta"},
    {XtNcacheRasters, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.color8Str), XtRImmediate, 
	(caddr_t)"purple"},
    {XtNcacheRasters, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.color9Str), XtRImmediate,
	(caddr_t)"darkslategray"},

    {XtNmarkerFillColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_fillColorStr),
	XtRImmediate,"DarkSlateGray"},
    {XtNmarkerFillBgColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_fillBgColorStr), XtRImmediate,"black"},

    {XtNmarkerHighlightColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_highlightColorStr),
	XtRImmediate,"green"},
    {XtNmarkerCursorFgColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_cursorFgColorStr),
	XtRImmediate,"yellow"},
    {XtNmarkerCursorBgColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_cursorBgColorStr),
	XtRImmediate,"black"},

    {XtNmarkerLineLineColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_LineLineColorStr),
	XtRImmediate,"green"},
    {XtNmarkerLineKnotColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_LineKnotColorStr),
	XtRImmediate,"blue"},

    {XtNmarkerTextLineColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_TextLineColorStr),
	XtRImmediate,"green"},
    {XtNmarkerTextColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_TextColorStr), XtRImmediate,"yellow"},
    {XtNmarkerTextBgColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_TextBgColorStr),
	XtRImmediate,"DarkSlateGray"},

    {XtNmarkerRectLineColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_RectLineColorStr),
	XtRImmediate,"green"},
    {XtNmarkerRectKnotColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_RectKnotColorStr),
	XtRImmediate,"blue"},

    {XtNmarkerBoxLineColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_BoxLineColorStr),
	XtRImmediate,"green"},
    {XtNmarkerBoxKnotColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_BoxKnotColorStr), XtRImmediate,"blue"},

    {XtNmarkerCircleLineColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_CircleLineColorStr),
	XtRImmediate,"green"},
    {XtNmarkerCircleKnotColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_CircleKnotColorStr),
	XtRImmediate,"blue"},

    {XtNmarkerEllipseLineColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_EllipseLineColorStr),
	XtRImmediate,"green"},
    {XtNmarkerEllipseKnotColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_EllipseKnotColorStr),
	XtRImmediate,"blue"},

    {XtNmarkerPgonLineColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_PgonLineColorStr),
	XtRImmediate,"green"},
    {XtNmarkerPgonKnotColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_PgonKnotColorStr),
	XtRImmediate,"blue"},

    {XtNmarkerPointLineColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_PointLineColorStr),
	XtRImmediate,"green"},
    {XtNmarkerPointKnotColorStr, XtCString, XtRString, sizeof(String),
        XtOffset(GtermWidget,gterm.gm_PointKnotColorStr),
	XtRImmediate,"blue"},


    {XtNcopyOnResize, XtCBoolean, XtRBoolean, sizeof(Boolean),
	XtOffset(GtermWidget,gterm.copyOnResize), XtRImmediate,
	(caddr_t)DEF_COPYONRESIZE},
    {XtNcmapName, XtCString, XtRString, sizeof(String),
	XtOffset(GtermWidget,gterm.cmapName), XtRImmediate,
	(caddr_t)"default"},
    {XtNuseGlobalCmap, XtCBoolean, XtRBoolean, sizeof(Boolean),
        XtOffset(GtermWidget,gterm.useGlobalCmap), XtRImmediate,
        (caddr_t)FALSE},
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

struct {
    unsigned short  index;
    unsigned short  r, g, b;
    unsigned long   value;
    char *name;
} static_colors[] = {
    {   0,     0,   0,   0,  0x000000, "black"           },/* static colors   */
    {   1,   255, 255, 255,  0xffffff, "white"           },
    {   2,   255,   0,   0,  0xff0000, "red"             },
    {   3,     0, 255,   0,  0x00ff00, "green"           },
    {   4,     0,   0, 255,  0x0000ff, "blue"            },
    {   5,     0, 255, 255,  0x00ffff, "cyan"            },
    {   6,   255, 255,   0,  0xffff00, "yellow"          },
    {   7,   255,   0, 255,  0xff00ff, "magenta"         },
    {   8,   155,  48, 255,  0xa020f0, "purple"          },
    {   9,   151, 255, 255,  0x2f4f4f, "darkslategray"   },

    { 202,     0,   0,   0,  0x000000, "black"           },/* overlay colors  */
    { 203,   255, 255, 255,  0xffffff, "white"           },
    { 204,   255,   0,   0,  0xff0000, "red"             },
    { 205,     0, 255,   0,  0x00ff00, "green"           },
    { 206,     0,   0, 255,  0x0000ff, "blue"            },
    { 207,   255, 255,   0,  0xffff00, "yellow"          },
    { 208,     0, 255, 255,  0x00ffff, "cyan"            },
    { 209,   255,   0, 255,  0xff00ff, "magenta"         },
    { 210,   255, 127,  80,  0xff7f50, "coral"           },
    { 211,   176,  48,  96,  0xb03060, "maroon"          },
    { 212,   255, 165,   0,  0xffa500, "orange"          },
    { 213,   240, 183, 107,  0xf0e68c, "khaki"           },
    { 214,   218, 112, 214,  0xda70d6, "orchid"          },
    { 215,    64, 224, 208,  0x40e0d0, "turquoise"       },
    { 216,   238, 130, 238,  0xee82ee, "violet"          },
    { 217,   245, 222, 179,  0xf5deb3, "wheat"           },
    { 218,     0,   0,   0,  0x000000, "dummy"           },
};
static int num_static_colors	= 27;

static int colormap_focus	= 512;


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
static void init_iomap(), init_global_cmap(), invalidate_cmap();
static void initColorResources ();
static void gm_rotate_indicator();				/* MF020 */
static Pixel get_pixel(), *get_cmap_in(), *get_cmap_out();
static Pixel ColorNameToPixel ();


/* Global Colormap declarations.
 */
#define CMAPNAME_SIZE 160

static char   	      global_cmapname[CMAPNAME_SIZE];
static Pixel  	      global_cmap[MAX_SZCMAP];
static XColor 	      global_color[MAX_SZCMAP];
static unsigned long  global_lut[MAX_SZCMAP];
static int            global_ncolors      = 0;
static int    	      global_nstatic      = SZ_STATIC_CMAP;
static int    	      global_noverlay     = SZ_OVERLAY_CMAP;
static int    	      global_mincolors    = 0;
static int	      valid_lut           = 0;

static int    SetGlobalCmap();
static int    ParseGlobalCmap();
static int    GetMaxCmapColors();
static int    GetGlobalColors();
static void   SetGlobalColors();


static void NewCachedXImage();					/* MF004 */
static void DestroyCachedXImage();				/* MF004 */
static XImage *GetCachedXImage();				/* MF004 */


static char 	*dbg_wSize();			/* debug utils		 */
static char 	*dbg_visStr();

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

    XVisualInfo info;                           /* Deep Frame */
    XStandardColormap std, *stdcmaps;
    int nstdcmaps;
    XColor colors[MAX_SZCMAP+1];
    char property[128], cname[12];


    for (i=0 ; i < MAX_SZCMAP; i++)
        memset (&colors[i], 0, sizeof(XColor)); /* Deep Frame */

    w->gterm.resetCallback = NULL;
    w->gterm.resizeCallback = NULL;
    w->gterm.inputCallback = NULL;

    w->gterm.display         = display = XtDisplay (w);
    w->gterm.screen          = screen  = XtScreen (w);
    w->gterm.root            = RootWindowOfScreen (screen);
    w->gterm.w_depth         = DefaultDepth (display, DefaultScreen(display));
    w->gterm.w_visual_class  = 
	DefaultVisual (display, DefaultScreen(display))->class;


    /* Deep Frame */

    /* Check if default visual is Pseudo color 8. If so, continue, else
     * force a Pseudo 8 visual and new StandardColorMap.
     */
    if (XMatchVisualInfo (display, DefaultScreen(display), 8, PseudoColor,
         &info) && info.visual == DefaultVisualOfScreen(screen)) {

        /* reset some gterm and core values to reflect the new visual */
        w->core.depth         = info.depth;
        w->gterm.visual       = info.visual;
        w->gterm.forcePseudo8 = True;

        /* Find the Standard Color Map */

	if (DBG_TRACE)
	    fprintf (stderr, "Gterm.initialize(Pseudo8): cmapName = '%s'\n",
		w->gterm.cmapName);

        /* calculate cmap name */
        /* if default, try to extract name from 'default[n:m,name]' */

        if (!strncmp(w->gterm.cmapName,"default",7)) {
            char *p;

            if (p=strstr(w->gterm.cmapName,"[")) {
                if (p=strstr(p,",")) {
                    strcpy(global_cmapname,++p);
                    if (p=strstr(global_cmapname,"]")) {
                        *p='\0';                /* cut off the last ']' */
                        strcpy(w->gterm.cmapName,global_cmapname);
                    }
                }
            }
        }

        /* if that didn't work, give it a name
	*/
        if (!strncmp(w->gterm.cmapName,"default",7))
            strcpy(w->gterm.cmapName,"ForcePseudo");

	if (DBG_TRACE)
	    fprintf (stderr, "Gterm.initialize 2(Pseudo8): cmapName = '%s'\n",
		w->gterm.cmapName);

        /* create atom */
        sprintf(property, "GT_%s", w->gterm.cmapName);
        w->gterm.cmapAtom = XInternAtom (display, property, False);


        w->core.colormap = 0;
        if (XGetRGBColormaps (display, w->gterm.root, &stdcmaps, &nstdcmaps,
            w->gterm.cmapAtom)) {
              if (stdcmaps[0].colormap && stdcmaps[0].visualid==info.visualid) {
                  w->core.colormap = stdcmaps[0].colormap;
                  w->gterm.base_pixel = stdcmaps[0].base_pixel;
              }
        }

	if (DBG_TRACE)
	    fprintf (stderr, 
		"Gterm.intialize(Pseudo8): colormap = 0x%x base=%d\n",
		w->core.colormap, w->gterm.base_pixel);

        /* create Standard Color Map */
        if (!w->core.colormap && info.visual) {
            w->core.colormap = XCreateColormap (display, w->gterm.root,
                info.visual, AllocAll);

            /* Set the Static Colors */
            for (i=0; i < SZ_STATIC_CMAP; i++) {
                colors[i].pixel = i;
                XParseColor(display, w->core.colormap, 
		     static_colors[i].name, &colors[i]);
            }
            XStoreColors (display, w->core.colormap, colors, SZ_STATIC_CMAP);

            /* save Standard Color Map */
            std.colormap = w->core.colormap;
            std.red_max = 1;
            std.red_mult = 0;
            std.green_max = std.green_mult = 0;
            std.blue_max = std.blue_mult = 0;
            std.base_pixel = w->gterm.base_pixel;
            std.visualid = info.visualid;
            std.killid = 0;
            XSetRGBColormaps (display, w->gterm.root, &std, 1,
                w->gterm.cmapAtom);
            XSetCloseDownMode (display, RetainPermanent);
        }

	if (DBG_TRACE)
	    fprintf (stderr,
		"Gterm.initialize(Pseudo8): defDepth=%d  cmap=0x%x\n", 
    		w->gterm.w_depth, w->core.colormap);

        /* Let gterm know what we have done.
	*/
        /* will be reset later on, but put here for clarity */
        w->gterm.useDefaultCM   = False; 
        w->gterm.useGlobalCmap  = False;
        w->gterm.haveColormap   = True;
        w->gterm.cmapInitialize = False;

    } else {
	XVisualInfo *vp, template;
	int i, j, k, nvis;
	unsigned short val;
	float	 scale;


	memset (&template, 0, sizeof (template));
	template.class = TrueColor;

	vp = XGetVisualInfo (display, VisualClassMask, &template, &nvis);

	if (DBG_TRACE) {
	    fprintf (stderr, "NO 8-bit PSEUDO FOUND.....USING TRUECOLOR\n");
	    fprintf (stderr, "nvis=%d  defDepth=%d  cmap=0x%x\n", 
	        nvis, w->gterm.w_depth, w->core.colormap);
	}

        /* Let gterm know what we have done.
	*/
        w->gterm.visual         = vp->visual;
        w->gterm.useDefaultCM   = True; 
        w->gterm.useGlobalCmap  = True;
        w->gterm.haveColormap   = True;
        w->gterm.forcePseudo8   = False;
        w->gterm.cmapInitialize = False;
        w->gterm.ncolors        = 200;
        w->gterm.base_pixel     = 0;
	w->gterm.cmapName 	= "default";

        /* Create a standard colormap of the static pixels.
	*/
        w->gterm.color0 = ColorNameToPixel (w, w->gterm.color0Str);
        w->gterm.color1 = ColorNameToPixel (w, w->gterm.color1Str);
        w->gterm.color2 = ColorNameToPixel (w, w->gterm.color2Str);
        w->gterm.color3 = ColorNameToPixel (w, w->gterm.color3Str);
        w->gterm.color4 = ColorNameToPixel (w, w->gterm.color4Str);
        w->gterm.color5 = ColorNameToPixel (w, w->gterm.color5Str);
        w->gterm.color6 = ColorNameToPixel (w, w->gterm.color6Str);
        w->gterm.color7 = ColorNameToPixel (w, w->gterm.color7Str);
        w->gterm.color8 = ColorNameToPixel (w, w->gterm.color8Str);
        w->gterm.color9 = ColorNameToPixel (w, w->gterm.color9Str);


        /* Set the Static part of the colormap.
        */
        for (i=0; i < MAX_SZCMAP; i++) {
	    global_cmap[i] = i;
	    colors[i].pixel = i;
	    colors[i].flags = (DoRed | DoGreen | DoBlue);
	}

        for (i=0; i < SZ_STATIC_CMAP; i++) {
            colors[i].pixel = global_color[i].pixel = static_colors[i].index;
	    colors[i].red   = global_color[i].red   = static_colors[i].r << 8;
	    colors[i].green = global_color[i].green = static_colors[i].g << 8;
	    colors[i].blue  = global_color[i].blue  = static_colors[i].b << 8;
        }

        /* Set the Dynamic part of the colormap.  Here we scale the 255 levels
	** to the 200 levels we gets from the client.
        */
	scale = 255. / 200.;
        for (i=0;  i < SZ_DYNAMIC_CMAP;  i++) {
	    j = SZ_STATIC_CMAP + i;
	    k = i + 1;
	    val =  ((unsigned short) ((i * scale) + 0.5)) << 8;

            colors[j].pixel = global_color[j].pixel = j;
	    colors[j].red   = global_color[j].red   = val;
	    colors[j].green = global_color[j].green = val;
	    colors[j].blue  = global_color[j].blue  = val;
	}

        /* Set the overlay colors in the colormap.  These indices are what's
	** defined in the graphcap file and can be extended as needed.
        */
        for (i=0; i < SZ_OVERLAY_CMAP; i++) {
	    j = SZ_STATIC_CMAP + i;
	    k = static_colors[j].index + SZ_STATIC_CMAP;

            colors[k].pixel = global_color[k].pixel = k;
	    colors[k].red   = global_color[k].red   = static_colors[j].r << 8;
	    colors[k].green = global_color[k].green = static_colors[j].g << 8;
	    colors[k].blue  = global_color[k].blue  = static_colors[j].b << 8;
        }


        /* Set the Static Colors.    FIXME ******
        if ((w->core.colormap = XCreateColormap (display, w->gterm.root,
    	    vp->visual, AllocNone))) {

            for (i=0; i < MAX_SZCMAP; i++) {
                sprintf (cname,"#%02x%02x%02x",
		    global_color[i].red,
		    global_color[i].green,
		    global_color[i].blue);  
                if (!XParseColor( display, w->core.colormap, cname, &colors[i]))
                    continue;
                colors[i].pixel = i;
                colors[i].flags = DoRed | DoGreen | DoBlue;
	    }
            XStoreColors (display, w->core.colormap, colors, MAX_SZCMAP);
	    XSetWindowColormap (display, w->gterm.root, w->core.colormap );
	}
	*/

	if (DBG_TRACE)
	    fprintf (stderr, 
		"Gterm.initialize (TrueColor): after colormap, cmap=0x%x\n",
		w->core.colormap);
    }
    /* Deep Frame */


    XtVaSetValues ((Widget)w, XtNbackground, (XtArgVal)w->gterm.color0, NULL);

    /* Initialize color map. 
    */
    if (w->gterm.useGlobalCmap) {
        w->gterm.cmap  = (Pixel *)  global_cmap;
        w->gterm.color = (XColor *) global_color;
        w->gterm.ncolors = SZ_STATIC_CMAP + SZ_DYNAMIC_CMAP + SZ_OVERLAY_CMAP;
        for (i=0; i < MAX_SZCMAP; i++)
	    w->gterm.cmap_in[i] = w->gterm.cmap_out[i] = w->gterm.iomap[i] = i;

    } else {
        pp = &w->gterm.color0;
        w->gterm.cmap  = (Pixel *) XtCalloc (MAX_SZCMAP, sizeof(Pixel));
        w->gterm.color = (XColor *)XtCalloc (MAX_SZCMAP, sizeof(XColor));

        for (i=0;  i < SZ_STATIC_CMAP;  i++) {
	    memset ((char *)&w->gterm.color[i], 0, sizeof(XColor));
	    w->gterm.color[i].pixel = w->gterm.cmap[i] = *pp++;
	    w->gterm.color[i].red   = 0;
	    w->gterm.color[i].green = 0;
	    w->gterm.color[i].blue  = 0;
        }
        for (   ;  i < MAX_SZCMAP;  i++) {
	    memset ((char *)&w->gterm.color[i], 0, sizeof(XColor));
	    w->gterm.color[i].pixel = w->gterm.cmap[i] = i;
        }
        XQueryColors(display, w->core.colormap, w->gterm.color, SZ_STATIC_CMAP);
        w->gterm.ncolors = SZ_STATIC_CMAP;

	init_iomap (w);
    }

    if (DBG_TRACE)
	fprintf (stderr, "Gterm.intialize: cmap initialized\n");

    
    /* if we have not yet allocated any colors ...   The SetGlobalCmap()
    ** function returns zero when useGlobalCmap is disabled, but it also
    ** allocates the gterm.cmap and gterm.color pointers in this case.
    */
    if (! w->gterm.useGlobalCmap) {
        if (SetGlobalCmap(w) == 0 && !w->gterm.useGlobalCmap) {
            pp = &w->gterm.color0;
            for (i=0;  i < SZ_STATIC_CMAP;  i++)
                w->gterm.color[i].pixel = w->gterm.cmap[i] = *pp++;
            for ( ; i < MAX_SZCMAP; i++) {
                memset ((char *)&w->gterm.color[i], 0, sizeof(XColor));
                w->gterm.color[i].pixel = w->gterm.cmap[i] = i;
            }
            XQueryColors (display, w->core.colormap, w->gterm.color,
		SZ_STATIC_CMAP);
            w->gterm.ncolors = SZ_STATIC_CMAP;
            init_iomap (w);

        } else {
            /* in the case of a default colormap, we might already have color
             * cells.
             */
            w->gterm.ncolors = GetGlobalColors ();
        }
        w->gterm.useDefaultCM = ParseGlobalCmap (w);

        w->gterm.useDefaultCM = (strcmp (w->gterm.cmapName, "default") == 0);
        w->gterm.haveColormap = w->gterm.useDefaultCM;

    } else
        i = GetGlobalColors ();


    /* Now fix up some color resources.
    */
    initColorResources (w);


    if (DBG_TRACE)
	fprintf (stderr, 
	    "Gterm.intialize: ncolors=%d useDefaultCM=%d  core.colormap=0x%x\n",
	    w->gterm.ncolors, w->gterm.useDefaultCM, w->core.colormap);

    w->gterm.cmapLastUpdate = 0;
    w->gterm.cmapLastShadow = 0;
    w->gterm.in_window      = 0;
    w->gterm.n_wmWindows    = 0;				/* MF012 */


    /* Get clear pixmap GC. */
    gc = XCreateGC (display, w->gterm.root, 0, NULL);
    if (w->gterm.useGlobalCmap) {
        XSetBackground (display, gc, static_colors[0].value);
        XSetForeground (display, gc, static_colors[0].value);
    } else {
        XSetBackground (display, gc, w->gterm.color0);
        XSetForeground (display, gc, w->gterm.color0);
    }
    w->gterm.clearGC = gc;
    XSetGraphicsExposures (display, gc, 0);			/* MF029 */

    /* Get expose GC. */
    gc = XCreateGC (display, w->gterm.root, 0, NULL);
    w->gterm.exposeGC = gc;
    XSetGraphicsExposures (display, gc, 0);			/* MF029 */

    /* Get graphics drawing GC. */
    gc = XCreateGC (display, w->gterm.root, 0, NULL);
    if (w->gterm.useGlobalCmap) {
        XSetBackground (display, gc, static_colors[0].value);
        XSetForeground (display, gc, static_colors[1].value);
    } else {
        XSetBackground (display, gc, w->gterm.color0);
        XSetForeground (display, gc, w->gterm.color1);
    }
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

    /* Deep Frame
    set_default_color_index (w);
       Deep Frame */

    /* Disable input until window is ready. */
    w->gterm.delay = 1;

    if (DBG_TRACE) {
	if (DBG_CM_VERB)
	    dbg_printCmaps(w);
  	fprintf (stderr, "Initialize[%s]  RETURNING\n\n", dbg_wSize(w));
    }
}


static void
initColorResources (w)
    GtermWidget w;
{
    w->gterm.color0 = ColorNameToPixel (w, w->gterm.color0Str);
    w->gterm.color1 = ColorNameToPixel (w, w->gterm.color1Str);
    w->gterm.color2 = ColorNameToPixel (w, w->gterm.color2Str);
    w->gterm.color3 = ColorNameToPixel (w, w->gterm.color3Str);
    w->gterm.color4 = ColorNameToPixel (w, w->gterm.color4Str);
    w->gterm.color5 = ColorNameToPixel (w, w->gterm.color5Str);
    w->gterm.color6 = ColorNameToPixel (w, w->gterm.color6Str);
    w->gterm.color7 = ColorNameToPixel (w, w->gterm.color7Str);
    w->gterm.color8 = ColorNameToPixel (w, w->gterm.color8Str);
    w->gterm.color9 = ColorNameToPixel (w, w->gterm.color9Str);

    w->gterm.dialogBgColor =
        ColorNameToPixel (w, w->gterm.dialogBgColorStr);
    w->gterm.dialogFgColor =
        ColorNameToPixel (w, w->gterm.dialogFgColorStr);

    w->gterm.idleCursorBgColor =
        ColorNameToPixel (w, w->gterm.idleCursorBgColorStr);
    w->gterm.idleCursorFgColor =
        ColorNameToPixel (w, w->gterm.idleCursorFgColorStr);

    w->gterm.busyCursorBgColor =
        ColorNameToPixel (w, w->gterm.busyCursorBgColorStr);
    w->gterm.busyCursorFgColor =
        ColorNameToPixel (w, w->gterm.busyCursorFgColorStr);

    w->gterm.ginmodeCursorBgColor =
        ColorNameToPixel (w, w->gterm.ginmodeCursorBgColorStr);
    w->gterm.ginmodeCursorFgColor =
        ColorNameToPixel (w, w->gterm.ginmodeCursorFgColorStr);
    w->gterm.crosshairCursorColor =
        ColorNameToPixel (w, w->gterm.crosshairCursorColorStr);

    w->gterm.gm_fillColor =
        ColorNameToPixel (w, w->gterm.gm_fillColorStr);
    w->gterm.gm_fillBgColor =
        ColorNameToPixel (w, w->gterm.gm_fillBgColorStr);

    w->gterm.gm_highlightColor =
        ColorNameToPixel (w, w->gterm.gm_highlightColorStr);

    w->gterm.gm_cursorFgColor =
        ColorNameToPixel (w, w->gterm.gm_cursorFgColorStr);
    w->gterm.gm_cursorBgColor =
        ColorNameToPixel (w, w->gterm.gm_cursorBgColorStr);

    w->gterm.gm_LineLineColor =
        ColorNameToPixel (w, w->gterm.gm_LineLineColorStr);
    w->gterm.gm_LineKnotColor =
        ColorNameToPixel (w, w->gterm.gm_LineKnotColorStr);

    w->gterm.gm_TextLineColor =
        ColorNameToPixel (w, w->gterm.gm_TextLineColorStr);
    w->gterm.gm_TextColor =
        ColorNameToPixel (w, w->gterm.gm_TextColorStr);
    w->gterm.gm_TextBgColor =
        ColorNameToPixel (w, w->gterm.gm_TextBgColorStr);

    w->gterm.gm_RectLineColor =
        ColorNameToPixel (w, w->gterm.gm_RectLineColorStr);
    w->gterm.gm_RectKnotColor =
        ColorNameToPixel (w, w->gterm.gm_RectKnotColorStr);

    w->gterm.gm_BoxLineColor =
        ColorNameToPixel (w, w->gterm.gm_BoxLineColorStr);
    w->gterm.gm_BoxKnotColor =
        ColorNameToPixel (w, w->gterm.gm_BoxKnotColorStr);

    w->gterm.gm_CircleLineColor =
        ColorNameToPixel (w, w->gterm.gm_CircleLineColorStr);
    w->gterm.gm_CircleKnotColor =
        ColorNameToPixel (w, w->gterm.gm_CircleKnotColorStr);

    w->gterm.gm_EllipseLineColor =
        ColorNameToPixel (w, w->gterm.gm_EllipseLineColorStr);
    w->gterm.gm_EllipseKnotColor =
        ColorNameToPixel (w, w->gterm.gm_EllipseKnotColorStr);

    w->gterm.gm_PgonLineColor =
        ColorNameToPixel (w, w->gterm.gm_PgonLineColorStr);
    w->gterm.gm_PgonKnotColor =
        ColorNameToPixel (w, w->gterm.gm_PgonKnotColorStr);
}



static void
Realize (gw, valueMask, attrs)
    Widget gw;
    XtValueMask *valueMask;
    XSetWindowAttributes *attrs;
{
    GtermWidget w = (GtermWidget) gw;
    GC gc;
    Pixmap pixmap;



    if (DBG_TRACE)
	fprintf (stderr, "Realize[%s] ENTER:\n", dbg_wSize(w));

    /* Set default window size. */
    XtMakeResizeRequest (gw, w->core.width, w->core.height,
	&w->core.width, &w->core.height);

    /* Should define pseudocolor visual here, if truecolor or directcolor
     * default visual.
     */

    /* Create graphics window. */ 				/* Deep Frame */
    if (w->gterm.forcePseudo8)  /* force Pseudo 8 visual */
        XtCreateWindow (gw, InputOutput, w->gterm.visual, *valueMask, attrs);

    else                        /* Default Visual is Pseudo */
        XtCreateWindow (gw, InputOutput, (Visual *)CopyFromParent, *valueMask,
            attrs); 						/* Deep Frame */


    w->gterm.window = XtWindow (gw);
    w->gterm.old_width = w->gterm.xres = w->core.width;
    w->gterm.old_height = w->gterm.yres = w->core.height;


    /* Deep Frame */
    /* Define GC here, so that we have a Drawable at the proper depth */

    /* Get 8-bit pixmap GC.
    */
    pixmap  = XCreatePixmap (w->gterm.display, w->gterm.window,
            w->core.width, w->core.height, RasterDepth);
    gc = XCreateGC (w->gterm.display, pixmap, 0, NULL);
    XSetBackground (w->gterm.display, gc, w->gterm.color0);
    XSetForeground (w->gterm.display, gc, w->gterm.color0);
    w->gterm.clear8GC = gc;

    gc = XCreateGC (w->gterm.display, pixmap, 0, NULL);
    w->gterm.expose8GC = gc;
    XFreePixmap (w->gterm.display, pixmap);



    /* Get clear pixmap GC. */
    gc = XCreateGC (w->gterm.display, w->gterm.window, 0, NULL);
    XSetBackground (w->gterm.display, gc, w->gterm.color0);
    XSetForeground (w->gterm.display, gc, w->gterm.color0);
    w->gterm.clearGC = gc;
/*  XSetGraphicsExposures (w->gterm.display, gc, 0);*/          /* MF029 */

    /* Get expose GC. */
    gc = XCreateGC (w->gterm.display, w->gterm.window, 0, NULL);
    w->gterm.exposeGC = gc;
/*  XSetGraphicsExposures (w->gterm.display, gc, 0);*/          /* MF029 */

    /* Get graphics drawing GC. */
    gc = XCreateGC (w->gterm.display, w->gterm.window, 0, NULL);
    XSetBackground (w->gterm.display, gc, w->gterm.color0);
    XSetForeground (w->gterm.display, gc, w->gterm.color1);
    XSetLineAttributes (w->gterm.display, gc, 1, LineSolid, CapButt, JoinMiter);
    w->gterm.drawGC = gc;

    /* Get dialog box GC. */
    gc = XCreateGC (w->gterm.display, w->gterm.window, 0, NULL);
    XSetBackground (w->gterm.display, gc, w->gterm.dialogBgColor);
    XSetForeground (w->gterm.display, gc, w->gterm.dialogFgColor);
    /* XSetFunction   (w->gterm.display, gc, GXcopyInverted); */
    w->gterm.dialogGC = gc;

    /* Get crosshair cursor GC. */
    gc = XCreateGC (w->gterm.display, w->gterm.window, 0, NULL);
    XSetBackground (w->gterm.display, gc, w->gterm.color0);
    XSetForeground (w->gterm.display, gc, w->gterm.crosshairCursorColor);
    XSetLineAttributes (w->gterm.display, gc, 0, LineSolid, CapButt, JoinMiter);
    w->gterm.cursorGC = gc;

    if (DBG_TRACE)
        fprintf (stderr, "Gterm.Realize:  force8=%d  core colormap 0x%x\n",
	    w->gterm.forcePseudo8, w->core.colormap);

    set_default_color_index (w);
    if (w->gterm.forcePseudo8) {
        XInstallColormap  (w->gterm.display, w->core.colormap);
        request_colormap_focus  (w);
    }
    /* Deep Frame */


    GtRasterInit (w);
    GtMarkerInit (w);

    if (w->core.visible)
	XDefineCursor (w->gterm.display, w->gterm.window,
	    w->gterm.cursor = w->gterm.idle_cursor);

    Resize (gw);

    if (DBG_TRACE)
	fprintf (stderr, "Realize[%s] DONE:\n", dbg_wSize(w));
}

static void
Destroy (gw)
    Widget gw;
{
    GtermWidget w = (GtermWidget) gw;
    register GtCallback *cb, *cb_next;
    Display *display = w->gterm.display;



    if (DBG_TRACE)
	fprintf (stderr, "Destroy[%s] ENTER:\n", dbg_wSize(w));


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
    XFreeGC (display, w->gterm.clear8GC);
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

    if (DBG_TRACE)
	fprintf (stderr, "Destroy[%s] DONE:\n", dbg_wSize(w));
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


    if (!w || !XtIsRealized(gw))
	return;

    if (DBG_TRACE)
	fprintf (stderr, "Resize[%s] ENTER:\n", dbg_wSize(w));


    /* Create new pixmap.
    */
    pixmap = XCreatePixmap (w->gterm.display, w->gterm.window,
	w->core.width + 1, w->core.height + 1, w->core.depth);
    if (pixmap)
	XFillRectangle (w->gterm.display, pixmap,
	    w->gterm.clearGC, 0, 0, w->core.width, w->core.height);

    /* Copy old pixmap into new and free old pixmap. 
    */
    if (w->gterm.pixmap) {
	XGetGeometry (w->gterm.display, w->gterm.pixmap,
	    &root, &junk, &junk, &width, &height, &u_junk, &u_junk);
	XSetClipMask (w->gterm.display, w->gterm.exposeGC, None);
	if (w->gterm.copyOnResize)
	    XCopyArea (w->gterm.display, w->gterm.pixmap, pixmap,
		w->gterm.exposeGC, 0, 0, width-1, height-1, 0, 0);
	XFreePixmap (w->gterm.display, w->gterm.pixmap);
    }

    /* Install new pixmap. 
    */
    w->gterm.pixmap = pixmap;
    w->gterm.preserve_valid = 0;

    /* Redraw window. 
    */
    if (w->gterm.pixmap) {
	XCopyArea (w->gterm.display, w->gterm.pixmap, w->gterm.window,
	    w->gterm.exposeGC, 0, 0, w->core.width, w->core.height, 0, 0);
    }

    /* Pick best alpha font. 
    */
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

    /* Pick best dialog font. 
    */
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

    /* Create dialog save area pixmap. 
    */
    if (w->gterm.d_pixmap)
	XFreePixmap (w->gterm.display, w->gterm.d_pixmap);
    w->gterm.d_pixmap = XCreatePixmap (w->gterm.display, w->gterm.window,
	w->core.width, char_height, w->core.depth);
    w->gterm.d_saved = 0;

    /* Adjust cursor position to allow for change in window size. 
    */
    w->gterm.cur_x = w->gterm.cur_x * (int)w->core.width / w->gterm.old_width;
    w->gterm.cur_y = w->gterm.cur_y * (int)w->core.height / w->gterm.old_height;
    w->gterm.old_width = w->core.width;
    w->gterm.old_height = w->core.height;
    if (w->gterm.cursor_type == GtGinmodeCursor) {
	XWarpPointer (w->gterm.display, w->gterm.window, w->gterm.window,
	    0,0,0,0, w->gterm.cur_x, w->gterm.cur_y);
	update_cursor (w);
    }

    /* Raster descriptor 0 must track the window size and depth. 
    */
    if (w->gterm.rasters) {
	Raster rp = &w->gterm.rasters[0];
	rp->width  = w->core.width;
	rp->height = w->core.height;
	rp->depth  = w->core.depth;

        if (rp->shadow_pixmap)
	    XFreePixmap (w->gterm.display, rp->shadow_pixmap);
        rp->shadow_pixmap = XCreatePixmap (w->gterm.display, w->gterm.window,
	    w->core.width+1, w->core.height+1, RasterDepth);

        if (DBG_TRACE)
	    fprintf (stderr, "Resize: Raster 0 => %d x %d x %d\n", 
		w->core.width, w->core.height, w->core.depth);
    }
    

    /* Mark gterm widget ready for further client input. 
    */
    w->gterm.delay = 0;

    /* Call any resize callbacks.  Callbacks can delete or add callbacks,
     * so make a copy of the callback list first.
     */
    for (cb = w->gterm.resizeCallback, ncb=0;  cb;  cb = cb->next)
	cbl[ncb++] = *cb;
    for (i=0;  i < ncb;  i++)
	(*cbl[i].proc) (cbl[i].client_data, w);


    if (DBG_TRACE)
	fprintf (stderr, "Resize[%s] DONE:\n", dbg_wSize(w));
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

    if (!w || !XtIsRealized (gw))
	return;

    if (DBG_TRACE)
	fprintf (stderr, "Redisplay[%s]:  ENTER\n", dbg_wSize(w));

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

    if (DBG_TRACE)
	fprintf (stderr, "Redisplay[%s] DONE:\n", dbg_wSize(w));
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

    /* Deep Frame */
    if (!w->gterm.useDefaultCM && w->gterm.haveColormap &&
        !w->gterm.forcePseudo8) {

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
	request_colormap_focus (w);
    }

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




#include "GtermCnv.c"
#include "GtermGraphics.c"
#include "GtermImaging.c"
#include "GtermCmap.c"
#include "GtermMapping.c"
#include "GtermMarker.c"
#include "GtermUtil.c"
#include "GtermDebug.c"
