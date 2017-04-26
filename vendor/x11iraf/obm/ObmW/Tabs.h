/* $Id: Tabs.h,v 1.6 1999/09/22 19:54:14 falk Exp $
 *
 * This widget manages one or more child widgets, exactly one of which is
 * visible.  Above the child widgets is a graphic that looks like index
 * tabs from file folders.  Each tab corresponds to one of the child widgets.
 * By clicking on a tab, the user can bring the corresponding widget to
 * the top of the stack.
 */


#ifndef _Tabs_h
#define _Tabs_h

#include <X11/Constraint.h>


/***********************************************************************
 *
 * Tabs Widget (subclass of CompositeClass)
 *
 ***********************************************************************/

/* Parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 font		     Font		XFontStruct*	XtDefaultFont
 internalWidth	     Width		Dimension	4	*1
 internalHeight	     Height		Dimension	2	*1
 topWidget	     TopWidget		Widget			*2
 callback	     Callback		XtCallbackList	NULL	*3
 popdownCallback     Callback		XtCallbackList	NULL	*4
 selectInsensitive   SelectInsensitive	Boolean		True	*5
 beNiceToColormap    BeNiceToColormap	Boolean		False	*6
 topShadowContrast   TopShadowContrast	int		20
 bottomShadowContrast BottomShadowContrast int		40
 insensitiveContrast InsensitiveContrast int		33	*7

 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 hSpace 	     HSpace		Dimension	4
 height		     Height		Dimension	0
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 orientation	     Orientation	XtOrientation	vertical
 vSpace 	     VSpace		Dimension	4
 width		     Width		Dimension	0
 x		     Position		Position	0
 y		     Position		Position	0

 Notes:

 1 internalWidth, internalHeight specify the margins around the text
   in the tabs.
 2 topWidget identifies the widget which is currently visible.
 3 callbacks are called whenever the user selects a tab.  Call_data is
   the new top widget.
 4 popdownCallbacks are called whenever the user selects a tab.  Call_data is
   the old (no longer visible) top widget.  Note that popdownCallbacks
   are called before callbacks.
 5 SelectInsensitive determines whether or not insensitive children may
   be selected anyway.
 6 BeNiceToColormap causes the Tabs widget to use fewer colors.
 7 InsensitiveContrast sets the contrast used for labels of insensitive widgets.

*/

/* Constraint parameters:
 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 tabLabel	     Label		String		widget name
 tabLeftBitmap	     LeftBitmap		Pixmap		None
 tabForeground	     Foreground		Pixel		XtDefaultForeground
 resizable	     Resizable		Boolean		False
*/

/* New fields */

#ifndef	XtNtabLabel
#define	XtNtabLabel		"tabLabel"
#define	XtNtabForeground	"tabForeground"
#endif

#ifndef	XtNtabLeftBitmap
#define	XtNtabLeftBitmap	"tabLeftBitmap"
#endif

#ifndef	XtCLeftBitmap
#define	XtCLeftBitmap	"LeftBitmap"
#endif

#ifndef	XtCResizable
#define	XtCResizable	"Resizable"
#endif

#ifndef	XtNselectInsensitive
#define	XtNselectInsensitive	"selectInsensitive"
#define	XtCSelectInsensitive	"SelectInsensitive"
#endif

#ifndef	XtNnlabels
#define	XtNnlabels	"nlabels"
#define	XtCNLabels	"NLabels"
#endif
#ifndef	XtNlabels
#define	XtNlabels	"labels"
#define	XtCLabels	"Labels"
#endif

#ifndef	XtNtopWidget
#define	XtNtopWidget	"topWidget"
#define	XtCTopWidget	"TopWidget"
#endif

#ifndef	XtNhSpace
#define	XtNhSpace	"hSpace"
#define	XtCHSpace	"HSpace"
#define	XtNvSpace	"vSpace"
#define	XtCVSpace	"VSpace"
#endif

#ifndef	XtNresizable
#define	XtNresizable	"resizable"
#endif

#ifndef	XtNinsensitiveContrast
#define	XtNinsensitiveContrast	"insensitiveContrast"
#define	XtCInsensitiveContrast	"InsensitiveContrast"
#endif

#ifndef	XtNshadowWidth
#define XtNshadowWidth "shadowWidth"
#define XtCShadowWidth "ShadowWidth"
#define XtNtopShadowPixel "topShadowPixel"
#define XtCTopShadowPixel "TopShadowPixel"
#define XtNbottomShadowPixel "bottomShadowPixel"
#define XtCBottomShadowPixel "BottomShadowPixel"
#define XtNtopShadowContrast "topShadowContrast"
#define XtCTopShadowContrast "TopShadowContrast"
#define XtNbottomShadowContrast "bottomShadowContrast"
#define XtCBottomShadowContrast "BottomShadowContrast"
#endif

#ifndef	XtNtopShadowPixmap
#define	XtNtopShadowPixmap	"topShadowPixmap"
#define	XtCTopShadowPixmap	"TopShadowPixmap"
#define	XtNbottomShadowPixmap	"bottomShadowPixmap"
#define	XtCBottomShadowPixmap	"BottomShadowPixmap"
#endif

#ifndef	XtNbeNiceToColormap
#define XtNbeNiceToColormap "beNiceToColormap"
#define XtCBeNiceToColormap "BeNiceToColormap"
#define XtNbeNiceToColourmap "beNiceToColormap"
#define XtCBeNiceToColourmap "BeNiceToColormap"
#endif

/* Class record constants */

extern WidgetClass tabsWidgetClass;

typedef struct _TabsClassRec *TabsWidgetClass;
typedef struct _TabsRec      *TabsWidget;

_XFUNCPROTOBEGIN

extern	void
XawTabsSetTop(
#if NeedFunctionPrototypes
	Widget	w,
	Bool	callCallbacks
#endif
) ;

extern	void
XawTabsSetHighlight(
#if NeedFunctionPrototypes
	Widget	tabs,
	Widget	w
#endif
) ;

_XFUNCPROTOEND

#endif /* _Tabs_h */
