/* $XConsortium: Scrollbar.h,v 1.7 91/07/26 21:59:31 converse Exp $ */


/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#ifndef _Scrollbar_h
#define _Scrollbar_h

/****************************************************************
 *
 * Scrollbar Widget
 *
 ****************************************************************/

#include <X11/Xmu/Converters.h>
#include <X11/Xraw/XawInit.h>
#include <X11/Xraw/Simple.h>

/* Scrollbar resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 accelerators	     Accelerators	AcceleratorTable NULL
 ancestorSensitive   AncestorSensitive	Boolean		True
 background	     Background		Pixel		XtDefaultBackground
 backgroundPixmap    Pixmap		Pixmap		XtUnspecifiedPixmap
 borderColor	     BorderColor	Pixel		XtDefaultForeground
 borderPixmap	     Pixmap		Pixmap		XtUnspecifiedPixmap
 borderWidth	     BorderWidth	Dimension	1
 colormap	     Colormap		Colormap	parent's colormap
 cursor		     Cursor		Cursor		None
 cursorName	     Cursor		String		NULL
 depth		     Depth		int		parent's depth
 destroyCallback     Callback		XtCallbackList	NULL
 foreground	     Foreground		Pixel		XtDefaultForeground
 height		     Height		Dimension	length or thickness
 insensitiveBorder   Insensitive	Pixmap		GreyPixmap
 length		     Length		Dimension	1
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 minimumThumb	     MinimumThumb	Dimension	7
 orientation	     Orientation	XtOrientation	XtorientVertical
 screen		     Screen		Screen		parent's screen
 sensitive	     Sensitive		Boolean		True
 shown		     Shown		Float		0.0
 thickness	     Thickness		Dimension	14
 thumb		     Thumb		Bitmap		GreyPixmap
 topOfThumb	     TopOfThumb		Float		0.0
 translations	     Translations	TranslationTable see source or doc
 width		     Width		Dimension	thickness or length
 x		     Position		Position	0
 y		     Position		Position	0

*/

/* 
 * Most things we need are in StringDefs.h 
 */

#ifndef XtNincrement
#define XtNincrement  "increment"
#endif

#ifndef XtNpageIncrement
#define XtNpageIncrement  "pageIncrement"
#endif

#ifndef XtNrepeatDelay
#define XtNrepeatDelay  "repeatDelay"
#endif

#ifndef XtNshowArrows
#define XtNshowArrows "showArrows"
#endif

#ifndef XtNminimumThumb
#define XtNminimumThumb "minimumThumb"
#endif

#ifndef XtNtopOfThumb
#define XtNtopOfThumb "topOfThumb"
#endif

#ifndef XtNshown
#define XtNshown "shown"
#endif

#ifndef XtNincrementProc
#define XtNincrementProc "incrementProc"
#endif

#ifndef XtNdecrementProc
#define XtNdecrementProc "decrementProc"
#endif

#ifndef XtNvalueChangedProc
#define XtNvalueChangedProc "valueChangedProc"
#endif

#ifndef XtNpageIncrementProc
#define XtNpageIncrementProc "pageIncrementProc"
#endif

#ifndef XtNpageDecrementProc
#define XtNpageDecrementProc "pageDecrementProc"
#endif

#ifndef XtNdragProc
#define XtNdragProc "dragProc"
#endif

#ifndef XtNtoTopProc
#define XtNtoTopProc "toTopProc"
#endif

#ifndef XtNtoBottomProc
#define XtNtoBottomProc "toBottomProc"
#endif

#ifndef XtCShowArrows
#define XtCShowArrows "ShowArrows"
#endif

#ifndef XtCIncrement
#define XtCIncrement  "Increment"
#endif

#ifndef XtCMinimumThumb
#define XtCMinimumThumb "MinimumThumb"
#endif

#ifndef XtCRepeatDelay
#define XtCRepeatDelay  "RepeatDelay"
#endif

#ifndef XtCShown
#define XtCShown "Shown"
#endif

#ifndef XtCTopOfThumb
#define XtCTopOfThumb "TopOfThumb"
#endif

#ifndef XtNthumbColor
#define XtNthumbColor "thumbColor"
#endif

#ifndef XtCThumbColor
#define XtCThumbColor "ThumbColor"
#endif

#ifndef XtNstripeColor
#define XtNstripeColor "stripeColor"
#endif

#ifndef XtCStripeColor
#define XtCStripeColor "StripeColor"
#endif


enum XawScrollBarReasons{
  XawSB_INCREMENT = Xraw_SCROLLBAR,
  XawSB_DECREMENT,
  XawSB_PAGE_INCREMENT,
  XawSB_PAGE_DECREMENT,
  XawSB_VALUE_CHANGED,
  XawSB_TOP,
  XawSB_BOTTOM,
  XawSB_DRAG
};

typedef struct {
  int        reason;
  XEvent    *event;
  float      top;
  float      shown;
  Dimension  topLoc;
  Dimension  shownLength;
  Dimension  length;
}XawScrollBarCallbackStruct;

typedef struct _ScrollbarRec	  *ScrollbarWidget;
typedef struct _ScrollbarClassRec *ScrollbarWidgetClass;

extern WidgetClass scrollbarWidgetClass;

extern void XawScrollbarSetThumb(
#if NeedFunctionPrototypes
    Widget		/* scrollbar */,
#if NeedWidePrototypes
    /* float */ double	/* top */,
    /* float */	double	/* shown */
#else
    float		/* top */,
    float		/* shown */
#endif
#endif		 
);

#endif /* _Scrollbar_h */
