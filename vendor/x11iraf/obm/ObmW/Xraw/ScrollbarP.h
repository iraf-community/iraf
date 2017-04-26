/*
 * $XConsortium: ScrollbarP.h,v 1.2 90/04/11 17:33:53 jim Exp $
 */


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

#ifndef _ScrollbarP_h
#define _ScrollbarP_h

#include <X11/Xraw/Scrollbar.h>
#include <X11/Xraw/SimpleP.h>
#include <X11/Xraw/Arrow.h>

#define BAR(w)       ((ScrollbarWidget)w)->scrollbar

typedef struct {
    /* public */
    Pixel	   foreground;	/* thumb foreground color */
    XtOrientation  orientation;	/* horizontal or vertical */
    float	   top;		/* What percent is above the win's top */
    float	   shown;	/* What percent is shown in the win */
    Dimension	   along;	/* either height or width */
    Dimension	   cross;	
    Dimension	   min_thumb;	/* minium size for the thumb. */
    Boolean        showArrows;
    Dimension      increment;
    Dimension      page_increment;
    int            delay;
    XtCallbackList incrementProc;
    XtCallbackList decrementProc;
    XtCallbackList pageIncrementProc;
    XtCallbackList pageDecrementProc;
    XtCallbackList valueChangedProc;
    XtCallbackList dragProc;
    XtCallbackList toTopProc;
    XtCallbackList toBottomProc;


    XtCallbackList scrollProc;	/* proportional scroll */
    XtCallbackList thumbProc;	/* jump (to position) scroll */
    XtCallbackList jumpProc;	/* same as thumbProc but pass data by ref */

    /* private */

    int                scrolling;  /* a scrollbar status: FREE|BESY|MOVE */
    GC		       gc;         /* a (shared) gc */
    int	               topLoc;	   /* Pixel that corresponds to top */
    Dimension	       shownLength;/* Num pixels corresponding to shown */

    Dimension          direction;

    int                margin;
    int                newLoc;
    int                shift;
    XtIntervalId       timer;
    XawDrawArrowStruct top_arrow;
    XawDrawArrowStruct bot_arrow;
} ScrollbarPart;

typedef struct _ScrollbarRec {
    CorePart		core;
    SimplePart		simple;
    ScrollbarPart	scrollbar;
} ScrollbarRec;

typedef struct {int empty;} ScrollbarClassPart;

typedef struct _ScrollbarClassRec {
    CoreClassPart		core_class;
    SimpleClassPart		simple_class;
    ScrollbarClassPart		scrollbar_class;
} ScrollbarClassRec;

extern ScrollbarClassRec scrollbarClassRec;

#endif /* _ScrollbarP_h */
