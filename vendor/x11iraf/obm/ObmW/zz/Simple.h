/*
 * $XConsortium: Simple.h,v 1.9 89/07/21 01:44:53 kit Exp $
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

#ifndef _Simple_h
#define _Simple_h

/****************************************************************
 *
 * Simple widgets
 *
 ****************************************************************/

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 cursor		     Cursor		Cursor		None
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	0
 insensitiveBorder   Insensitive	Pixmap		Gray
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	0
 x		     Position		Position	0
 y		     Position		Position	0

 shadowWidth	      ShadowWidth          Dimension     2
 topShadowPixel       TopShadowPixel       Pixel         dynamic
 bottomShadowPixel    BottomShadowPixel    Pixel         dynamic
 topShadowContrast    TopShadowContrast    Int           20
 bottomShadowContrast BottomShadowContrast Int           40
 userData             UserData             XtPointer     NULL
 beNiceToColormap     BeNiceToColormap     Boolean       False

*/

#define XtNcursor "cursor"
#define XtNinsensitiveBorder "insensitiveBorder"
#define XtNuserData "userData"
#define XtCUserData "UserData"

#define XtCInsensitive "Insensitive"

#define XtNtopShadowPixmap "topShadowPixmap"
#define XtCTopShadowPixmap "TopShadowPixmap"
#define XtNbottomShadowPixmap "bottomShadowPixmap"
#define XtCBottomShadowPixmap "BottomShadowPixmap"
#define XtNhighlightPixmap "highlightPixmap"
#define XtCHighlightPixmap "HighlightPixmap"
#define XtNhighlightThickness "highlightThickness"
#define XtCHighlightThickness "HighlightThickness"


#define XtNshadowWidth "shadowWidth"
#define XtCShadowWidth "ShadowWidth"
#define XtNtopShadowPixel "topShadowPixel"
#define XtCTopShadowPixel "TopShadowPixel"
#define XtNbottomShadowPixel "bottomShadowPixel"
#define XtCBottomShadowPixel "BottomShadowPixel"
#define XtNhighlightPixel "highlightPixel"
#define XtCHighlightPixel "HighlightPixel"

typedef struct _SimpleClassRec	*SimpleWidgetClass;
typedef struct _SimpleRec	*SimpleWidget;

extern WidgetClass simpleWidgetClass;

#endif /* _Simple_h */
