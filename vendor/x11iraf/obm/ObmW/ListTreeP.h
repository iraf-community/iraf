/*-----------------------------------------------------------------------------
** ListTree.c	A Specialized List widget
**
** Private header file
**
** Copyright (c) 1995 Robert W. McMullen
**
** Permission to use, copy, modify, distribute, and sell this software and its
** documentation for any purpose is hereby granted without fee, provided that
** the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  The author makes no representations about the suitability
** of this software for any purpose.  It is provided "as is" without express
** or implied warranty.
**
** THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
** ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT SHALL
** THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
** ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
** WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
** ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
** SOFTWARE.
*/

#ifndef _ListTreeP_H
#define _ListTreeP_H

#include <X11/Core.h>

#include "ListTree.h"
#define ListTreeRET_ALLOC 10

#define TIMER_CLEAR 0
#define TIMER_SINGLE 1
#define TIMER_DOUBLE 2
#define TIMER_WAITING 3

typedef struct {
	int		dummy;		/* keep compiler happy with dummy field */
} ListTreeClassPart;

typedef struct _ListTreeClassRec {
	CoreClassPart		core_class;
	ListTreeClassPart	ListTree_class;
} ListTreeClassRec;

extern ListTreeClassRec listtreeClassRec;

typedef struct {
	Pixmap		bitmap;
	Pixmap		pix;
	int		width,height;
	int		xoff;
} Pixinfo;

typedef struct {
	/* Public stuff ... */
	long		foreground_pixel;
	XFontStruct	*font;
	int		NumItems;
	Dimension	HSpacing;
	Dimension	VSpacing;
/*	Dimension	LabelSpacing;*/
	Dimension	Margin;
	Dimension	Indent;
	Pixinfo		Open;
	Pixinfo		Closed;
	Pixinfo		Leaf;
	Pixinfo		LeafOpen;
	Dimension	LineWidth;
	XtCallbackList  BranchCallback;
	XtCallbackList  LeafCallback;
	XtCallbackList  PathCallback;
	XtCallbackList  HighlightCallback;
	XtCallbackList  ActivateCallback;

	/* Private stuff ... */
	GC		drawGC;
	GC		eraseGC;
        GC              eorGC;
	GC		highlightGC;
	int		exposeTop,exposeBot;
	int		pixWidth;
	int		preferredWidth,preferredHeight;
	ListTreeItem	*first,		/* always points to a top level entry */
			*highlighted,
                        *drop_highlighted;

	XtIntervalId	timer_id;	/* timer for double click test */
	ListTreeItem	*timer_item;	/* item to make sure both clicks */
					/* occurred on the same item */
  int timer_type; /* flag for type of click that just happened */
	int		timer_y;
	int		timer_x;
	int		multi_click_time;

  ListTreeItem  **ret_item_list;
  int           ret_item_alloc;

	Boolean		Refresh;
} ListTreePart;

typedef struct _ListTreeRec {
	CorePart	core;
	ListTreePart	list;
} ListTreeRec;


#endif /* _ListTreeP_H */
