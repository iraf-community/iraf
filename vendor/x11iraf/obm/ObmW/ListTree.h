/*-----------------------------------------------------------------------------
** ListTree.c	A Specialized List widget
**
** Public header file
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

#ifndef _ListTree_H
#define _ListTree_H

#include <X11/Core.h>

#define _ListTree_WIDGET_VERSION	2.0

#define XtNmargin		"margin"
#define XtNindent		"indent"
#define XtNhorizontalSpacing	"horizontalSpacing"
#define XtNverticalSpacing	"verticalSpacing"
#define XtNlineWidth		"lineWidth"
#define XtNbranchPixmap		"branchPixmap"
#define XtNbranchOpenPixmap	"branchOpenPixmap"
#define XtNleafPixmap		"leafPixmap"
#define XtNleafOpenPixmap	"leafOpenPixmap"
#define XtNbranchCallback	"branchCallback"
#define XtNleafCallback		"leafCallback"
#define XtNpathCallback		"pathCallback"
#define XtNhighlightCallback	"highlightCallback"
#define XtNactivateCallback	"activateCallback"

#define XtBRANCH	1
#define XtLEAF		2

extern WidgetClass listtreeWidgetClass;

typedef struct _ListTreeClassRec *ListTreeWidgetClass;
typedef struct _ListTreeRec      *ListTreeWidget;

typedef struct _ListTreeItem {
	Boolean		open;
	Boolean		highlighted;
	char		*text;
	int		length;
	int		x,y,ytext;
	Dimension	height;
	struct _ListTreeItem 	*parent,
			*firstchild,
			*prevsibling,*nextsibling;
	XtPointer	user_data;
} ListTreeItem;

typedef struct _ListTreeReturnStruct {
	int		reason;
	ListTreeItem	*item;
	ListTreeItem	**path;
	int		count;
	Boolean		open;
} ListTreeReturnStruct;

typedef struct _ListTreeMultiReturnStruct {
	ListTreeItem	**items;
	int		count;
} ListTreeMultiReturnStruct;

typedef struct _ListTreeActivateStruct {
	int		reason;
	ListTreeItem	*item;
	Boolean		open;
	ListTreeItem	**path;
	int		count;
} ListTreeActivateStruct;

/*
** Public function declarations
*/
#ifndef _ListTree_
#if __STDC__ || defined(__cplusplus)
#define P_(s) s
#else
#define P_(s) ()
#endif

/* ListTree.c */
void ListTreeRefresh P_((Widget w));
void ListTreeRefreshOff P_((Widget w));
void ListTreeRefreshOn P_((Widget w));
ListTreeItem *ListTreeAdd P_((Widget w, ListTreeItem *parent, char *string));
void ListTreeRenameItem P_((Widget w, ListTreeItem *item, char *string));
int ListTreeDelete P_((Widget w, ListTreeItem *item));
int ListTreeDeleteChildren P_((Widget w, ListTreeItem *item));
int ListTreeReparent P_((Widget w, ListTreeItem *item, ListTreeItem *newparent));
int ListTreeReparentChildren P_((Widget w, ListTreeItem *item, ListTreeItem *newparent));
int ListTreeOrderSiblings P_((Widget w, ListTreeItem *item));
int ListTreeOrderChildren P_((Widget w, ListTreeItem *item));
ListTreeItem *ListTreeFindSiblingName P_((Widget w, ListTreeItem *item, char *name));
ListTreeItem *ListTreeFindChildName P_((Widget w, ListTreeItem *item, char *name));
void ListTreeHighlightItem P_((Widget w, ListTreeItem *item));
ListTreeItem *ListTreeFirstItem P_((Widget w));
#ifdef USE_RDD
void ListTreeHighlightDrop P_((Widget w, XEvent *event, String *params, Cardinal *num_params));
ListTreeReturnStruct *ListTreeGetDrop P_((Widget w));
#endif

#undef P_
#endif

#endif /* _ListTree_H */
