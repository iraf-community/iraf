/*-----------------------------------------------------------------------------
** ListTree.c	A Specialized List widget
**
** Widget source code
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

#define _ListTree_

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include <stdio.h>
/*#include <stdlib.h> */

#include "ListTreeP.h"
#ifdef USE_RDD
#include "rdd.h"
#endif


#define USE_FOLDERS

#ifdef USE_FOLDERS
#define folder_width 16
#define folder_height 12
static unsigned char folder_bits[] =
{
  0x00, 0x1f, 0x80, 0x20, 0x7c, 0x5f, 0x02, 0x40, 0x02, 0x40, 0x02, 0x40,
  0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 0x02, 0x40, 0xfc, 0x3f,
};

#define folderopen_width 16
#define folderopen_height 12
static unsigned char folderopen_bits[] =
{
  0x00, 0x3e, 0x00, 0x41, 0xf8, 0xd5, 0xac, 0xaa, 0x54, 0xd5, 0xfe, 0xaf,
  0x01, 0xd0, 0x02, 0xa0, 0x02, 0xe0, 0x04, 0xc0, 0x04, 0xc0, 0xf8, 0x7f,
};

#define document_width 9
#define document_height 14
static unsigned char document_bits[] =
{
  0x1f, 0x00, 0x31, 0x00, 0x51, 0x00, 0x91, 0x00, 0xf1, 0x01, 0x01, 0x01,
  0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
  0x01, 0x01, 0xff, 0x01,};

#else
#define folder_width 12
#define folder_height 12
static unsigned char folder_bits[] = {
   0xff, 0x0f, 0x01, 0x08, 0x61, 0x08, 0x61, 0x08, 0x61, 0x08, 0xfd, 0x0b,
   0xfd, 0x0b, 0x61, 0x08, 0x61, 0x08, 0x61, 0x08, 0x01, 0x08, 0xff, 0x0f};

#define folderopen_width 12
#define folderopen_height 12
static unsigned char folderopen_bits[] = {
   0xff, 0x0f, 0x01, 0x08, 0x01, 0x08, 0x01, 0x08, 0x01, 0x08, 0xfd, 0x0b,
   0xfd, 0x0b, 0x01, 0x08, 0x01, 0x08, 0x01, 0x08, 0x01, 0x08, 0xff, 0x0f};

#define document_width 12
#define document_height 12
static unsigned char document_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x00, 0xf0, 0x01, 0xf8, 0x03,
   0xf8, 0x03, 0xf8, 0x03, 0xf0, 0x01, 0xe0, 0x00, 0x00, 0x00, 0x00, 0x00};

#endif

#define offset(field) XtOffsetOf(ListTreeRec, list.field)
static XtResource resources[] =
{
  {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
   offset(foreground_pixel), XtRString, XtDefaultForeground},
  {XtNmargin, XtCMargin, XtRDimension, sizeof(Dimension),
   offset(Margin), XtRString, "2"},
  {XtNindent, XtCMargin, XtRDimension, sizeof(Dimension),
   offset(Indent), XtRString, "0"},
  {XtNhorizontalSpacing, XtCMargin, XtRDimension, sizeof(Dimension),
   offset(HSpacing), XtRString, "2"},
  {XtNverticalSpacing, XtCMargin, XtRDimension, sizeof(Dimension),
   offset(VSpacing), XtRString, "0"},
  {XtNlineWidth, XtCMargin, XtRDimension, sizeof(Dimension),
   offset(LineWidth), XtRString, "0"},
  {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
   offset(font), XtRString, XtDefaultFont},
  {XtNbranchPixmap, XtCPixmap, XtRBitmap, sizeof(Pixmap),
   offset(Closed.bitmap), XtRImmediate, (XtPointer) XtUnspecifiedPixmap},
  {XtNbranchOpenPixmap, XtCPixmap, XtRBitmap, sizeof(Pixmap),
   offset(Open.bitmap), XtRImmediate, (XtPointer) XtUnspecifiedPixmap},
  {XtNleafPixmap, XtCPixmap, XtRBitmap, sizeof(Pixmap),
   offset(Leaf.bitmap), XtRImmediate, (XtPointer) XtUnspecifiedPixmap},
  {XtNleafOpenPixmap, XtCPixmap, XtRBitmap, sizeof(Pixmap),
   offset(LeafOpen.bitmap), XtRImmediate, (XtPointer) XtUnspecifiedPixmap},
  {XtNleafCallback, XtCCallback, XtRCallback, sizeof(XtPointer),
   offset(LeafCallback), XtRCallback, NULL},
  {XtNbranchCallback, XtCCallback, XtRCallback, sizeof(XtPointer),
   offset(BranchCallback), XtRCallback, NULL},
  {XtNpathCallback, XtCCallback, XtRCallback, sizeof(XtPointer),
   offset(PathCallback), XtRCallback, NULL},
  {XtNhighlightCallback, XtCCallback, XtRCallback, sizeof(XtPointer),
   offset(HighlightCallback), XtRCallback, NULL},
  {XtNactivateCallback, XtCCallback, XtRCallback, sizeof(XtPointer),
   offset(ActivateCallback), XtRCallback, NULL},
};

#undef offset

static void Initialize();
static void Destroy();
static void Redisplay();
static void Resize();
static void ChangeSize();
static XtGeometryResult QueryGeometry();
static Boolean SetValues();
static void Draw();
static void DrawAll();
static void DrawItemHighlight(), DrawItemHighlightClear();
static ListTreeItem *GetItem();
static void Select(), Notify(), Unset(), Extend();
static void DeleteChildren();
static Boolean Layout();

#ifdef USE_RDD
static void StartDrag(), EndDrag(), Drop();

#endif

#ifdef USE_RDD
static char defaultTranslations[] =
"<Btn2Down>:		StartDrag()\n\
<Btn2Motion>:		rddDragAction()\n\
<Btn2Up>:		EndDrag()\n\
<Btn1Up>:		Notify()\n\
<Btn1Down>:		Select()\n\
Button1 <Btn1Motion>:	Extend()";

#else
static char defaultTranslations[] =
"<Btn1Down>:		Select()\n\
<Btn1Up>:		Notify()\n\
Button1 <Btn1Motion>:	Extend()";

#endif

static XtActionsRec actions[] =
{
  {"Notify", Notify},
  {"Select", Select},
  {"Extend", Extend},
  {"Unset", Unset},
#ifdef USE_RDD
  {"StartDrag", StartDrag},
  {"EndDrag", EndDrag},
#endif
};

ListTreeClassRec listtreeClassRec =
{
  {
	/* core_class fields     */
	/* superclass            */ (WidgetClass) & widgetClassRec,
	/* class_name            */ "ListTree",
	/* widget_size           */ sizeof(ListTreeRec),
	/* class_initialize      */ NULL,
	/* class_part_initialize */ NULL,
	/* class_inited          */ False,
	/* initialize            */ Initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XtInheritRealize,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressMultiple,
	/* compress_enterleave   */ True,
	/* visible_interest      */ True,
	/* destroy               */ Destroy,
	/* resize                */ Resize,
	/* expose                */ Redisplay,
	/* set_values            */ SetValues,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback_private      */ NULL,
	/* tm_table              */ defaultTranslations,
	/* query_geometry        */ XtInheritQueryGeometry,
	/* display_accelerator   */ XtInheritDisplayAccelerator,
	/* extension             */ NULL
  },
  {
        /* some stupid compilers barf on empty structures */ 0
  }
};

WidgetClass listtreeWidgetClass = (WidgetClass) & listtreeClassRec;

static void
MakePixmap(w, pix)
ListTreeWidget w;
Pixinfo *pix;
{
  Window root;
  int x, y;
  unsigned int width, height, bw, depth;

  if (pix->bitmap && XGetGeometry(XtDisplay((Widget) w), pix->bitmap,
			     &root, &x, &y, &width, &height, &bw, &depth)) {
    pix->width = (int) width;
    pix->height = (int) height;
    if (depth == 1) {
      pix->pix = XmuCreatePixmapFromBitmap(XtDisplay((Widget) w),
				   RootWindowOfScreen(XtScreen((Widget) w)),
					   pix->bitmap,
					   width, height, w->core.depth,
					   w->list.foreground_pixel,
					   w->core.background_pixel);
    }
    else
      pix->pix = pix->bitmap;
  }
  else {
    pix->width = pix->height = 0;
    pix->pix = (Pixmap) NULL;
  }
}

static void
FreePixmap(w, pix)
ListTreeWidget w;
Pixinfo *pix;
{
  if (pix->pix)
    XFreePixmap(XtDisplay((Widget) w), pix->pix);
}


static void
Initialize(treq, tnew, args, num)
Widget treq, tnew;
ArgList args;
Cardinal *num;
{
  ListTreeWidget new;
  XGCValues values;
  XtGCMask mask;

  new = (ListTreeWidget) tnew;

  values.line_style = LineSolid;
  values.line_width = new->list.LineWidth;
  values.fill_style = FillSolid;
  values.font = new->list.font->fid;
  values.background = new->core.background_pixel;
  values.foreground = new->list.foreground_pixel;

  mask = GCLineStyle | GCLineWidth | GCFillStyle | GCForeground | GCBackground | GCFont;
  new->list.drawGC = XtGetGC((Widget) new, mask, &values);

  values.function = GXinvert;
  mask = GCLineStyle | GCLineWidth | GCFillStyle | GCForeground | GCBackground | GCFont | GCFunction;
  new->list.eorGC = XtGetGC((Widget) new, mask, &values);

  values.background = new->list.foreground_pixel;
  values.foreground = new->core.background_pixel;
  mask = GCLineStyle | GCLineWidth | GCFillStyle | GCForeground | GCBackground | GCFont;
  new->list.highlightGC = XtGetGC((Widget) new, mask, &values);

  new->list.ret_item_list=NULL;
  new->list.ret_item_alloc=0;
  new->list.first = new->list.highlighted = NULL;
  new->list.Refresh = True;

  new->list.timer_id = (XtIntervalId) 0;
  new->list.multi_click_time = XtGetMultiClickTime(XtDisplay((Widget) new));

  if (new->list.Closed.bitmap == XtUnspecifiedPixmap)
    new->list.Closed.bitmap = XCreateBitmapFromData(XtDisplay((Widget) new),
				 RootWindowOfScreen(XtScreen((Widget) new)),
			         (char *)folder_bits,
				 folder_width, folder_height);
  MakePixmap(new, &new->list.Closed);

  if (new->list.Open.bitmap == XtUnspecifiedPixmap)
    new->list.Open.bitmap = XCreateBitmapFromData(XtDisplay((Widget) new),
				 RootWindowOfScreen(XtScreen((Widget) new)),
		  	 	 (char *)folderopen_bits,
				 folderopen_width, folderopen_height);
  MakePixmap(new, &new->list.Open);

  if (new->list.Leaf.bitmap == XtUnspecifiedPixmap)
    new->list.Leaf.bitmap = XCreateBitmapFromData(XtDisplay((Widget) new),
				 RootWindowOfScreen(XtScreen((Widget) new)),
				 (char *)document_bits,
				 document_width, document_height);
  MakePixmap(new, &new->list.Leaf);

  if (new->list.LeafOpen.bitmap == XtUnspecifiedPixmap)
    new->list.LeafOpen.bitmap = XCreateBitmapFromData(XtDisplay((Widget) new),
				 RootWindowOfScreen(XtScreen((Widget) new)),
				 (char *)document_bits,
				 document_width, document_height);
  MakePixmap(new, &new->list.LeafOpen);

  new->list.pixWidth = new->list.Closed.width;
  if (new->list.Open.width > new->list.pixWidth)
    new->list.pixWidth = new->list.Open.width;
  if (new->list.Leaf.width > new->list.pixWidth)
    new->list.pixWidth = new->list.Leaf.width;
  if (new->list.LeafOpen.width > new->list.pixWidth)
    new->list.pixWidth = new->list.LeafOpen.width;
  new->list.Closed.xoff = (new->list.pixWidth - new->list.Closed.width) / 2;
  new->list.Open.xoff = (new->list.pixWidth - new->list.Open.width) / 2;
  new->list.Leaf.xoff = (new->list.pixWidth - new->list.Leaf.width) / 2;
  new->list.LeafOpen.xoff = (new->list.pixWidth - new->list.LeafOpen.width) / 2;
  
  if (new->core.height<2) new->core.height=2;
  if (new->core.width<2) new->core.width=2;
}

static void
Destroy(w)
ListTreeWidget w;
{
  ListTreeItem *item, *sibling;

  XtReleaseGC((Widget) w, w->list.drawGC);
  XtReleaseGC((Widget) w, w->list.highlightGC);
  item = w->list.first;
  while (item) {
    if (item->firstchild) {
      DeleteChildren(w, item->firstchild);
    }
    sibling = item->nextsibling;
    XtFree((char *) item->text);
    XtFree((char *) item);
    item = sibling;
  }
  FreePixmap(w, &w->list.Closed);
  FreePixmap(w, &w->list.Open);
  FreePixmap(w, &w->list.Leaf);
  FreePixmap(w, &w->list.LeafOpen);
}

static void
Redisplay(w, event, region)
Widget w;
XExposeEvent *event;
Region region;
{
  ListTreeWidget lw = (ListTreeWidget) w;

  if (!XtIsRealized(w))
    return;

  if (event) {
    Draw(lw, (Boolean) True, (int) event->y, (int) event->height);
  }
  else {			/* event==NULL ==> repaint the entire list */
    DrawAll(lw);
  }
}

static Boolean 
SetValues(current, request, new, args, nargs)
Widget current, request, new;
ArgList args;
Cardinal *nargs;
{
  if (!XtIsRealized(current))
    return False;

  return True;
}


static void 
Resize(w)
Widget w;
{
  ListTreeWidget lw = (ListTreeWidget) w;
  Dimension width, height;

  if (!XtIsRealized(w))
    return;
  width = lw->list.preferredWidth;
  height = lw->list.preferredHeight;

/*      w->core.width=width; */
/*      w->core.height=height; */

  if (Layout(w, FALSE, FALSE, &width, &height))
    XtAppWarning(XtWidgetToApplicationContext(w),
     "ListTree Widget: Size changed when it shouldn't have when resising.");
}

/* Display functions */

#define FontHeight(f)  (int)(f->max_bounds.ascent + f->max_bounds.descent)
#define FontDescent(f) (int)(f->max_bounds.descent)
#define FontAscent(f)  (int)(f->max_bounds.ascent)
#define FontTextWidth(f,c) (int)XTextWidth(f, c, strlen(c))

static void
PreferredSize(lw, w, h)
ListTreeWidget lw;
Dimension *w, *h;
{
  *w = (Dimension) lw->list.preferredWidth;
  *h = (Dimension) lw->list.preferredHeight;
}

/* #define DEBUG_GEOM */
static void
ChangeSize(w)
ListTreeWidget w;
{
  XtWidgetGeometry request, reply;

  request.request_mode = CWHeight | CWWidth;
  request.height = (Dimension) w->list.preferredHeight;
  request.width = (Dimension) w->list.preferredWidth;

/*      printf("requesting w=%d h=%d\n",request.width,request.height); */
/*      XtMakeGeometryRequest((Widget)w, &request, &reply); */
/*      printf("replying w=%d h=%d\n",reply.width,reply.height); */
/*      return; */

#ifdef DEBUG_GEOM
  printf("requesting w=%d h=%d\n", request.width, request.height);
#endif
  switch (XtMakeGeometryRequest((Widget) w, &request, &reply)) {
  case XtGeometryYes:
  case XtGeometryNo:
#ifdef DEBUG_GEOM
    printf("Yes/No: replying w=%d h=%d\n", reply.width, reply.height);
#endif
    break;
  case XtGeometryAlmost:
#ifdef DEBUG_GEOM
    printf("Almost: replying w=%d h=%d\n", reply.width, reply.height);
#endif
    PreferredSize(w, &(reply.width), &(reply.height));
    request = reply;
    switch (XtMakeGeometryRequest((Widget) w, &request, &reply)) {
    case XtGeometryYes:
    case XtGeometryNo:
      break;
    case XtGeometryAlmost:
#ifdef DEBUG_GEOM
      printf("Almost again: replying w=%d h=%d\n", reply.width, reply.height);
#endif
      request = reply;
      request.request_mode = CWHeight | CWWidth;
      XtMakeGeometryRequest((Widget) w, &request, &reply);
      break;
    default:
      break;
    }
    break;
  default:
    break;
  }
}

#ifdef OLD_QUERY
static XtGeometryResult
oldQueryGeometry(widget, intended, reply)
Widget widget;
XtWidgetGeometry *intended, *reply;
{
  ListTreeWidget lw = (ListTreeWidget) widget;
  Dimension w, h;

/*#ifdef DEBUG_GEOM */
/*printf("Querying geometry...\n"); */
/*#endif */
/**/
/*  if (intended->request_mode & (~(CWWidth | CWHeight))) */
/*      return  (XtGeometryYes); */

  PreferredSize(lw, &w, &h);

  if (w == 0)
    w = lw->core.width;
  if (h == 0)
    h = lw->core.height;

#ifdef DEBUG_GEOM
  printf("Querying geometry...  intended=(%d,%d) preferred=(%d,%d)\n",
	 (int) intended->width, (int) intended->height,
	 (int) w, (int) h);
#endif

  if (intended->request_mode & CWWidth &&
      intended->width == w &&
      intended->request_mode & CWHeight &&
      intended->height == h)
    return (XtGeometryYes);

  if (w == widget->core.width &&
      h == widget->core.height)
    return (XtGeometryNo);

  reply->request_mode = CWWidth | CWHeight;
  reply->width = w;
  reply->height = h;
  return XtGeometryAlmost;
}
#endif

static Boolean
Layout(w, xfree, yfree, width, height)
Widget w;
Boolean xfree, yfree;
Dimension *width, *height;
{
  ListTreeWidget lw = (ListTreeWidget) w;
  Boolean change = FALSE;

#ifdef DEBUG_GEOM
  printf("Layout...  xfree=%d yfree=%d size=(%d,%d)\n",
	 (int) xfree, (int) yfree, (int) (*width), (int) (*height));
#endif
/*
 * If both width and height are free to change the use default_cols
 * to determine the number columns and set new width and height to
 * just fit the window.
 */

  if (xfree && yfree) {
    *width = lw->list.preferredWidth;
    *height = lw->list.preferredHeight;
    change = TRUE;
  }

/* 
 * If the width is fixed then use it to determine the number of columns.
 * If the height is free to move (width still fixed) then resize the height
 * of the widget to fit the current list exactly.
 */
  else if (!xfree) {
    if (yfree) {
      *height = lw->list.preferredHeight;
      change = TRUE;
    }
  }

/* 
 * The last case is xfree and !yfree we use the height to determine
 * the number of rows and then set the width to just fit the resulting
 * number of columns.
 */
  else if (!yfree) {		/* xfree must be TRUE. */
    *width = lw->list.preferredWidth;
    change = TRUE;
  }
  return (change);
}

static XtGeometryResult
QueryGeometry(lw, parent_idea, our_idea)
ListTreeWidget lw;
XtWidgetGeometry *parent_idea, *our_idea;
{
  Dimension nw, nh;
  Boolean parent_wants_w, parent_wants_h, we_changed_size;

  parent_wants_w = (parent_idea->request_mode) & CWWidth;
  parent_wants_h = (parent_idea->request_mode) & CWHeight;

  if (parent_wants_w)
    nw = parent_idea->width;
  else
    nw = (Dimension) lw->list.preferredWidth;

  if (parent_wants_h)
    nh = parent_idea->height;
  else
    nh = (Dimension) lw->list.preferredHeight;

#ifdef DEBUG_GEOM
  printf("Querying geometry...  intended=(%d,%d) preferred=(%d,%d)\n",
	 (int) parent_idea->width, (int) parent_idea->height,
	 (int) nw, (int) nh);
#endif

  our_idea->request_mode = 0;
  if (!parent_wants_w && !parent_wants_h)
    return (XtGeometryYes);

  we_changed_size = Layout(lw, !parent_wants_w, !parent_wants_h, &nw, &nh);
  our_idea->request_mode |= (CWWidth | CWHeight);
  our_idea->width = nw;
  our_idea->height = nh;

  if (we_changed_size)
    return (XtGeometryAlmost);
  else
    return (XtGeometryYes);
}				/* End PreferredGeometry */


/* DEBUGGING FUNCTIONS */

#ifdef DEBUG_TREE
void
ItemCheck(ListTreeWidget w, ListTreeItem * item)
{
  ListTreeItem *p;
  char text[1024];

  p = item;
/*      if (p->parent) fprintf(stderr,"%x %x \t",p,p->parent); */
/*      else fprintf(stderr,"%x 00000000 \t",p); */
/*      while (p) { fprintf(stderr," "); p=p->parent; } */
/*      p=item; */
/*      while (p) { */
/*              fprintf(stderr,"%s/",p->text); */
/*              p=p->parent; */
/*      } */
/*      fprintf(stderr,"\n"); */

  if (strcmp(item->text, "pixmaps") == 0) {
    fprintf(stderr, "parent:      %x\n", item->parent);
    fprintf(stderr, "firstchild:  %x\n", item->firstchild);
    fprintf(stderr, "prevsibling: %x\n", item->prevsibling);
    fprintf(stderr, "nextsibling: %x\n", item->nextsibling);
  }
}

void
ChildrenCheck(ListTreeWidget w, ListTreeItem * item)
{
  while (item) {
    ItemCheck(w, item);
    if (item->firstchild)
      ChildrenCheck(w, item->firstchild);
    item = item->nextsibling;
  }
}

void
TreeCheck(ListTreeWidget w, char *txt)
{
  ListTreeItem *item;

  fprintf(stderr, "\n\n%s\n", txt);
  item = w->list.first;
  while (item) {
    ItemCheck(w, item);
    if (item->firstchild)
      ChildrenCheck(w, item->firstchild);
    item = item->nextsibling;
  }
}
#else
#define TreeCheck(a,b)
#endif

/* Highlighting Utilities ----------------------------------------------- */

static void
HighlightItem(ListTreeWidget w, ListTreeItem * item, Boolean state, Boolean draw)
{
  if (item) {
    if (item == w->list.highlighted && !state) {
      w->list.highlighted = NULL;
      if (draw)
	DrawItemHighlightClear(w, item);
    }
    else if (state != item->highlighted) {
      /*      printf("Highlighting '%s' state=%d x=%d y=%d\n", item->text, draw, item->x, item->ytext); */
      item->highlighted = state;
      if (draw)
	DrawItemHighlightClear(w, item);
    }
  }
}

static void
HighlightChildren(ListTreeWidget w, ListTreeItem * item, Boolean state, Boolean draw)
{
  while (item) {
    HighlightItem(w, item, state, draw);
    if (item->firstchild) {
      Boolean drawkids;

      if (item->open)
	drawkids = draw;
      else
	drawkids = False;
      HighlightChildren(w, item->firstchild, state, drawkids);
    }
    item = item->nextsibling;
  }
}

static void
HighlightAll(ListTreeWidget w, Boolean state, Boolean draw)
{
  ListTreeItem *item;

  item = w->list.first;
  while (item) {
    HighlightItem(w, item, state, draw);
    if (item->firstchild) {
      Boolean drawkids;

      if (item->open)
	drawkids = draw;
      else
	drawkids = False;
      HighlightChildren(w, item->firstchild, state, drawkids);
    }
    item = item->nextsibling;
  }
}

static void
HighlightVisibleChildren(ListTreeWidget w, ListTreeItem * item, Boolean state, Boolean draw)
{
  while (item) {
    HighlightItem(w, item, state, draw);
    if (item->firstchild && item->open) {
      HighlightVisibleChildren(w, item->firstchild, state, draw);
    }
    item = item->nextsibling;
  }
}

static void
HighlightAllVisible(ListTreeWidget w, Boolean state, Boolean draw)
{
  ListTreeItem *item;

  item = w->list.first;
  while (item) {
    HighlightItem(w, item, state, draw);
    if (item->firstchild && item->open) {
      HighlightVisibleChildren(w, item->firstchild, state, draw);
    }
    item = item->nextsibling;
  }
}

static void
AddItemToReturnList(ListTreeWidget w, ListTreeItem * item,int loc)
{
  if (loc>=w->list.ret_item_alloc) {
    w->list.ret_item_alloc+=ListTreeRET_ALLOC;
    w->list.ret_item_list=(ListTreeItem **)XtRealloc((char *)w->list.ret_item_list,w->list.ret_item_alloc*sizeof(ListTreeItem *));
  }
  w->list.ret_item_list[loc]=item;
}

static void
MultiAddToReturn(ListTreeWidget w, ListTreeItem * item, ListTreeMultiReturnStruct * ret)
{
  AddItemToReturnList(w,item,ret->count);
  ret->items=w->list.ret_item_list;
  ret->count++;
}

static void
HighlightCount(ListTreeWidget w, ListTreeItem * item, ListTreeMultiReturnStruct * ret)
{
  while (item) {
    if (item->highlighted)
      MultiAddToReturn(w,item,ret);
    if (item->firstchild && item->open)
      HighlightCount(w,item->firstchild,ret);
    item = item->nextsibling;
  }
}

static void
MakeMultiCallbackStruct(ListTreeWidget w,ListTreeMultiReturnStruct * ret)
{
  ListTreeItem *item;

  ret->items=NULL;
  ret->count = 0;
  item = w->list.first;
  while (item) {
    if (item->highlighted)
      MultiAddToReturn(w,item,ret);
    if (item->firstchild && item->open)
      HighlightCount(w,item->firstchild,ret);
    item = item->nextsibling;
  }
}

static void
HighlightDoCallback(ListTreeWidget w)
{
  ListTreeMultiReturnStruct ret;

  if (w->list.HighlightCallback) {
    MakeMultiCallbackStruct(w,&ret);
    XtCallCallbacks((Widget) w, XtNhighlightCallback, &ret);
  }
}

/* Events ------------------------------------------------------------------ */


static ListTreeReturnStruct *
MakeV1CallbackStruct(w, item)
ListTreeWidget w;
ListTreeItem *item;
{
  ListTreeItem *parent;
  ListTreeReturnStruct *ret;
  int count, size;
  char *ptr;

  TreeCheck(w, "in MakeV1CallbackStruct");
  count = 1;
  parent = item;
  while (parent->parent) {
    parent = parent->parent;
    count++;
  }
  size = sizeof(ListTreeReturnStruct) + ((count + 1) * sizeof(ListTreeItem *));
  ptr = (char *) XtMalloc(size);
  ret = (ListTreeReturnStruct *) ptr;
  ret->path = (ListTreeItem **) (ptr +
				 (((sizeof(ListTreeReturnStruct) +
				    sizeof(ListTreeItem *) - 1) /
				   sizeof(ListTreeItem *)) *
				  sizeof(ListTreeItem *)));
/*      fprintf(stderr,"ret=%x ret->path=%x\n",ret,ret->path); */
  ret->item = item;
  ret->count = count;
  ret->open = item->open;
  while (count > 0) {
    count--;
    ret->path[count] = item;
    item = item->parent;
  }

  TreeCheck(w, "exiting MakeV1CallbackStruct");
  return ret;
}

/* Do the historical callbacks.  After Further Review, I don't really
   like requiring the user to free the data.  But, I don't want to
   break everybody's code, so I will mark these routines as HISTORICAL.

   MIGRATE TO THE NEW CALLBACKS! */
static void
HistoricalCallbacks(ListTreeWidget w)
{
  ListTreeReturnStruct *ret;

    if (w->list.PathCallback) {
      ret = MakeV1CallbackStruct(w, w->list.timer_item);
      if (w->list.timer_item->firstchild)
	ret->reason = XtBRANCH;
      else
	ret->reason = XtLEAF;
      XtCallCallbacks((Widget) w, XtNpathCallback, (XtPointer) ret);
    }
    if (w->list.BranchCallback && w->list.timer_item->firstchild) {
      ret = MakeV1CallbackStruct(w, w->list.timer_item);
      ret->reason = XtBRANCH;
      XtCallCallbacks((Widget) w, XtNbranchCallback, (XtPointer) ret);
    }
    else if (w->list.LeafCallback && !w->list.timer_item->firstchild) {
      ret = MakeV1CallbackStruct(w, w->list.timer_item);
      ret->reason = XtLEAF;
      XtCallCallbacks((Widget) w, XtNleafCallback, (XtPointer) ret);
    }
}

static void
MakeActivateCallbackStruct(ListTreeWidget w, ListTreeItem *item,ListTreeActivateStruct *ret)
{
  int count;
  ListTreeItem *parent;

  count = 1;
  parent = item;
  while (parent->parent) {
    parent = parent->parent;
    count++;
  }

  ret->item = item;
  ret->count = count;
  ret->open = item->open;
  if (item->firstchild)
    ret->reason = XtBRANCH;
  else
    ret->reason = XtLEAF;
  while (count > 0) {
    count--;
    AddItemToReturnList(w,item,count);
    item = item->parent;
  }
  ret->path=w->list.ret_item_list;
}

static void
SelectDouble(ListTreeWidget w)
{
  ListTreeActivateStruct ret;

  TreeCheck(w, "in SelectDouble");
  if (w->list.timer_item) {
    w->list.timer_type=TIMER_DOUBLE;
    w->list.timer_item->open = !w->list.timer_item->open;
    w->list.highlighted = w->list.timer_item;
    HighlightAll(w, False, True);
    w->list.timer_item->highlighted = True;
    DrawAll(w);

    if (w->list.ActivateCallback) {
      MakeActivateCallbackStruct(w, w->list.timer_item,&ret);
      XtCallCallbacks((Widget) w, XtNactivateCallback, (XtPointer) &ret);
    }

    HistoricalCallbacks(w);
  }
  TreeCheck(w, "exiting SelectDouble");
}

/* ARGSUSED */
static void 
SelectSingle(XtPointer client_data, XtIntervalId * idp)
{
  ListTreeWidget w = (ListTreeWidget) client_data;

  w->list.timer_id = (XtIntervalId) 0;
/*
  if (w->list.timer_x<w->list.timer_item->x) {
*/
    SelectDouble(w);
/*
  }
  else {
    HighlightAll(w, False, True);
    HighlightItem(w, w->list.timer_item, True, True);
    if (w->list.timer_type!=TIMER_CLEAR)
      HighlightDoCallback(w);
    w->list.timer_type=TIMER_SINGLE;
  }
*/
}

/* ARGSUSED */
static void
Select(aw, event, params, num_params)
Widget aw;
XEvent *event;
String *params;
Cardinal *num_params;
{
  ListTreeWidget w = (ListTreeWidget) aw;

  w->list.timer_type=TIMER_WAITING;
  w->list.timer_item = GetItem(w, event->xbutton.y);
  w->list.timer_x=event->xbutton.x;
  w->list.timer_y=event->xbutton.y;
  if (!w->list.timer_item) {
    if (w->list.timer_id) {
      XtRemoveTimeOut(w->list.timer_id);
      w->list.timer_id = (XtIntervalId) 0;
    }
  }
  else {
    if (w->list.timer_id) {
      XtRemoveTimeOut(w->list.timer_id);
      w->list.timer_id = (XtIntervalId) 0;
      SelectDouble(w);
    }
    else {
      w->list.timer_id = XtAppAddTimeOut(
				   XtWidgetToApplicationContext((Widget) w),
				   (unsigned long) w->list.multi_click_time,
					  SelectSingle,
					  (XtPointer) w);

    }
  }
}

/* ARGSUSED */
static void
Extend(aw, event, params, num_params)
Widget aw;
XEvent *event;
String *params;
Cardinal *num_params;
{
  ListTreeItem *item;
  ListTreeWidget w = (ListTreeWidget) aw;
  int y, yend;

/* If we are waiting for a double click, return before doing anything */
  if (w->list.timer_id)
    return;

/* We need the timer_item to be pointing to the first selection in this */
/* group.  If we got here without it being set, something is very wrong. */
  if (!w->list.timer_item)
    return;

  y = w->list.timer_y;
  yend = event->xbutton.y;
  item = GetItem(w, y);
  if (y < yend) {
    while (item && y < yend && y < w->core.height) {
      if (item) {
	HighlightItem(w, item, True, True);
	y += item->height;
      }
      item = GetItem(w, y);
    }
  }
  else {
    while (item && y > yend && y > 0) {
      if (item) {
	HighlightItem(w, item, True, True);
	y -= item->height;
      }
      item = GetItem(w, y);
    }
  }
/*   HighlightDoCallback(w); */
}

/* ARGSUSED */
static void
Unset(aw, event, params, num_params)
Widget aw;
XEvent *event;
String *params;
Cardinal *num_params;
{
  ListTreeItem *item;
  ListTreeWidget w = (ListTreeWidget) aw;

  item = GetItem(w, event->xbutton.y);
  if (item) {
/*              item->open=False; */
/*              lw->list.highlighted=item; */
/*              DrawAll(lw); */
/*              ListTreeDelete(lw,item); */
  }
}

/* ARGSUSED */
static void
Notify(aw, event, params, num_params)
Widget aw;
XEvent *event;
String *params;
Cardinal *num_params;
{
  ListTreeWidget w = (ListTreeWidget) aw;
  ListTreeItem *item;


    /* don't call highlightCallback if we are waiting for a double click */
/*
  if (w->list.timer_id) {
  }
  else if (w->list.timer_type==TIMER_WAITING) {
*/
    HighlightAll(w, False, True);
    item = GetItem(w, event->xbutton.y);
    HighlightItem(w, item, True, True);
    HighlightDoCallback(w);
    w->list.timer_type=TIMER_CLEAR;
/*
  }
*/
}

/* ListTree private drawing functions */

static void
DrawItemHighlight(w, item)
ListTreeWidget w;
ListTreeItem *item;
{
  int width;

  if (item->highlighted || item == w->list.highlighted) {
    width = w->core.width - item->x;
    XFillRectangle(XtDisplay(w), XtWindow(w),
		   w->list.drawGC,
		   item->x, item->ytext,
		   width, FontHeight(w->list.font));
    XDrawString(XtDisplay(w), XtWindow(w), w->list.highlightGC,
		item->x, item->ytext + FontAscent(w->list.font),
		item->text, item->length);
  }
  else {
    XDrawString(XtDisplay(w), XtWindow(w), w->list.drawGC,
		item->x, item->ytext + FontAscent(w->list.font),
		item->text, item->length);
  }
}

static void
DrawItemHighlightClear(w, item)
ListTreeWidget w;
ListTreeItem *item;
{
  int width;

  width = w->core.width - item->x;
  if (item->highlighted || item == w->list.highlighted) {
    XFillRectangle(XtDisplay(w), XtWindow(w),
		   w->list.drawGC,
		   item->x, item->ytext,
		   width, FontHeight(w->list.font));
    XDrawString(XtDisplay(w), XtWindow(w), w->list.highlightGC,
		item->x, item->ytext + FontAscent(w->list.font),
		item->text, item->length);
  }
  else {
    XFillRectangle(XtDisplay(w), XtWindow(w),
		   w->list.highlightGC,
		   item->x, item->ytext,
		   width, FontHeight(w->list.font));
    XDrawString(XtDisplay(w), XtWindow(w), w->list.drawGC,
		item->x, item->ytext + FontAscent(w->list.font),
		item->text, item->length);
  }
}

static void
DrawItem(w, draw, item, x, y, xroot, yroot, retwidth, retheight)
ListTreeWidget w;
Boolean draw;
ListTreeItem *item;
int x, y;
int *xroot, *yroot, *retwidth, *retheight;
{
  int height, xpix, ypix, xbranch, ybranch, xtext, ytext, yline;
  Pixinfo *pix;

/* Select the pixmap to use, if any */
  if (item->firstchild) {
    if (item->open)
      pix = &w->list.Open;
    else
      pix = &w->list.Closed;
  }
  else {
    if (item->open)
      pix = &w->list.LeafOpen;
    else
      pix = &w->list.Leaf;
  }

/* Compute the height of this line */
  height = FontHeight(w->list.font);
  xpix = x - w->list.pixWidth + pix->xoff;
  xtext = x + (int) w->list.HSpacing;
  if (pix) {
    if (pix->height > height) {
      ytext = y + ((pix->height - height) / 2);
      height = pix->height;
      ypix = y;
    }
    else {
      ytext = y;
      ypix = y + ((height - pix->height) / 2);
    }
    xbranch = xpix + (w->list.pixWidth / 2);
    ybranch = ypix + pix->height;
    yline = ypix + (pix->height / 2);
  }
  else {
    ypix = ytext = y;
    xbranch = xpix + (w->list.pixWidth / 2);
    yline = ybranch = ypix + (height / 2);
    yline = ypix + (height / 2);
  }

/* Save the basic graphics info for use by other functions */
  item->x = xtext;
  item->y = y;
  item->ytext = ytext;
  item->height = height;

  if ((*xroot >= 0) &&
      ((*yroot >= w->list.exposeTop && *yroot <= w->list.exposeBot) ||
       (yline >= w->list.exposeTop && yline <= w->list.exposeBot) ||
       (*yroot < w->list.exposeTop && yline > w->list.exposeBot)))
    XDrawLine(XtDisplay(w), XtWindow(w), w->list.drawGC,
	      *xroot, *yroot,
	      *xroot, yline);
  if (draw && y >= w->list.exposeTop && y <= w->list.exposeBot) {
    if (*xroot >= 0)
      XDrawLine(XtDisplay(w), XtWindow(w), w->list.drawGC,
		*xroot, yline,
		xbranch, yline);
    if (pix && pix->pix)
      XCopyArea(XtDisplay(w), pix->pix, XtWindow(w),
		w->list.drawGC,
		0, 0, pix->width, pix->height,
		xpix, ypix);
/*if (1) { */
/*ListTreeItem *p; */
/*char text[1024]; */
/**/
/*p=item;*/
/*if (p->parent) printf("%x %x \t",p,p->parent); */
/*else printf("%x 00000000 \t",p); */
/*while (p) { printf(" "); p=p->parent; } */
/*p=item; */
/*while (p) { */
/*printf("%s/",p->text); */
/*p=p->parent; */
/*} */
/*printf("\n"); */
/*} */
    DrawItemHighlight(w, item);
  }
  *xroot = xbranch;
  *yroot = ybranch;
  *retwidth = FontTextWidth(w->list.font, item->text);
  *retheight = height;
}

static int
DrawChildren(w, draw, item, x, y, xroot, yroot)
ListTreeWidget w;
Boolean draw;
ListTreeItem *item;
int x, y, xroot, yroot;
{
  int width, height;
  int xbranch, ybranch;

  x += (int) w->list.Indent + w->list.pixWidth;
  while (item) {
    xbranch = xroot;
    ybranch = yroot;
    DrawItem(w, draw, item, x, y, &xbranch, &ybranch, &width, &height);

    width += x + (int) w->list.HSpacing + (int) w->list.Margin;

    if (width > w->list.preferredWidth)
      w->list.preferredWidth = width;

    y += height + (int) w->list.VSpacing;
    if ((item->firstchild) && (item->open))
      y = DrawChildren(w, draw, item->firstchild,
		       x, y, xbranch, ybranch);

    item = item->nextsibling;
  }
  return y;
}

static void
Draw(w, draw, yevent, hevent)
ListTreeWidget w;
Boolean draw;
int yevent, hevent;
{
  int x, y, height, width;
  int xbranch, ybranch;
  ListTreeItem *item;
  int saveheight, savewidth;

  TreeCheck(w, "Draw");
#ifdef DEBUG_GEOM
  printf("Draw: y=%d h=%d\n", yevent, hevent);
#endif
/* Overestimate the expose region to be sure to draw an item that gets */
/* cut by the region */
  w->list.exposeTop = yevent - FontHeight(w->list.font);
  w->list.exposeBot = yevent + hevent + FontHeight(w->list.font);
  saveheight = w->list.preferredHeight;
  savewidth = w->list.preferredWidth;
  w->list.preferredWidth = w->list.preferredHeight = 2;

  x = (int) w->list.Margin + w->list.pixWidth;
  y = (int) w->list.Margin;
  item = w->list.first;
  while (item) {
    xbranch = -1;
    DrawItem(w, draw, item, x, y, &xbranch, &ybranch, &width, &height);

    width += x + (int) w->list.HSpacing + (int) w->list.Margin;

    if (width > w->list.preferredWidth)
      w->list.preferredWidth = width;

    y += height + (int) w->list.VSpacing;
    if ((item->firstchild) && (item->open))
      y = DrawChildren(w, draw, item->firstchild,
		       x, y, xbranch, ybranch);

    item = item->nextsibling;
  }
  w->list.preferredHeight = y + (int) w->list.Margin;
  if (draw && (saveheight != w->list.preferredHeight ||
	       savewidth != w->list.preferredWidth))
    ChangeSize(w);
}


static void
DrawAll(w)
ListTreeWidget w;
{
  XClearWindow(XtDisplay((Widget) w), XtWindow((Widget) w));
  Draw(w, (Boolean) True, 0, (int) w->core.height);
}


/* Private Functions --------------------------------------------------------- */


/* This function removes the specified item from the linked list.  It does */
/* not do anything with the data contained in the item, though. */
static void
RemoveReference(w, item)
ListTreeWidget w;
ListTreeItem *item;
{

/* If there exists a previous sibling, just skip over item to be dereferenced */
  if (item->prevsibling) {
    item->prevsibling->nextsibling = item->nextsibling;
    if (item->nextsibling)
      item->nextsibling->prevsibling = item->prevsibling;
  }
/* If not, then the deleted item is the first item in some branch. */
  else {
    if (item->parent)
      item->parent->firstchild = item->nextsibling;
    else
      w->list.first = item->nextsibling;
    if (item->nextsibling)
      item->nextsibling->prevsibling = NULL;
  }
}

static void
DeleteChildren(w, item)
ListTreeWidget w;
ListTreeItem *item;
{
  ListTreeItem *sibling;

  while (item) {
    if (item->firstchild) {
      DeleteChildren(w, item->firstchild);
      item->firstchild = NULL;
    }
    sibling = item->nextsibling;
    XtFree((char *) item->text);
    XtFree((char *) item);
    item = sibling;
  }
}

static void
InsertChild(w, parent, item)
ListTreeWidget w;
ListTreeItem *parent;
ListTreeItem *item;
{
  ListTreeItem *i;

  item->parent = parent;
  item->nextsibling = item->prevsibling = NULL;
  if (parent) {
    if (parent->firstchild) {
      i = parent->firstchild;
      while (i->nextsibling) {
	i = i->nextsibling;
      }
      i->nextsibling = item;
      item->prevsibling = i;
    }
    else {
      parent->firstchild = item;
    }

  }
  else {			/* if parent==NULL, this is a top level entry */
    if (w->list.first) {
      i = w->list.first;
      while (i->nextsibling) {
	i = i->nextsibling;
      }
      i->nextsibling = item;
      item->prevsibling = i;
    }
    else {
      w->list.first = item;
    }
  }
}

/* Insert a list of ALREADY LINKED children into another list */
static void
InsertChildren(w, parent, item)
ListTreeWidget w;
ListTreeItem *parent;
ListTreeItem *item;
{
  ListTreeItem *next, *newnext;

/*      while (item) { */
/*              next=item->nextsibling; */
/*              InsertChild(w,parent,item); */
/*              item=next; */
/*      } */
/*      return; */


/* Save the reference for the next item in the new list */
  next = item->nextsibling;

/* Insert the first item in the new list into the existing list */
  InsertChild(w, parent, item);

/* The first item is inserted, with its prev and next siblings updated */
/* to fit into the existing list.  So, save the existing list reference */
  newnext = item->nextsibling;

/* Now, mark the first item's next sibling to point back to the new list */
  item->nextsibling = next;

/* Mark the parents of the new list to the new parent.  The order of the */
/* rest of the new list should be OK, and the second item should still */
/* point to the first, even though the first was reparented. */
  while (item->nextsibling) {
    item->parent = parent;
    item = item->nextsibling;
  }

/* Fit the end of the new list back into the existing list */
  item->nextsibling = newnext;
  if (newnext)
    newnext->prevsibling = item;
}

static int
SearchChildren(w, item, y, findy, finditem)
ListTreeWidget w;
ListTreeItem *item;
ListTreeItem **finditem;
int y, findy;
{
  int height;
  Pixinfo *pix;

  while (item) {
/* Select the pixmap to use, if any */
    if (item->firstchild) {
      if (item->open)
	pix = &w->list.Open;
      else
	pix = &w->list.Closed;
    }
    else {
      if (item->open)
	pix = &w->list.LeafOpen;
      else
	pix = &w->list.Leaf;
    }

/* Compute the height of this line */
    height = FontHeight(w->list.font);
    if (pix && pix->height > height)
      height = pix->height;

    if (findy >= y && findy <= y + height) {
      *finditem = item;
      return -1;
    }
    y += height + (int) w->list.VSpacing;
    if ((item->firstchild) && (item->open)) {
      y = SearchChildren(w, item->firstchild,
			 y, findy, finditem);
      if (*finditem)
	return -1;
    }
    item = item->nextsibling;
  }
  return y;
}

static ListTreeItem *
GetItem(w, findy)
ListTreeWidget w;
int findy;
{
  int y, height;
  ListTreeItem *item, *finditem;
  Pixinfo *pix;

  TreeCheck(w, "in GetItem");
  y = (int) w->list.Margin;
  item = w->list.first;
  finditem = NULL;
  while (item && !finditem) {
/* Select the pixmap to use, if any */
    if (item->firstchild) {
      if (item->open)
	pix = &w->list.Open;
      else
	pix = &w->list.Closed;
    }
    else {
      if (item->open)
	pix = &w->list.LeafOpen;
      else
	pix = &w->list.Leaf;
    }

/* Compute the height of this line */
    height = FontHeight(w->list.font);
    if (pix && pix->height > height)
      height = pix->height;

    if (findy >= y && findy <= y + height)
      return item;
    y += height + (int) w->list.VSpacing;
    if ((item->firstchild) && (item->open)) {
      y = SearchChildren(w, item->firstchild,
			 y, findy, &finditem);
/*                      if (finditem) return finditem; */
    }
    item = item->nextsibling;
  }
  TreeCheck(w, "exiting GetItem");
  return finditem;
}

static int
SearchPosition(w, item, y, finditem, found)
ListTreeWidget w;
ListTreeItem *item, *finditem;
int y;
Boolean *found;
{
  int height;
  Pixinfo *pix;

  while (item) {
/*              printf("Checking y=%d  item=%s\n",y,item->text); */
    if (item == finditem) {
      *found = True;
      return y;
    }

/* Select the pixmap to use, if any */
    if (item->firstchild) {
      if (item->open)
	pix = &w->list.Open;
      else
	pix = &w->list.Closed;
    }
    else {
      if (item->open)
	pix = &w->list.LeafOpen;
      else
	pix = &w->list.Leaf;
    }

/* Compute the height of this line */
    height = FontHeight(w->list.font);
    if (pix && pix->height > height)
      height = pix->height;

    y += height + (int) w->list.VSpacing;
    if ((item->firstchild) && (item->open)) {
      y = SearchPosition(w, item->firstchild, y, finditem, found);
      if (*found)
	return y;
    }
    item = item->nextsibling;
  }
  return y;
}

static Position
GetPosition(w, finditem)
ListTreeWidget w;
ListTreeItem *finditem;
{
  int y, height;
  ListTreeItem *item;
  Pixinfo *pix;
  Boolean found;

  TreeCheck(w, "in GetPosition");
  y = (int) w->list.Margin;
  item = w->list.first;
  found = False;
  while (item && item != finditem) {

/*              printf("Checking y=%d  item=%s\n",y,item->text); */
/* Select the pixmap to use, if any */
    if (item->firstchild) {
      if (item->open)
	pix = &w->list.Open;
      else
	pix = &w->list.Closed;
    }
    else {
      if (item->open)
	pix = &w->list.LeafOpen;
      else
	pix = &w->list.Leaf;
    }

/* Compute the height of this line */
    height = FontHeight(w->list.font);
    if (pix && pix->height > height)
      height = pix->height;

    y += height + (int) w->list.VSpacing;
    if ((item->firstchild) && (item->open)) {
      y = SearchPosition(w, item->firstchild, y, finditem, &found);
      if (found)
	return (Position) y;
    }
    item = item->nextsibling;
  }
  TreeCheck(w, "exiting GetPosition");
  if (item != finditem)
    y = 0;
  return (Position) y;
}


#ifdef USE_RDD
/* --------------------------------------------------------------------------
   ** ListTree drag & drop operations
 */

/* ARGSUSED */
static void
StartDrag(w, event, params, num_params)
Widget w;
XButtonEvent *event;
String *params;
Cardinal *num_params;
{
  ListTreeItem *item;
  ListTreeWidget lw = (ListTreeWidget) w;
  static Pixmap pixmap;
  Display *dpy = XtDisplay(w);
  static Dimension wid = 40, hgt = 40;
  static GC gc;

  item = lw->list.highlighted;
  if (item) {
    printf("Drag %s\n", item->text);
  }

  rddStartAction(w, event, params, num_params);		/* then use default action */
}

/* ARGSUSED */
static void
EndDrag(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
  ListTreeItem *item;
  ListTreeWidget lw = (ListTreeWidget) w;
  ListTreeReturnStruct *ret;

  item = lw->list.highlighted;
  if (item) {
    printf("Ending %s\n", item->text);

    ret = MakeV1CallbackStruct(w, item);
    rddSetDropDataType(ret, sizeof(ListTreeReturnStruct),
		       RDD_LISTTREE_TYPE);

    /* then use default action */
    rddDropAction(w, event, params, num_params);
  }
}

/* ARGSUSED */
void
ListTreeHighlightDrop(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
  ListTreeItem *item;
  ListTreeWidget lw = (ListTreeWidget) w;

  if (event && event->xbutton.state) {
    item = GetItem(lw, event->xbutton.y);
    if (item) {
      printf("highlighting %d,%d  item=%s\n",
	     event->xbutton.x, event->xbutton.y, item->text);
      if (lw->list.drop_highlighted)
	XDrawRectangle(XtDisplay(w), XtWindow(w),
		       lw->list.eorGC,
		       lw->list.drop_highlighted->x,
		       lw->list.drop_highlighted->ytext,
	      FontTextWidth(lw->list.font, lw->list.drop_highlighted->text),
		       FontHeight(lw->list.font));
      XDrawRectangle(XtDisplay(w), XtWindow(w),
		     lw->list.eorGC,
		     item->x, item->ytext,
		     FontTextWidth(lw->list.font, item->text),
		     FontHeight(lw->list.font));
      lw->list.drop_highlighted = item;
    }
  }
  else if (lw->list.drop_highlighted) {
    XDrawRectangle(XtDisplay(w), XtWindow(w),
		   lw->list.eorGC,
		   lw->list.drop_highlighted->x,
		   lw->list.drop_highlighted->ytext,
	      FontTextWidth(lw->list.font, lw->list.drop_highlighted->text),
		   FontHeight(lw->list.font));
    lw->list.drop_highlighted = NULL;
  }
}


ListTreeReturnStruct *
ListTreeGetDrop(Widget w)
{
  ListTreeWidget lw = (ListTreeWidget) w;
  ListTreeReturnStruct *ret;

  if (lw->list.drop_highlighted) {
    ret = MakeV1CallbackStruct(w, lw->list.drop_highlighted);
    ListTreeHighlightDrop(w, NULL, NULL, 0);
  }
  else
    ret = NULL;
  return ret;
}
#endif

/* Public Functions --------------------------------------------------------- */

void
ListTreeRefresh(ListTreeWidget w)
{
  if (XtIsRealized((Widget) w) && w->list.Refresh)
    DrawAll(w);
}

void
ListTreeRefreshOff(ListTreeWidget w)
{
  w->list.Refresh = False;
}

void
ListTreeRefreshOn(ListTreeWidget w)
{
  w->list.Refresh = True;
  ListTreeRefresh(w);
}

ListTreeItem *
ListTreeAdd(ListTreeWidget w,ListTreeItem *parent,char *string)
{
  ListTreeItem *item;
  int len;
  char *copy;

  TreeCheck(w, "in ListTreeAdd");
  len = strlen(string);
  item = (ListTreeItem *) XtMalloc(sizeof(ListTreeItem));
  copy = (char *) XtMalloc(len + 1);
  strcpy(copy, string);
  item->text = copy;
  item->length = len;
  item->parent = parent;
  item->open = False;
  item->highlighted = False;
  item->firstchild = item->prevsibling = item->nextsibling = NULL;
  InsertChild(w, parent, item);

  ListTreeRefresh(w);

  return item;
}

void
ListTreeRenameItem(ListTreeWidget w,ListTreeItem *item,char *string)
{
  int len;
  char *copy;

  TreeCheck(w, "in ListTreeRename");
  XtFree(item->text);
  len = strlen(string);
  copy = (char *) XtMalloc(len + 1);
  strcpy(copy, string);
  item->text = copy;
  item->length = len;

  ListTreeRefresh(w);
}

int
ListTreeDelete(ListTreeWidget w,ListTreeItem *item)
{
  if (item->firstchild)
    DeleteChildren(w, item->firstchild);
  item->firstchild = NULL;

  RemoveReference(w, item);

  XtFree((char *) item->text);
  XtFree((char *) item);

  ListTreeRefresh(w);

  return 1;
}

int
ListTreeDeleteChildren(ListTreeWidget w,ListTreeItem *item)
{
  if (item->firstchild)
    DeleteChildren(w, item->firstchild);
  item->firstchild = NULL;

  ListTreeRefresh(w);

  return 1;
}

int
ListTreeReparent(ListTreeWidget w, ListTreeItem *item,ListTreeItem *newparent)
{
  TreeCheck(w, "in ListTreeReparent");
/* Remove the item from its old location. */
  RemoveReference(w, item);

/* The item is now unattached.  Reparent it.                     */
  InsertChild(w, newparent, item);

  ListTreeRefresh(w);

  return 1;
}

int
ListTreeReparentChildren(ListTreeWidget w, ListTreeItem *item,ListTreeItem *newparent)
{
  ListTreeItem *first;

  TreeCheck(w, "in ListTreeReparentChildren");
  if (item->firstchild) {
    first = item->firstchild;
    item->firstchild = NULL;

    InsertChildren(w, newparent, first);

    ListTreeRefresh(w);
    return 1;
  }
  return 0;
}

int
AlphabetizeItems(const void *item1, const void *item2)
{
  return strcmp((*((ListTreeItem **) item1))->text,
		(*((ListTreeItem **) item2))->text);
}

int
ListTreeUserOrderSiblings(ListTreeWidget w, ListTreeItem *item,int (*func)())
{
  ListTreeItem *first, *parent, **list;
  size_t i, count, size;

  TreeCheck(w, "in ListTreeUserOrderSiblings");
/* Get first child in list; */
  while (item->prevsibling)
    item = item->prevsibling;
  first = item;
  parent = first->parent;

/* Count the children */
  count = 1;
  while (item->nextsibling)
    item = item->nextsibling, count++;
  if (count <= 1)
    return 1;

  size = sizeof(ListTreeItem *);
  list = (ListTreeItem **) XtMalloc(size * count);
  list[0] = first;
  count = 1;
  while (first->nextsibling) {
    list[count] = first->nextsibling;
    count++;
    first = first->nextsibling;
  }

  qsort(list, count, size, func);

  list[0]->prevsibling = NULL;
  for (i = 0; i < count; i++) {
    if (i < count - 1)
      list[i]->nextsibling = list[i + 1];
    if (i > 0)
      list[i]->prevsibling = list[i - 1];
  }
  list[count - 1]->nextsibling = NULL;
  if (parent)
    parent->firstchild = list[0];
  else
    w->list.first = list[0];
  XtFree((char *) list);

  ListTreeRefresh(w);
  TreeCheck(w, "exiting ListTreeOrderSiblings");

  return 1;
}

int
ListTreeOrderSiblings(ListTreeWidget w, ListTreeItem *item)
{
  TreeCheck(w, "in ListTreeOrderSiblings");
  return ListTreeUserOrderSiblings(w, item, AlphabetizeItems);
}

int
ListTreeUserOrderChildren(ListTreeWidget w, ListTreeItem *item,int (*func)())
{
  ListTreeItem *first;

  TreeCheck(w, "in ListTreeUserOrderChildren");
  if (item) {
    first = item->firstchild;
    if (first)
      ListTreeUserOrderSiblings(w, first, func);
  }
  else {
    if (w->list.first)
      ListTreeUserOrderSiblings(w, w->list.first, func);
  }
  TreeCheck(w, "exiting ListTreeUserOrderChildren");
  return 1;
}

int
ListTreeOrderChildren(ListTreeWidget w, ListTreeItem *item)
{
  ListTreeItem *first;

  TreeCheck(w, "in ListTreeOrderChildren");
  if (item) {
    first = item->firstchild;
    if (first)
      ListTreeOrderSiblings(w, first);
  }
  else {
    if (w->list.first)
      ListTreeOrderSiblings(w, w->list.first);
  }
  TreeCheck(w, "exiting ListTreeOrderChildren");
  return 1;
}

ListTreeItem *
ListTreeFindSiblingName(ListTreeWidget w, ListTreeItem *item, char *name)
{
  ListTreeItem *first;

  TreeCheck(w, "in ListTreeFindSiblingName");
/* Get first child in list; */
  if (item) {
    while (item->prevsibling)
      item = item->prevsibling;
    first = item;

    while (item) {
      if (strcmp(item->text, name) == 0)
	return item;
      item = item->nextsibling;
    }
    return item;
  }
  return NULL;
}

ListTreeOpen(ListTreeWidget w, ListTreeItem *item)
{
  item->open = True;
  ListTreeRefresh(w);
}

ListTreeClose(ListTreeWidget w, ListTreeItem *item)
{
  item->open = False;
  ListTreeRefresh(w);
}

ListTreeOpenAll(ListTreeWidget w, ListTreeItem *start, int child_only)
{
  ListTreeItem *item;

  /* Get first child in list; */
  if (start == (ListTreeItem *)NULL)
      item = w->list.first;
  else 
      item = start;

  while (item) {
    if (item->firstchild)
        ChildrenOpen (w, item->firstchild);
    item->open = True;
    item = item->nextsibling;
    if (child_only)
	break;
  }
  ListTreeRefresh(w);
}

ChildrenOpen (ListTreeWidget w, ListTreeItem *item)
{
  while (item) {
    if (item->firstchild)
      ChildrenOpen(w, item->firstchild);
    item->open = True;
    item = item->nextsibling;
  }
}


ListTreeCloseAll(ListTreeWidget w, ListTreeItem *start, int child_only)
{
  ListTreeItem *item;

  /* Get first child in list; */
  if (start == (ListTreeItem *)NULL)
      item = w->list.first;
  else 
      item = start;

  while (item) {
    if (item->firstchild)
        ChildrenClose (w, item->firstchild);
    item->open = False;
    item = item->nextsibling;
    if (child_only)
	break;
  }
  ListTreeRefresh(w);
}

ChildrenClose (ListTreeWidget w, ListTreeItem *item)
{
  while (item) {
    if (item->firstchild)
      ChildrenClose(w, item->firstchild);
    item->open = False;
    item = item->nextsibling;
  }
}


ListTreeItem *
ListTreeFindChildName(ListTreeWidget w, ListTreeItem *item, char *name)
{
  TreeCheck(w, "in ListTreeFindChildName");
/* Get first child in list; */
  if (item && item->firstchild) {
    item = item->firstchild;
  }
  else if (!item && w->list.first){
    item =w->list.first;
  }
  else item=NULL;

  while (item) {
    if (strcmp(item->text, name) == 0)
      return item;
    item = item->nextsibling;
  }
  return NULL;
}


ListTreeItem *
ListTreeFindChildNameInTree(ListTreeWidget w, ListTreeItem *item, char *name)
{
  ListTreeItem *found = (ListTreeItem *)NULL, *ChildFind();

  TreeCheck(w, "in ListTreeFindChildName");
/* Get first child in list; */
  if (item && item->firstchild)
    item = item->firstchild;
  else if (!item && w->list.first)
    item =w->list.first;
  else 
    item=NULL;

  while (item) {
    if (item && strcmp(item->text, name) == 0)
      return item;
    if (item->firstchild) {
      found = ChildFind(w, item->firstchild, name);
      if (found)
        return found;
    }
    item = item->nextsibling;
  }
  return NULL;
}

ListTreeItem *
ChildFind (ListTreeWidget w, ListTreeItem *item, char*name)
{
  ListTreeItem *found = (ListTreeItem *)NULL, *ChildFind();

  while (item) {
    if (item && strcmp(item->text, name) == 0)
      return item;
    if (item->firstchild) {
      found = ChildFind(w, item->firstchild, name);
      if (found)
        return found;
    }
    item = item->nextsibling;
  }
  return NULL;
}


void
ListTreeHighlightItem(ListTreeWidget w, ListTreeItem *item)
{
  HighlightAll(w,False,False);
  HighlightItem(w,item,True,False);
  ListTreeRefresh(w);
}

void
ListTreeHighlightAll(ListTreeWidget w)
{
  HighlightAllVisible(w,True,False);
  ListTreeRefresh(w);
}

void
ListTreeClearHighlighted(ListTreeWidget w)
{
  HighlightAll(w,False,False);
  ListTreeRefresh(w);
}

void
ListTreeGetHighlighted(ListTreeWidget w,ListTreeMultiReturnStruct *ret)
{
  if (ret) MakeMultiCallbackStruct(w,ret);
}

void
ListTreeSetHighlighted(ListTreeWidget w,ListTreeItem **items,int count,Boolean clear)
{
  if (clear) HighlightAll(w,False,False);
  if (count<0) {
    while (*items) {
      HighlightItem(w,*items,True,False);
      items++;
    }
  }
  else {
    int i;
    
    for (i=0; i<count; i++){
      HighlightItem(w,items[i],True,False);
    }
  }
  ListTreeRefresh(w);
}

ListTreeItem *
ListTreeFirstItem(w)
ListTreeWidget w;
{
  ListTreeItem *first;

/* Get first child in widget */
  first = w->list.first;
  return first;
}

Position
ListTreeGetItemPosition(ListTreeWidget w, ListTreeItem *item)
{
  return GetPosition(w, item);
}

void
ListTreeGetPathname(ListTreeReturnStruct * ret, char *dir)
{
  int count;

  if (*ret->path[0]->text != '/')
    strcpy(dir, "/");
  else
    strcpy(dir, "");
  strcat(dir, ret->path[0]->text);
  count = 1;
  while (count < ret->count) {
    strcat(dir, "/");
    strcat(dir, ret->path[count]->text);
    count++;
  }
}

void
ListTreeGetPathnameFromItem(ListTreeItem *item, char *dir)
{
  char tmppath[1024];

  *dir='\0';
  while (item) {
    sprintf(tmppath,"/%s%s",item->text,dir);
    strcpy(dir,tmppath);
    item=item->parent;
  }
}
