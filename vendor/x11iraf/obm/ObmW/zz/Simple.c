#include <stdio.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include <X11/Xmu/Drawing.h>

#include <X11/Xraw/XawInit.h>
#include <X11/Xraw/3d.h>
#include <X11/Xraw/SimpleP.h>
#include <X11/Xraw/ContainerP.h>

#define UnspecifiedPixmap (Pixmap)2
#define UndefinedGC       (GC)2
  
static void InsPixel();

static XtResource resources[] = {
#define offset(field) XtOffset(SimpleWidget, simple.field)
  {
    XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
    offset(cursor), XtRImmediate, (XtPointer) None
  },
  {
    XtNinsensitiveBorder, XtCInsensitive, XtRPixmap, sizeof(Pixmap),
    offset(insensitive_border), XtRImmediate, (XtPointer) None
  },
  {
    XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
    offset(foreground), XtRImmediate, (XtPointer) XtDefaultForeground
  },
  {
    "top.gc", "Top.gc", XtRString, sizeof(String),
    offset(top_shadow_GC), XtRImmediate, (XtPointer)NULL 
  },
  {
    "bottom.gc", "Bottom.gc", XtRString, sizeof(String),
    offset(bottom_shadow_GC), XtRImmediate, (XtPointer)NULL 
  },
  {
    "highlight.gc", "Highlight.gc", XtRString, sizeof(String),
    offset(highlight_GC), XtRImmediate, (XtPointer)NULL 
  },
  {
    XtNshadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
    offset(shadow_thickness), XtRImmediate, (XtPointer) 0
  },
  {
    XtNtopShadowPixmap, XtCTopShadowPixmap, XtRPixmap, sizeof(Pixmap),
    offset(top_shadow_pixmap), XtRImmediate, (XtPointer) UnspecifiedPixmap
  },
  {
    XtNtopShadowPixel, XtCTopShadowPixel, XtRPixel, sizeof(Pixel),
    offset(top_shadow_color), XtRCallProc, (XtPointer) InsPixel
  },
  {
    XtNbottomShadowPixmap, XtCBottomShadowPixmap, XtRPixmap, sizeof(Pixmap),
    offset(bottom_shadow_pixmap), XtRImmediate, (XtPointer) UnspecifiedPixmap
  },
  {
    XtNbottomShadowPixel, XtCBottomShadowPixel, XtRPixel, sizeof(Pixel),
    offset(bottom_shadow_color), XtRCallProc, (XtPointer) InsPixel
  },
  {
    XtNhighlightPixmap, XtCHighlightPixmap, XtRPixmap, sizeof(Pixmap),
    offset(highlight_pixmap), XtRImmediate, (XtPointer) UnspecifiedPixmap
  },
  {
    XtNhighlightPixel, XtCHighlightPixel, XtRPixel, sizeof(Pixel),
    offset(highlight_color), XtRCallProc, (XtPointer) InsPixel
  },
  {
    XtNhighlightThickness, XtCHighlightThickness, XtRDimension,
    sizeof(Dimension),
    offset(highlight_thickness), XtRImmediate, (XtPointer) 2
  },
  {
    XtNuserData, XtCUserData, XtRPixmap, sizeof(Pixmap),
     offset(user_data), XtRImmediate, (XtPointer) NULL
  },
  {
    XtNborderWidth, XtCBorderWidth, XtRDimension, sizeof(Dimension),
    XtOffsetOf(RectObjRec,rectangle.border_width), XtRImmediate, (XtPointer)0
  }

};

static void InsPixel(w, off, value)
     Widget w;
     int off;
     XrmValue *value;
{
  register SimpleWidget p = (SimpleWidget) w;
  static Pixel pixel;
  
  if (off == offset(top_shadow_color))
  {
    p->simple.top_shadow_GC = UndefinedGC;
  }
  else if (off == offset(bottom_shadow_color))
  {  
    p->simple.bottom_shadow_GC = UndefinedGC;
  }
  else 
  {  
    p->simple.highlight_GC = UndefinedGC;
  }
  value->addr = (XtPointer) &pixel;
}

#undef offset

static void ClasstInitialize();
static void ClassPartInitialize();
static void Initialize();
static void Realize();
static void Redisplay();
static void Destroy();

static Boolean SetValues();
static Boolean DisplayRectProc();
static Boolean ChangeSensitive();

static XtGeometryResult QueryGeometry();

SimpleClassRec simpleClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &widgetClassRec,
    /* class_name		*/	"Simple",
    /* widget_size		*/	sizeof(SimpleRec),
    /* class_initialize		*/	ClasstInitialize,
    /* class_part_initialize	*/	ClassPartInitialize,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	NULL,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	QueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* simple fields */
    /* change_sensitive		*/	ChangeSensitive,
    /* display_rect		*/	DisplayRectProc,
    /* extension		*/	NULL				
  }
};

WidgetClass simpleWidgetClass = (WidgetClass)&simpleClassRec;

static void ClasstInitialize()
{
/* EMPTY */
}
     
static void ClassPartInitialize(class)
    WidgetClass class;
{
  register SimpleWidgetClass c = (SimpleWidgetClass)class;

  if (c->simple_class.change_sensitive == NULL)
  {
    char buf[BUFSIZ];

    sprintf(buf,
	    "%s Widget: The Simple Widget class method 'change_sensitive' is undefined.\nA function must be defined or inherited.",
	    c->core_class.class_name);
    XtWarning(buf);
	c->simple_class.change_sensitive = ChangeSensitive;
  }

  if (c->simple_class.change_sensitive == XtInheritChangeSensitive)
    c->simple_class.change_sensitive = ChangeSensitive;
  
  if (c->simple_class.display_rect == XtInheritDisplayRectProc)
    c->simple_class.display_rect = DisplayRectProc;
}

/* ARGSUSED */
static void Initialize(request, new, args, num_args)
    Widget request, new;
    ArgList args;
    Cardinal *num_args;
{
  SimpleWidget sw = (SimpleWidget)new;
  SimplePart*  sp = (SimplePart*)&(sw->simple);
  
  if (sp->top_shadow_pixmap == UnspecifiedPixmap)
    sp->top_shadow_pixmap = None;
  
  if (sp->top_shadow_GC == NULL){
    if (sp->top_shadow_pixmap != None)
      sp->top_shadow_GC = AllocGCFromPixmap (new, sp->top_shadow_pixmap);
    else
      sp->top_shadow_GC = AllocGCFromPixel (new, sp->top_shadow_color);
  } else if (sp->top_shadow_GC == UndefinedGC)
    sp->top_shadow_GC = MakeTopShadowGC (new, new->core.background_pixel);

  
  if (sp->bottom_shadow_pixmap == UnspecifiedPixmap)
    sp->bottom_shadow_pixmap = None;
  
  if (sp->bottom_shadow_GC == NULL){
    if (sp->bottom_shadow_pixmap != None)
      sp->bottom_shadow_GC = AllocGCFromPixmap (new, sp->bottom_shadow_pixmap);
    else
      sp->bottom_shadow_GC = AllocGCFromPixel (new, sp->bottom_shadow_color);
  } else if (sp->bottom_shadow_GC == UndefinedGC)
    sp->bottom_shadow_GC =MakeBottomShadowGC (new, new->core.background_pixel);


  if (sp->highlight_pixmap == UnspecifiedPixmap)
    sp->highlight_pixmap = None;
  
  if (sp->highlight_GC == NULL){
    if (sp->highlight_pixmap != None)
      sp->highlight_GC = AllocGCFromPixmap (new, sp->highlight_pixmap);
    else
      sp->highlight_GC = AllocGCFromPixel (new, sp->highlight_color);
  } else if (sp->highlight_GC == UndefinedGC)
    sp->highlight_GC = MakeBottomShadowGC (new, new->core.background_pixel);

}

static void Realize(w, valueMask, attributes)
     Widget w;
     Mask *valueMask;
     XSetWindowAttributes *attributes;
{
  Pixmap border_pixmap;
  
  if (!XtIsSensitive(w))
  {
    /* change border to gray; have to remember the old one,
     * so XtDestroyWidget deletes the proper one */
    if (((SimpleWidget)w)->simple.insensitive_border == None)
      ((SimpleWidget)w)->simple.insensitive_border =
	XmuCreateStippledPixmap(XtScreen(w),
				w->core.border_pixel, 
				w->core.background_pixel,
				w->core.depth);
    border_pixmap = w->core.border_pixmap;
    attributes->border_pixmap =
      w->core.border_pixmap = ((SimpleWidget)w)->simple.insensitive_border;
    
    *valueMask |= CWBorderPixmap;
    *valueMask &= ~CWBorderPixel;
  }
  
  if ((attributes->cursor = ((SimpleWidget)w)->simple.cursor) != None)
    *valueMask |= CWCursor;
  
  XtCreateWindow( w, (unsigned int)InputOutput, (Visual *)CopyFromParent,
		 *valueMask, attributes );
  
  if (!XtIsSensitive(w))
    w->core.border_pixmap = border_pixmap;
}

/* ARGSUSED */
static Boolean SetValues(current, request, new, args, num_args)
     Widget   current, request, new;
     ArgList  args;
     Cardinal *num_args;
{
  SimpleWidget s_old  = (SimpleWidget) current;
  SimpleWidget s_new  = (SimpleWidget) new;
  SimplePart*  sp     = (SimplePart*)&(s_new->simple);
  Boolean      redraw = False;
  
#define NE(field) (s_new->simple.field != s_old->simple.field)  

  if ( XtIsSensitive(current) != XtIsSensitive(new) )
    (*((SimpleWidgetClass)XtClass(new))->
     simple_class.change_sensitive) ( new );

  if ( NE(cursor) && XtIsRealized(new))
    XDefineCursor(XtDisplay(new), XtWindow(new), s_new->simple.cursor);

  if (NE(top_shadow_pixmap))
  {
    XtReleaseGC (new, sp->top_shadow_GC); 
    sp->top_shadow_GC = AllocGCFromPixmap (new, sp->top_shadow_pixmap);
    redraw = True;
  }
  else if (NE(top_shadow_color) && sp->top_shadow_pixmap == None)
  {
    XtReleaseGC (new, sp->top_shadow_GC);
    sp->top_shadow_GC = AllocGCFromPixel (new, sp->top_shadow_color);
    redraw = True;
  }

  if (NE(bottom_shadow_pixmap))
  {
    XtReleaseGC (new, sp->bottom_shadow_GC); 
    sp->bottom_shadow_GC = AllocGCFromPixmap (new, sp->bottom_shadow_pixmap);
    redraw = True;
  }
  else if (NE(bottom_shadow_color) && sp->bottom_shadow_pixmap == None)
  {
    XtReleaseGC (new, sp->bottom_shadow_GC);
    sp->bottom_shadow_GC = AllocGCFromPixel (new, sp->bottom_shadow_color);
    redraw = True;
  }

  if (NE(highlight_pixmap))
  {
    XtReleaseGC (new, sp->highlight_GC); 
    sp->highlight_GC = AllocGCFromPixmap (new, sp->highlight_pixmap);
    redraw = True;
  }
  else if (NE(highlight_color) && sp->highlight_pixmap)
  {
    XtReleaseGC (new, sp->highlight_GC);
    sp->highlight_GC = AllocGCFromPixel (new, sp->highlight_color);
    redraw = True;
  }

#undef NE
  
  return redraw;
}


static void Unhighlight (gw)
     Widget gw;
{
  register Dimension thick  = ((SimpleWidget)gw)->simple.highlight_thickness;
  register Dimension  width  = gw->core.width;
  register Dimension  height = gw->core.height;
  
  if (!XtIsRealized(gw))
          return;
  
  if ( XtIsSubclass(XtParent(gw), containerWidgetClass))
  {
    XRectangle rectangles[4];
    GC gc = ((ContainerWidget) XtParent(gw))->container.background_GC;

#define SET_RECTANGLE(I,X,Y,W,H)  \
    rectangles[I].x = X;     rectangles[I].y = Y; \
    rectangles[I].width = W; rectangles[I].height = H

    SET_RECTANGLE(0, 0, 0, thick, height);
    SET_RECTANGLE(1, 0, height - thick, width, thick);
    SET_RECTANGLE(2, 0, 0, width, thick);
    SET_RECTANGLE(3, width - thick, 0, thick, height);

    XFillRectangles (XtDisplay(gw), XtWindow(gw), gc, rectangles, 4);

  }
  else
  {
    Display *dpy = XtDisplay(gw);
    Window   win = XtWindow(gw);
    
    XClearArea( dpy, win, 0, 0, thick, height, False);
    XClearArea( dpy, win, 0, height - thick, width, thick, False);
    XClearArea( dpy, win, 0, 0, width, thick, False);
    XClearArea( dpy, win, width - thick, 0, thick, height, False);
  }
}


/* ARGSUSED */
static void Redisplay( gw, event, region )
   Widget gw;
   XEvent *event;
   Region region;
{
  register SimpleWidget s = (SimpleWidget)gw;

  if (XtIsRealized(gw))
  {
    Unhighlight (gw);

    if (s->simple.shadow_thickness)
      XawDrawFrame (gw,
		    s->simple.highlight_thickness,
		    s->simple.highlight_thickness,
		    s->core.width - 2 * s->simple.highlight_thickness,
		    s->core.height - 2 * s->simple.highlight_thickness,
		    XawRAISED,
		    s->simple.shadow_thickness,
		    s->simple.top_shadow_GC,
		    s->simple.bottom_shadow_GC);
  }
}

static void Destroy(w)
     Widget w;
{
  register SimpleWidget s = (SimpleWidget)w;

  XtReleaseGC(w, s->simple.top_shadow_GC);
  XtReleaseGC(w, s->simple.bottom_shadow_GC);
}

static Boolean DisplayRectProc (w, rect)
     Widget w;
     XRectangle *rect;
{
  register SimpleWidget s = (SimpleWidget)w;
  
  rect->x      =
  rect->y      = s->simple.highlight_thickness + s->simple.shadow_thickness;
  rect->width  = s->core.width - 2 * rect->x;
  rect->height = s->core.height - 2 * rect->y;
  
  return True;
}


static XtGeometryResult QueryGeometry(w, intended, preferred)
    Widget w;
    XtWidgetGeometry *intended, *preferred;
{
    register SimpleWidget sw = (SimpleWidget)w;


    preferred->request_mode = CWWidth | CWHeight;

    preferred->width = preferred->height = 2 * SIMPLE_MARGIN(sw) + 1;

#define WIDTH_HEIGHT (CWWidth | CWHeight)
    
    if (intended
	&& ((intended->request_mode & WIDTH_HEIGHT) == WIDTH_HEIGHT)
	&& intended->width == preferred->width
	&& intended->height == preferred->height)
    {  
	return XtGeometryYes;
    }
    else if (preferred->width == w->core.width
	     && preferred->height == w->core.height)
    {
	return XtGeometryNo;
    }
    else
    {
      return XtGeometryAlmost;
    }
}


static Boolean ChangeSensitive(w)
    register Widget w;
{
    if (XtIsRealized(w)) {
	if (XtIsSensitive(w))
	    if (w->core.border_pixmap != UnspecifiedPixmap)
		XSetWindowBorderPixmap( XtDisplay(w), XtWindow(w),
				        w->core.border_pixmap );
	    else
		XSetWindowBorder( XtDisplay(w), XtWindow(w), 
				  w->core.border_pixel );
	else {
	    if (((SimpleWidget)w)->simple.insensitive_border == None)
		((SimpleWidget)w)->simple.insensitive_border =
		    XmuCreateStippledPixmap(XtScreen(w),
					    w->core.border_pixel, 
					    w->core.background_pixel,
					    w->core.depth);
	    XSetWindowBorderPixmap( XtDisplay(w), XtWindow(w),
				    ((SimpleWidget)w)->
				        simple.insensitive_border );
	}
    }
    return False;
}
