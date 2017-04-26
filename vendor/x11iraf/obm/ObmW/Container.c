#include <stdio.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xraw/XawInit.h>
#include <X11/Xraw/3d.h>
#include <X11/Xraw/ContainerP.h>



#define UnspecifiedPixmap (Pixmap)2
#define UndefinedGC       (GC)2
  
static void InsPixel();

static XtResource resources[] = {
#define offset(field) XtOffsetOf(ContainerRec, container.field)
  {
    "top.gc", "Top.gc", XtRString, sizeof(String),
    offset(top_shadow_GC), XtRImmediate, (XtPointer)NULL 
  },
  {
    "bottom.gc", "Bottom.gc", XtRString, sizeof(String),
    offset(bottom_shadow_GC), XtRImmediate, (XtPointer)NULL 
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
    XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(foreground), XtRString, (caddr_t) XtDefaultForeground
  },
  {
    XtNuserData, XtCUserData, XtRPixmap, sizeof(Pixmap),
     offset(user_data), XtRImmediate, (caddr_t) NULL
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
  register ContainerWidget p = (ContainerWidget) w;
  static Pixel pixel;
  
  if (off == offset(top_shadow_color))
  {
    p->container.top_shadow_GC = UndefinedGC;
  }
  else /*bottom_shadow_color */
  {  
    p->container.bottom_shadow_GC = UndefinedGC;
  }
  value->addr = (caddr_t) &pixel;
}

#undef offset


static void ClassInitialize();
static void ClassPartInitialize();
static void initialize();
static void realize();
static void destroy();
static void Redisplay();

static Boolean SetValues();

/*static XtGeometryResult query_geometry();*/
/*static XtGeometryResult geometry_manager();*/
/*static void changed_managed();*/

ContainerClassRec containerClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &constraintClassRec,
        /* class_name            */ "Container",
	/* widget_size           */ sizeof(ContainerRec),
	/* class_cnitialize      */ ClassInitialize,
	/* class_part_initialize */ ClassPartInitialize,
	/* class_inited          */ FALSE,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ TRUE,
	/* compress_exposure     */ XtExposeCompressMultiple,
	/* compress_enterleave   */ TRUE,
	/* visible_interest      */ FALSE,
	/* destroy               */ destroy,
	/* resize                */ NULL,
	/* expose                */ Redisplay,
	/* set_values            */ SetValues,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ XtInheritDisplayAccelerator,
	/* extension             */ NULL
    },
    /* Composite class part */
    {
	/* geometry manager */ XtInheritGeometryManager,
        /* change_managed   */ XtInheritChangeManaged,
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ NULL,
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,
        /* subresource_count */ 0,   
        /* constraint_size   */ 0,   
        /* initialize        */ NULL,
        /* destroy           */ NULL,
        /* set_values        */ NULL,
        /* extension         */ NULL,
    },
    /* Container class part */
    {
	/* unused            */ 0,
    }
};

WidgetClass containerWidgetClass = (WidgetClass)&containerClassRec;

static void 
ClassInitialize()
{
}

static void
ClassPartInitialize(widget_class)
     WidgetClass widget_class;
{
}

/* ARGSUSED */
static void initialize(request, new, args, num_args)
     Widget request, new;
     ArgList args;
     Cardinal *num_args;
{
  ContainerWidget sw = (ContainerWidget)new;
  ContainerPart*  cp = (ContainerPart*)&(sw->container);
  

  if (cp->top_shadow_pixmap == UnspecifiedPixmap)
    cp->top_shadow_pixmap = None;
  
  if (cp->top_shadow_GC == NULL){
    if (cp->top_shadow_pixmap != None)
      cp->top_shadow_GC = AllocGCFromPixmap (new, cp->top_shadow_pixmap);
    else
      cp->top_shadow_GC = AllocGCFromPixel (new, cp->top_shadow_color);
  } else if (cp->top_shadow_GC == UndefinedGC)
    cp->top_shadow_GC = MakeTopShadowGC (new, new->core.background_pixel);

  
  if (cp->bottom_shadow_pixmap == UnspecifiedPixmap)
    cp->bottom_shadow_pixmap = None;
  
  if (cp->bottom_shadow_GC == NULL){
    if (cp->bottom_shadow_pixmap != None)
      cp->bottom_shadow_GC = AllocGCFromPixmap (new, cp->bottom_shadow_pixmap);
    else
      cp->bottom_shadow_GC = AllocGCFromPixel (new, cp->bottom_shadow_color);
  } else if (cp->bottom_shadow_GC == UndefinedGC)
    cp->bottom_shadow_GC =MakeBottomShadowGC (new, new->core.background_pixel);

  cp->background_GC = AllocGCFromPixel (new, new->core.background_pixel);
}

static void destroy(w)
     Widget w;
{
  register ContainerWidget c = (ContainerWidget)w;

  XtReleaseGC(w, c->container.top_shadow_GC);
  XtReleaseGC(w, c->container.bottom_shadow_GC);
  XtReleaseGC(w, c->container.background_GC);

}

static void realize(w, valueMask, attributes)
    Widget w;
    Mask *valueMask;
    XSetWindowAttributes *attributes;
{
  (*coreClassRec.core_class.realize) (w, valueMask, attributes);
}

/* ARGSUSED */
static Boolean SetValues(current, request, new, args, num_args)
     Widget   current, request, new;
     ArgList  args;
     Cardinal *num_args;
{
  ContainerWidget s_old  = (ContainerWidget) current;
  ContainerWidget s_new  = (ContainerWidget) new;
  ContainerPart*  sp     = (ContainerPart*)&(s_new->container);
  Boolean      redraw = False;
  
#define NE(field) (s_new->container.field != s_old->container.field)  
  
  if (NE(top_shadow_pixmap))
  {

    XtReleaseGC (new, sp->top_shadow_GC); 
    sp->top_shadow_GC = AllocGCFromPixmap (new, sp->top_shadow_pixmap);
    redraw = True;

  }
  else  if (NE(top_shadow_color) && sp->top_shadow_pixmap == None)
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
  else  if (NE(bottom_shadow_color) && sp->bottom_shadow_pixmap == None)
  {

    XtReleaseGC (new, sp->bottom_shadow_GC);
    sp->bottom_shadow_GC = AllocGCFromPixel (new, sp->bottom_shadow_color);
    redraw = True;

  }

#undef NE
  
  return redraw;
}


/* ARGSUSED */
static void Redisplay(gw, event, region)
    Widget gw;
    XEvent *event;		/* unused */
    Region region;		/* unused */
{
  register ContainerWidget c = (ContainerWidget) gw;
  
  XClearWindow (XtDisplay(gw), XtWindow(gw));

  XawDrawFrame (gw,
		0,
		0,
		c->core.width,
		c->core.height,
		XawRAISED,
		c->container.shadow_thickness,
		c->container.top_shadow_GC,
		c->container.bottom_shadow_GC);

}

void _XawQueryGeometry (widget, reply_return)
     Widget widget;
     XtWidgetGeometry *reply_return;
{
  XtGeometryResult result;
  String subs[1];
  Cardinal num_subs;
  WidgetClass class;

  if (widget != (Widget)NULL)
  {

    reply_return->request_mode = CWWidth | CWHeight;

    result = XtQueryGeometry (widget, NULL, reply_return);

    switch (result) 
    {
    case XtGeometryNo     :
    case XtGeometryYes    :
                            reply_return->width  = widget->core.width;
			    reply_return->height = widget->core.height;
      break;
    case XtGeometryAlmost :
                            if (!(reply_return->request_mode & CWWidth))
			      reply_return->width  = widget->core.width;

			    if (!(reply_return->request_mode & CWHeight))
			      reply_return->height = widget->core.height;
      break;
    default :
                            class = XtClass(widget);
                            subs[0] = class->core_class.class_name;
			    num_subs = 1;
                            XtAppWarningMsg(
					 XtWidgetToApplicationContext(widget),
					    "QueryGeometry", 
					    "QueryGeometry",
					    "WidgetToolkit",
	   "WidgetClass '%s' returns invalid value for XtQueryGeometry",
					    subs, &num_subs);
      break;
    }
    reply_return->request_mode = CWWidth | CWHeight;
  }
}



