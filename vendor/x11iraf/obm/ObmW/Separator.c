/*
 * Separator.c - Separator widget Vladimir Romanovski
 *
 */
#include <X11/IntrinsicP.h>
#include <X11/RectObjP.h>
#include <X11/StringDefs.h>
#include <X11/Xos.h>

#include <X11/Xraw/XawInit.h>
#include <X11/Xraw/SeparatorP.h>

#include <X11/Xmu/Converters.h>
#include <X11/Xmu/CharSet.h>
#include <X11/Xmu/Drawing.h>

#include <stdio.h>
#include <ctype.h>

/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

/* Private Data */

#define offset(field) XtOffsetOf(SeparatorRec, field)

static XtResource resources[] = {
  {
    XtNorientation, XtCOrientation, XtROrientation, sizeof(XtOrientation),
    offset(separator.orientation), XtRImmediate, (XtPointer) XtorientHorizontal
  },
  {
    XtNmargin, XtCMargin, XtRDimension, sizeof(Dimension),
    offset(separator.margin), XtRImmediate, (caddr_t)1
  },
  {
    XtNseparatorType, XtCSeparatorType, XtRSeparatorType,
    sizeof(XawSeparatorType), offset(separator.separatorType),
    XtRImmediate,(caddr_t)XawSHADOW_ETCHED_IN
  },
  {
    XtNshadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
    offset(simple.shadow_thickness), XtRImmediate, (XtPointer)0
  }
};
#undef offset

static void Initialize();
static void Resize();
static void Redisplay();
static Boolean SetValues();
static void ClassInitialize();
static void Destroy();

SeparatorClassRec separatorClassRec = {
  {
    /* core_class fields */	
    /* superclass	  	*/	(WidgetClass) &simpleClassRec,
    /* class_name	  	*/	"Separator",
    /* widget_size	  	*/	sizeof(SeparatorRec),
    /* class_initialize   	*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited       	*/	FALSE,
    /* initialize	  	*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize		  	*/	XtInheritRealize,
    /* actions		  	*/	NULL,
    /* num_actions	  	*/	0,
    /* resources	  	*/	resources,
    /* num_resources	  	*/	XtNumber(resources),
    /* xrm_class	  	*/	NULLQUARK,
    /* compress_motion	  	*/	TRUE,
    /* compress_exposure  	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest	  	*/	FALSE,
    /* destroy		  	*/	Destroy,
    /* resize		  	*/	Resize,
    /* expose		  	*/	Redisplay,
    /* set_values	  	*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus	 	*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private   	*/	NULL,
    /* tm_table		   	*/	NULL,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* Simple class fields initialization */
    /* change_sensitive         */      XtInheritChangeSensitive,
    /* display_rect             */      XtInheritDisplayRectProc,
    /* extension                */      NULL				
  },
  { /* Separator class fields initialization */
    /* ignore 			*/	0
  }
};

WidgetClass separatorWidgetClass = (WidgetClass)&separatorClassRec;

/****************************************************************
 *
 * Private Procedures
 *
 ****************************************************************/

/*---------------------------------------------*/
#define done( type, value )			\
{						\
    if ( toVal->addr != NULL )			\
    {						\
	if ( toVal->size < sizeof( type ) )	\
	{					\
	    toVal->size = sizeof( type );	\
	    return False;			\
	}					\
	*(type*)(toVal->addr) = (value);	\
    }						\
    else					\
    {						\
	static type static_val;			\
	static_val = (value);			\
	toVal->addr = (caddr_t)&static_val;	\
    }						\
    toVal->size = sizeof(type);			\
    return True;				\
}                                               \
/*---------------------------------------------*/

static  XrmQuark  QSingle, QDouble, QShadowIn, QShadowOut;

/* ARGSUSED */
static Boolean
CvtStringToSeparatorType(dpy, args, num_args, fromVal, toVal, convData)
     Display    *dpy;             
     XrmValuePtr args;		/* unused */
     Cardinal	*num_args;	/* unused */
     XrmValuePtr fromVal;
     XrmValuePtr toVal;
     XtPointer  *convData;       /* unused */
{
  static XawSeparatorType separatorType;
  XrmQuark    q;
  char        lowerName[BUFSIZ];

  XmuCopyISOLatin1Lowered (lowerName, (char *)fromVal->addr);
  q = XrmStringToQuark(lowerName);

  separatorType = XawSINGLE_LINE;
  
  if      (q == QSingle)          separatorType = XawSINGLE_LINE;
  else if (q == QDouble)          separatorType = XawDOUBLE_LINE;
  else if (q == QShadowIn)        separatorType = XawSHADOW_ETCHED_IN;
  else if (q == QShadowOut)       separatorType = XawSHADOW_ETCHED_OUT;
  else 
    XtDisplayStringConversionWarning( dpy, (char *)fromVal->addr,
				     XtRSeparatorType);
  
  done(XawSeparatorType, separatorType);
}
#undef done

static void ClassInitialize()
{
  XawInitializeWidgetSet();
  XtSetTypeConverter( XtRString, XtRSeparatorType, CvtStringToSeparatorType, 
		     (XtConvertArgList)NULL, (Cardinal)0,
		     XtCacheNone, (XtDestructor)NULL);

  QSingle    = XrmStringToQuark(XawSingle_Line);
  QDouble    = XrmStringToQuark(XawDouble_Line);
  QShadowIn  = XrmStringToQuark(XawShadow_Etched_In);
  QShadowOut = XrmStringToQuark(XawShadow_Etched_Out);
}

static void GetGC(sw)
    SeparatorWidget sw;
{
  XGCValues values;
  unsigned long mask;
  
  values.foreground = sw->simple.foreground;
  values.line_width = 0;
  mask = GCForeground | GCLineWidth;
    
  sw->separator.gc = XtGetGC ((Widget)sw, mask, (XGCValues*)&values);
}

/* ARGSUSED */
static void Initialize(request, new, args, num_args)
    Widget request, new;
    ArgList args;
    Cardinal *num_args;
{
  SeparatorWidget newsw = (SeparatorWidget) new;

  if (newsw->core.width == 0)
    newsw->core.width = 8 + 2*SIMPLE_MARGIN(new);

  if (newsw->core.height == 0)
    newsw->core.height = 8 + 2*SIMPLE_MARGIN(new);
  
  GetGC(newsw);

}

static void Resize(w)
    Widget w;
{
 /* If widget is realized, clear and redisplay it */

  if (XtIsRealized(w)) {
    XClearWindow(XtDisplay(w), XtWindow(w));
    (*separatorClassRec.core_class.expose) (w, (XEvent*)NULL, (Region)NULL);
  }
}

/* ARGSUSED */
static Boolean SetValues(current, request, new, args, num_args)
    Widget current, request, new;
    ArgList args;
    Cardinal *num_args;
{
  SeparatorWidget cursw = (SeparatorWidget) current;
  SeparatorWidget newsw = (SeparatorWidget) new;
  Boolean redisplay     = False;

#define NE(field) (cursw->separator.field != newsw->separator.field)

  if (newsw->core.width == 0)
    newsw->core.width = 8 + 2*SIMPLE_MARGIN(new);

  if (newsw->core.height == 0)
    newsw->core.height = 8 + 2*SIMPLE_MARGIN(new);
  
  if (NE(margin) || NE(separatorType)) 
    redisplay = True;
  
  if (cursw->simple.foreground != newsw->simple.foreground)
  {
    XtReleaseGC(new, cursw->separator.gc);
    GetGC(newsw);
    redisplay = True;
  }

  return redisplay;
}

static void Destroy(w)
    Widget w;
{
  XtReleaseGC( w, ((SeparatorWidget)w)->separator.gc );
}

static void Redisplay(gw, event, region)
    Widget gw;
    XEvent *event;
    Region region;
{
  register SeparatorWidget sw = (SeparatorWidget) gw;
  int x1, y1, x2, y2;
  
  if (!XtIsRealized(gw))
    return;
  
  if (sw->simple.shadow_thickness > 0)
    (*simpleWidgetClass->core_class.expose) (gw, event, region);

  if (sw->separator.orientation == XtorientHorizontal) {
    x1 = sw->separator.margin;
    x2 = sw->core.width - (x1 * 2);
    
    switch(sw->separator.separatorType) {
      
    case XawSHADOW_ETCHED_IN :
      y1 = (sw->core.height - 2) / 2;
      y2 = y1 + 1;
      
      XDrawLine(XtDisplay(gw), XtWindow(gw), sw->simple.bottom_shadow_GC,
		x1, y1, x2, y1);
      XDrawLine(XtDisplay(gw), XtWindow(gw), sw->simple.top_shadow_GC,
		x1, y2, x2, y2);
      break;
    case XawSHADOW_ETCHED_OUT:
      y1 = (sw->core.height - 2) / 2;
      y2 = y1 + 1;
      XDrawLine(XtDisplay(gw), XtWindow(gw), sw->simple.bottom_shadow_GC,
		x1, y2, x2, y2);
      XDrawLine(XtDisplay(gw), XtWindow(gw), sw->simple.top_shadow_GC,
		x1, y1, x2, y1);
      break;
      
    case XawDOUBLE_LINE:
      y1 = (sw->core.height - 2)/ 2;
      y2 = y1 + 2;
      XDrawLine(XtDisplay(gw), XtWindow(gw), sw->separator.gc,
		x1, y1, x2, y1);
      XDrawLine(XtDisplay(gw), XtWindow(gw), sw->separator.gc,
		x1, y2, x2, y2);
      break;
      
    case XawSINGLE_LINE :
      y1 = sw->core.height / 2;
      XDrawLine(XtDisplay(gw), XtWindow(gw), sw->separator.gc,
		x1, y1, x2, y1);
    default:
    break;
    }
  } else {
    y1 = sw->separator.margin;
    y2 = sw->core.height - (y1 * 2);
    
    switch(sw->separator.separatorType) {
      
    case XawSHADOW_ETCHED_IN :
      x1 = (sw->core.width - 2) / 2;
      x2 = x1 + 1;
      
      XDrawLine(XtDisplay(gw), XtWindow(gw), sw->simple.bottom_shadow_GC,
		x1, y1, x1, y2);
      XDrawLine(XtDisplay(gw), XtWindow(gw), sw->simple.top_shadow_GC,
		x2, y1, x2, y2);
      break;
    case XawSHADOW_ETCHED_OUT:
      x1 = (sw->core.width - 2) / 2;
      x2 = x1 + 1;
      XDrawLine(XtDisplay(gw), XtWindow(gw), sw->simple.bottom_shadow_GC,
		x1, y1, x1, y2);
      XDrawLine(XtDisplay(gw), XtWindow(gw), sw->simple.top_shadow_GC,
		x2, y1, x2, y2);
      break;
      
    case XawDOUBLE_LINE:
      x1 = (sw->core.width - 2)/ 2;
      x2 = x1 + 2;
      XDrawLine(XtDisplay(gw), XtWindow(gw), sw->separator.gc,
		x1, y1, x1, y2);
      XDrawLine(XtDisplay(gw), XtWindow(gw), sw->separator.gc,
		x2, y1, x2, y2);
      break;
      
    case XawSINGLE_LINE :
      x1 = sw->core.width / 2;
      XDrawLine(XtDisplay(gw), XtWindow(gw), sw->separator.gc,
		x1, y1, x1, y2);
    default:
    break;
    }
  }
}


