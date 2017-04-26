#ifndef _SimpleP_h
#define _SimpleP_h

#include <X11/IntrinsicP.h>

#include <X11/Xraw/XawInit.h>
#include <X11/Xraw/Simple.h>

#define SIMPLE(w) ((SimpleWidget) w)->simple

typedef Boolean (*XawDisplayRectProc) Xraw_PROTO((Widget, XRectangle * ));
typedef Boolean (*XawChangeSensitive) Xraw_PROTO((Widget));

typedef struct {
  XawChangeSensitive  change_sensitive;
  XawDisplayRectProc  display_rect;
  caddr_t 	      extension;
} SimpleClassPart;

typedef struct _SimpleClassRec {
    CoreClassPart	core_class;
    SimpleClassPart	simple_class;
} SimpleClassRec;

extern SimpleClassRec simpleClassRec;

typedef struct {
    /* resources */
  Cursor        cursor;
  Pixmap        insensitive_border;
  Pixel   	foreground;
  
  Dimension   	shadow_thickness;
  Pixel   	top_shadow_color;
  Pixmap  	top_shadow_pixmap;
  Pixel   	bottom_shadow_color;
  Pixmap  	bottom_shadow_pixmap;
  Pixel   	highlight_color;
  Pixmap  	highlight_pixmap;
  Dimension	highlight_thickness;
  
  caddr_t	user_data;
  
    /* private state */
  GC		top_shadow_GC;
  GC		bottom_shadow_GC;
  GC            highlight_GC;
} SimplePart;

typedef struct _SimpleRec {
    CorePart	core;
    SimplePart	simple;
} SimpleRec;

#define SIMPLE_MARGIN(w) (((SimpleWidget)w)->simple.highlight_thickness + \
			  ((SimpleWidget)w)->simple.shadow_thickness)

#define XtSimpleClass(w) ((SimpleWidgetClass)XtClass(w))
#define XtInheritDisplayRectProc ((Boolean (*)())_XtInherit)
#define XtInheritChangeSensitive ((Boolean (*)())_XtInherit)

#endif /* _SimpleP_h */
