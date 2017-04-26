#ifndef _CONTEINER_P_H_
#define _CONTEINER_P_H_ 


#include <X11/ConstrainP.h>
#include <X11/Xraw/Container.h>

#define CORE(w)      ((Widget)(w))->core
#define COMPOSITE(w) ((CompositeWidget)(w))->composite
#define CONTAINER(w) ((ContainerWidget)(w))->container

typedef struct {

  /* public instance variables */

    Pixel foreground;

    Pixel  top_shadow_color;
    Pixmap top_shadow_pixmap;
    Pixel  bottom_shadow_color;
    Pixmap bottom_shadow_pixmap;

    Dimension shadow_thickness;

    XtPointer user_data;

  /* private instance variables */

    GC bottom_shadow_GC;
    GC top_shadow_GC;
    GC background_GC;

} ContainerPart;


typedef struct _ContainerRec {
    CorePart       core;
    CompositePart  composite;
    ConstraintPart constraint;
    ContainerPart  container;
} ContainerRec;

typedef struct _ContainerClassPart{
  int unused;
} ContainerClassPart;


typedef struct _ContainerClassRec {
    CoreClassPart       core_class;
    CompositeClassPart  composite_class;
    ConstraintClassPart constraint_class;
    ContainerClassPart  container_class;
} ContainerClassRec;

/* container constraints */

typedef struct _ContainerConstraintPart {
    int unused;
} ContainerConstraintPart;

typedef struct _ContainerConstraintRec {
    ContainerConstraintPart container;
} ContainerCosntraintRec, *ContainerConstraintPtr;

extern ContainerClassRec containerClassRec;


extern void _XawQueryGeometry  Xraw_PROTO((Widget widget, 
					   XtWidgetGeometry *reply_return));

#endif /* _CONTEINER_P_H_ */
