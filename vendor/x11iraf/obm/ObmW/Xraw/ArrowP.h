#ifndef _XawArrowP_h
#define _XawArrowP_h

#include <X11/Xraw/RepeaterP.h>
#include <X11/Xraw/Arrow.h>

#define ARROW(w) ((ArrowWidget) w)->arrow

typedef struct {/* new fields in widget class */
  int dummy;    
} ArrowClassPart;

typedef struct _ArrowClassRec {
  CoreClassPart     core_class;
  SimpleClassPart   simple_class;
  LabelClassPart    label_class;
  CommandClassPart  command_class;
  RepeaterClassPart repeater_class;
  ArrowClassPart    arrow_class;
} ArrowClassRec;

typedef struct {
  /* resources */
  Dimension      arrowShadow;
  XawDrawArrowStruct outline;
  /* private state */
  GC             arrowgc;
  Boolean        set_a2_a3;
} ArrowPart;

typedef struct _ArrowRec {
  CorePart     core;
  SimplePart   simple;
  LabelPart    label;
  CommandPart  command;
  RepeaterPart repeater;
  ArrowPart    arrow;
} ArrowRec;

/*
 * external declarations
 */
extern ArrowClassRec arrowClassRec;

#endif /* _XawArrowP_h */
