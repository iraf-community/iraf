/* 
 * FrameP.h - Private definitions for Frame widget
 * 
 * Author:	Vladimir Romanovski 
 *
 * Date:	Mon Feb 27, 1995
 */

/***********************************************************************
 *
 * Frame Widget Private Data
 *
 ***********************************************************************/

#ifndef _XawFrameP_h
#define _XawFrameP_h

#include <X11/ConstrainP.h>
#include <X11/Xraw/3d.h>
#include <X11/Xraw/ContainerP.h>
#include <X11/Xraw/Frame.h>

#define FRAME(w) ((FrameWidget)(w))->frame

/* New fields for the Frame widget class record */

typedef struct _FrameClassPart{
  XtPointer       extension;
} FrameClassPart;

typedef struct _FrameClassRec {
  CoreClassPart        core_class;
  CompositeClassPart   composite_class;
  ConstraintClassPart  constraint_class;
  ContainerClassPart   container_class;
  FrameClassPart       frame_class;
} FrameClassRec;

/* New fields for the frame  widget. */

typedef struct _FramePart {
  /* Constraint contstraint resources */
  Dimension       h_space;
  Dimension       v_space;

  XawFrameType    frame_type;         

  int             x_fraction;
  int             y_fraction;
  XawLayoutPolicy policy;

  char           *label;
  XFontStruct    *font;
  Boolean         caption;
  XtJustify       justify;
  unsigned char   encoding;

  /* Private resources. */
  Dimension       preferred_width;
  Dimension       preferred_height;

  GC              gc;

} FramePart;

typedef  struct _FrameRec{
  CorePart 	 core;
  CompositePart  composite;
  ConstraintPart constraint;
  ContainerPart  container;
  FramePart	 frame;
} FrameRec;

typedef struct _FrameConstraintsPart {
  /* Constraint contstraint resources */

  int left;
  int right;
  int top;
  int bottom;

  /* Private contstraint resources. */

} FrameConstraintsPart;

typedef struct _FrameConstraintsRec {
    FrameConstraintsPart frame;
} FrameConstraintsRec, *FrameConstraints;



extern FrameClassRec frameClassRec;


#endif  /* _XawFrameP_h */
