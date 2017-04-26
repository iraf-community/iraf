#ifndef _XawScrolledTableP_H_
#define _XawScrolledTableP_H_

#include <X11/Xraw/ScrolledTable.h>
#include <X11/Xraw/ContainerP.h>
#include <X11/Xraw/FrameP.h>

typedef struct {
 int unused;         /* class variables */
} ScrolledTableClassPart;

typedef struct _ScrolledTableClassRec {
  CoreClassPart           core_class;
  CompositeClassPart      composite_class;
  ConstraintClassPart     constraint_class;
  ContainerClassPart      container_class;
  ScrolledTableClassPart  scrolled_table_class;
} ScrolledTableClassRec;

typedef struct _ScrolledTablePart{

  /* resources */
  
  Dimension     distance;
  Dimension     scrollbar_width;
  Dimension     frame_width;
  XawFrameType  frame_type;
  Boolean       allow_vertical_scrollbar;
  Boolean       allow_horizontal_scrollbar;
  Boolean       force_scrollbar;

  Widget  sign_widget;
  Widget  row_widget;
  Widget  column_widget;
  Widget  stuff_widget;
  
  /* private state */

  Widget  row_clip;
  Widget  column_clip;
  Widget  stuff_clip;

  Position   stuff_x;
  Position   stuff_y;
  Dimension  stuff_width;
  Dimension  stuff_height;
  
  Widget  v_scroll;
  Widget  h_scroll;

} ScrolledTablePart;

typedef struct _ScrolledTableRec {
  CorePart          core;
  CompositePart     composite;
  ConstraintPart    constraint;
  ContainerPart     container;
  ScrolledTablePart scrolled_table;
} ScrolledTableRec;

extern ScrolledTableClassRec scrolledTableClassRec;

#endif /* _XawScrolledTableP_H_ */
/* DON'T ADD STUFF AFTER THIS #endif */
