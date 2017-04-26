/* 
 * SeparatorP.h - Private definitions for Separator widget
 * 
 */

#ifndef _XawSeparatorP_h
#define _XawSeparatorP_h

/***********************************************************************
 *
 * Separator Widget Private Data
 *
 ***********************************************************************/

#include <X11/Xmu/Converters.h>

#include "XrawInit.h"
#include "Separator.h"


/* New fields for the Separator widget class record */

typedef struct {int foo;} SeparatorClassPart;

/* Full class record declaration */
typedef struct _SeparatorClassRec {
    CoreClassPart	core_class;
    SimpleClassPart     simple_class;
    SeparatorClassPart	separator_class;
} SeparatorClassRec;

extern SeparatorClassRec separatorClassRec;

/* New fields for the Separator widget record */

typedef struct {

  /* Public Resources */
  XtOrientation    orientation;
  Dimension        margin;
  XawSeparatorType separatorType;

  /* Private part */
  GC               gc;

} SeparatorPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _SeparatorRec {
    CorePart	  core;
    SimplePart    simple;
#include <X11/Xaw3d/Simple.h>
    SeparatorPart separator;
} SeparatorRec;

#endif /* _XawSeparatorP_h */
