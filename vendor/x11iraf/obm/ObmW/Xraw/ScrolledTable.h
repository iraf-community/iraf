/***********************************************************************
  
                         ScrolledTable widget
		   Copyright by Vladimir T. Romanovski
			 All rights reserved.

This library is designed  for  free,  non-commercial  software  creation.
It is changeable and can be improved. The author would greatly appreciate
any  advice, new  components  and  patches  of  the  existing  programs.
Commercial  usage is  also  possible  with  participation of the author.

                      romsky@hp1.oea.ihep.su (Russia)
                      romsky@munin.ucsf.edu  (USA)
	
*************************************************************************/

#ifndef _XawScrolledTable_H_
#define _XawScrolledTable_H_

#include <X11/Xraw/Container.h>
#include <X11/Xraw/Viewport.h>

/*#########################################################################*/
/*#                                                                       #*/
/*#                           New Resources                               #*/
/*#                                                                       #*/
/*#########################################################################*/
#ifndef XtNdistance
#define XtNdistance "distance"
#endif

#ifndef XtNsignWidget
#define XtNsignWidget "signWidget"
#endif

#ifndef XtNscrollbarWidth
#define XtNscrollbarWidth "scrollbarWidth"
#endif

#ifndef XtNframeWidth
#define XtNframeWidth "frameWidth"
#endif

#ifndef XtNrowWidget
#define XtNrowWidget "rowWidget"
#endif

#ifndef XtNstuffWidget
#define XtNstuffWidget "stuffWidget"
#endif

#ifndef XtNcolumnWidget
#define XtNcolumnWidget "columnWidget"
#endif



/*#########################################################################*/
/*#                                                                       #*/
/*#                           New Resource Classes                        #*/
/*#                                                                       #*/
/*#########################################################################*/
#ifndef XtCColumnWidget
#define XtCColumnWidget "ColumnWidget"
#endif

#ifndef XtCScrollbarWidth
#define XtCScrollbarWidth "ScrollbarWidth"
#endif

#ifndef XtCSignWidget
#define XtCSignWidget    "SignWidget"
#endif

#ifndef XtCStuffWidget
#define XtCStuffWidget "StuffWidget"
#endif

#ifndef XtCDistance
#define XtCDistance      "Distance"
#endif

#ifndef XtCFrameWidth
#define XtCFrameWidth    "FrameWidth"
#endif

#ifndef XtCRowWidget
#define XtCRowWidget     "RowWidget"
#endif



/*#########################################################################*/
/*#                                                                       #*/
/*#                           Widget Class Pointer                        #*/
/*#                                                                       #*/
/*#########################################################################*/
extern WidgetClass scrolledTableWidgetClass;

typedef struct _ScrolledTableClassRec *XrawScrolledTableWidgetClass;
typedef struct _ScrolledTableRec      *XrawScrolledTableWidget;

#endif /* _XawScrolledTable_H_ */
/* DON'T ADD STUFF AFTER THIS #endif */
