/***********************************************************************

                             Table widget 
       		Copyright by Vladimir T. Romanovski
			 All rights reserved.
			
This  library is desined  for  free,  non-commercial  software  creation. 
It is changeable and can be improved. The author would greatly appreciate 
any  advises, new  components  and  patches  of  the  existing  programs.
Commercial  usage is  also  possible  with  participation of it's author.

*************************************************************************/

#ifndef _XawTableP_h
#define _XawTableP_h

/***********************************************************************
 *
 * Table Widget Private Data
 *
 ***********************************************************************/


#include <X11/Xraw/ContainerP.h>
#include <X11/Xraw/Table.h>
#include <X11/Xraw/table.h>

/* New fields for the Table widget class record */

typedef struct {int foo;} TableClassPart;

/* Full class record declaration */
typedef struct _TableClassRec {
    CoreClassPart       core_class;
    CompositeClassPart  composite_class;
    ConstraintClassPart constraint_class;
    ContainerClassPart  container_class;
    TableClassPart	table_class;
} TableClassRec;

#define MAX_ROWS 50

typedef struct _NormalReverseGC {
  int   used;
  Pixel fore;
  Pixel back;
  GC    normal;
  GC    reverse;
}NormalReverseGC;

typedef struct _ShadowGC {
  int   used;
  Pixel back;
  GC    top;
  GC    bottom;
}ShadowGC;



/* New fields for the Table widget record */
typedef struct {
    /* ------------------------ resources -----------------------*/
    Pixel	  row_fore;
    Pixel	  column_fore;
    Pixel	  edit_fore;
    Pixel	  edit_back;
    Boolean       row_oriented;
    Boolean       editable;
    Boolean       literal;

    int           mask_number;
    int           columns;
    int           rows;
    Dimension     tab_margin;
    Dimension     row_margin;
    Dimension     col_margin;
    Dimension	  internal_width;
    Dimension	  internal_height;
    Dimension     label_shadow_thickness;
    unsigned char encoding;

    /* Default Values */
    Pixel	  foreground;
    XtJustify	  justify;
    XFontStruct	 *font;
    int           width;

    /* Allowance CallbackList */
    XtCallbackList   allow_add_row;
    XtCallbackList   allow_add_column;
    XtCallbackList   allow_delete_column;
    XtCallbackList   allow_delete_row;
    XtCallbackList   allow_delete_table;

    /* Information CallbackList */
    XtCallbackList   add_row;
    XtCallbackList   add_column;
    XtCallbackList   changed_cell;
    XtCallbackList   create_table;
    XtCallbackList   delete_column;
    XtCallbackList   delete_row;
    XtCallbackList   delete_table;
    XtCallbackList   what_cell;
    XtCallbackList   changed_column_width;
    XtCallbackList   changed_row_height;

    Widget           v_scroll;
    Widget           h_scroll;

    int              row_height;
    int              column_default_width;
    int              literal_width;

    /* ------------------------ private state -----------------------*/

    int         no_refigure;   /* no re-layout while > 0 */
    int         no_redraw;     /* no re-draw while > 0 */
    Boolean     was_resized;


    XawTableColumn column_data;
    
    Dimension   prefer_width;
    Dimension   prefer_height;
    Widget      edit;
    int         edit_row;
    int         edit_column;
    XawTableCell   cell_own;
    XawTableCell   table_stuff;

    GC          row_gc;        /* Intrinsics sharedable GC */
    GC          column_gc;     /* Intrinsics sharedable GC */

    GC          normal;        /* Table sharedable GC */
    GC          reverse;       /* Table sharedable GC */
    GC          top;
    GC          bottom;

    GC          edit_top;
    GC          edit_bottom;
    
    NormalReverseGC *normal_hash_table;
    ShadowGC        *shadow_hash_table;
    int              mask_hash_table;

    Atom             selections[30];
    int              num_selections;
} TablePart;

/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _TableRec {
    CorePart	   core;
    CompositePart  composite;
    ConstraintPart constraint;
    ContainerPart  container;
    TablePart	   table;
} TableRec;

extern TableClassRec tableClassRec;

#endif /* _XawTableP_h */
