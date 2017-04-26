/***********************************************************************
  
                             Table widget
		   Copyright by Vladimir T. Romanovski
			 All rights reserved.

This library is designed  for  free,  non-commercial  software  creation.
It is changeable and can be improved. The author would greatly appreciate
any  advice, new  components  and  patches  of  the  existing  programs.
Commercial  usage is  also  possible  with  participation of the author.

                       romsky@hp1.oea.ihep.su  (Russia)
                       romsky@munin.ucsf.edu   (USA)
	 
*************************************************************************/

#ifndef _table_h_
#define _table_h_

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/Xraw/Table.h>

#if __STDC__ || defined(__cplusplus)
#define F_PROTO(s) s
#else
#define F_PROTO(s) ()
#endif

typedef struct _XawTableNodeRec {              /* Node of table grid */
  struct _XawTableNodeRec *l;
  struct _XawTableNodeRec *r;
  struct _XawTableNodeRec *t;
  struct _XawTableNodeRec *b;
}XawTableNodeRec, *XawTableNode;


extern XtPointer create_table 	    F_PROTO((int rows,
					     int columns, 
					     int node_size));

extern Boolean row_insert_after     F_PROTO((XtPointer d,
					     int node_size));
     
extern Boolean row_insert_before    F_PROTO((XtPointer f,
					     int node_size));
     
extern Boolean column_insert_after  F_PROTO((XtPointer d,
					     int node_size));
     
extern Boolean column_insert_before F_PROTO((XtPointer f,
					     int node_size));
     
extern XtPointer get_table 	    F_PROTO((XtPointer f));
     
extern XtPointer get_cell 	    F_PROTO((XtPointer p,
					     int i,
					     int j));
     
extern void get_table_size 	    F_PROTO((XtPointer p,
					     int *i,
					     int *j));
     
extern void get_cell_positions 	    F_PROTO((XtPointer p,
					     int *i,
					     int *j));

extern void row_delete 		    F_PROTO((XtPointer p));
     
extern void column_delete 	    F_PROTO((XtPointer p));
     
extern void delete_table 	    F_PROTO((XtPointer p));

extern Boolean go_table 	    F_PROTO((XtPointer w, 
					     XawTableProc proc, 
					     XtPointer table, 
					     int begin_row,
					     int end_row,
					     int begin_column, 
					     int end_column,
					     int direction,
					     register int *row,
					     register int *column,
					     XtPointer client_data));

#ifdef EBUG_XRAW_MALLOC
extern void _check_table            F_PROTO((XtPointer table, 
					     int rows, 
					     int columns));
#endif

#undef F_PROTO

#endif /* _table_h_ */
