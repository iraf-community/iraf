/***********************************************************************
  
                             Table widget
		   Copyright by Vladimir T. Romanovski
			 All rights reserved.

This library is designed  for  free,  non-commercial  software  creation.
It is changeable and can be improved. The author would greatly appreciate
any  advice, new  components  and  patches  of  the  existing  programs.
Commercial  usage is  also  possible  with  participation of the author.
  
                       romsky@hp1.oea.ihep.su (Russia)
		       romsky@munin.ucsf.edu  (USA)
	
*************************************************************************/

#include <X11/Xraw/table.h>

#ifdef EBUG_XRAW_MALLOC
#include <dbmalloc/malloc.h>
#endif

/************************************************************************
 *
 *             TABLE IS A GRID OF POINTER BINDED NODES
 *
 ************************************************************************/

#define null (XawTableNode)NULL
#define FREE(t) if((t) != null)XtFree((char*)(t))

static void vert_tab_node_insert(f,s,p)
     register XawTableNode f; /* insert after   */
     register XawTableNode s; /* insert before  */
     register XawTableNode p; /* to be inserted */
{
  if (f != null) f->b = p;
  if (s != null) s->t = p;
  if (p != null) {p->t = f;  p->b = s;}
}

static void horiz_tab_node_insert(f,s,p)
     register XawTableNode f; /* insert after   */
     register XawTableNode s; /* insert before  */
     register XawTableNode p; /* to be inserted */
{
  if (f != null) f->r = p;
  if (s != null) s->l = p;
  if (p != null) {p->l = f;  p->r = s;}
}

static void vert_tab_node_reject(p)
     register XawTableNode p;
{
  if (p == null) return;
  
  if (p->t != null) p->t->b = p->b;
  if (p->b != null) p->b->t = p->t;
}

static void horiz_tab_node_reject(p)
     register XawTableNode p;
{
  if (p == null) return;
  
  if (p->r != null) p->r->l = p->l;
  if (p->l != null) p->l->r = p->r;
}

/* ARGSUSED */
static XawTableProc del_cell (w, i, j, call_data, client_data)
     XtPointer w;   
     int i;
     int j;  
     XtPointer call_data;
     XtPointer client_data;
{
  XtFree((char*)call_data);
  return False;
}

/*
**	Function name : row_delete
**
**	Description   : delete row which the node belongs to 
**	Input         : pointer to the node 
**	Output        : void
*/

void
#if NeedFunctionPrototypes
row_delete (XtPointer f)
#else
row_delete(f)
     XtPointer f;
#endif
{
  register XawTableNode p = (XawTableNode) f;

  if (p == null)
    return;

  /* Go to left edge */
  for(; p->l != null; p=p->l )
    /*EMPTY*/;

  /* Go to right with dettaching cell */
  for(; p->r != null; p=p->r){
    vert_tab_node_reject(p->l);
    FREE(p->l);
  }

  /* Detach last but one cell in the row */
  if (p->l != null){
    vert_tab_node_reject(p->l);
    FREE(p->l);
  }

  /* Detach very last cell in the row */
  vert_tab_node_reject(p);
  FREE(p);
}

/*
**	Function name : column_delete
**
**	Description   : delete column which the node belongs to 
**	Input         : pointer to the node 
**	Output        : void
*/

void
#if NeedFunctionPrototypes
column_delete (XtPointer f)
#else
column_delete(f)
     XtPointer f;
#endif
{
  register XawTableNode p = (XawTableNode) f;

  if (p == null)
    return;

  /* Go up to the edge */
  for(; p->t != null; p=p->t )
    /*EMPTY*/;

  /* Go down with dettaching cell */
  for(; p->b != null; p=p->b){
    horiz_tab_node_reject(p->t);
    FREE(p->t);
  }

  /* Detach bottom but one cell in the column */
  if (p->t != null){
    horiz_tab_node_reject(p->t);
    FREE(p->t);
  }

  /* Detach very bottom cell in the column */
  horiz_tab_node_reject(p);
  FREE(p);
}



/*
**	Function name : row_insert_after
**
**	Description   : insert row down to the row which the node belongs to
**	Input         : poiter to the node, size of node
**	Output        : True if successful
*/

Boolean
#if NeedFunctionPrototypes
row_insert_after (XtPointer d, int node_size)
#else
row_insert_after(d, node_size)
     XtPointer d;
     int node_size;
#endif
{
  register XawTableNode f = (XawTableNode) d;
  register XawTableNode p;
  XawTableNode          left;
  
  if (f == null)
    return False;

  /* go left till row edge */
  for(; f->l != null; f=f->l )
    /*EMPTY*/;

  /* save very left node */
  left = f;
  
  /* go right and attach new cells via vertical ponters */
  for(; f->r != null; f=f->r){
    if( (p = (XawTableNode) XtMalloc (node_size)) == null){
      for(; left != f; left = left->r){
	vert_tab_node_reject(left->b);
	FREE(left->b);
      }
      return False;
    }
    vert_tab_node_insert(f, f->b, p);
  }

  /* attach vertically very right cell */
  if( (p = (XawTableNode) XtMalloc (node_size)) == null){
    for(; left != f; left = left->r){
      vert_tab_node_reject(left->b);
      FREE(left->b);
    }
    return False;
  }
  vert_tab_node_insert(f, f->b, p);

  /* if only one column */
  if ( f->l == null ) {
    f->b->l = f->b->r = null;
    return True;
  }
  
  /* attach horizontally very right cell */
  horiz_tab_node_insert(f->l->b,null,f->b);
  
  /* bind via horizontal ponters */
  for(f=f->l; f->l != null; f=f->l )
    horiz_tab_node_insert(f->l->b,f->r->b,f->b);
  
  /* attach horizontally very left cell */
  horiz_tab_node_insert(null,f->r->b,f->b);

  return True;
}

/*
**	Function name : row_insert_before 
**
**	Description   : the same as previous, but only on top from row
**	Input         : pointer to the node, size of node
**	Output        : True if successful
*/

Boolean 
#if NeedFunctionPrototypes
row_insert_before (XtPointer d, int node_size)
#else
row_insert_before(d, node_size)
     XtPointer d;
     int node_size;
#endif
{
  register XawTableNode f = (XawTableNode) d;
  register XawTableNode p;
  XawTableNode          left;
  
  if (f == null)
    return False;

  /* go left till row edge */
  for(; f->l != null; f=f->l )
    /*EMPTY*/;

  /* save very left node */
  left = f;
  
  /* go right and attach new cells via vertical ponters */
  for(; f->r != null; f=f->r){
    if( (p = (XawTableNode) XtMalloc (node_size)) == null){
      for(; left != f; left = left->r){
	vert_tab_node_reject(left->t);
	FREE(left->t);
      }
      return False;
    }
    vert_tab_node_insert(f->t, f, p);
  }

  /* attach vertically very right cell */
  if( (p = (XawTableNode) XtMalloc (node_size)) == null){
    for(; left != f; left = left->r){
      vert_tab_node_reject(left->t);
      FREE(left->t);
    }
    return False;
  }
  vert_tab_node_insert(f->t, f, p);

  /* if only one column */
  if ( f->l == null ) {
    f->t->l = f->t->r = null;
    return True;
  }
  
  /* attach horizontally very right cell */
  horiz_tab_node_insert(f->l->t,null,f->t);
  
  /* bind via horizontal ponters */
  for(f=f->l; f->l != null; f=f->l )
    horiz_tab_node_insert(f->l->t,f->r->t,f->t);
  
  /* attach horizontally very left cell */
  horiz_tab_node_insert(null,f->r->t,f->t);

  return True;
}

/*
**	Function name : column_insert_after
**
**	Description   : insert column right to the column which the node belongs to 
**	Input         : pointer to the node, size of node
**	Output        : True if successful
[<*/

Boolean
#if NeedFunctionPrototypes
column_insert_after (XtPointer d, int node_size)
#else
column_insert_after(d, node_size)
     XtPointer d;
     int node_size;
#endif
{
  register XawTableNode f = (XawTableNode) d;
  register XawTableNode p;
  XawTableNode          top;
  
  if (f == null)
    return False;

  /* go top till column edge */
  for(; f->t != null; f=f->t )
    /*EMPTY*/;

  /* save very top node */
  top = f;
  
  /* go down and attach new cells via horizontal ponters */
  for(; f->b != null; f=f->b){
    if( (p = (XawTableNode) XtMalloc (node_size)) == null){
      for(; top != f; top = top->b){
	horiz_tab_node_reject(top->r);
	FREE(top->r);
      }
      return False;
    }
    horiz_tab_node_insert(f, f->r, p);
  }

  /* attach horizontally very down cell */
  if( (p = (XawTableNode) XtMalloc (node_size)) == null){
    for(; top != f; top = top->b){
      horiz_tab_node_reject(top->r);
      FREE(top->r);
    }
    return False;
  }
    
  horiz_tab_node_insert(f, f->r, p);

  /* if only one row */
  if ( f->t == null ) {
    f->r->t = f->r->b = null;
    return True;
  }
  
  /* attach vertically very down cell */
  vert_tab_node_insert(f->t->r,null,f->r);
  
  /* bind via vertical ponters */
  for(f=f->t; f->t != null; f=f->t )
    vert_tab_node_insert(f->t->r,f->b->r,f->r);
  
  /* attach vertically very top cell */
  vert_tab_node_insert(null,f->b->r,f->r);

  return True;
}

/*
**	Function name : column_insert_before
**
**	Description   : the same as previous, but only to left from column 
**	Input         : pointer to node, size of node
**	Output        : True if successful 
*/

Boolean
#if NeedFunctionPrototypes
column_insert_before (XtPointer d, int node_size)
#else
column_insert_before(d, node_size)
     XtPointer d;
     int node_size;
#endif
{
  register XawTableNode f = (XawTableNode)d;
  register XawTableNode p;
  XawTableNode top;
  
  if (f == null)
    return False;

  /* go top till column top */
  for(; f->t != null; f=f->t )
    /*EMPTY*/;

  /* save very top node */
  top = f;
  
  /* go down and attach new cells via horizontal ponters */
  for(; f->b != null; f=f->b){
    if( (p = (XawTableNode) XtMalloc (node_size)) == null){
      for(; top != f; top = top->b){
	horiz_tab_node_reject(top->l);
	FREE(top->l);
      }
      return False;
    }
    horiz_tab_node_insert(f->l, f, p);
  }

  /* attach horizontally very down cell */
  if( (p = (XawTableNode) XtMalloc (node_size)) == null){
    for(; top != f; top = top->b){
      horiz_tab_node_reject(top->l);
      FREE(top->l);
    }
    return False;
  }
  horiz_tab_node_insert(f->l, f, p);

  /* if only one row */
  if ( f->t == null ) {
    f->l->t = f->l->b = null;
    return True;
  }
  
  /* attach vertically very down cell */
  vert_tab_node_insert(f->t->l, null, f->l);
  
  /* bind via vertical ponters */
  for(f=f->t; f->t != null; f=f->t )
    vert_tab_node_insert(f->t->l, f->b->l, f->l);
  
  /* attach vertically very top cell */
  vert_tab_node_insert(null, f->b->l, f->l);

  return True;
}

/*
**	Function name : get_table
**
**	Description   : spot the node (0,0) in the table
**	Input         : pointer to any node in table
**	Output        : pointer to the node (0,0) 
*/

XtPointer 
#if NeedFunctionPrototypes
get_table (XtPointer f)
#else
get_table(f)
     XtPointer f;
#endif
{
  register XawTableNode p = (XawTableNode) f;

  if (p == null)
    return (XtPointer)NULL;

  for(; p->t != null; p=p->t)
    /*EMPTY*/;

  for(; p->l != null; p=p->l)
    /*EMPTY*/;

  return (XtPointer)p;
}

/*
**	Function name : get_cell
**
**	Description   : spot the node (i,j) regarding to given node
**	Input         : pointer to the node, column, row (may be negative)
**	Output        : pointer to the node or NULL 
*/

XtPointer
#if NeedFunctionPrototypes
get_cell (XtPointer f, register int i, register int j)
#else
get_cell (f, i, j)
     XtPointer    f;
     register int i;
     register int j;
#endif
{
  register XawTableNode p = (XawTableNode) f;
  
  if (p == null)
    return (XtPointer)NULL;

  if (i > 0) {
    for(; i>0; i--)
      if (p != null) p = p->b; 
      else           return (XtPointer)NULL; 
  } else {
    for(; i<0; i++)
      if (p != null) p = p->t; 
      else           return (XtPointer)NULL;
  }
  
  if (j > 0) {
    for(; j>0; j--)
      if (p != null) p = p->r; 
      else           return (XtPointer)NULL;
  } else {
    for(; j<0; j++)
      if (p != null) p = p->l;
      else           return (XtPointer)NULL;
  }

  return (XtPointer)p;
}

#if 0
static Boolean go_row(w, proc, p, begin_column, end_column, i, j, client_data)
     XtPointer w;
     XawTableProc proc;
     XawTableNode   p;       /* p == get_cell(table, ... , begin_column) */
     int  begin_column;
     int  end_column;  
     int *i;         /* returned */
     int *j;         /* returned */
     XtPointer   client_data;
{
  for ((*j) = begin_column; ((*j) <= end_column) && (p != null); (*j)++) 
  {
    XawTableProc sp = p;
    p = p->r;

    if (proc(w, *i, *j, (XtPointer)sp, client_data))
      return True;
  }

  return False;
}
#endif

/*
**	Function name : go_table
**
**	Description   : invoke given rutine for every node in given region
**	Input         : rutine, begin/end rows, begin/end columns...
**	Output        : True if given rutine returned True for a node,
**                      numbers of row and column for that node
 */                

Boolean 
#if NeedFunctionPrototypes
go_table (
	  XtPointer w,
	  XawTableProc proc,
	  XtPointer table,
	  int  begin_row,    
	  int  end_row,
	  int  begin_column,
	  int  end_column,
	  int  direction,
	  register int *i,            /* returned */
	  register int *j,            /* returned */
	  XtPointer   client_data)
#else
go_table(w, proc, table, begin_row, end_row, begin_column, end_column,
	 direction, i, j, client_data)
     XtPointer w;
     XawTableProc proc;
     XtPointer table;
     int  begin_row;
     int  end_row;
     int  begin_column;
     int  end_column;
     int direction;
     register int *i;               /* returned */
     register int *j;               /* returned */
     XtPointer   client_data;
#endif
{
  register XawTableNode p;
  register XawTableNode n;

  table = get_table(table);

  switch (direction) {
  case XawTABLE_DOWN_RIGHT :
    p = (XawTableNode)get_cell(table, begin_row, begin_column);
    
    for (*j = begin_column; *j <= end_column && p != null; (*j)++) 
    {
      register XawTableNode sp = p;  /* protect against deallocated node !! */
      p = p->r;
      
      for (*i = begin_row, n = sp; *i <= end_row && n != null; (*i)++) 
      {
	register XawTableNode sn = n; /* protect against deallocated node !! */
	n = n->b;
	
	if (proc(w, *i, *j, (XtPointer)sn, client_data))
	  return True;
      }
    }
    break;
  case XawTABLE_RIGHT_DOWN :
  default :
    p = (XawTableNode)get_cell(table, begin_row, begin_column);
    
    for (*i = begin_row; *i <= end_row && p != null; (*i)++) 
    {
      register XawTableNode sp = p;  /* protect against deallocated node !! */
      p = p->b;
      
      for (*j = begin_column, n = sp; *j <= end_column && n != null; (*j)++) 
      {
	register XawTableNode sn = n; /* protect against deallocated node !! */
	n = n->r;
	
	if (proc(w, *i, *j, (XtPointer)sn, client_data))
	  return True;
      }
    }
    break;
  }
  return False;
}

/*
**	Function name : get_table_size
**
**	Description   : define dimention on the table
**	Input         : pointer to any node in the table
**	Output        : void
*/

void
#if NeedFunctionPrototypes
get_table_size (XtPointer f,
		register int *i,   /* returned */
		register int *j)   /* returned */
#else
get_table_size(f,i,j)
     XtPointer f;
     register int *i;   /* returned */
     register int *j;   /* returned */
#endif
{
  register XawTableNode p = (XawTableNode)f;
  if (p == null){
    *i = 0;
    *j = 0;
  }

  p = (XawTableNode)get_table(f);
  
  for (*i = 1; p->b != null; p = p->b, (*i)++)
    /*EMPTY*/;

  for (*j = 1; p->r != null; p = p->r, (*j)++)
    /*EMPTY*/;
}

/*
**	Function name : delete_table
**
**	Description   : destroy table
**	Input         : pointer to any node in the table
**	Output        : void
*/

void
#if NeedFunctionPrototypes
delete_table (XtPointer f)
#else
delete_table(f)
     XtPointer f;
#endif
{
  register XawTableNode p = (XawTableNode)f;
  int i,j;
  int end_row, end_column;

  if (p == null)
    return;

  p = (XawTableNode)get_table(f);
  get_table_size((XtPointer)p, (int*)&end_row, (int*)&end_column);
  (void)go_table(NULL, (XawTableProc)del_cell, (XtPointer)p,
		 0, (int)(end_row-1), 0, (int)(end_column-1),
		 XawTABLE_RIGHT_DOWN,
		 (int*)&i, (int*)&j, NULL);
}

/*
**	Function name : get_cell_positions
**
**	Description   : define number of row & column for node
**	Input         : pointer to the node
**	Output        : void
*/

void 
#if NeedFunctionPrototypes
get_cell_positions (XtPointer f,
                    register int *i,   /* returned */
                    register int *j)   /* returned */
#else
get_cell_positions(f, i, j)
     XtPointer          f;
     register int *i;  /* returned */
     register int *j;  /* returned */
#endif
{
  register XawTableNode p = (XawTableNode)f;
  if ( p == null )
    return;
  
  if ( i != (int*)NULL ) {
    for (*i = 0; p->t != null; p = p->t, (*i)++)
      /*EMPTY*/;
  }

  if ( j != (int*)NULL ) {
    for (*j = 0; p->l != null; p = p->l, (*j)++)
      /*EMPTY*/;
  }
}

/*
**	Function name : create_table
**
**	Description   : create the table
**	Input         : row & column dimestions, size of node
**	Output        : pointer to the node (0,0)
*/

XtPointer
#if NeedFunctionPrototypes
create_table ( int rows, int columns, int node_size)
#else
create_table(rows, columns, node_size)
     int rows;
     int columns;
     int node_size;
#endif
{
  register XawTableNode *area;
  register XawTableNode  p;
  register int i,j;
  XawTableNode           table;
  
  if (rows == 0 || columns == 0)
    return (XtPointer)NULL;
  else{
    register XawTableNode *s;

    /* allocate temporary two-dimension array to make first node's binding */
    if ( (s = area = (XawTableNode*)
	  XtCalloc ((unsigned)(rows * columns), sizeof(XawTableNode))) == NULL)
      return (XtPointer)NULL;
    
    /* allocate nodes */
    for (i = 0, j = rows*columns;  i < j; i++)
      if((*s++ = (XawTableNode) XtMalloc (node_size)) == NULL){
	int h;
	for (h = 0, s = area;  h < i; h++)
	  XtFree((char*)*s++);
	
	XtFree((char*)area);
	return (XtPointer)NULL;
      }
  }
  
#define a(i,j) (XawTableNode)*(area + (i)*columns + (j))

  /* initialize the boundary nodes */
  for (i = 0; i < rows; i++) {
    p = a(i,0);         p->l = null;
    p = a(i,columns-1); p->r = null;
  }
  for (j = 0; j < columns; j++) {
    p = a(0,j);         p->t = null;
    p = a(rows-1,j);    p->b = null;
  }

#undef a
#define a(i,j) (( (i)>=0 && (i)<rows ) && ( (j)>=0 && (j)<columns ) ? \
		(XawTableNode)*(area + (i)*columns + (j)) : null)

  /* make internode's binding */
  for (i = 0; i < rows; i++) {
    for (j = i % 2; j < columns; j += 2) {
      horiz_tab_node_insert(a(i,j-1), a(i,j+1), a(i,j));
      vert_tab_node_insert (a(i-1,j), a(i+1,j), a(i,j));
    }
  }

#undef a

  table = *area;
  XtFree((char*)area);
  return (XtPointer)table;
}


#ifdef EBUG_XRAW_MALLOC
/* ARGSUSED */
static Boolean check_cell (w, row, column, call_data, client_data)
     XtPointer w;   /* unused */
     int row;      
     int column;      
     XtPointer call_data;
     XtPointer client_data;
{
  register XawTableNode p = (XawTableNode)call_data;
  int real_row, real_column;
  char *halt = NULL;;
  
  get_cell_positions(p, &real_row, &real_column);

  if (real_row != row){
    XtWarning("check_table: wrong initial table row size");
    *halt = '\0';
  }

  if (real_column != column){
    XtWarning("check_table: wrong initial table column size");
    *halt = '\0';
  }

  if (p->l != null) 
    if (p->l->r != p) {
      XtWarning("check_table: wrong cell");
      *halt = '\0';
    }
  if (p->r != null) 
    if (p->r->l != p) {
      XtWarning("check_table: wrong cell");
      *halt = '\0';
    }
  if (p->t != null) 
    if (p->t->b != p) {
      XtWarning("check_table: wrong cell");
      *halt = '\0';
	}
  if (p->b != null) 
    if (p->b->t != p) {
      XtWarning("check_table: wrong cell");
      *halt = '\0';
    }
  
  return False;
}



void
#if NeedFunctionPrototypes
_check_table (XtPointer f, int rows, int columns)
#else
_check_table (f, rows, columns)
     XtPointer f;
     int rows;
     int columns;
#endif
{
  register XawTableNode table = (XawTableNode)f;
  int real_rows, real_columns;
  register int i,j;
  char *halt = NULL;

  if (f == NULL && (rows == 0 || columns == 0))
    return;

  if (table != get_table(f)){
    XtWarning("check_table: wrong initial table cell");
    *halt = '\0';
  }
    
  get_table_size (f, &real_rows, &real_columns);

  if (real_rows != rows){
    XtWarning("check_table: wrong initial table row size");
    *halt = '\0';
  }

  if (real_columns != columns){
    XtWarning("check_table: wrong initial table column size");
    *halt = '\0';
  }


  (void) go_table (NULL, check_cell, table, 
		   0, rows-1, 0, columns-1,
		   XawTABLE_RIGHT_DOWN, 
		   &real_rows, &real_columns, (XtPointer)NULL);
  
}
#endif /* EBUG_XRAW_MALLOC */

#undef null
#undef FREE

