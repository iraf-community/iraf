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

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include <X11/Xatom.h>
#include <X11/IntrinsicP.h>
#include <X11/RectObjP.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>
#include <X11/Xos.h>

#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/Converters.h>
#include <X11/Xmu/Drawing.h>

#include <X11/Xraw/XawInit.h>
#include <X11/Xraw/3d.h>
#include <X11/Xraw/TableP.h>
#include <X11/Xraw/Frame.h>
#include <X11/Xraw/AsciiText.h>
#include <X11/Xraw/Viewport.h>
#include <X11/Xraw/Scrollbar.h>
#include <X11/Xraw/ScrolledTable.h>

#ifdef EBUG_XRAW_MALLOC
#include <dbmalloc/malloc.h>
#endif

#define MULTI_LINE_TABLE 32767


#undef CELL_IN /* */

#ifdef MIN
#undef MIN
#endif
#define MIN(a,b) ((a)<(b) ? (a) : (b))

#ifdef MAX
#undef MAX
#endif
#define MAX(a,b) ((a) > (b) ? (a) : (b))

#define CALLOC(num,type) (num * sizeof(type) <= 0 ? (type*)NULL : \
  (type*)XtMalloc((Cardinal)(num) * (Cardinal)sizeof(type)))

#define NOT_A_CUT_BUFFER (-1)

#define MAXCUT         30000
#define EDIT(w)        (((XawTableWidget) w)->table.edit)
#define InRange(n,a,b)  MAX(a,MIN(n,b))

#define LEFT_EDGE(tw)  (((XawTableWidget)tw)->table.internal_width + \
		       ((XawTableWidget)tw)->table.label_shadow_thickness)

#define TOP_EDGE(tw)   (((XawTableWidget)tw)->table.internal_height + \
		       ((XawTableWidget)tw)->table.label_shadow_thickness)

#define CELL(field)    cell->field
#define STUFF(w)       ((XawTableWidget)w)->table.table_stuff
#define COLUMNS(w)     ((XawTableWidget)w)->table.columns
#define ROWS(w)        ((XawTableWidget)w)->table.rows
#define GET_CELL_LABEL ((CELL(label) == NULL ? Dummy : CELL(label)))

#define COLUMN_DATA(tw) ((XawTableColumnRec*)tw->table.column_data)
#define COLUMN_WIDTH(tw,column) \
(tw->table.literal ?                                               \
	             (                                             \
                       (COLUMN_DATA(tw)[column].flag & _CL_width) ?\
                          COLUMN_DATA(tw)[column].width :          \
                          tw->table.column_default_width           \
                     )                                             \
                     * tw->table.literal_width +                   \
		     2 * tw->table.internal_width                  \
		   :                                               \
                    (                                              \
                       (COLUMN_DATA(tw)[column].flag & _CL_width) ?\
                          COLUMN_DATA(tw)[column].width :          \
                          tw->table.column_default_width           \
                     )                                             \
/* _COLUMN_WIDTH_ */)


#define MANAGE_EDIT(tw)                                            \
  if ( ! XtIsManaged(EDIT(tw)))                                    \
  {                                                                \
    XtManageChild (EDIT(tw));                                      \
    XtSetKeyboardFocus ((Widget)tw, EDIT(tw));                     \
  }


#define UNMANAGE_EDIT(tw)                                          \
  if ( XtIsManaged(EDIT(tw)))                                      \
  {                                                                \
    XtUnmanageChild (tw->table.edit);                              \
    XtSetKeyboardFocus((Widget)tw, (Widget)None);                  \
  }


#define DO_CALLBACK(w,callback,data)                               \
  if (XtCallbackHasSome == XtHasCallbacks(w, callback))            \
    XtCallCallbacks (w, callback, (XtPointer)&data)
  

#define IsEditInRowColumn(tw, row, column)                         \
  (XtIsManaged(EDIT(tw)) && (row == tw->table.edit_row)            \
   && (column == tw->table.edit_column))



#define REJECT (-1)
#define ACCEPT (0)

#define CHECK_TABLE(tw)                                            \
  _check_table ((XtPointer)STUFF(tw), ROWS(tw), COLUMNS(tw));      \
  CheckAllLabels(tw)



typedef struct _XawTableCellRec {
                               /* Node communication entity */
  XawTableNodeRec node;
                               /* Cell label entity */
  char        *label;
  Position     label_x;
  Dimension    label_width;
  Dimension    label_len;
                               /* Cell colour entity */
  Boolean      highlight;
  Boolean      special_colour;
  Pixel        fore;
  Pixel        back;
  GC           normal;
  GC           reverse;
  GC           top;
  GC           bottom;

}XawTableCellRec;

#define _CL_width      (1L<<0)
#define _CL_font       (1L<<1)
#define _CL_label      (1L<<2)
#define _CL_justify    (1L<<3)
#define _CL_background (1L<<4)
#define _CL_foreground (1L<<5)

typedef struct _XawTableColumnRec {
  int           flag;
  int           width;
  XFontStruct  *font;
  char         *label;
  XtJustify     justify;
  Pixel         background;
  Pixel         foreground;
  GC            normal;
  GC            reverse;
  GC            top;
  GC            bottom;
}XawTableColumnRec;


#ifdef CRAY
#define WORD64
#endif

/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

/* Private Data */

static void Def_pixel();
static void Def_scroll();
static void Def_column_default_width();
static void Def_literal_width();
static void Def_shadow_thickness();

#define Offset(field) XtOffsetOf(TableRec, field)

static XtResource resources[] = {
    {
      XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
      Offset(table.foreground), XtRString, XtDefaultForeground
    },
    {
      XtNeditForeground, XtCForeground, XtRPixel, sizeof(Pixel),
      Offset(table.edit_fore), XtRCallProc, (XtPointer)Def_pixel
    },
    {
      XtNeditBackground, XtCBackground, XtRPixel, sizeof(Pixel),
      Offset(table.edit_back), XtRCallProc, (XtPointer)Def_pixel
    },
    {
      XtNrowForeground, XtCForeground, XtRPixel, sizeof(Pixel),
      Offset(table.row_fore), XtRCallProc, (XtPointer)Def_pixel
    },
    {
      XtNcolumnForeground, XtCForeground, XtRPixel, sizeof(Pixel),
      Offset(table.column_fore), XtRCallProc, (XtPointer)Def_pixel
    },
    {
      XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
      Offset(table.font), XtRString, XtDefaultFont
    },
    {
      XtNliteral,  XtCLiteral, XtRBoolean, sizeof(Boolean),
      Offset(table.literal), XtRImmediate, (XtPointer)False
    },
    {
      XtNrowOriented,  XtCRowOriented, XtRBoolean, sizeof(Boolean),
      Offset(table.row_oriented), XtRImmediate, (XtPointer)True
    },
    {
      XtNmaskNumber,  XtCMaskNumber, XtRInt, sizeof(int),
      Offset(table.mask_number), XtRImmediate, (XtPointer)7
    },
    {
      XtNcolumns,  XtCColumns, XtRInt, sizeof(int),
      Offset(table.columns), XtRImmediate, (XtPointer)0
    },
    {
      XtNrows,  XtCRows, XtRInt, sizeof(int),
      Offset(table.rows), XtRImmediate, (XtPointer)0
    },
    {
      XtNtableMargin,  XtCTableMargin, XtRDimension, sizeof(Dimension),
      Offset(table.tab_margin), XtRImmediate, (XtPointer)2
    },
    {
      XtNrowMargin,  XtCRowMargin, XtRDimension, sizeof(Dimension),
      Offset(table.row_margin), XtRImmediate, (XtPointer)2
    },
    {
      XtNcolumnMargin,  XtCColumnMargin, XtRDimension, sizeof(Dimension),
      Offset(table.col_margin), XtRImmediate, (XtPointer)2
    },
    {
      XtNlabelShadowWidth,  XtCLabelShadowWidth, XtRDimension,
      sizeof(Dimension), Offset(table.label_shadow_thickness), XtRImmediate,
      (XtPointer)1
    },
    {
      XtNjustify, XtCJustify, XtRJustify, sizeof(XtJustify),
      Offset(table.justify), XtRImmediate, (XtPointer)XtJustifyCenter
    },
    {
      XtNinternalWidth, XtCWidth, XtRDimension,  sizeof(Dimension),
      Offset(table.internal_width), XtRImmediate, (XtPointer)4
    },
    {
      XtNinternalHeight, XtCHeight, XtRDimension, sizeof(Dimension),
      Offset(table.internal_height), XtRImmediate, (XtPointer)2
    },
    {
      XtNencoding, XtCEncoding, XtRUnsignedChar, sizeof(unsigned char),
      Offset(table.encoding), XtRImmediate, (XtPointer)XawTextEncoding8bit
    },
    {
      XtNshadowWidth, XtCShadowWidth, XtRDimension, sizeof(Dimension),
      Offset(container.shadow_thickness), XtRCallProc,
      (XtPointer) Def_shadow_thickness
    },
    {
      XtNvetricalScroll, XtCScroll, XtRWidget, sizeof(Widget),
      Offset(table.v_scroll), XtRCallProc, (XtPointer)Def_scroll
    },
    {
      XtNhorizontalScroll, XtCScroll, XtRWidget, sizeof(Widget),
      Offset(table.h_scroll), XtRCallProc, (XtPointer)Def_scroll
    },
    {
      XtNrowHeight, XtCRowHeight, XtRInt, sizeof(int),
      Offset(table.row_height), XtRImmediate, (XtPointer)0
    },
    {
      XtNdefaultWidth, XtCDefaultWidth, XtRInt, sizeof(int),
      Offset(table.column_default_width), XtRCallProc,
      (XtPointer) Def_column_default_width
    },
    {
      XtNeditable, XtCEditable, XtRBoolean, sizeof(Boolean),
      Offset(table.editable), XtRImmediate, (XtPointer) TRUE
    },
    {
      XtNliteralWidth, XtCLiteralWidth, XtRInt, sizeof(int),
      Offset(table.literal_width), XtRCallProc, (XtPointer) Def_literal_width
    },

                      /* ALLOWANCE CALLBACKS */
    {
      XtNallowAddColumn, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.allow_add_column), XtRCallback, NULL
    },
    {
      XtNallowAddRow, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.allow_add_row), XtRCallback, NULL
    },
    {
      XtNallowDeleteColumn, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.allow_delete_column), XtRCallback, NULL
    },
    {
      XtNallowDeleteRow, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.allow_delete_row), XtRCallback, NULL
    },
    {
      XtNallowDeleteTable, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.allow_delete_table), XtRCallback, NULL
    },
                      /* INFORMATION CALLBACKS */

    {
      XtNaddColumn, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.add_column), XtRCallback, NULL
    },
    {
      XtNaddRow, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.add_row), XtRCallback, NULL
    },
    {
      XtNcreateTable, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.create_table), XtRCallback, NULL
    },
    {
      XtNchangedCell, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.changed_cell), XtRCallback, NULL
    },
    {
      XtNchangedColumnWidth, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.changed_column_width), XtRCallback, NULL
    },
    {
      XtNchangedRowHeight, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.changed_row_height), XtRCallback, NULL
    },
    {
      XtNdeleteColumn, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.delete_column), XtRCallback, NULL
    },
    {
      XtNdeleteRow, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.delete_row), XtRCallback, NULL
    },
    {
      XtNdeleteTable, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.delete_table), XtRCallback, NULL
    },
    {
      XtNwhatCell, XtCCallback, XtRCallback, sizeof(XtPointer),
      Offset(table.what_cell), XtRCallback, NULL
    },
};

#define PRINTF_BOOL(a) ((a) ? "TRUE":"FALSE")
#define PRINTF_NULL(a) ((a)==NULL ? "NULL":"not NULL")

static void Def_pixel(w, offset, value)
     Widget    w;
     int       offset;
     XrmValue *value;
{
  XawTableWidget tw = (XawTableWidget)w;
  static Pixel pix = 0;
  
  
  if (      offset == (int)Offset(table.edit_fore))
    pix = tw->table.foreground;
  else if ( offset == (int)Offset(table.edit_back))
  {
    if (!FetchPixel(w, "#d0d000", &pix))
      pix = WhitePixelOfScreen(XtScreen(w));
  }
  else 
    pix = tw->core.background_pixel;

  value->addr = (XtPointer)& pix;
}

/* ARGSUSED */
static void Def_scroll(w, offset, value)
     Widget    w;
     int       offset;
     XrmValue *value;
{
  static Widget view;

  for (view = XtParent(w);
       (view != (Widget)NULL) && (!XtIsSubclass(view, viewportWidgetClass));
       view = XtParent(view))
    /* EMPTY */;

  if (view) 
  {
    if (offset == Offset(table.v_scroll))
    {
      if (((view = XtNameToWidget(view, "vertical")) == (Widget)NULL)
	  || !XtIsSubclass(view, scrollbarWidgetClass))
	view = (Widget)NULL;
    }
    else
    if (offset == Offset(table.h_scroll))
    {
      if (((view = XtNameToWidget(view, "horizontal")) == (Widget)NULL)
	  || !XtIsSubclass(view, scrollbarWidgetClass))
	view = (Widget)NULL;
    }
  }
  
  value->addr = (XtPointer)&view;
}

/* ARGSUSED */
static void Def_column_default_width(w, offset, value)
     Widget    w;
     int       offset;
     XrmValue *value;
{
  XawTableWidget tw = (XawTableWidget) w;
  static int column_default_width;

  if (tw->table.literal)
    column_default_width = 10;
  else
    column_default_width = 60;

  value->addr = (XtPointer)&column_default_width;
}

#ifndef WORD64
#define TXT_16 XChar2b
#else
#define TXT_16 char
#endif

/* ARGSUSED */
static void Def_literal_width(w, offset, value)
     Widget    w;
     int       offset;
     XrmValue *value;
{
  XawTableWidget tw = (XawTableWidget) w;
  static int literal_width;

  if (tw->table.encoding)
    tw->table.literal_width = XTextWidth16(tw->table.font, (TXT_16*)"mmm", 3);
  else
    tw->table.literal_width = XTextWidth(tw->table.font, "mmm", 3);

  tw->table.literal_width /= 3;
  
  value->addr = (XtPointer)& literal_width;
}

/* ARGSUSED */
static void Def_shadow_thickness(w, offset, value)
     Widget    w;
     int       offset;
     XrmValue *value;
{
  Widget parent = XtParent (w);
  static Dimension shadow_thickness;
  
  if (XtIsSubclass (parent, scrolledTableWidgetClass)
      ||
      XtIsSubclass (parent, viewportWidgetClass))
    shadow_thickness = 0;
  else
    shadow_thickness = 2;

  value->addr = (XtPointer)& shadow_thickness;
}


#undef offset

static void MultipleChangeGC();
static void Initialize();
static void Realize();
static void Resize();
static void Redisplay();
static void Destroy();

static Boolean SetValues();

static void WalkForCells();
static void LoseSelection();

static void CallEdit();
static void InsertSelection();
static void StoreBuffer();
static void WhatCell();
static void KeyReturn();
static void HighlightCell();
static void UnhighlightCell();
static void DoingNothing();

static char* DummyString();

static Boolean InitCell();

static XtGeometryResult QueryGeometry();
static XtGeometryResult GeometryManager();

static XtActionsRec actions[] = {
  {"call-edit",        (XtActionProc)CallEdit       },
  {"insert-selection", (XtActionProc)InsertSelection},
  {"store-in-buffer",  (XtActionProc)StoreBuffer    },
  {"what_cell",        (XtActionProc)WhatCell       },
  {"key_return",       (XtActionProc)KeyReturn      },
  {"highlight",        (XtActionProc)HighlightCell  },
  {"unhighlight",      (XtActionProc)UnhighlightCell},
  {"no-op",            (XtActionProc)DoingNothing   }
};

static char translations[] =
  "Ctrl<Btn1Up>:     what_cell()                                    \n\
   <Btn1Up>:         call-edit()                                    \n\
   <Btn2Up>:         insert-selection(PRIMARY, CUT_BUFFER0)         \n\
   <Btn3Up>:         highlight() store-in-buffer(PRIMARY,CUT_BUFFER0) ";

static char edit_translations[] =
  " <Key>Return:     key_return()  \n\
    <Key>Linefeed:   key_return()  \n\
    <Key>Down:       no-op(r)      \n\
    <Key>Up:         no-op(r)      ";

static char* Dummy = "";


#define SuperClass ((ContainerWidgetClass)&containerClassRec)

TableClassRec tableClassRec = {
  { /* core_class fields */	
    /* superclass	  	*/	(WidgetClass) SuperClass,
    /* class_name	  	*/	"Table",
    /* widget_size	  	*/	sizeof(TableRec),
    /* classInitialize   	*/	NULL,
    /* class_partInitialize	*/	NULL,
    /* class_inited       	*/	FALSE,
    /* initialize	  	*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize		  	*/	Realize,
    /* actions		  	*/	actions,
    /* num_actions	  	*/	XtNumber(actions),
    /* resources	  	*/	resources,
    /* num_resources	  	*/	XtNumber(resources),
    /* xrm_class	  	*/	NULLQUARK,
    /* compress_motion	  	*/	TRUE,
    /* compress_exposure  	*/	FALSE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest	  	*/	TRUE,
    /* destroy		  	*/	Destroy,
    /* resize		  	*/	Resize,
    /* expose		  	*/	Redisplay,
    /* set_values	  	*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus	 	*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private   	*/	NULL,
    /* tm_table		   	*/	translations,
    /* query_geometry		*/	QueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
    /* Composite class part */
  {
    /* geometry manager         */      GeometryManager,
    /* change_managed           */      XtInheritChangeManaged,
    /* insert_child             */      XtInheritInsertChild,
    /* delete_child             */      XtInheritDeleteChild,
    /* extension                */      NULL,
  },
    /* Constraint class part */
  {
    /* subresources             */      NULL,
    /* subresource_count        */      0,   
    /* constraint_size          */      0,   
    /* initialize               */      NULL,
    /* destroy                  */      NULL,
    /* set_values               */      NULL,
    /* extension                */      NULL,
  },
    /* Container class part */
  {
    /* ignore                   */      0,
  },
  { /* Table class fields initialization */
    /* ignore 			*/	0
  }
};

WidgetClass tableWidgetClass = (WidgetClass)&tableClassRec;


/****************************************************************
 *
 *                   Private procedures
 *
 ****************************************************************/
#define NORMAL_INDEX(fore, back) \
  (int)(((int)fore * (int)back) & tw->table.mask_hash_table)

#define SHADOW_INDEX(back) (int)(((int)back) & tw->table.mask_hash_table)

#define GET_NORMAL(tw,i) (tw->table.normal_hash_table+(i))
#define GET_SHADOW(tw,i) (tw->table.shadow_hash_table+(i))

#define NEXT(i) (int)((i+1) & tw->table.mask_hash_table)

static int MaskStaticArray[] = {
                             0x1,   0x3,   0x7,   0xF,
			     0x1F,  0x3F,  0x7F,  0xFF,
			     0x1FF, 0x3FF, 0x7FF, 0xFFF
			  };

static NormalReverseGC*  GetNormalGC(w, fore, back, font)
     Widget w;
     Pixel fore;
     Pixel back;
     Font font;
{
  register XawTableWidget tw = (XawTableWidget)w;
  int i = NORMAL_INDEX(fore, back);

  while (GET_NORMAL(tw,i)->used > 0)
  {

    if (GET_NORMAL(tw,i)->fore == fore && GET_NORMAL(tw,i)->back == back)
    {
      GET_NORMAL(tw,i)->used++;
      return GET_NORMAL(tw,i);
    }
    i = NEXT(i);
  }
  
  if (GET_NORMAL(tw,i)->used == 0)
  {
    XtGCMask mask;
    XGCValues values;
    Display *dpy = XtDisplay(w);
    Window   win = XtIsRealized(w) ? XtWindow(w) : XDefaultRootWindow(dpy);
    

    mask = GCForeground | GCBackground | GCFont;
    values.foreground = fore;
    values.background = back;
    values.font       = font;

    GET_NORMAL(tw,i)->normal = XCreateGC(dpy, win, mask, &values);
    values.foreground = back;
    values.background = fore;
    GET_NORMAL(tw,i)->reverse = XCreateGC(dpy, win, mask, &values);

    GET_NORMAL(tw,i)->used = 1;
    GET_NORMAL(tw,i)->fore = fore;
    GET_NORMAL(tw,i)->back = back;

    return GET_NORMAL(tw,i);
  } 

  return (NormalReverseGC*)NULL;
}

static void GetGCByForeground(w, gc, fore)
     Widget  w;
     GC     *gc;
     Pixel   fore;
{
  XGCValues values;
  
  values.foreground = fore;
  (*gc) = XtGetGC(w, GCForeground, &values);
}


static ShadowGC* GetShadowGC(w, back)
     Widget w;
     Pixel back;
{
  register XawTableWidget tw = (XawTableWidget)w;
  int i = SHADOW_INDEX(back);
  
  while (GET_SHADOW(tw,i)->used > 0)
  {
    if (GET_SHADOW(tw,i)->back == back)
    {
      GET_SHADOW(tw,i)->used++;
      return GET_SHADOW(tw,i);
    }
    i = NEXT(i);
  }
  
  if (GET_SHADOW(tw,i)->used == 0)
  {
    Pixel top;
    Pixel bottom;
    
    GET_SHADOW(tw,i)->used   = 1;
    GET_SHADOW(tw,i)->back   = back;

    (void)TopShadowColor(w, back, &top);
    (void)BottomShadowColor(w, back, &bottom);
#ifdef CELL_IN
    GetGCByForeground(w, (GC*)&GET_SHADOW(tw,i)->bottom, top);
    GetGCByForeground(w, (GC*)&GET_SHADOW(tw,i)->top, bottom);
#else
    GetGCByForeground(w, (GC*)&GET_SHADOW(tw,i)->bottom, bottom);
    GetGCByForeground(w, (GC*)&GET_SHADOW(tw,i)->top, top);
#endif
    return GET_SHADOW(tw,i);
  } 

  return (ShadowGC*)NULL;
}

static void ReleaseNormalGC(w, fore, back)
     Widget w;
     Pixel fore;
     Pixel back;
{
  register XawTableWidget tw = (XawTableWidget)w;
  int i = NORMAL_INDEX(fore, back);
  
  while (GET_NORMAL(tw,i)->used > 0)
  {
    if ( GET_NORMAL(tw,i)->fore == fore &&  GET_NORMAL(tw,i)->back == back)
    {
      if (--GET_NORMAL(tw,i)->used == 0) {
	XFreeGC(XtDisplay(w), GET_NORMAL(tw,i)->normal);
	XFreeGC(XtDisplay(w), GET_NORMAL(tw,i)->reverse);
      }
      break;
    }

    i = NEXT(i);
  }
}

static void ReleaseShadowGC(w, back)
     Widget w;
     Pixel back;
{
  register XawTableWidget tw = (XawTableWidget)w;
  int i = SHADOW_INDEX(back);
  
  while (GET_SHADOW(tw,i)->used > 0)
  {
    if ( GET_SHADOW(tw,i)->back == back)
    {
      if (--GET_SHADOW(tw,i)->used == 0)
      {
	XtReleaseGC(w, GET_SHADOW(tw,i)->top);
	XtReleaseGC(w, GET_SHADOW(tw,i)->bottom);
      }
      break;
    }
    i = NEXT(i);
  }
}

static void MultipleChangeGC(w, fore, back, font, normal, reverse)
     Widget w;
     Pixel *fore;
     Pixel *back;
     Font  *font;
     GC    *normal;
     GC    *reverse;
{
  XtGCMask mask;
  XGCValues values;

  if (normal != (GC*)NULL){
    mask = (XtGCMask)0;
    if (fore != (Pixel*)NULL) {
      mask |= GCForeground;
      values.foreground = *fore;
    }
    if (back != (Pixel*)NULL) {
      mask |= GCBackground;
      values.background = *back;
    }
    if (font != (Font*)NULL) {
      mask |= GCFont;
      values.font = *font;
    }
    XChangeGC(XtDisplay(w), *normal, mask, &values);
  }

  if (reverse != (GC*)NULL){
    mask = (XtGCMask)0;
    if (fore != (Pixel*)NULL) {
      mask |= GCForeground;
      values.foreground = *back;        /* reverse */
    }
    if (back != (Pixel*)NULL) {
      mask |= GCBackground;
      values.background = *fore;        /* reverse */
    }
    if (font != (Font*)NULL) {
      mask |= GCFont;
      values.font = *font;
    }
    XChangeGC(XtDisplay(w), *reverse, mask, &values);
  }
}

#ifndef WORD64

#define TXT16 XChar2b

#else

#define TXT16 char

static XChar2b *buf2b;
static int buf2blen = 0;

#define XTextWidth16 _XawTableWidth16
#define XDrawString16 _XawTableDraw16

#endif /* WORD64 */


static void CalculatePreferredSize(w, width, height)
     Widget w;
     Dimension *width;
     Dimension *height;
{
  register XawTableWidget tw = (XawTableWidget)w;
  register int wid;
  register int hei;
  int i;
  
  /*
   * Calculate preferred width
   */

  wid  = 2 * (tw->table.tab_margin + tw->container.shadow_thickness);
  wid += COLUMNS(tw) ? (COLUMNS(tw) - 1) * tw->table.col_margin : 0;
  wid += 2 * COLUMNS(tw) * tw->table.label_shadow_thickness;
  
  for(i = 0 ; i < COLUMNS(tw); i++)
    wid += (Dimension)COLUMN_WIDTH(tw,i);

  /*
   * Calculate preferred height
   */

  hei  = 2 * (tw->table.tab_margin + tw->container.shadow_thickness);
  hei += ROWS(tw) ? (ROWS(tw) - 1) * tw->table.row_margin : 0;
  hei += ROWS(tw) * tw->table.row_height;
  hei += 2 * ROWS(tw) * tw->table.label_shadow_thickness;

  tw->table.prefer_width  = wid;
  tw->table.prefer_height = hei;

  if (width)  *width  = wid;
  if (height) *height = hei;

}

static Position GetX(tw,j)
     register XawTableWidget tw;
     int j;
{
  register TablePart* table = (TablePart*)&tw->table;
  register Position x;

  x = j * (table->col_margin + 2 * table->label_shadow_thickness) +
          (table->tab_margin + tw->container.shadow_thickness);

  for(; j > 0 ; j--)
  {
    register int tmp = j - 1;
    x += (Position)COLUMN_WIDTH(tw, tmp);
  }
  return x;
}

static Position GetY(tw,i)
     XawTableWidget tw;
     int i;
{
  return(i * (tw->table.row_margin + tw->table.row_height +
	       2 * tw->table.label_shadow_thickness) +
	      (tw->table.tab_margin + tw->container.shadow_thickness));
}


/*
 * Calculate width and height of displayed text in pixels
 */

static void SetLabelHeight(tw)
     XawTableWidget tw;
{
  register XFontStruct *fs = tw->table.font;
  int row_height = tw->table.row_height;

  if (tw->table.row_height < 1)
    tw->table.row_height = (fs->max_bounds.ascent  +
			    fs->max_bounds.descent +
			    2 * tw->table.internal_height);
  if (row_height != tw->table.row_height)
  {
    XawTableCallbackStruct callback_str;
    callback_str.reason   = XawTABLE_CHANGED_ROW_HEIGHT;
    callback_str.event    = (XEvent*)NULL;
    callback_str.old_cell = (XawTableCell)NULL;
    callback_str.new_cell = (XawTableCell)NULL;
    callback_str.row      = 0;
    callback_str.column   = 0;
    callback_str.do_it    = True;
    
    DO_CALLBACK((Widget)tw, XtNchangedRowHeight, callback_str);
  }
}

static void SetLiteralWidth(tw)
     XawTableWidget tw;
{
  if (tw->table.encoding)
    tw->table.literal_width = XTextWidth16(tw->table.font, (TXT16*)"mmm", 3);
  else
    tw->table.literal_width = XTextWidth(tw->table.font, "mmm", 3);

  tw->table.literal_width /= 3;
}


static void SetLabelWidth(tw,i,j)
     XawTableWidget tw;
     int i,j;
{
  register XFontStruct *fs = tw->table.font;
  XawTableCell cell;

  cell = (XawTableCell)get_cell(STUFF(tw),i,j);

  if (CELL(label) == NULL) {
    CELL(label_len) = 0;
    CELL(label_width) = 0;
  } else {
    CELL(label_len) = strlen(CELL(label));
    if (tw->table.encoding)
      CELL(label_width) =
	XTextWidth16(fs, (TXT16*)CELL(label), (int) CELL(label_len)/2);
    else
      CELL(label_width) =
	XTextWidth(fs, CELL(label), (int) CELL(label_len));
  }
}


static void CreateTableCellGC(w)
    Widget w;
{
  Display   *dpy = XtDisplay(w);
  Drawable     d = XtWindow(w);
  XawTableWidget tw = (XawTableWidget)w;
  
  tw->table.normal  = XCreateGC(dpy, d, (XtGCMask)0, (XGCValues*)NULL);
  tw->table.reverse = XCreateGC(dpy, d, (XtGCMask)0, (XGCValues*)NULL);

  MultipleChangeGC(w,
		   (Pixel*)&tw->table.foreground,
		   (Pixel*)&tw->core.background_pixel,
		   (Font*)&tw->table.font->fid,
		   (GC*)&tw->table.normal,
		   (GC*)&tw->table.reverse);

}

static void DrawColumns(tw, b_column, e_column)
     XawTableWidget tw;
     int b_column, e_column;
{
  int j,y;

  y = (int)GetY(tw,0);
  for(j = MAX(b_column,0); j <= MIN(e_column, COLUMNS(tw) - 2); j++)
  {
    XFillRectangle(XtDisplay((Widget)tw),
		   XtWindow((Widget)tw),
		   tw->table.column_gc,
		   (int) (GetX(tw,j+1) - tw->table.col_margin),
		   y,
		   (unsigned int) tw->table.col_margin,
		   (unsigned int)(tw->table.prefer_height-(2*y)));
    
  }
}

static void DrawRows(tw, b_row, e_row)
     XawTableWidget tw;
     int b_row, e_row;
{
  int i,x;

  x = (int)GetX(tw,0);
  for(i = MAX(b_row, 0); i <= MIN(e_row, ROWS(tw) - 2); i++)
  {
    XFillRectangle(XtDisplay((Widget)tw),
		   XtWindow((Widget)tw),
		   tw->table.row_gc,
		   x,
		   (int) (GetY(tw,i+1) - tw->table.row_margin),
		   (unsigned int) (tw->table.prefer_width-(2*x)),
		   (unsigned int) tw->table.row_margin);
    
  }
}

static void DrawCage(tw, b_row, e_row, b_column, e_column)
     XawTableWidget tw;
     int b_row, e_row, b_column, e_column;
{
  int i,j;
  Display* dpy = XtDisplay((Widget)tw);
  Window   win = XtWindow((Widget)tw);
  
  if (tw->table.row_oriented)
  {
    if (tw->table.col_margin > (Dimension)0)
    {
      DrawRows(tw, b_row - 1, e_row);

      for(i = b_row; i <= MIN(e_row,ROWS(tw) - 1); i++)
      {
	int y = (int) GetY(tw,i);
	  
	for(j = MAX(b_column - 1, 0); j <= MIN(e_column, COLUMNS(tw) - 2); j++)
	{
	  XFillRectangle (dpy,
			  win,
			  tw->table.column_gc,
			  (int) (GetX(tw, j + 1) - tw->table.col_margin),
			  y,
			  (unsigned int) tw->table.col_margin,
			  (unsigned int) (tw->table.row_height +
			     2 * tw->table.label_shadow_thickness));

	}
      }
    }
  }
  else
  {
    if (tw->table.row_margin > (Dimension)0)
    {
      DrawColumns(tw, b_column - 1, e_column);

      for(j = b_column; j <= MIN(e_column, COLUMNS(tw) - 1); j++)
      {
	int x = (int) GetX(tw,j);
	  
	for(i = MAX(b_row - 1, 0); i <=  MIN(e_row, ROWS(tw) - 2); i++)
	{
	  XFillRectangle(XtDisplay((Widget)tw),
			 XtWindow((Widget)tw),
			 tw->table.row_gc,
			 x,
			 (int) (GetY(tw,i+1) - tw->table.row_margin),
			 (unsigned int) (COLUMN_WIDTH(tw,j) +
					 2 * tw->table.label_shadow_thickness),
			 (unsigned int) tw->table.row_margin);
	}
      }
    }
  }
}

static void Reposition(tw, cell, i, j)
     register XawTableWidget tw;
     XawTableCell  cell;
     int i,j;
{
  Position newPos;
  XtJustify justify;
  
  if (cell == NULL)
    cell = (XawTableCell)get_cell(STUFF(tw),i,j);
  
  if (COLUMN_DATA(tw)[j].flag & _CL_justify)
    justify = COLUMN_DATA(tw)[j].justify;
  else
    justify = tw->table.justify;

  switch (justify) {
  case XtJustifyLeft   : newPos = tw->table.internal_width;
    break;
  case XtJustifyRight  : newPos = (COLUMN_WIDTH(tw,j) -
				   CELL(label_width)         -
				   tw->table.internal_width);
    break;
  case XtJustifyCenter :
  default              : newPos = (COLUMN_WIDTH(tw,j) -
				   CELL(label_width)) / 2;
    break;
  }

  CELL(label_x) = MAX(newPos, tw->table.internal_width);
}

/* ARGSUSED */
static Boolean DeleteCell(p, i, j, call_data, client_data)
     XtPointer p;
     int i;               /* unused */
     int j;               /* unused */
     XtPointer call_data;
     XtPointer client_data; /* unused */
{
  Widget w = (Widget) p;
  XawTableCell cell = (XawTableCell)call_data;


  if (CELL(label) != (char*)NULL) 
    XtFree(CELL(label));

  if (CELL(special_colour)) 
  {
    ReleaseNormalGC(w, CELL(fore), CELL(back));
    ReleaseShadowGC(w, CELL(back));
  }
  return False;
}


static void UpdateTable(tw)
     XawTableWidget tw;
{
  Dimension width;
  Dimension height;

  if (tw->table.no_refigure > 0)
    return;

  CalculatePreferredSize((Widget)tw, &width, &height);

  tw->table.was_resized = False;

  (void)XtMakeResizeRequest((Widget)tw, width, height,
			    (Dimension*)NULL, (Dimension*)NULL);
  if (tw->table.was_resized == False)
  {
    tw->table.was_resized = True;
    (*tableClassRec.core_class.resize) ((Widget)tw);
  }
}


static int SetTableSize(w, rows, columns)
     Widget w;
     int    rows;
     int    columns;
{
  XawTableWidget tw = (XawTableWidget)w;
  XawTableCallbackStruct callback_str;
  int i;
  
  if (rows < 0 || columns < 0) {
    String subs[1];
    Cardinal num_subs = 1;
    subs[0] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "SetTableSize", "SetTableSize","XawToolkitError",
     "rows or columns for a new table in TableWidget '%s' less then zero",
		    subs, &num_subs);
    return REJECT;
  }

  if (STUFF(tw))
  { 
    callback_str.reason   = XawTABLE_ALLOW_DELETE_TABLE;
    callback_str.event    = (XEvent*)NULL;
    callback_str.old_cell = STUFF(tw);
    callback_str.new_cell = STUFF(tw);
    callback_str.row      = 0;
    callback_str.column   = 0;
    callback_str.do_it    = True;

    DO_CALLBACK(w, XtNallowDeleteTable, callback_str);
    
    if (callback_str.do_it != True)
      return REJECT;
      
    callback_str.reason   = XawTABLE_DELETE_TABLE;
    callback_str.event    = (XEvent*)NULL;
    callback_str.old_cell = STUFF(tw);
    callback_str.new_cell = STUFF(tw);
    callback_str.row      = 0;
    callback_str.column   = 0;
    callback_str.do_it    = True;

    DO_CALLBACK (w, XtNdeleteTable, callback_str);
    
    WalkForCells (w, (XawTableProc)DeleteCell, 
		  0, ROWS(tw)-1, 0, COLUMNS(tw)-1);
    
    delete_table ((XtPointer)STUFF(tw));

    STUFF(tw)   = (XawTableCell)NULL;
    ROWS(tw)    = 0;
    COLUMNS(tw) = 0;
  }

  if (rows == 0 || columns == 0)
    return ACCEPT;
      
  STUFF(tw) = (XawTableCell)
    create_table(rows, columns, sizeof(XawTableCellRec));

  if (STUFF(tw) == (XawTableCell)NULL)
    return ACCEPT;

  ROWS(tw)    = rows;
  COLUMNS(tw) = columns;
  
  WalkForCells((Widget)tw, (XawTableProc)InitCell, 0, rows-1, 0, columns-1);

  if (COLUMNS(tw)) {
    tw->table.column_data = (XawTableColumn)
      XtRealloc((XtPointer)COLUMN_DATA(tw), 
		(Cardinal)(COLUMNS(tw) * sizeof(XawTableColumnRec)));
    for (i = 0; i < COLUMNS(tw); i++)
      COLUMN_DATA(tw)[i].flag = 0;
  }
  else {
    XtFree((XtPointer)tw->table.column_data);
    tw->table.column_data = NULL;
  }

  callback_str.reason   = XawTABLE_CREATE_TABLE;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = (XawTableCell)NULL;
  callback_str.new_cell = (XawTableCell)NULL;
  callback_str.row      = 0;
  callback_str.column   = 0;
  callback_str.do_it    = True;
  
  DO_CALLBACK(w, XtNcreateTable, callback_str);

  return ACCEPT;
}


/* ARGSUSED */
static void Initialize(request, new, args, num_args)
     Widget    request;
     Widget    new;
     ArgList   args;
     Cardinal *num_args;
{
  register XawTableWidget tw = (XawTableWidget) new;
  Dimension   width;
  Dimension   height;
  int         i;

  tw->table.no_refigure    = 0;
  tw->table.no_redraw      = 0;
  tw->table.num_selections = 0;
  tw->table.cell_own       = (XawTableCell)NULL;
  STUFF(tw)                = (XawTableCell)NULL;
  tw->table.normal         = (GC)NULL;
  tw->table.reverse        = (GC)NULL;

  COLUMNS(tw) = MAX (COLUMNS(tw), 0);
  ROWS(tw)    = MAX (ROWS(tw), 0);

  if (COLUMNS(tw)) {
    tw->table.column_data = (XawTableColumn)
      CALLOC (COLUMNS(tw), XawTableColumnRec);
    for (i = 0; i < COLUMNS(tw); i++)
      COLUMN_DATA(tw)[i].flag = 0;
  }
  else
    tw->table.column_data = NULL;

  SetLiteralWidth(tw);
		  
  EDIT(tw) = XtVaCreateWidget
    ("edit", asciiTextWidgetClass, new,
     XtNeditType,         XawtextEdit,
     XtNforeground,       tw->table.edit_fore,
     XtNbackground,       tw->table.edit_back,
     XtNborderWidth,      (XtArgVal)0,
     XtNleftMargin,       2,
     XtNrightMargin,      2,
     XtNtopMargin,        0,        /* to be fixed in future */
     XtNbottomMargin,     0,        /* to be fixed in future */
     XtNuseStringInPlace, False,
     XtNfont,             tw->table.font,
     XtNuserData,         (XtArgVal)new,
     NULL);

  XtOverrideTranslations(EDIT(tw), XtParseTranslationTable(edit_translations));

  GetGCByForeground (new, (GC*)&tw->table.row_gc,    tw->table.row_fore);
  GetGCByForeground (new, (GC*)&tw->table.column_gc, tw->table.column_fore);

  if ((COLUMNS(tw) > 0) && (ROWS(tw) > 0))
  {
    if (REJECT == SetTableSize(new, ROWS(tw), COLUMNS(tw)))
    {
      ROWS(tw)    = 0;
      COLUMNS(tw) = 0;
      STUFF(tw)   = (XawTableCell)NULL;
    }
  }

  if ((tw->table.mask_number < 0)
      || (tw->table.mask_number >= XtNumber(MaskStaticArray)))
    tw->table.mask_number = 7;
  
  tw->table.mask_hash_table = MaskStaticArray[tw->table.mask_number];
  
  tw->table.normal_hash_table =
    CALLOC(tw->table.mask_hash_table, NormalReverseGC);

  tw->table.shadow_hash_table = CALLOC(tw->table.mask_hash_table, ShadowGC);
  
  for(i = 0; i < tw->table.mask_hash_table; i++)
  {
    GET_NORMAL(tw,i)->used    = 0;
    GET_NORMAL(tw,i)->normal  = (GC)NULL;
    GET_NORMAL(tw,i)->reverse = (GC)NULL;
    GET_SHADOW(tw,i)->used   = 0;
    GET_SHADOW(tw,i)->top    = (GC)NULL;
    GET_SHADOW(tw,i)->bottom = (GC)NULL;
  }

  SetLabelHeight(tw);

  CalculatePreferredSize(new, &width, &height);
  
  if (new->core.width == 0)  new->core.width = width;
  if (new->core.height == 0) new->core.height = height;
}


static void Realize(w, valueMask, attributes)
     Widget w;
     XtValueMask *valueMask;
     XSetWindowAttributes *attributes;
{
  XawTableWidget tw = (XawTableWidget) w;
  ShadowGC *shadow;

  (*SuperClass->core_class.realize) (w, valueMask, attributes);

  CreateTableCellGC(w);

  shadow = GetShadowGC(w, tw->table.edit_back);
  tw->table.edit_top    = shadow->top;
  tw->table.edit_bottom = shadow->bottom;

  shadow = GetShadowGC(w, tw->core.background_pixel);
  tw->table.top    = shadow->top;
  tw->table.bottom = shadow->bottom;
  
  XtRealizeWidget(EDIT(w));
}

/* ARGSUSED */
static Boolean MatchLabel(w, i, j, call_data, client_data)
     Widget w;            /* unused */
     int i;               /* unused */
     int j;               /* unused */
     XtPointer call_data;
     XtPointer client_data; 
{
  XawTableCell cell = (XawTableCell)call_data;
  XrmQuark* templ = (XrmQuark*)client_data;
  
  return ((*templ) == XrmStringToQuark (GET_CELL_LABEL));
}

/* ARGSUSED */
static Boolean InitCell(p, i, j, call_data, client_data)
     XtPointer p;         /* unused */
     int i;               /* unused */
     int j;               /* unused */
     XtPointer call_data;
     XtPointer client_data; /* unused */
{
  XawTableCell cell = (XawTableCell)call_data;

  CELL(label)          = DummyString();
  CELL(label_len)      = 0;
  CELL(label_width)    = 0;
  CELL(highlight)      = False;
  CELL(special_colour) = False;
  CELL(normal)         = (GC)NULL;
  CELL(reverse)        = (GC)NULL;
  CELL(top)            = (GC)NULL;
  CELL(bottom)         = (GC)NULL;

  return False;
}

/*
 *
 * Shadow drawing around cell
 *
 */
static void PaintShadow(w, i, j, x, y, cell)
     Widget   w;
     int      i;      
     int      j;      
     Position x;
     Position y;
     XawTableCell  cell;
{
  XawTableWidget tw = (XawTableWidget) w;
  
  if (tw->table.label_shadow_thickness != (Dimension)0) {
    GC top,bottom;

    if (IsEditInRowColumn(tw,i,j))
    {
      top    = tw->table.edit_bottom;
      bottom = tw->table.edit_top;
    }
    else if (CELL(special_colour))
    {
      top    = CELL(top);
      bottom = CELL(bottom);
    }else {
      top    = tw->table.top;
      bottom = tw->table.bottom;
    }
    
    XawDrawFrame(w,
		 x,
		 y,
		 (Dimension)(COLUMN_WIDTH(tw,j) +
			     2 * tw->table.label_shadow_thickness),
		 (Dimension)(tw->table.row_height +
			     2 * tw->table.label_shadow_thickness),
		 XawRAISED,
		 tw->table.label_shadow_thickness,
		 top,
		 bottom);
  }

}

/* ARGSUSED */
static void PaintLabel(w, i, j, x, y, cell)
     Widget w;
     int i;      
     int j;      
     Position x,y;
     XawTableCell cell;
{
  XawTableWidget tw = (XawTableWidget) w;
  XRectangle rectangle[1];
  Position label_x;
  Position label_y;
  unsigned int   width  = (unsigned int) COLUMN_WIDTH(tw,j);
  unsigned int   height = (unsigned int) tw->table.row_height;

  x += tw->table.label_shadow_thickness;
  y += tw->table.label_shadow_thickness;
  
  if (CELL(special_colour) || CELL(highlight))
  { 
    GC gc;
    
    if (CELL(special_colour))
      gc = CELL(highlight) ? CELL(normal) : CELL(reverse);
    else
      gc = tw->table.normal ;

    /* Fill background for cell with a special colour */
    XFillRectangle(XtDisplay(w), XtWindow(w), gc, 
		   (int)x, (int)y, width, height);
  }
  else
  {
    XClearArea (XtDisplay(w), XtWindow(w), (int)x, (int)y, 
		width, height, FALSE);
  }

  if (CELL(label_len) > 0)
  {
    GC gc;
    register XFontStruct *fs = tw->table.font;

    if (CELL(special_colour))
      gc = CELL(highlight) ? CELL(reverse) : CELL(normal);
    else
      gc = CELL(highlight) ? tw->table.reverse : tw->table.normal;
    
    /* Set clip rectangle for label in cell */
    rectangle[0].x      = (short)x;
    rectangle[0].y      = (short)y;
    rectangle[0].width  = (unsigned short)COLUMN_WIDTH(tw,j);
    rectangle[0].height = (unsigned short)tw->table.row_height;
    
    XSetClipRectangles(XtDisplay(w), gc, 0, 0, rectangle, 1, YSorted);

    /* Drawing label */
    label_x = x + CELL(label_x);
    label_y = y + 
      (tw->table.row_height-(fs->max_bounds.ascent+fs->max_bounds.descent))/2
	/* tw->table.internal_height  */
	+ fs->max_bounds.ascent;

    if (tw->table.encoding)
      XDrawString16(XtDisplay(w), XtWindow(w), gc,
		    label_x, label_y, (TXT16*)CELL(label),
		    (int)CELL(label_len)/2);
    else
      XDrawString(XtDisplay(w), XtWindow(w), gc,
		  label_x, label_y, CELL(label), (int)CELL(label_len));
    
    XSetClipMask(XtDisplay(w), gc, (Pixmap)None);
  
  }
}

/* ARGSUSED */
static Boolean PaintCell(p, i, j, call_data, client_data)
     XtPointer p;   
     int i;      
     int j;      
     XtPointer call_data;
     XtPointer client_data; /* unused */
{
  Widget       w = (Widget) p;
  XawTableWidget tw = (XawTableWidget) w;
  XawTableCell   cell = (XawTableCell) call_data;
  Position     x;
  Position     y;

  x = GetX(tw,j);
  y = GetY(tw,i);

  PaintLabel (w, i, j, x, y, cell);
  PaintShadow (w, i, j, x, y, cell);

  XFlush(XtDisplay(w));
  return False;
}

static void WhatCellsToBeDraw(tw, rect, b_row, e_row, b_column , e_column)
     XawTableWidget tw;
     XRectangle  rect;
     register int *b_row, *e_row, *b_column , *e_column;
{
  Position    x1 = (Position)rect.x,
              y1 = (Position)rect.y,
              x2 = (Position)(rect.x + rect.width - 1),
              y2 = (Position)(rect.y + rect.height - 1);

  for ((*b_column) = 0; (*b_column) < COLUMNS(tw); (*b_column)++)
  {
    if (x1 < (GetX(tw,(*b_column) + 1) - tw->table.col_margin))
      break;
  }

  for ((*e_column) = (*b_column); (*e_column) < COLUMNS(tw); (*e_column)++)
  {
    if (x2 < (GetX(tw,(*e_column) + 1) - tw->table.col_margin))
      break;
  }


  for ((*b_row) = 0; (*b_row) < ROWS(tw); (*b_row)++)
  {
    if (y1 < (GetY(tw,(*b_row) + 1) - tw->table.row_margin))
      break;
  }

  for ((*e_row) = (*b_row); (*e_row) < ROWS(tw); (*e_row)++)
  {
    if (y2 < (GetY(tw,(*e_row) + 1) - tw->table.row_margin))
      break;
  }
}
       

static void Redisplay(w, event, region)
    Widget w;
    XEvent *event;
    Region region;
{
  XawTableWidget  tw = (XawTableWidget) w;
  XRectangle   rect;
  int b_row, e_row, b_column , e_column ;
  
  if (!XtIsRealized(w) || !w->core.visible || tw->table.no_redraw)
    return;

  if (tw->container.shadow_thickness > 0)
    (*SuperClass->core_class.expose) (w, event, region);
  
  if (region != NULL)
  {
    XClipBox(region, (XRectangle*)&rect);

    WhatCellsToBeDraw(tw, rect, &b_row, &e_row, &b_column , &e_column);
  }
  else if (event != (XEvent*)NULL)
  {
    rect.x      = (short) event->xexpose.x;
    rect.y      = (short) event->xexpose.y;
    rect.width  = (unsigned short) event->xexpose.width;
    rect.height = (unsigned short) event->xexpose.height;

    WhatCellsToBeDraw(tw, rect, &b_row, &e_row, &b_column , &e_column);
  }
  else if (tw->table.v_scroll || tw->table.h_scroll)
  {
    rect.x      = (short) 0;
    rect.y      = (short) 0;
    rect.width  = (unsigned short) tw->core.width;
    rect.height = (unsigned short) tw->core.height;

    if (tw->table.v_scroll && XtIsManaged(tw->table.v_scroll))
    {
      float top;
      float shown;
    
      XtVaGetValues (tw->table.v_scroll,
		     XtNtopOfThumb, &top,
		     XtNshown,      &shown,
		     NULL);
    
      rect.y      = (short) ((float)(tw->core.height) * top);
      rect.height = (unsigned short) ((float)(tw->core.height) * (top+shown));
    }
    
    if (tw->table.h_scroll && XtIsManaged(tw->table.h_scroll))
    {
      float top;
      float shown;
    
      XtVaGetValues (tw->table.h_scroll,
		     XtNtopOfThumb, &top,
		     XtNshown,      &shown,
		     NULL);
    
      rect.x     = (short) ((float)(tw->core.width) * top);
      rect.width = (unsigned short) ((float)(tw->core.width) * (top+shown));
    }

    
    WhatCellsToBeDraw(tw, rect, &b_row, &e_row, &b_column , &e_column);
  }
  else
  {
    b_row = 0;
    e_row = ROWS(tw) - 1;
    b_column = 0;
    e_column = COLUMNS(tw) - 1;
  }

  DrawCage(tw, b_row, e_row, b_column, e_column);

  WalkForCells(w, (XawTableProc)PaintCell, b_row, e_row, b_column, e_column);

  XFlush(XtDisplay(w));
}

/* ARGSUSED */
static Boolean SetValues(current, request, new, args, num_args)
     Widget    current;
     Widget    request;    /* unused */
     Widget    new;
     ArgList   args;       /* unused */
     Cardinal *num_args;   /* unused */
{
    XawTableWidget curtw = (XawTableWidget) current;
    XawTableWidget newtw = (XawTableWidget) new;
    register int    i;
    int             j;
    Boolean    resize = False;
    Boolean redisplay = False;

#define NE(field) (curtw->table.field != newtw->table.field)

    if (NE(mask_number))
      XtWarning("Resource XtNmaskNumber in Table widget can not changed");
    
    newtw->table.mask_number = curtw->table.mask_number;
    newtw->table.literal     = curtw->table.literal;
    
    if (NE(font->fid))
      XtVaSetValues (EDIT(newtw), XtNfont, curtw->table.font, NULL);

    if (NE(foreground)             ||
	NE(row_fore)               ||
	NE(column_fore)            ||
	NE(row_oriented)           ||
	NE(font->fid)              ||
	NE(justify)                ||
	NE(tab_margin)             ||
	NE(row_margin)             ||
	NE(col_margin)             ||
	NE(internal_width)         ||
	NE(internal_height)        ||
	NE(label_shadow_thickness) ||
	NE(encoding))
      redisplay = True;

    if (NE(font->fid)              ||
	NE(columns)                ||
	NE(rows)                   ||
	NE(tab_margin)             ||
	NE(row_margin)             ||
	NE(col_margin)             ||
	NE(internal_width)         ||
	NE(internal_height)        ||
	NE(row_height)             ||
        NE(label_shadow_thickness))
      resize = True;

    if (NE(columns) || NE(rows)) 
      if (REJECT == SetTableSize(new, ROWS(newtw), COLUMNS(newtw))) {
	newtw->table.columns = curtw->table.columns;
	newtw->table.rows    = curtw->table.rows;
      }

    if(NE(row_height))
    {
      XawTableCallbackStruct callback_str;
      callback_str.reason   = XawTABLE_CHANGED_ROW_HEIGHT;
      callback_str.event    = (XEvent*)NULL;
      callback_str.old_cell = (XawTableCell)NULL;
      callback_str.new_cell = (XawTableCell)NULL;
      callback_str.row      = 0;
      callback_str.column   = 0;
      callback_str.do_it    = True;
      
      DO_CALLBACK(new, XtNchangedRowHeight, callback_str);
    }

    if(NE(font->fid))
    {
      for(i = 0; i < ROWS(newtw); i++)
	for(j = 0; j < COLUMNS(newtw); j++)
	  SetLabelWidth(newtw, i, j);

      SetLabelHeight(newtw);
    }

    if(NE(font->fid) || NE(encoding))
    {
      SetLiteralWidth(newtw);
      
      for(i = 0; i < ROWS(newtw); i++)
	for(j = 0; j < COLUMNS(newtw); j++)
	  Reposition(newtw, NULL, i, j);
    }
    else if(NE(internal_width) || NE(label_shadow_thickness)) {
      for(i = 0; i < ROWS(newtw); i++)
	for(j = 0; j < COLUMNS(newtw); j++)
	  Reposition(newtw, NULL, i, j);
    }
    
    
    if (NE(internal_height) ||	NE(label_shadow_thickness))
    {
      SetLabelHeight(newtw);
    }
    
    if (resize) 
      CalculatePreferredSize(new, &newtw->core.width, &newtw->core.height);
    
    if (NE(foreground) || NE(font->fid) ||
	curtw->core.background_pixel != newtw->core.background_pixel)
    {
      MultipleChangeGC(new,
		       (Pixel*)&newtw->table.foreground,
		       (Pixel*)&newtw->core.background_pixel,
		       (Font*)&newtw->table.font->fid,
		       (GC*)&newtw->table.normal,
		       (GC*)&newtw->table.reverse);

    }

    if (NE(row_fore))
    {
      XtReleaseGC(new, curtw->table.row_gc);
      GetGCByForeground(new,(GC*)&newtw->table.row_gc, newtw->table.row_fore);
    }

    if (NE(column_fore))
    {
      XtReleaseGC(new, curtw->table.column_gc);
      GetGCByForeground(new, &newtw->table.column_gc,newtw->table.column_fore);
    }

    if (resize)
      CalculatePreferredSize(new, &(new->core.width), &(new->core.height));

    return redisplay;
}


static void Destroy(w)
    Widget w;
{
  XawTableWidget tw = (XawTableWidget)w;
  XawTableCallbackStruct callback_str;

  if (STUFF(tw) != (XawTableCell)NULL)
  {
    callback_str.reason   = XawTABLE_DELETE_TABLE;
    callback_str.event    = (XEvent*)NULL;
    callback_str.old_cell = (XawTableCell)NULL;
    callback_str.new_cell = (XawTableCell)NULL;
    callback_str.row      = 0;
    callback_str.column   = 0;
    callback_str.do_it    = True;

    DO_CALLBACK(w, XtNdeleteTable, callback_str);
  }
  
  if (tw->table.row_gc != (GC)NULL) {
    XtReleaseGC(w, tw->table.row_gc);
    tw->table.row_gc = (GC)NULL;
  }
  if (tw->table.column_gc != (GC)NULL) {
    XtReleaseGC(w, tw->table.column_gc);
    tw->table.column_gc = (GC)NULL;
  }
  if (tw->table.normal != (GC)NULL) {
    XFreeGC(XtDisplay(w), tw->table.normal);
    tw->table.normal = (GC)NULL;
  }

  if (tw->table.reverse != (GC)NULL) {
    XFreeGC(XtDisplay(w), tw->table.reverse);
    tw->table.reverse = (GC)NULL;
  }

  WalkForCells(w, (XawTableProc)DeleteCell, 0, ROWS(tw)-1, 0, COLUMNS(tw)-1);

  XtFree((XtPointer)tw->table.normal_hash_table);
  XtFree((XtPointer)tw->table.shadow_hash_table);

  delete_table(tw->table.table_stuff);
  STUFF(tw) = (XawTableCell)NULL;

  if (tw->table.column_data != NULL)
    XtFree((XtPointer)tw->table.column_data);

  if (EDIT(w) != WNULL && !EDIT(w)->core.being_destroyed)
      XtDestroyWidget(EDIT(w));
}


static void Resize(w)
    Widget w;
{
  XawTableWidget tw = (XawTableWidget)w;

  /* If widget is realized, just redisplay it w/o clearing */
  if (XtIsRealized(w) && tw->table.no_redraw == 0)
  {
    /* XClearWindow(XtDisplay(w), XtWindow(w)); */
    (*tableClassRec.core_class.expose) (w, (XEvent*)NULL,(Region)NULL);
  }
  
  tw->table.was_resized = True;
}   


static XtGeometryResult QueryGeometry(w, intended, preferred)
    Widget w;
    XtWidgetGeometry *intended, *preferred;
{
  preferred->request_mode = CWWidth | CWHeight;

  CalculatePreferredSize(w, &preferred->width, &preferred->height);

#define Set(bit) (intended->request_mode & bit)

  if (Set(CWWidth)  && intended->width  == preferred->width &&
      Set(CWHeight) && intended->height == preferred->height) {
    return XtGeometryYes;
  }
  
  if (preferred->width  == w->core.width &&
      preferred->height == w->core.height) {
    return XtGeometryNo;
  }

  return XtGeometryAlmost;

#undef Set
}

/* ARGSUSED */
static XtGeometryResult GeometryManager(w, desired, allowed)
     Widget            w;
     XtWidgetGeometry *desired;
     XtWidgetGeometry *allowed;
{
  return XtGeometryYes;
}



static void ExtractPosition(event, x, y , t)
     XEvent *event;
     Position *x, *y;		/* RETURN */
     Time *t;                   /* RETURN */
{
  if (event == NULL)
    return;
  
  switch(event->type) {
  case MotionNotify:
		           *x = event->xmotion.x;
		           *y = event->xmotion.y;
		           *t = event->xmotion.time;
	 break;
  case ButtonPress:
  case ButtonRelease:
		           *x = event->xbutton.x;
			   *y = event->xbutton.y;
			   *t = event->xbutton.time;
       	 break;
  case KeyPress:
  case KeyRelease:
		           *x = event->xkey.x;
			   *y = event->xkey.y;
			   *t = event->xkey.time;
	 break;
  case EnterNotify:
  case LeaveNotify:
		           *x = event->xcrossing.x;
			   *y = event->xcrossing.y;
			   *t = event->xcrossing.time;
	 break;
  default:
			   *x = 0;
			   *y = 0;
			   *t = XtLastTimestampProcessed(event->xany.display);
  }
}

/* ARGSUSED */
static Boolean ExtractCell(tw, px, py, row, column)
     XawTableWidget tw;
     Position px,py;
     int *row, *column;              /* RETURN */
{
  Position x;
  Position y;
#define SH (tw->table.label_shadow_thickness)
  for(*row = 0; *row < ROWS(tw); (*row)++) {
    for(*column = 0; *column < COLUMNS(tw); (*column)++) 
      if ((x=GetX(tw,*column)+SH) <= px)
	if ((y=GetY(tw,*row)+SH) <= py) 
	  if ((x+COLUMN_WIDTH(tw,*column)) >= px)
	    if ((y+tw->table.row_height) >= py) 
	      return False;
  }
#undef SH

  return True;
}


/* ARGSUSED */
static void WalkForCells(w, proc, b_r, e_r, b_c, e_c)
     Widget    w;
     XawTableProc proc;
     int       b_r, e_r, b_c, e_c;
{
  XawTableWidget tw = (XawTableWidget)w;
  int i,j;

  (void)go_table((XtPointer)w, proc, STUFF(tw),
		 b_r, e_r, b_c, e_c, 
		 XawTABLE_RIGHT_DOWN,
		 &i, &j, (XtPointer)NULL);
}

static char* DummyString()
{
  return XtNewString("");
}

static char* CopyOnlyPrintable(raw)
     char* raw;
{
  char* clear;
  char *s,*h;
  int lenght;

  for(s = raw, lenght = 0; (*s) != '\0';)
  {
    if (isprint(*s++))
      lenght++;
  }

  clear = CALLOC(++lenght, char);

  for(s = raw, h = clear; (*s) != '\0';)
  {
    if (isprint(*s))
      (*h++) = (*s++);
  }

  (*h) = '\0';

  return clear;
}

#ifdef EBUG_XRAW_MALLOC
/* ARGSUSED */
static Boolean CheckLabel(p, i, j, call_data, client_data)
     XtPointer p;   
     int i;      
     int j;      
     XtPointer call_data;
     XtPointer client_data; /* unused */
{
  Widget       w = (Widget) p;
  XawTableWidget tw = (XawTableWidget) p;
  XawTableCell cell = (XawTableCell)call_data;
  char* label = CELL(label);

  for (label = CELL(label); *label != '\0'; label++)
    if (!isprint(*label))
    {
      char message[80];
      char *p = NULL;
   sprintf(message, "wrong label '%s' in cell (%4d,%4d) in Table widget '%s'",
	   CELL(label), i, j, w->core.name);
      XtWarning(message);
      *p = '\0';
      break;
    }

  return False;
}
#endif

#ifdef EBUG_XRAW_MALLOC
static void CheckAllLabels(tw)
     XawTableWidget tw;
{
  WalkForCells((Widget)tw, (XawTableProc)CheckLabel, 
	       0, ROWS(tw)-1, 0, COLUMNS(tw)-1);
}
#endif

static void MoveEditCell (tw, row, column)
  XawTableWidget tw;
  int row;
  int column;
{
  Position x,y;

  x = GetX(tw,column);
  y = GetY(tw,row);
  
  XtConfigureWidget(EDIT(tw),
		    (Position)(x + tw->table.label_shadow_thickness),
		    (Position)(y + tw->table.label_shadow_thickness),
		    (Dimension) COLUMN_WIDTH(tw, column),
		    (Dimension) tw->table.row_height,
		    (Dimension)0);
  XawDrawFrame((Widget)tw,
	       x,
	       y,
	       (Dimension)(COLUMN_WIDTH(tw, column) +
	       2 * tw->table.label_shadow_thickness),
	       (Dimension)(tw->table.row_height +
	       2 * tw->table.label_shadow_thickness),
	       XawSUNKEN,
	       tw->table.label_shadow_thickness,
	       tw->table.edit_top,
	       tw->table.edit_bottom);

}  

/* ARGSUSED */
static Boolean CompareCells(p, i, j, call_data, client_data)
     XtPointer p;
     int i;               /* unused */
     int j;               /* unused */
     XtPointer call_data;
     XtPointer client_data; /* unused */
{
  XawTableCell      cell = (XawTableCell)call_data;
  XawTableCell test_cell = (XawTableCell)client_data;
  
  return(cell == test_cell);
}

/******************************************************************
 *
 *                    CONVENIENCE FUNCTIONS
 *
 ******************************************************************/


void
#ifdef Xraw_NEED_PROTO
XawTableGetSize (Widget w, 
		 int *rows,
		 int *columns)
#else
XawTableGetSize (w, rows, columns)
     Widget w;
     int *rows;
     int *columns;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;
  
  *rows    = ROWS(tw);
  *columns = COLUMNS(tw);
}

void
#ifdef Xraw_NEED_PROTO
XawTableDoLayout (Widget w, 
		  Boolean do_layout)
#else
XawTableDoLayout (w, do_layout)
     Widget w;
     Boolean do_layout;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;

  if (do_layout)
    tw->table.no_redraw--;
  else
    tw->table.no_redraw++;

  tw->table.no_redraw = MAX (tw->table.no_redraw, 0);

  if (tw->table.no_redraw == 0)
    Redisplay (w, (XEvent *)NULL, (Region)NULL);
}

int
#ifdef Xraw_NEED_PROTO
XawTableSetNewSize (Widget w,
	     int rows,
	     int columns)
#else
XawTableSetNewSize (w, rows, columns)
     Widget w;
     int  rows;
     int  columns;
#endif
{
  if (REJECT == SetTableSize(w, rows, columns))
    return REJECT;
  
  UpdateTable((XawTableWidget)w);
  return ACCEPT;
}

char *
#ifdef Xraw_NEED_PROTO
XawTableGetLabelByCell (XawTableCell cell)
#else
XawTableGetLabelByCell (cell)
     XawTableCell cell;
#endif
{
  return GET_CELL_LABEL;
}

void 
#ifdef Xraw_NEED_PROTO
XawTableGetEditPosition (Widget w, int *row, int *column)
#else
XawTableGetEditPosition(w, row, column)
     Widget w;
     int *row;
     int *column;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;

  *row    = tw->table.edit_row;
  *column = tw->table.edit_column;
}
     
Boolean 
#ifdef Xraw_NEED_PROTO
XawTableWalk (
	      Widget w,
	      XawTableProc proc,
	      int b_row,
	      int e_row,
	      int b_column,
	      int e_column,
	      int direction,
	      int *i,                              /* returned */
	      int *j,                              /* returned */
	      XtPointer client_data)
#else
XawTableWalk(w, proc, b_row, e_row, b_column, e_column, 
	     direction, i, j, client_data)
     Widget w;
     XawTableProc proc;
     int b_row, e_row, b_column, e_column;
     int direction;
     int *i;                              /* returned */
     int *j;                              /* returned */
     XtPointer client_data;
#endif
{
  return go_table((XtPointer)w, proc, STUFF(w),
		  b_row, e_row, b_column, e_column, 
		  direction,
		  i, j, client_data);
}

Boolean 
#ifdef Xraw_NEED_PROTO
XawTableSearchLabel (Widget w, char *name, int *row, int *column)
#else
XawTableSearchLabel (w, name, row, column)
     Widget w;
     char *name;
     int *row, *column;
#endif
{
  int row_start    = *row;
  int column_start = *column;
  XrmQuark templ   = XrmStringToQuark (name);

  if (XawTableWalk(w, (XawTableProc)MatchLabel,
		   row_start, row_start, column_start, COLUMNS(w)-1,
		   XawTABLE_RIGHT_DOWN,
		   row, column, (XtPointer)&templ)) {
    return (True);
  }
  if (XawTableWalk(w, (XawTableProc)MatchLabel,
		   row_start+1, ROWS(w)-1, 0, COLUMNS(w)-1,
		   XawTABLE_RIGHT_DOWN,
		   row, column, (XtPointer)&templ)) {
    return (True);
  }
  if (XawTableWalk(w, (XawTableProc)MatchLabel,
		   0, row_start-1, 0, COLUMNS(w)-1,
		   XawTABLE_RIGHT_DOWN,
		   row, column, (XtPointer)&templ)) {
    return (True);
  }
  if (XawTableWalk(w, (XawTableProc)MatchLabel,
		   row_start, row_start, 0, column_start,
		   XawTABLE_RIGHT_DOWN,
		   row, column, (XtPointer)&templ)) {
    return (True);
  }
  return (False);

}

/**************************************************************
 *
 *
 *
 *                        ROW  routines
 *
 *
 *
 **************************************************************/

int
#ifdef Xraw_NEED_PROTO
XawTablePrependRow (Widget w)
#else
XawTablePrependRow(w)
     Widget w;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;
  XawTableCell cell;
  XawTableCallbackStruct callback_str;
  
  if (STUFF(tw) == NULL) {
    String subs[1];
    Cardinal num_subs = 1;
    subs[0] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "prependRow", "XawTablePrependRow","XawToolkitError",
	    "An attempt to add a row in empty table in TableWidget '%s'",
		    subs, &num_subs);
    return REJECT;
  }


  /* request for allowance */

  cell = STUFF(tw);

  callback_str.reason   = XawTABLE_ALLOW_ADD_ROW;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = cell;
  callback_str.new_cell = cell;
  callback_str.row      = 0;
  callback_str.column   = 0;
  callback_str.do_it    = True;

  DO_CALLBACK(w, XtNallowAddRow, callback_str);

  if (callback_str.do_it != True)
    return REJECT;

  
#ifdef EBUG_XRAW_MALLOC
  CHECK_TABLE(tw);
#endif

  row_insert_before(STUFF(tw), sizeof(XawTableCellRec));
  ROWS(tw)++;

  STUFF(tw) = (XawTableCell)get_table(STUFF(tw));

  WalkForCells(w, (XawTableProc)InitCell, 0, 0, 0, COLUMNS(tw)-1);

  if (XawTableIsEditManaged(w)) 
    MoveEditCell (tw, ++(tw->table.edit_row), tw->table.edit_column);
  
  UpdateTable(tw);


#ifdef EBUG_XRAW_MALLOC
  CHECK_TABLE(tw);
#endif

  if (XtCallbackHasSome == XtHasCallbacks(w, XtNaddRow)) 
  {
    cell = STUFF(tw);

    callback_str.reason   = XawTABLE_ADD_ROW;
    callback_str.event    = (XEvent*)NULL;
    callback_str.old_cell = cell;
    callback_str.new_cell = cell;
    callback_str.row      = 0;
    callback_str.column   = 0;
    callback_str.do_it    = True;
    
    XtCallCallbacks (w, XtNaddRow, (XtPointer)&callback_str);
  }

  return ACCEPT;
}

int
#ifdef Xraw_NEED_PROTO
XawTableAppendRow (Widget w)
#else
XawTableAppendRow (w)
     Widget w;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;
  XawTableCell cell;
  XawTableCallbackStruct callback_str;
  
  if (STUFF(tw) == NULL)
  {
    String subs[1];
    Cardinal num_subs = 1;
    subs[0] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "appendRow", "XawTableAppendRow","XawToolkitError",
 "An attempt to add a row in empty table in TableWidget '%s'",
		    subs, &num_subs);
    return REJECT;
  }


  /* request for allowance */

  cell = (XawTableCell)get_cell(STUFF(tw), ROWS(tw)-1, 0);
  
  callback_str.reason   = XawTABLE_ALLOW_ADD_ROW;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = cell;
  callback_str.new_cell = cell;
  callback_str.row      = 0;
  callback_str.column   = 0;
  callback_str.do_it    = True;

  DO_CALLBACK(w, XtNallowAddRow, callback_str);

  if (callback_str.do_it != True)
    return REJECT;

  
#ifdef EBUG_XRAW_MALLOC
  CHECK_TABLE(tw);
#endif

  row_insert_after(get_cell(STUFF(tw), ROWS(tw) - 1, 0), 
		   sizeof(XawTableCellRec));

  ROWS(tw)++;

  WalkForCells(w, (XawTableProc)InitCell, 
	       ROWS(tw)-1, ROWS(tw)-1, 0, COLUMNS(tw)-1);

  UpdateTable(tw);

#ifdef EBUG_XRAW_MALLOC
  CHECK_TABLE(tw);
#endif

  if (XtCallbackHasSome == XtHasCallbacks(w, XtNaddRow))
  {
    cell = (XawTableCell)get_cell(STUFF(tw), ROWS(tw)-1, 0);
  
    callback_str.reason   = XawTABLE_ADD_ROW;
    callback_str.event    = (XEvent*)NULL;
    callback_str.old_cell = cell;
    callback_str.new_cell = cell;
    callback_str.row      = ROWS(tw)-1;
    callback_str.column   = 0;
    callback_str.do_it    = True;
    
    XtCallCallbacks (w, XtNaddRow, (XtPointer)&callback_str);
  }

  return ACCEPT;
}

int
#ifdef Xraw_NEED_PROTO
XawTableInsertRow (Widget w, int row)
#else
XawTableInsertRow (w, row)
     Widget w;
     int row;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;
  XawTableCell cell;
  XawTableCallbackStruct callback_str;
  
  if (STUFF(tw) == NULL) {
    String subs[1];
    Cardinal num_subs = 1;
    subs[0] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "insertRow", "XawTableInsertRow","XawToolkitError",
 "An attempt to add a row in empty table in TableWidget '%s'",
		    subs, &num_subs);
    return REJECT;
  }

  if (row != InRange(row, 0, ROWS(tw)-1)) {
    String subs[1];
    Cardinal num_subs = 1;
    subs[0] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "insertRow", "XawTableInsertRow","XawToolkitError",
		    "Incorrect attempt to insert a row in TableWidget '%s'",
		    subs, &num_subs);
    return REJECT;
  }

  /* request for allowance */

  cell = (XawTableCell)get_cell(STUFF(tw), row, 0);

  callback_str.reason   = XawTABLE_ALLOW_ADD_ROW;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = cell;
  callback_str.new_cell = cell;
  callback_str.row      = 0;
  callback_str.column   = 0;
  callback_str.do_it    = True;

  DO_CALLBACK(w, XtNallowAddRow, callback_str);

  if (callback_str.do_it != True)
    return REJECT;

  
#ifdef EBUG_XRAW_MALLOC
  CHECK_TABLE(tw);
#endif

  row_insert_before(get_cell((XtPointer)STUFF(tw), row, 0), 
		    sizeof(XawTableCellRec));

  ROWS(tw)++;

  if (row == 0)
    STUFF(tw) = (XawTableCell)get_table(STUFF(tw));
  
  WalkForCells(w, (XawTableProc)InitCell, row, row, 0, COLUMNS(tw)-1);
  
#ifdef EBUG_XRAW_MALLOC
  CHECK_TABLE(tw);
#endif

  if (XawTableIsEditManaged(w) && row <= tw->table.edit_row){
    tw->table.no_redraw++;
    MoveEditCell (tw, ++(tw->table.edit_row), tw->table.edit_column);
    tw->table.no_redraw--;
  }

  UpdateTable(tw);

  if (XtCallbackHasSome == XtHasCallbacks(w, XtNaddRow)) 
  {
    cell = (XawTableCell)get_cell(STUFF(tw), row, 0);

    callback_str.reason   = XawTABLE_ADD_ROW;
    callback_str.event    = (XEvent*)NULL;
    callback_str.old_cell = cell;
    callback_str.new_cell = cell;
    callback_str.row      = row;
    callback_str.column   = 0;
    callback_str.do_it    = True;

    XtCallCallbacks (w, XtNaddRow, (XtPointer)&callback_str);
  }

  return ACCEPT;
}

int
#ifdef Xraw_NEED_PROTO
XawTableDeleteRow (Widget w, int row)
#else
XawTableDeleteRow (w, row)
     Widget w;
     int    row;
#endif
{
  XawTableWidget         tw = (XawTableWidget)w;
  XawTableCell           cell;
  XawTableCallbackStruct callback_str;

  if (STUFF(tw) == NULL) {
    String subs[1];
    Cardinal num_subs = 1;
    subs[0] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "deleteRow", "XawTableDeleteRow","XawToolkitError",
 "An attempt to delete a row in empty table in TableWidget '%s'",
		    subs, &num_subs);
    return REJECT;
  }

  if (row != InRange(row, 0, ROWS(tw)-1)) {
    String subs[3];
    Cardinal num_subs = 3;
    sprintf(subs[0], "%5d", row);
    sprintf(subs[1], "%5d", ROWS(tw)-1);
    subs[2] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "deleteRow", "XawTableDeleteRow","XawToolkitError",
		"Incorrect value of row (%s, max is %s) in TableWidget '%s'",
		    subs, &num_subs);
    return REJECT;
  }


  /* request for allowance */

  cell = (XawTableCell)get_cell(STUFF(tw), row, 0);

  callback_str.reason   = XawTABLE_ALLOW_DELETE_ROW;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = cell;
  callback_str.new_cell = cell;
  callback_str.row      = 0;
  callback_str.column   = 0;
  callback_str.do_it    = True;

  DO_CALLBACK(w, XtNallowDeleteRow, callback_str);

  if (callback_str.do_it != True)
    return REJECT;

  
#ifdef EBUG_XRAW_MALLOC
  CHECK_TABLE(tw);
#endif


  if (XtCallbackHasSome == XtHasCallbacks(w, XtNdeleteRow))
  {
    cell = (XawTableCell)get_cell(STUFF(tw), row, 0);
  
    callback_str.reason   = XawTABLE_DELETE_ROW;
    callback_str.event    = (XEvent*)NULL;
    callback_str.old_cell = cell;
    callback_str.new_cell = cell;
    callback_str.row      = row;
    callback_str.column   = 0;
    callback_str.do_it    = True;
    
    XtCallCallbacks (w, XtNdeleteRow, (XtPointer)&callback_str);
  }

  /* if selections owner locates here, then disown selections */

  if (tw->table.cell_own != (XawTableCell)NULL) 
  { 
    Boolean have_find;
    int i, j;
    
    have_find = go_table((XtPointer)w, CompareCells, STUFF(tw),
			 row, row, 0, COLUMNS(tw)-1, 
			 XawTABLE_RIGHT_DOWN, &i, &j,
			 (XtPointer)tw->table.cell_own);
    if (have_find) {
      for (; 0 < tw->table.num_selections;)
	XtDisownSelection  (w, 
			    tw->table.selections[--(tw->table.num_selections)],
			    XtLastTimestampProcessed(XtDisplay(w)));
      
      tw->table.cell_own = (XawTableCell)NULL;
    }
  }

  if (row == 0)
    STUFF(tw) = (XawTableCell)get_cell(STUFF(tw), 1, 0);

  if (--ROWS(tw) == 0)
    STUFF(tw) = (XawTableCell)NULL;
  
  WalkForCells(w, (XawTableProc)DeleteCell, row, row, 0, COLUMNS(tw)-1);
  
  row_delete(cell);

  if (XawTableIsEditManaged(w)) 
  {
    tw->table.no_redraw++;
    if (row < tw->table.edit_row){
      MoveEditCell (tw, --(tw->table.edit_row), tw->table.edit_column);
    }
    else if (row == tw->table.edit_row){
      UNMANAGE_EDIT(tw);
      tw->table.edit_row = InRange (tw->table.edit_row, 0, ROWS(tw)-1);
      XawTableSetEdit (w, tw->table.edit_row, tw->table.edit_column);
    }
    tw->table.no_redraw--;
  }

  UpdateTable(tw);
    
#ifdef EBUG_XRAW_MALLOC
    CHECK_TABLE(tw);
#endif

  return ACCEPT;
}



/**************************************************************
 *
 *
 *
 *                        COLUMN  routines
 *
 *
 *
 **************************************************************/

int 
#ifdef Xraw_NEED_PROTO
XawTablePrependColumn (Widget w, int width)
#else
XawTablePrependColumn (w, width)
     Widget w;
     int width;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;
  int j;
  XawTableCell cell;
  XawTableCallbackStruct callback_str;
  
  if (STUFF(tw) == NULL) {
    String subs[1];
    Cardinal num_subs = 1;
    subs[0] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "prependColumn", "XawTablePrependColumn","XawToolkitError",
 "An attempt to add a column in empty table in TableWidget '%s'",
		    subs, &num_subs);
    return REJECT;
  }

  /* request for allowance */

  cell = STUFF(tw);

  callback_str.reason   = XawTABLE_ALLOW_ADD_COLUMN;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = cell;
  callback_str.new_cell = cell;
  callback_str.row      = 0;
  callback_str.column   = 0;
  callback_str.do_it    = True;

  DO_CALLBACK(w, XtNallowAddColumn, callback_str);

  if (callback_str.do_it != True)
    return REJECT;

  
#ifdef EBUG_XRAW_MALLOC
  CHECK_TABLE(tw);
#endif

  column_insert_before(STUFF(tw), sizeof(XawTableCellRec));
  COLUMNS(tw)++;

  STUFF(tw) = (XawTableCell)get_table(STUFF(tw));

  WalkForCells(w, (XawTableProc)InitCell, 0, ROWS(tw)-1, 0, 0);
  
  /* 
   * insert new item in `column_data' list 
   */

  if (tw->table.column_data)
  {
    tw->table.column_data = (XawTableColumn)
      XtRealloc((XtPointer)tw->table.column_data,
		(Cardinal)(COLUMNS(tw) * sizeof(XawTableColumnRec)));
  }
  else
  {
    tw->table.column_data = CALLOC(COLUMNS(tw), XawTableColumnRec);
    for(j = 0; j < COLUMNS(tw); j++)
      COLUMN_DATA(tw)[j].flag = 0;
  }
  
  for(j = COLUMNS(tw)-2; j >= 0; j--)
    (void)memcpy((char*)&(COLUMN_DATA(tw)[j+1]), (char*)&(COLUMN_DATA(tw)[j]),
		 sizeof(XawTableColumnRec));

  COLUMN_DATA(tw)[0].flag |= _CL_width;
  COLUMN_DATA(tw)[0].width = width;


  if (XawTableIsEditManaged(w)) 
    MoveEditCell (tw, tw->table.edit_row, ++(tw->table.edit_column));

  /* Let's look the table which we have made now */
  UpdateTable(tw);


#ifdef EBUG_XRAW_MALLOC
  CHECK_TABLE(tw);
#endif

  if (XtCallbackHasSome == XtHasCallbacks(w, XtNaddColumn)) 
  {
    cell = STUFF(tw);

    callback_str.reason   = XawTABLE_ADD_COLUMN;
    callback_str.event    = (XEvent*)NULL;
    callback_str.old_cell = cell;
    callback_str.new_cell = cell;
    callback_str.row      = 0;
    callback_str.column   = 0;
    callback_str.do_it    = True;

    XtCallCallbacks (w, XtNaddColumn, (XtPointer)&callback_str);
  }
  
  return ACCEPT;
}

int 
#ifdef Xraw_NEED_PROTO
XawTableAppendColumn (Widget w, int width)
#else
XawTableAppendColumn (w, width)
     Widget w;
     int width;
#endif
{
  XawTableWidget         tw = (XawTableWidget)w;
  int                    j;
  XawTableCell           cell;
  XawTableCallbackStruct callback_str;
  
  if (STUFF(tw) == NULL) {
    String subs[1];
    Cardinal num_subs = 1;
    subs[0] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "addingColumn", "XawTableAppendColumn","XawToolkitError",
	"An attempt to add a column in empty table in TableWidget '%s'",
		    subs, &num_subs);
    return REJECT;
  }

  /* request for allowance */

  cell = (XawTableCell)get_cell(STUFF(tw), 0, COLUMNS(tw)-1);
  
  callback_str.reason   = XawTABLE_ALLOW_ADD_COLUMN;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = cell;
  callback_str.new_cell = cell;
  callback_str.row      = 0;
  callback_str.column   = 0;
  callback_str.do_it    = True;

  DO_CALLBACK(w, XtNallowAddColumn, callback_str);

  if (callback_str.do_it != True)
    return REJECT;

  

#ifdef EBUG_XRAW_MALLOC
  CHECK_TABLE(tw);
#endif

  column_insert_after(get_cell(STUFF(tw), 0, COLUMNS(tw)-1), 
		      sizeof(XawTableCellRec));

  COLUMNS(tw)++;
  
  WalkForCells(w, (XawTableProc)InitCell, 
	       0, ROWS(tw)-1, COLUMNS(tw)-1, COLUMNS(tw)-1);




  /* 
   * insert new item in `column_data' list 
   */

  if (tw->table.column_data)
  {
    tw->table.column_data = (XawTableColumn)
      XtRealloc((XtPointer)tw->table.column_data,
		(Cardinal)(COLUMNS(tw) * sizeof(XawTableColumnRec)));
  }
  else
  {
    tw->table.column_data = CALLOC(COLUMNS(tw), XawTableColumnRec);
    for(j = 0; j < COLUMNS(tw); j++)
      COLUMN_DATA(tw)[j].flag = 0;
  }
  
  COLUMN_DATA(tw)[COLUMNS(tw)-1].flag |= _CL_width;
  COLUMN_DATA(tw)[COLUMNS(tw)-1].width = width;


  /* Let's look the table which we have made now */
  UpdateTable(tw);


#ifdef EBUG_XRAW_MALLOC
  CHECK_TABLE(tw);
#endif

  if (XtCallbackHasSome == XtHasCallbacks(w, XtNaddColumn))
  {
    cell = (XawTableCell)get_cell(STUFF(tw), 0, COLUMNS(tw)-1);
    
    callback_str.reason   = XawTABLE_ADD_COLUMN;
    callback_str.event    = (XEvent*)NULL;
    callback_str.old_cell = cell;
    callback_str.new_cell = cell;
    callback_str.row      = 0;
    callback_str.column   = COLUMNS(tw)-1;
    callback_str.do_it    = True;

    XtCallCallbacks (w, XtNaddColumn, (XtPointer)&callback_str);
  }

  return ACCEPT;
}

int 
#ifdef Xraw_NEED_PROTO
XawTableInsertColumn (Widget w, int column, int width)
#else
XawTableInsertColumn(w, column, width)
     Widget w;
     int column;
     int width;
#endif
{
  XawTableWidget         tw = (XawTableWidget)w;
  XawTableCell           cell;
  int                    j;
  XawTableCallbackStruct callback_str;
  
  if (STUFF(tw) == NULL) {
    String subs[1];
    Cardinal num_subs = 1;
    subs[0] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "insertColumn", "XawTableInsertColumn","XawToolkitError",
 "An attempt to add a column in empty table in TableWidget '%s'",
		    subs, &num_subs);
    return REJECT;
  }

  if (column != InRange(column, 0, COLUMNS(tw)-1)) {
    String subs[1];
    Cardinal num_subs = 1;
    subs[0] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "insertColumn", "XawTableInsertColumn","XawToolkitError",
		 "It detected incorrect value of column in TableWidget '%s'",
		    subs, &num_subs);
    return REJECT;
  }

  /* request for allowance */

  cell = (XawTableCell)get_cell(STUFF(tw), 0, column);

  callback_str.reason   = XawTABLE_ALLOW_ADD_COLUMN;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = cell;
  callback_str.new_cell = cell;
  callback_str.row      = 0;
  callback_str.column   = 0;
  callback_str.do_it    = True;

  DO_CALLBACK(w, XtNallowAddColumn, callback_str);

  if (callback_str.do_it != True)
    return REJECT;

  
#ifdef EBUG_XRAW_MALLOC
  CHECK_TABLE(tw);
#endif

  column_insert_before (get_cell ((XtPointer)STUFF(tw), 0, column), 
			sizeof(XawTableCellRec));
  COLUMNS(tw)++;

  if (column == 0)
    STUFF(tw) = (XawTableCell) get_table (STUFF(tw));
  
  WalkForCells(w, (XawTableProc)InitCell, 0, ROWS(tw)-1, column, column);

#ifdef EBUG_XRAW_MALLOC
  CHECK_TABLE(tw);
#endif

  /* 
   * insert new item in `column_data' list 
   */

  if (tw->table.column_data)
  {
    tw->table.column_data = (XawTableColumn)
      XtRealloc((XtPointer)tw->table.column_data,
		(Cardinal)(COLUMNS(tw) * sizeof(XawTableColumnRec)));
  }
  else
  {
    tw->table.column_data = CALLOC(COLUMNS(tw), XawTableColumnRec);
    for(j = 0; j < COLUMNS(tw); j++)
      COLUMN_DATA(tw)[j].flag = 0;
  }
  
  for(j = COLUMNS(tw)-2; j >= column; j--)
    (void)memcpy((char*)&(COLUMN_DATA(tw)[j+1]), (char*)&(COLUMN_DATA(tw)[j]),
		 sizeof(XawTableColumnRec));

  COLUMN_DATA(tw)[column].flag |= _CL_width;
  COLUMN_DATA(tw)[column].width = width;

  /*
   * move the edit cell
   */

  if (XawTableIsEditManaged(w) && column <= tw->table.edit_column){
    tw->table.no_redraw++;
    MoveEditCell (tw, tw->table.edit_row, ++(tw->table.edit_column));
    tw->table.no_redraw--;
  }

  /* Let's look the table which we have made now */
  UpdateTable(tw);
 
  if (XtCallbackHasSome == XtHasCallbacks(w, XtNaddColumn)) 
  {
    cell = (XawTableCell)get_cell(STUFF(tw), 0, column);

    callback_str.reason   = XawTABLE_ADD_COLUMN;
    callback_str.event    = (XEvent*)NULL;
    callback_str.old_cell = cell;
    callback_str.new_cell = cell;
    callback_str.row      = 0;
    callback_str.column   = column;
    callback_str.do_it    = True;

    XtCallCallbacks (w, XtNaddColumn, (XtPointer)&callback_str);
  }

  return ACCEPT;
}

int 
#ifdef Xraw_NEED_PROTO
XawTableDeleteColumn (Widget w, int column)
#else
XawTableDeleteColumn(w, column)
     Widget w;
     int    column;
#endif
{
  XawTableWidget         tw = (XawTableWidget)w;
  XawTableCell           cell;
  int                    j;
  XawTableCallbackStruct callback_str;
  

  if (STUFF(tw) == NULL) {
    String subs[1];
    Cardinal num_subs = 1;
    subs[0] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "deleteColumn", "XawTableDeleteColumn","XawToolkitError",
 "An attempt to delete a column in empty table in TableWidget '%s'",
		    subs, &num_subs);
    return REJECT;
  }

  if (column != InRange(column, 0, COLUMNS(tw)-1)) {
    String subs[1];
    Cardinal num_subs = 1;
    subs[0] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "deleteColumn", "XawTableDeleteColumn","XawToolkitError",
		  "It detected incorrect value of column in TableWidget '%s'",
		    subs, &num_subs);
    return REJECT;
  }

  /* request for allowance */

  cell = (XawTableCell)get_cell(STUFF(tw), 0, column);

  callback_str.reason   = XawTABLE_ALLOW_DELETE_COLUMN;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = cell;
  callback_str.new_cell = cell;
  callback_str.row      = 0;
  callback_str.column   = 0;
  callback_str.do_it    = True;

  DO_CALLBACK(w, XtNallowDeleteColumn, callback_str);

  if (callback_str.do_it != True)
    return REJECT;

  

#ifdef EBUG_XRAW_MALLOC
  CHECK_TABLE(tw);
#endif

  if (XtCallbackHasSome == XtHasCallbacks(w, XtNdeleteColumn))
  {
    cell = (XawTableCell)get_cell(STUFF(tw), 0, column);
  
    callback_str.reason   = XawTABLE_DELETE_COLUMN;
    callback_str.event    = (XEvent*)NULL;
    callback_str.old_cell = cell;
    callback_str.new_cell = cell;
    callback_str.row      = 0;
    callback_str.column   = column;
    callback_str.do_it    = True;
    
    XtCallCallbacks (w, XtNdeleteColumn, (XtPointer)&callback_str);
  }

  /* if selections owner locates here, then disown selections */

  if (tw->table.cell_own != (XawTableCell)NULL) 
  { 
    Boolean have_find;
    int i, j;
    
    have_find = go_table((XtPointer)w, CompareCells, STUFF(tw),
			 0, ROWS(tw)-1, column, column, 
			 XawTABLE_RIGHT_DOWN, &i, &j,
			 (XtPointer)tw->table.cell_own);
    if (have_find) {
      for (; 0 < tw->table.num_selections;)
	XtDisownSelection  (w, 
			    tw->table.selections[--(tw->table.num_selections)],
			    XtLastTimestampProcessed(XtDisplay(w)));
      
      tw->table.cell_own = (XawTableCell)NULL;
    }
  }

  if (column == 0)
    STUFF(tw) = (XawTableCell)get_cell(STUFF(tw), 0, 1);
  
  if (--COLUMNS(tw) == 0)
    STUFF(tw) = (XawTableCell)NULL;
  

  WalkForCells(w, (XawTableProc)DeleteCell, 0, ROWS(tw)-1, column, column);

  column_delete(cell);

  /*
   * shrink column data list
   */

  if (COLUMNS(tw) == 0) {
    XtFree((XtPointer)tw->table.column_data);
    tw->table.column_data = NULL;
  }
  else
  {
    for(j = column; j < COLUMNS(tw); j++)
      (void)memcpy((char*)&(COLUMN_DATA(tw)[j]),(char*)&(COLUMN_DATA(tw)[j+1]),
		   sizeof(XawTableColumnRec));

    tw->table.column_data = (XawTableColumn)
      XtRealloc((XtPointer)tw->table.column_data,
		(Cardinal)(COLUMNS(tw) * sizeof(XawTableColumnRec)));
  }


  /* 
   * shift the edit cell 
   */

  if (XawTableIsEditManaged(w)) 
  {
    tw->table.no_redraw++;
    if (column < tw->table.edit_column){
      MoveEditCell (tw, tw->table.edit_row, --(tw->table.edit_column));
    }
    else if (column == tw->table.edit_column){
      UNMANAGE_EDIT(tw);
      tw->table.edit_column = InRange (tw->table.edit_column,0,COLUMNS(tw)-1);
      XawTableSetEdit (w, tw->table.edit_row, tw->table.edit_column);
    }
    tw->table.no_redraw--;
  }

  /* Let's look the table which we have made now */
  UpdateTable(tw);
    
#ifdef EBUG_XRAW_MALLOC
    CHECK_TABLE(tw);
#endif

  return ACCEPT;
}


Boolean 
#ifdef Xraw_NEED_PROTO
XawTableIsEditManaged (Widget w)
#else
XawTableIsEditManaged (w)
     Widget w;
#endif
{
  return XtIsManaged(((XawTableWidget)w)->table.edit);
}



int
#ifdef Xraw_NEED_PROTO
XawTableSetLabel (Widget w, int row, int column, char *raw_label)
#else
XawTableSetLabel(w, row, column, raw_label)
     Widget w;
     int    row;
     int    column;
     char  *raw_label;
#endif
{
  register XawTableWidget tw = (XawTableWidget)w;
  XawTableCallbackStruct callback_str;
  XawTableCell cell;
  char* label = NULL;
  XawTableCellRec new;

  if ((ROWS(tw) < 1) || (COLUMNS(tw) < 1) ||
      (row    != InRange(row, 0, ROWS(tw)-1))    || 
      (column != InRange(column, 0, COLUMNS(tw)-1)))
  {
    String subs[3];
    Cardinal num_subs = 3;
    sprintf(subs[0], "%5d", row);
    sprintf(subs[1], "%5d", column);
    subs[2] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "SetLabel", "XawTableSetLabel","XawToolkitError",
"XawTableSetLabel\nIncorrect value of rows or columns (%s,%s) in TableWidget '%s' ",
		    subs, &num_subs);
    return REJECT;
  }
  
  cell = (XawTableCell)get_cell(STUFF(tw), row, column);

  if (raw_label == (char*)NULL)
    label = DummyString();
  else
    label = CopyOnlyPrintable(raw_label);

  if (streq(label,CELL(label))) {
    XtFree(label);                 /* XtMalloc in CopyOnlyPrintable */
    return ACCEPT;
  }

  (void)memcpy((char*)&new, (char*)cell, sizeof(XawTableCellRec));
  new.label = label;

  callback_str.reason   = XawTABLE_CHANGED_CELL;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = cell;
  callback_str.new_cell = &new;
  callback_str.row      = row;
  callback_str.column   = column;
  callback_str.do_it    = True;
  
  DO_CALLBACK(w, XtNchangedCell, callback_str);
  
  if (callback_str.do_it != True) {
    XtFree(label);
    return REJECT;
  }
  
  
  if (CELL(label))
    XtFree(CELL(label));
  
  (void)memcpy((char*)cell, (char*)&new, sizeof(XawTableCellRec));
  
  SetLabelWidth (tw, row, column);
  Reposition (tw, cell, row, column);
    
  if (IsEditInRowColumn (tw, row, column)) 
  {
    XtVaSetValues (EDIT(w), "string", CELL(label), NULL);
  }
  else if (XtIsRealized (w) && w->core.visible && !tw->table.no_redraw) 
  {
    int x               = GetX(tw,column);
    int y               = GetY(tw,row);
    unsigned int width  = (unsigned int) COLUMN_WIDTH(tw,column);
    unsigned int height = (unsigned int) tw->table.row_height;
    
    XClearArea (XtDisplay(w), XtWindow(w), x, y, width, height, FALSE);
    (void) PaintCell (w, row, column, (XtPointer)cell, (XtPointer)NULL);
  }
  return ACCEPT;
}




int
#ifdef Xraw_NEED_PROTO
XawTableSetCellColours (Widget w, int row, int column, 
			Pixel fore, Pixel back)
#else
XawTableSetCellColours (w, row, column, fore, back)
     Widget w;
     int row;
     int column;
     Pixel fore;
     Pixel back;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;
  XawTableCallbackStruct callback_str;
  XawTableCell cell;
  XawTableCellRec        new;
  NormalReverseGC*       normal;
  ShadowGC*              shadow;



  if ((ROWS(tw) < 1) || (COLUMNS(tw) < 1) ||
      (row    != InRange (row, 0, ROWS(tw)-1)) || 
      (column != InRange (column, 0, COLUMNS(tw)-1)))
  {
    String subs[3];
    Cardinal num_subs = 3;
    sprintf(subs[0], "%5d", row);
    sprintf(subs[1], "%5d", column);
    subs[2] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "SetCellColours", "XawTableSetCellColours","XawToolkitError",
"XawTableSetCellColours\nIncorrect value of rows or columns (%s,%s) in TableWidget '%s' ",
		    subs, &num_subs);
    return REJECT;
  }
  cell = (XawTableCell)get_cell(STUFF(tw), row, column);

  if (CELL(special_colour) && fore == CELL(fore) && back == CELL(back))
    return ACCEPT;
  
  /*
   * prepare new cell stuff from old one
   */
  (void)memcpy((char*)&new, (char*)cell, sizeof(XawTableCellRec));
  new.special_colour = True;
  new.back           = back;
  new.fore           = fore;

  /* normal and reverse GC's */
  normal = GetNormalGC(w, fore, back, tw->table.font->fid);
  
  new.normal  = normal->normal;
  new.reverse = normal->reverse;


  /* shadow GC's */
  shadow = GetShadowGC(w, back);
  
  new.top    = shadow->top;
  new.bottom = shadow->bottom;


  callback_str.reason   = XawTABLE_CHANGED_CELL;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = cell;
  callback_str.new_cell = &new;
  callback_str.row      = row;
  callback_str.column   = column;
  callback_str.do_it    = True;

  DO_CALLBACK(w, XtNchangedCell, callback_str);

  if (callback_str.do_it) 
  {
    int x               = GetX(tw,column);
    int y               = GetY(tw,row);
    unsigned int width  = (unsigned int) COLUMN_WIDTH(tw,column);
    unsigned int height = (unsigned int) tw->table.row_height;
    
    if (XtIsRealized(w) && w->core.visible && 
	!tw->table.no_redraw && !tw->table.no_refigure) {
      XClearArea (XtDisplay(w), XtWindow(w), 
		  x + tw->table.label_shadow_thickness, 
		  y + tw->table.label_shadow_thickness, 
		  width, height, FALSE);
      PaintLabel (w, row, column, x, y, &new);
      PaintShadow (w, row, column, x, y, &new);
      XFlush(XtDisplay(w));
    }

    if (CELL(special_colour)) {
      ReleaseNormalGC(w, CELL(fore), CELL(back));
      ReleaseShadowGC(w, CELL(back));
    }
    (void)memcpy((char*)cell, (char*)&new, sizeof(XawTableCellRec));

    return ACCEPT;
  }
  else
  {
    ReleaseNormalGC(w, fore, back);
    ReleaseShadowGC(w, back);
    return REJECT;
  }
}


int
#ifdef Xraw_NEED_PROTO
XawTableSetCellDefaultColours (Widget w, int row, int column)
#else
XawTableSetCellDefaultColours (w, row, column)
     Widget w;
     int row;
     int column;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;
  XawTableCallbackStruct callback_str;
  XawTableCell cell;
  XawTableCellRec new;

  if ((ROWS(tw) < 1) || (COLUMNS(tw) < 1) ||
      (row    != InRange (row, 0, ROWS(tw)-1)) || 
      (column != InRange (column, 0, COLUMNS(tw)-1)))
  {
    String subs[3];
    Cardinal num_subs = 3;
    sprintf(subs[0], "%5d", row);
    sprintf(subs[1], "%5d", column);
    subs[2] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
        "SetCellColours", "XawTableSetCellDefaultColours","XawToolkitError",
"XawTableSetCellDefaultColours\n\
Incorrect value of row or column (%s,%s) in Table widget '%s'",
		    subs, &num_subs);
    return REJECT;
  }
  
  cell = (XawTableCell)get_cell(STUFF(tw), row, column);

  if (!CELL(special_colour))
    return ACCEPT;
  
  /*
   * prepare new cell stuff from old one
   */
  (void)memcpy((char*)&new, (char*)cell, sizeof(XawTableCellRec));
  new.special_colour = False;
  new.fore           = tw->table.foreground;
  new.back           = tw->core.background_pixel;

  callback_str.reason   = XawTABLE_CHANGED_CELL;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = cell;
  callback_str.new_cell = &new;
  callback_str.row      = row;
  callback_str.column   = column;
  callback_str.do_it    = True;

  DO_CALLBACK(w, XtNchangedCell, callback_str);

  if (callback_str.do_it) 
  {
    int x               = GetX(tw,column);
    int y               = GetY(tw,row);
    unsigned int width  = (unsigned int) COLUMN_WIDTH(tw,column);
    unsigned int height = (unsigned int) tw->table.row_height;
    
    if (XtIsRealized(w) && w->core.visible && !tw->table.no_redraw) {
      XClearArea (XtDisplay(w), XtWindow(w), 
		  x + tw->table.label_shadow_thickness, 
		  y + tw->table.label_shadow_thickness, 
		  width, height, FALSE);
      PaintLabel (w, row, column, x, y, &new);
      PaintShadow (w, row, column, x, y, &new);
      XFlush(XtDisplay(w));
    }

    if (CELL(special_colour)) {
      ReleaseNormalGC(w, CELL(fore), CELL(back));
      ReleaseShadowGC(w, CELL(back));
    }
    (void)memcpy((char*)cell, (char*)&new, sizeof(XawTableCellRec));

    return ACCEPT;
  }
  else
  {
    return REJECT;
  }
}

int
#ifdef Xraw_NEED_PROTO
XawTableSetCellBackground (Widget w, int row, int column, Pixel back)
#else
XawTableSetCellBackground (w, row, column, back)
     Widget w;
     int row;
     int column;
     Pixel back;
#endif
{
  XawTableWidget         tw = (XawTableWidget)w;
  XawTableCallbackStruct callback_str;
  XawTableCell           cell;
  XawTableCellRec        new;
  NormalReverseGC*       normal;
  ShadowGC*              shadow;

  if ((ROWS(tw) < 1) || (COLUMNS(tw) < 1) ||
      (row    != InRange (row, 0, ROWS(tw)-1)) || 
      (column != InRange (column, 0, COLUMNS(tw)-1)))
  {
    String subs[3];
    Cardinal num_subs = 3;
    sprintf(subs[0], "%5d", row);
    sprintf(subs[1], "%5d", column);
    subs[2] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
	    "SetCellColours", "XawTableSetCellBackground","XawToolkitError",
"XawTableSetCellBackground\n\
Incorrect value of row or column (%s,%s) in Table widget '%s'",
		    subs, &num_subs);
    return REJECT;
  }
  
  cell = (XawTableCell)get_cell(STUFF(tw), row, column);

  if (CELL(special_colour) && back == CELL(back))
    return ACCEPT;
  
  /*
   * prepare new cell stuff from old one
   */
  (void)memcpy((char*)&new, (char*)cell, sizeof(XawTableCellRec));
  new.special_colour = True;
  new.back           = back;

  if (!CELL(special_colour))
      new.fore = tw->table.foreground;

  /* normal and reverse GC's */
  normal = GetNormalGC(w, new.fore, back, tw->table.font->fid);
  
  new.normal  = normal->normal;
  new.reverse = normal->reverse;


  /* shadow GC's */
  shadow = GetShadowGC(w, back);
  
  new.top    = shadow->top;
  new.bottom = shadow->bottom;


  callback_str.reason   = XawTABLE_CHANGED_CELL;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = cell;
  callback_str.new_cell = &new;
  callback_str.row      = row;
  callback_str.column   = column;
  callback_str.do_it    = True;

  DO_CALLBACK(w, XtNchangedCell, callback_str);

  if (callback_str.do_it) 
  {
    int x               = GetX(tw,column);
    int y               = GetY(tw,row);
    unsigned int width  = (unsigned int) COLUMN_WIDTH(tw,column);
    unsigned int height = (unsigned int) tw->table.row_height;
    
    if (XtIsRealized(w) && w->core.visible && !tw->table.no_redraw) {
      XClearArea (XtDisplay(w), XtWindow(w), 
		  x + tw->table.label_shadow_thickness, 
		  y + tw->table.label_shadow_thickness, 
		  width, height, FALSE);
      PaintLabel (w, row, column, x, y, &new);
      PaintShadow (w, row, column, x, y, &new);
      XFlush(XtDisplay(w));
    }

    if (CELL(special_colour)) {
      ReleaseNormalGC(w, CELL(fore), CELL(back));
      ReleaseShadowGC(w, CELL(back));
    }
    (void)memcpy((char*)cell, (char*)&new, sizeof(XawTableCellRec));

    return ACCEPT;
  }
  else
  {
    ReleaseNormalGC(w, new.fore, back);
    ReleaseShadowGC(w, back);
    return REJECT;
  }

}

int
#ifdef Xraw_NEED_PROTO
XawTableSetCellForeground (Widget w, int row, int column, Pixel fore)
#else
XawTableSetCellForeground (w, row, column, fore)
     Widget w;
     int row;
     int column;
     Pixel fore;
#endif
{
  XawTableWidget         tw = (XawTableWidget)w;
  XawTableCallbackStruct callback_str;
  XawTableCell           cell;
  XawTableCellRec        new;
  NormalReverseGC*       normal;
  ShadowGC*              shadow;

  if ((ROWS(tw) < 1) || (COLUMNS(tw) < 1) ||
      (row    != InRange (row, 0, ROWS(tw)-1)) || 
      (column != InRange (column, 0, COLUMNS(tw)-1)))
  {
    String subs[3];
    Cardinal num_subs = 3;
    sprintf(subs[0], "%5d", row);
    sprintf(subs[1], "%5d", column);
    subs[2] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
	    "SetCellColours", "XawTableSetCellBackground","XawToolkitError",
"XawTableSetCellBackground\n\
Incorrect value of row or column (%s,%s) in Table widget '%s'",
		    subs, &num_subs);
    return REJECT;
  }
  
  cell = (XawTableCell)get_cell(STUFF(tw), row, column);

  if (CELL(special_colour) && fore == CELL(fore))
    return ACCEPT;
  
  /*
   * prepare new cell stuff from old one
   */
  (void)memcpy((char*)&new, (char*)cell, sizeof(XawTableCellRec));
  new.special_colour = True;
  new.fore           = fore;

  if (!CELL(special_colour))
      new.back = tw->core.background_pixel;

  /* normal and reverse GC's */
  normal = GetNormalGC(w, fore, new.back, tw->table.font->fid);
  
  new.normal  = normal->normal;
  new.reverse = normal->reverse;


  /* shadow GC's */
  shadow = GetShadowGC(w, new.back);
  
  new.top    = shadow->top;
  new.bottom = shadow->bottom;


  callback_str.reason   = XawTABLE_CHANGED_CELL;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = cell;
  callback_str.new_cell = &new;
  callback_str.row      = row;
  callback_str.column   = column;
  callback_str.do_it    = True;

  DO_CALLBACK(w, XtNchangedCell, callback_str);

  if (callback_str.do_it) 
  {
    int x               = GetX(tw,column);
    int y               = GetY(tw,row);
    unsigned int width  = (unsigned int) COLUMN_WIDTH(tw,column);
    unsigned int height = (unsigned int) tw->table.row_height;
    
    if (XtIsRealized(w) && w->core.visible && !tw->table.no_redraw) {
      XClearArea (XtDisplay(w), XtWindow(w), 
		  x + tw->table.label_shadow_thickness, 
		  y + tw->table.label_shadow_thickness, 
		  width, height, FALSE);
      PaintLabel (w, row, column, x, y, &new);
      PaintShadow (w, row, column, x, y, &new);
      XFlush(XtDisplay(w));
    }

    if (CELL(special_colour)) {
      ReleaseNormalGC(w, CELL(fore), CELL(back));
      ReleaseShadowGC(w, CELL(back));
    }
    (void)memcpy((char*)cell, (char*)&new, sizeof(XawTableCellRec));

    return ACCEPT;
  }
  else
  {
    ReleaseNormalGC(w, fore, new.back);
    ReleaseShadowGC(w, new.back);
    return REJECT;
  }
}

void
#ifdef Xraw_NEED_PROTO
XawTableGetCellColours (Widget w, int row, int column, 
			Pixel *fore, Pixel *back)
#else
XawTableGetCellColours (w, row, column, fore, back)
     Widget w;
     int row;
     int column;
     Pixel *fore;
     Pixel *back;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;
  XawTableCell   cell = (XawTableCell)get_cell(STUFF(tw), row, column);


  if (cell && cell->special_colour) {
    *fore = cell->fore;
    *back = cell->back;
  }
  else {
    *fore = tw->table.foreground;
    *back = tw->core.background_pixel;
  }
}

void
#ifdef Xraw_NEED_PROTO
XawTableGetCellColoursByCell (Widget w, XawTableCell cell, 
			      Pixel *fore, Pixel *back)
#else
XawTableGetCellColoursByCell (w, cell, fore, back)
     Widget w;
     XawTableCell cell;
     Pixel *fore;
     Pixel *back;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;

  if (cell && cell->special_colour) {
    *fore = cell->fore;
    *back = cell->back;
  }
  else {
    *fore = tw->table.foreground;
    *back = tw->core.background_pixel;
  }
}

/*#########################################################################*/
/*#                                                                       #*/
/*#                           Column Defaults                             #*/
/*#                                                                       #*/
/*#########################################################################*/

#ifdef notdef
/* ARGSUSED */
static Boolean PaintCellWithClear(p, i, j, call_data, client_data)
     XtPointer p;   
     int i;      
     int j;      
     XtPointer call_data;
     XtPointer client_data; /* unused */
{
  Widget         w      = (Widget) p;
  XawTableWidget tw     = (XawTableWidget) w;
  XawTableCell   cell   = (XawTableCell) call_data;
  unsigned int   width  = (unsigned int) COLUMN_WIDTH(tw,j);
  unsigned int   height = (unsigned int) tw->table.row_height;
  Position       x;
  Position       y;
    

  x = GetX(tw,j);
  y = GetY(tw,i);

  XClearArea (XtDisplay(w), XtWindow(w), 
	      x + tw->table.label_shadow_thickness, 
	      y + tw->table.label_shadow_thickness, 
	      width, height, FALSE);

  PaintLabel (w, i, j, x, y, cell);
  PaintShadow (w, i, j, x, y, cell);

  XFlush(XtDisplay(w));
  return False;
}
#endif

void
#ifdef Xraw_NEED_PROTO
XawTableSetColumnJustify (Widget w, int column, XtJustify justify)
#else
XawTableSetColumnJustify (w, column, justify)
     Widget w;
     int   column;
     XtJustify justify;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;
  int i, j;

  if (column != InRange (column, 0, COLUMNS(tw)-1)) 
    return;
    
  COLUMN_DATA(tw)[column].flag |= _CL_justify;
  COLUMN_DATA(tw)[column].justify = justify;
  
  for(i = 0; i < ROWS(tw); i++)
    Reposition(tw, NULL, i, column);
  
  if (XtIsRealized(w) && w->core.visible && 
      !tw->table.no_redraw && !tw->table.no_refigure) 
  {
    (void)go_table((XtPointer)w, PaintCell, STUFF(tw),
		   0, ROWS(tw)-1, column, column, 
		   XawTABLE_DOWN_RIGHT,
		   &i, &j, (XtPointer)NULL);
    XFlush(XtDisplay(w));
  }
}


XtJustify
#ifdef Xraw_NEED_PROTO
XawTableGetColumnJustify (Widget w, int column)
#else
XawTableGetColumnJustify (w, column)
     Widget w;
     int   column;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;

  if (column != InRange (column, 0, COLUMNS(tw)-1)) 
    return XtJustifyCenter;
    
  if (COLUMN_DATA(tw)[column].flag & _CL_justify)
    return COLUMN_DATA(tw)[column].justify;
  
  return XtJustifyCenter;
}


void
#ifdef Xraw_NEED_PROTO
XawTableSetColumnWidth (Widget w, int column, int width)
#else
XawTableSetColumnWidth(w, column, width)
     Widget w;
     int   column;
     int   width;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;
  XawTableCallbackStruct callback_str;
  register int row;

  if (column != InRange (column, 0, COLUMNS(tw)-1)) 
    return;
    
  if ((COLUMN_DATA(tw)[column].flag & _CL_width) && 
       COLUMN_DATA(tw)[column].width == width)
    return;

  COLUMN_DATA(tw)[column].flag |= _CL_width;
  COLUMN_DATA(tw)[column].width = width;

  for(row = 0; row < ROWS(tw); row++)
    Reposition(tw, NULL, row, column);
  
  UpdateTable(tw);

  callback_str.reason   = XawTABLE_CHANGED_COLUMN_WIDTH;
  callback_str.event    = (XEvent*)NULL;
  callback_str.old_cell = (XawTableCell)NULL;
  callback_str.new_cell = (XawTableCell)NULL;
  callback_str.row      = 0;
  callback_str.column   = column;
  callback_str.do_it    = True;
  
  DO_CALLBACK(w, XtNchangedColumnWidth, callback_str);
}



int
#ifdef Xraw_NEED_PROTO
XawTableGetColumnWidth (Widget w, int column)
#else
XawTableGetColumnWidth(w, column)
     Widget w;
     int   column;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;

  if (column != InRange (column, 0, COLUMNS(tw)-1)) 
    return tw->table.column_default_width;
    
  if ((COLUMN_DATA(tw)[column].flag & _CL_width))
    return COLUMN_DATA(tw)[column].width;

  return tw->table.column_default_width;
}

int
#ifdef Xraw_NEED_PROTO
XawTableGetColumnPixelWidth (Widget w, int column)
#else
XawTableGetColumnPixelWidth(w, column)
     Widget w;
     int   column;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;
  int width;

  if (column != InRange (column, 0, COLUMNS(tw)-1)) 
    width = tw->table.column_default_width;
  else   if ((COLUMN_DATA(tw)[column].flag & _CL_width))
    width = COLUMN_DATA(tw)[column].width;
  else
    width = tw->table.column_default_width;

  if (tw->table.literal) {
    width = width * tw->table.literal_width + 2 * tw->table.internal_width;
  }

  return width;
}

char *
#ifdef Xraw_NEED_PROTO
XawTableGetLabelByPosition (Widget w, int i, int j)
#else
     XawTableGetLabelByPosition(w,i,j)
     Widget w;
     int i,j;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;
  XawTableCell cell;
    

  if ((ROWS(tw) < 1) || (COLUMNS(tw) < 1) ||
      (i != InRange(i, 0, ROWS(tw)-1)) || 
      (j != InRange(j, 0, COLUMNS(tw)-1)))
  {
    String subs[3];
    Cardinal num_subs = 3;
    sprintf(subs[0], "%5d", i);
    sprintf(subs[1], "%5d", j);
    subs[2] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "GetLabelByPosition", "XawTableGetLabelByPosition","XawToolkitError",
"XawTableGetLabelByPosition\nIncorrect value of rows or columns (%s,%s) in TableWidget '%s' ",
		    subs, &num_subs);
    return NULL;
  }
  
  cell = (XawTableCell)get_cell(STUFF(tw), i, j);
  return GET_CELL_LABEL;
}

void
#ifdef Xraw_NEED_PROTO
XawTableUnsetEdit (Widget w)
#else
     XawTableUnsetEdit(w)
     Widget w;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;
  char* label;
  XawTableCell cell;
  
  if (XawTableIsEditManaged(w))
  {
    tw->table.no_redraw++;
    UNMANAGE_EDIT(tw);
    tw->table.no_redraw--;
    
    XtVaGetValues(EDIT(w), "string", &label, NULL);

    label = CopyOnlyPrintable(label);

    cell = (XawTableCell)
      get_cell(STUFF(tw),tw->table.edit_row, tw->table.edit_column);

    if (!streq(label,CELL(label))) 
      XawTableSetLabel(w, tw->table.edit_row, tw->table.edit_column, label);

    XtFree(label); /* XtMalloc in CopyOnlyPrintable */
  }
}

void 
#ifdef Xraw_NEED_PROTO
XawTableSetEdit (Widget w, int row, int column)
#else
     XawTableSetEdit(w, row, column)
     Widget w;
     int    row;
     int    column;
#endif
{
  XawTableWidget tw = (XawTableWidget)w;
  XawTableCell     cell;

  if (IsEditInRowColumn(tw, row, column) || STUFF(tw) == NULL)
    return;

  if ((ROWS(tw) < 1) || (COLUMNS(tw) < 1) ||
      (row != InRange(row, 0, ROWS(tw)-1)) || 
      (column != InRange(column, 0, COLUMNS(tw)-1)))
  {
    String subs[3];
    Cardinal num_subs = 3;
    sprintf(subs[0], "%5d", row);
    sprintf(subs[1], "%5d", column);
    subs[2] = w->core.name;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "SetEdit", "XawTableSetEdit","XawToolkitError",
"XawTableSetEdit\nIncorrect value of rows or columns (%s,%s) in TableWidget '%s' ",
		    subs, &num_subs);
    return;
  }
  
  XawTableUnsetEdit(w);

  cell = (XawTableCell)get_cell(STUFF(tw), row, column);
  
  if (CELL(label) == NULL)
    XtVaSetValues(EDIT(tw), "string", Dummy, NULL);
  else
    XtVaSetValues(EDIT(tw), "string", CELL(label), NULL);
    
  tw->table.edit_row    = row;
  tw->table.edit_column = column;

  MoveEditCell (tw, tw->table.edit_row, tw->table.edit_column); 

  MANAGE_EDIT(tw);
}

/******************************************************************
 *
 *                              Actions
 *
 ******************************************************************/

static void HighlightCell(w,event,params,num_params)
     Widget w;
     XEvent *event;
     String *params;         
     Cardinal *num_params;
{
  XawTableWidget tw = (XawTableWidget)w;
  XawTableCell cell;
  int row, column;
  Position x,y;
  Time tm;

  if (!tw->table.editable)
    return;
    
  if (*num_params == 2) 
  {
    row    = atoi(params[0]);
    column = atoi(params[1]);
  }
  else if (event != (XEvent*)NULL) 
  {
    ExtractPosition(event, &x, &y , &tm);
    if (ExtractCell(tw, x, y, &row, &column))
      return ;
  }
  else
  {
    return;
  }
  
  cell = (XawTableCell) get_cell (STUFF(tw), row, column);

  if (CELL(highlight))
    return ;

  CELL(highlight) = True;

  x = GetX(tw,column);
  y = GetY(tw,row);
  
  PaintLabel(w, row, column, x, y, cell);
  XFlush(XtDisplay(w));
}

/* ARGSUSED */
static void UnhighlightCell(w,event,params,num_params)
     Widget w;
     XEvent *event;
     String *params;         /* unused */
     Cardinal *num_params;
{
  XawTableWidget tw = (XawTableWidget)w;
  XawTableCell      cell;
  int          row, column;
  Position     x,y;
  Time         tm;
  unsigned int width;
  unsigned int height;
  
  if (!tw->table.editable)
    return;
    
  if (*num_params == 2) 
  {
    row    = atoi(params[0]);
    column = atoi(params[1]);
  }
  else if (event != (XEvent*)NULL) 
  {
    ExtractPosition(event, &x, &y , &tm);
    if (ExtractCell(tw, x, y, &row, &column))
      return ;
  }
  else
  {
    return;
  }
  
  cell = (XawTableCell)get_cell(STUFF(tw), row, column);

  if (!CELL(highlight))
        return ;

  CELL(highlight) = False;

  width   = (unsigned int) COLUMN_WIDTH(tw, column)  +
    2 * tw->table.label_shadow_thickness;
  height  = (unsigned int) tw->table.row_height +
    2 * tw->table.label_shadow_thickness;

  XClearArea(XtDisplay(w), XtWindow(w),
	     (int)GetX(tw,column),
	     (int)GetY(tw,row),
	     width, height, FALSE);

  PaintLabel(w, row, column, x, y, cell);
  
  XFlush(XtDisplay(w));

}

/* ARGSUSED */
static void WhatCell(w,event,params,num_params)
     Widget w;
     XEvent *event;
     String *params;         /* unused */
     Cardinal *num_params;
{
  XawTableWidget tw = (XawTableWidget)w;
  Position x,y;
  int row, column;
  XawTableCallbackStruct callback_str;
  XawTableCell cell;
  Time tm;
  
  if (event->type != ButtonPress && event->type != ButtonRelease)
    return;

  ExtractPosition(event, &x, &y , &tm);

  if (ExtractCell(tw, x, y, &row, &column))
    return ;

  cell = (XawTableCell) get_cell (STUFF(tw), row, column);

  callback_str.reason   = XawTABLE_WHAT_CELL;
  callback_str.event    = event;
  callback_str.old_cell = cell;
  callback_str.new_cell = cell;
  callback_str.row      = row;
  callback_str.column   = column;
  callback_str.do_it    = True;

  DO_CALLBACK(w, XtNwhatCell, callback_str);
}

/* ARGSUSED */
static void KeyReturn(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;           /* unused */
     Cardinal *num_params;     /* unused */
{
  Widget tw;
  KeySym  ksSymbol;
  
  ksSymbol = XtGetActionKeysym(event, (Modifiers*)NULL);

  if (ksSymbol == XK_Return || ksSymbol == XK_Linefeed)
  {
    XtVaGetValues(w, XtNuserData, &tw, NULL);
    XawTableUnsetEdit(tw);
  }
  
}

static Atom FetchAtom(w, name)
    Widget w;
    String name;
{
    Atom a;
    XrmValue source, dest;

    source.size = strlen(name)+1;
    source.addr = name;
    dest.size = sizeof(Atom);
    dest.addr = (XtPointer) &a;
	
    (void) XtConvertAndStore(w, XtRString, &source, XtRAtom, &dest);
    return a;
}
    
/* ARGSUSED */
static Boolean DeliverSelection(w, selection, target,
	type, value, length, format)
    Widget w;
    Atom *selection, *target, *type;
    XtPointer *value;
    unsigned long *length;
    int *format;
{
    XawTableWidget tw = (XawTableWidget) w;
    static Atom targets = 0;

    if (targets == 0) {
	targets = FetchAtom(w, "TARGETS");
    }

    if (*target == targets) {
	*type = XA_ATOM;
	*value = (XtPointer) CALLOC(1, Atom);
	*(Atom *) *value = XA_STRING;
	*length = 1;
	*format = 32;
	return TRUE;
    }

    if (*target == XA_STRING) {
	*type = XA_STRING;
	*value = (XtPointer) XtNewString(tw->table.cell_own->label);
	*length = tw->table.cell_own->label_len;
	*format = 8;
	return TRUE;
    }
    
    return FALSE;    
}

/* ARGSUSED */
static void LoseSelection(w, selection)
    Widget w;
    Atom *selection;
{
  XawTableWidget tw = (XawTableWidget) w;
  XawTableCell cell;
  int row, column;
  int x,y;
  unsigned int width, height;

  cell = tw->table.cell_own;
  tw->table.cell_own = (XawTableCell)NULL;
  
  if (!CELL(highlight)) 
    return ;

  get_cell_positions(cell, &row, &column);
  
  CELL(highlight) = False;

  x      = GetX(tw,column) + tw->table.label_shadow_thickness;
  y      = GetY(tw,row) + tw->table.label_shadow_thickness;
  width  = (unsigned int) COLUMN_WIDTH(tw, column);
  height = (unsigned int) tw->table.row_height;
    
  XClearArea(XtDisplay(w), XtWindow(w), x, y, width, height, FALSE);
  (void)PaintCell(w, row, column, (XtPointer)cell, (XtPointer)NULL);

  XFlush(XtDisplay(w));
}

static int GetCutBufferNumber();

static void StoreBuffer(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;        /* selections in precedence order */
     Cardinal *num_params;
{
  XawTableWidget tw = (XawTableWidget)w;
  Position    x, y;
  int         row, column;
  int         i, buffer;
  Time        tm;
  Atom        selection;

  if (!tw->table.editable)
    return;
  
  if (event->type != ButtonPress && event->type != ButtonRelease)
    return;
  
  ExtractPosition(event, &x, &y ,&tm);

  if (ExtractCell(tw, x, y, &row, &column))
    return ;

  if (tw->table.cell_own != (XawTableCell)NULL)
    LoseSelection(w, (Atom*)NULL);
  
  tw->table.cell_own = (XawTableCell)get_cell(STUFF(tw), row, column);

  for(i=0; i<(int)*num_params; i++) {
    selection = FetchAtom(w, *(String *)(params+i));

    buffer =  GetCutBufferNumber(selection);

    if (buffer >= 0) {
      if (buffer == 0) {

#define Create(buffer) \
      XChangeProperty(XtDisplay(w), DefaultRootWindow(XtDisplay(w)),\
		      buffer,XA_STRING, 8,PropModeAppend, NULL, 0)

        Create(XA_CUT_BUFFER0);
        Create(XA_CUT_BUFFER1);
        Create(XA_CUT_BUFFER2);
        Create(XA_CUT_BUFFER3);
        Create(XA_CUT_BUFFER4);
        Create(XA_CUT_BUFFER5);
        Create(XA_CUT_BUFFER6);
        Create(XA_CUT_BUFFER7);
#undef Create

        XRotateBuffers(XtDisplay(w), 1);
      }
    
      XStoreBuffer(XtDisplay(w), tw->table.cell_own->label,
		   (int)MIN(tw->table.cell_own->label_len, MAXCUT), buffer);
    
    } else {
      tw->table.selections[tw->table.num_selections++] = selection;

      XtOwnSelection (w, selection, tm,
		      DeliverSelection, LoseSelection,
		      (XtSelectionDoneProc) NULL);
    }
  }
}


/* ARGSUSED */
static void  CallEdit(w,event,params,num_params)
     Widget w;
     XEvent *event;
     String *params;         /* unused */
     Cardinal *num_params;
{
  XawTableWidget tw = (XawTableWidget)w;
  Position x,y;
  int row, column;
  Time tm;

  if (!tw->table.editable)
    return;
    
  ExtractPosition(event, &x, &y , &tm);

  if (ExtractCell(tw, x, y, &row, &column))
    return ;

  XawTableSetEdit(w, row, column);
}

    

typedef struct {
  Widget         w;
  Atom          *selections;
  Time           time;
  int            row;
  int            column;
  int            count;
  int            num;
}RowColumn;

static void GetProc();

static void GetSelection (rc)
  RowColumn *rc;
{
  Atom selection;
  int buffer;
  int nbytes;
  char *label;
  
  selection = *(rc->selections + rc->num);

  if (rc->num < rc->count){

    buffer = GetCutBufferNumber(selection);

    if (buffer >= 0) {
      label = XFetchBuffer(XtDisplay(rc->w), &nbytes, buffer);
      XawTableSetLabel(rc->w, rc->row, rc->column, label);
      XtFree((XtPointer)rc->selections);
      XtFree((XtPointer)rc);
    }else {
      XtGetSelectionValue(rc->w, selection, XA_STRING, GetProc,
			  (XtPointer)rc, rc->time);
      rc->num++;
    }
  }
}

/* ARGSUSED */
static void GetProc(w, client_data, selection,
			    type, value, length, format)
     Widget w;
     XtPointer client_data;
     Atom *selection;
     Atom *type;
     XtPointer value;
     unsigned long *length;
     int *format;
{
  RowColumn *rc = (RowColumn*)client_data;
    
  if ((*type == XA_STRING) && (value != NULL)) {
    XawTableSetLabel(w, rc->row, rc->column, (char*)value);
    XtFree((XtPointer)value);
    XtFree((XtPointer)client_data);
  }else {
    GetSelection(rc);
  }

}

static void InsertSelection(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;        /* selections in precedence order */
     Cardinal *num_params;
{
  XawTableWidget tw = (XawTableWidget)w;
  Position    x,y;
  int         row, column;
  RowColumn  *rc;
  Time        tm;

  if (!tw->table.editable)
    return;
  
  if (event->type != ButtonPress && event->type != ButtonRelease)
    return;

  ExtractPosition(event, &x, &y ,&tm);
  
  if (ExtractCell(tw, x, y, &row, &column))
    return ;

  if (*num_params > 0) {
      
    rc = XtNew(RowColumn);

    rc->time       = tm;
    rc->w          = w;
    rc->row        = row;
    rc->column     = column;
    rc->count      = (int)*num_params;
    rc->num        = 0;

    if (*num_params > (Cardinal)0) {
      rc->selections = CALLOC(*num_params, Atom);
      XmuInternStrings(XtDisplay(w), params, *num_params, rc->selections);
    } else {
      rc->selections = XtNew(Atom);
      *rc->selections = FetchAtom(w, "PRIMARY");
    }
    GetSelection(rc);
  }
}


static int GetCutBufferNumber(atom)
     register Atom atom;
{
  if (atom == XA_CUT_BUFFER0) return(0);
  if (atom == XA_CUT_BUFFER1) return(1);
  if (atom == XA_CUT_BUFFER2) return(2);
  if (atom == XA_CUT_BUFFER3) return(3);
  if (atom == XA_CUT_BUFFER4) return(4);
  if (atom == XA_CUT_BUFFER5) return(5);
  if (atom == XA_CUT_BUFFER6) return(6);
  if (atom == XA_CUT_BUFFER7) return(7);
  return(-1);
}


/* ARGSUSED */
static void DoingNothing(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;       
     Cardinal *num_params;
{
  /* doing nothing */
}

