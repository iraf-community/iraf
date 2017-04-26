/* $Id: TabsP.h,v 1.8 1999/12/16 18:01:25 falk Exp $
 *
 * TabsP.h - Private definitions for Index Tabs widget
 * 
 */

#ifndef _TabsP_h
#define _TabsP_h

/***********************************************************************
 *
 * Tabs Widget Private Data
 *
 ***********************************************************************/

#include <X11/IntrinsicP.h>

#ifdef	USE_MOTIF
#include <Xm/XmP.h>
#include <Xm/ManagerP.h>
#endif

#include "Tabs.h"

/* New fields for the Tabs widget class record */
typedef struct {XtPointer extension;} TabsClassPart;

/* Full class record declaration */
typedef struct _TabsClassRec {
    CoreClassPart	core_class;
    CompositeClassPart  composite_class;
    ConstraintClassPart	constraint_class;
#ifdef	USE_MOTIF
    XmManagerClassPart	manager_class;
#endif
    TabsClassPart	tabs_class;
} TabsClassRec;

extern TabsClassRec tabsClassRec;



/****************************************************************
 *
 * instance record declaration
 *
 ****************************************************************/

/* New fields for the Tabs widget record */
typedef struct {
    /* resources */
    XFontStruct	*font ;
    Dimension   internalHeight, internalWidth ;
    Widget	topWidget ;
    XtCallbackList callbacks ;
    XtCallbackList popdownCallbacks ;
    Boolean	selectInsensitive ;
    Boolean	be_nice_to_cmap ;
    int		top_shadow_contrast ;
    int		bot_shadow_contrast ;
    int		insensitive_contrast ;

    /* private state */
    Widget	hilight ;
    GC		foregroundGC ;
    GC		backgroundGC ;
    GC		greyGC ;
    GC		topGC ;
    GC		botGC ;
    Dimension	tab_height ;		/* height of tabs (all the same) */
    					/* Note: includes top shadow only */
    Dimension	tab_total ;		/* total height of all tabs */
    Dimension	child_width, child_height; /* child size, including borders */
    Dimension	max_cw, max_ch ;	/* max child preferred size */
    Cardinal	numRows ;
    XtGeometryMask last_query_mode;
    Boolean	needs_layout ;
    Pixmap	grey50 ;		/* TODO: cache this elsewhere */
} TabsPart;


typedef struct _TabsRec {
    CorePart		core;
    CompositePart	composite;
    ConstraintPart	constraint;
#ifdef	USE_MOTIF
    XmManagerPart	manager;
#endif
    TabsPart		tabs;
} TabsRec;




/****************************************************************
 *
 * constraint record declaration
 *
 ****************************************************************/

typedef	struct _TabsConstraintsPart {
	/* resources */
	String	label ;
	Pixmap	left_bitmap ;
	Pixel	foreground ;
	Boolean	resizable ;

	/* private state */
	Pixel		grey ;
	Boolean		greyAlloc ;
	Dimension	width ;		/* tab width */
	Position	x,y ;		/* tab base position */
	short		row ;		/* tab row */
	Position	l_x, l_y ;	/* label position */
	Position	lbm_x, lbm_y ;	/* bitmap position */
	unsigned int	lbm_width, lbm_height, lbm_depth ;
} TabsConstraintsPart ;

typedef	struct _TabsConstraintsRec {
#ifdef	USE_MOTIF
	XmManagerConstraintPart	manager;
#endif
	TabsConstraintsPart	tabs ;
} TabsConstraintsRec, *TabsConstraints ;


#endif /* _TabsP_h */
