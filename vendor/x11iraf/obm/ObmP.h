/* Copyright(c) 1993 Association of Universities for Research in Astronomy Inc.
 */

/*
 * ObmP.h -- Private or internal global definitions for the Object Manager.
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Core.h>
#include <X11/Object.h>

#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Grip.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Panner.h>
#include <X11/Xaw/Porthole.h>
#include <X11/Xaw/Repeater.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Simple.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/StripChart.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Tree.h>
#include <X11/Xaw/Viewport.h>

#include <ObmW/Arrow.h>
#include <ObmW/Board.h>
#include <ObmW/Button.h>
#include <ObmW/Common.h>
#include <ObmW/Frame.h>
#include <ObmW/Group.h>
#include <ObmW/Gterm.h>
#include <ObmW/HTML.h>
#include <ObmW/Icon.h>
#include <ObmW/Label.h>
#include <ObmW/Layout.h>
#include <ObmW/ListTree.h>
#include <ObmW/MenuBar.h>
#include <ObmW/MultiList.h>
#include <ObmW/RadioGrp.h>
#include <ObmW/RowCol.h>
#include <ObmW/Scrollbar.h>
#include <ObmW/Slider2.h>
#include <ObmW/Tabs.h>
#include <ObmW/Toggle.h>

#include <X11/Xraw/Separator.h>
#include <X11/Xraw/Table.h>

#include <X11/xpm.h>
#include <Tcl/tcl.h>

/* Size limiting definitions. */
#define SZ_NAME		128		/* class or object name */
#define SZ_LINE		256		/* line buffer */
#define SZ_INDEX	197		/* hash index */
#define SZ_MESSAGE	512		/* random string buffers */
#define SZ_COMMAND	8192		/* general command buffers */
#define SZ_NUMBER	64		/* numeric string buffers */
#define SZ_GEOMETRY	32		/* geometry specification */
#define MAX_RESOURCES	2048		/* max resources in application */
#define	MAX_HASHCHARS	8		/* max chars used for hashval */
#define	MAX_ARGS	128		/* scratch arg list */
#define MAX_MENUITEMS	64		/* max items in a menu */
#define	MAX_LEVELS	128		/* recursive nesting */	
#define	MAXNDC		32767		/* NDC graphics coordinates */
#define	MAXCOLORSYM	20		/* max color resources for icons */

#define	MI_IGNORE	0		/* menu item type codes */
#define	MI_EXEC		1
#define	MI_LINE		2
#define	MI_DBLLINE	3
#define	MI_MENU		4
#define	MI_SPACE	5
#define	MI_TITLE	6

#define	M_Insensitive		000001	/* menu attribute bitflags */
#define	M_FreeBackground	000002
#define	M_FreeForeground	000004
#define	M_FreeAccel		000010
#define	M_FreeLabel		000020
#define	M_FreeData		000040

/* Object class types */
#define	OtShell		1		/* shell widget class */
#define	OtNonShell	2		/* nonshell widget class */
#define	OtParameter	3		/* UI control parameter class */
#define	OtServer	4		/* UI server class */
#define	OtClient	5		/* UI client class */
#define	OtMarker	6		/* gterm marker class */
#define	OtNClasses	6

typedef	int (*ObmFunc)();
typedef	void (*ObmMethod)();

extern	void ServerClassInit(), ClientClassInit(), ParameterClassInit();
extern	void WidgetClassInit(), GenericClassDestroy();
extern	void GtermClassInit(), MarkerClassInit(), HTMLClassInit();

/* Dummy WtClass bit flag definitions for initializers. */
#define	WtServer		0, 0
#define	WtClient		0, 0
#define	WtParameter		0, 0
#define	WtMarker		0, 0

/* Widget class bit flags.
 */
#define WtCore			00000000000, 00000000001
#define WtObject		00000000000, 00000000002
#define WtSimple		00000000000, 00000000004
#define WtShell			00000000000, 00000000010

#define WtAsciiSink		00000000000, 00000000020
#define WtAsciiSrc		00000000000, 00000000040
#define WtAsciiText		00000000000, 00000000100
#define WtBox			00000000000, 00000000200
#define WtCommand		00000000000, 00000000400
#define WtDialog		00000000000, 00000001000
#define WtForm			00000000000, 00000002000
#define WtGrip			00000000000, 00000004000
#define WtLabel			00000000000, 00000010000
#define WtList			00000000000, 00000020000
#define WtMenuButton		00000000000, 00000040000
#define WtPaned			00000000000, 00000100000
#define WtPanner		00000000000, 00000200000
#define WtPorthole		00000000000, 00000400000
#define WtRepeater		00000000000, 00001000000
#define WtScrollbar		00000000000, 00002000000
#define WtSimpleMenu		00000000000, 00004000000
#define WtSme			00000000000, 00010000000
#define WtSmeBSB		00000000000, 00020000000
#define WtSmeLine		00000000000, 00040000000
#define WtStripChart		00000000000, 00100000000
#define WtToggle		00000000000, 00200000000
#define WtTree			00000000000, 00400000000
#define WtViewport		00000000000, 01000000000

#define WtGterm			00000000001, 00000000000
#define WtLayout		00000000002, 00000000000
#define WtHTML			00000000004, 00000000000
#define WtArrow			00000000010, 00000000000
#define WtBoard			00000000020, 00000000000
#define WtScrollbar2		00000000040, 00000000000
#define WtSlider2d		00000000100, 00000000000
#define WtFrame			00000000200, 00000000000
#define WtGroup			00000000400, 00000000000
#define WtIcon			00000001000, 00000000000
#define WtMultiList		00000002000, 00000000000
#define WtRadioGroup		00000004000, 00000000000
#define WtRowCol		00000010000, 00000000000
#define WtTextBox		00000020000, 00000000000
#define WtTextButton		00000040000, 00000000000
#define WtTextToggle		00000100000, 00000000000
#define WtXfwfCommon		00000200000, 00000000000
#define WtXfwfMenuBar		00000400000, 00000000000
#define WtTabs			00001000000, 00000000000
#define WtListTree		00002000000, 00000000000
#define WtSeparator		00004000000, 00000000000
#define WtTable			00010000000, 00000000000

/* Object base classes. */
typedef struct {
	int class;			/* class type code */
	char name[SZ_NAME];		/* class name */
} baseClassRec, *BaseClassRec;

/* UI object class descriptor. */
typedef struct {
	char name[SZ_NAME];		/* object class name */
	int object_type;		/* widget type (shell etc.) */
	WidgetClass *widget_class;	/* for Xt/Athena widgets */
	unsigned long flag1, flag2;	/* widget class bit flags. */
	ObmMethod ClassInit;		/* initializes class record */
	ObmMethod ClassDestroy;		/* close class record */
	ObmFunc Create;			/* create proc */
	ObmMethod Destroy;		/* destroy proc */
	ObmFunc Evaluate;		/* evaluate proc */
	XtPointer class_data;		/* class specific data */
} objClassRec, *ObjClassRec;

/* Class descriptors for all UI object classes and subclasses.  In the
 * following only the class initializer function needs to be set statically,
 * since the class initializer function will initialize the remaining fields
 * of the class descriptor at run time when the object manager is opened.
 */
#ifdef Obm_Main
baseClassRec baseClasses[] = {
	{ OtShell,	"Widget", },
	{ OtNonShell,	"Widget", },
	{ OtParameter,	"Parameter", },
	{ OtServer,	"Server", },
	{ OtClient,	"Client", },
	{ OtMarker,	"Marker", },
};

objClassRec UiObjects[] = {
	{ "Server",	OtServer, NULL, WtServer,
			ServerClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Client",	OtClient, NULL,  WtClient,
			ClientClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Parameter",	OtParameter, NULL, WtParameter,
			ParameterClassInit, NULL, NULL, NULL, NULL, NULL },

	{ "Core",	OtNonShell, &coreWidgetClass, WtCore,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Object",	OtNonShell, &objectClass, WtObject,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Simple",	OtNonShell, &simpleWidgetClass, WtSimple,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Widget",	OtNonShell, &coreWidgetClass, WtCore,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },

	{ "AsciiText",	OtNonShell, &asciiTextWidgetClass, WtAsciiText,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Box",	OtNonShell, &boxWidgetClass, WtBox,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Command",	OtNonShell, &commandWidgetClass, WtCommand,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Dialog",	OtNonShell, &dialogWidgetClass, WtDialog,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Form",	OtNonShell, &formWidgetClass, WtForm,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Grip",	OtNonShell, &gripWidgetClass, WtGrip,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Label",	OtNonShell, &labelWidgetClass, WtLabel,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "List",	OtNonShell, &listWidgetClass, WtList,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "ListTree",	OtNonShell, &listtreeWidgetClass, WtListTree,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "MenuButton",	OtNonShell, &menuButtonWidgetClass, WtMenuButton,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Paned",	OtNonShell, &panedWidgetClass, WtPaned,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Panner",	OtNonShell, &pannerWidgetClass, WtPanner,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Porthole",	OtNonShell, &portholeWidgetClass, WtPorthole,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Repeater",	OtNonShell, &repeaterWidgetClass, WtRepeater,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Scrollbar",	OtNonShell, &scrollbarWidgetClass, WtScrollbar,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Separator",	OtNonShell, &separatorWidgetClass, WtSeparator,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "SimpleMenu",	OtShell, &simpleMenuWidgetClass, WtSimpleMenu,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Sme",	OtNonShell, &smeObjectClass, WtSme,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "SmeBSB",	OtNonShell, &smeBSBObjectClass, WtSmeBSB,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "SmeLine",	OtNonShell, &smeLineObjectClass, WtSmeLine,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "StripChart",	OtNonShell, &stripChartWidgetClass, WtStripChart,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Table",	OtNonShell, &tableWidgetClass, WtTable,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Tabs",	OtNonShell, &tabsWidgetClass, WtTabs,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Toggle",	OtNonShell, &toggleWidgetClass, WtToggle,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Tree",	OtNonShell, &treeWidgetClass, WtTree,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Viewport",	OtNonShell, &viewportWidgetClass, WtViewport,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },

	{ "Gterm",	OtNonShell, &gtermWidgetClass, WtGterm,
			GtermClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Marker",	OtMarker, NULL, WtMarker,
			MarkerClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Layout",	OtNonShell, &layoutWidgetClass, WtLayout,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "HTML",	OtNonShell, &htmlWidgetClass, WtHTML,
			HTMLClassInit, NULL, NULL, NULL, NULL, NULL },

	{ "Arrow",	OtNonShell, &xfwfArrowWidgetClass, WtArrow,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Board",	OtNonShell, &xfwfBoardWidgetClass, WtBoard,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Frame",	OtNonShell, &xfwfFrameWidgetClass, WtFrame,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Group",	OtNonShell, &xfwfGroupWidgetClass, WtGroup,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Icon",	OtNonShell, &xfwfIconWidgetClass, WtIcon,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "MultiList",	OtNonShell, &xfwfMultiListWidgetClass, WtMultiList,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "RadioGroup",	OtNonShell,&xfwfRadioGroupWidgetClass,WtRadioGroup,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "RowCol",	OtNonShell, &xfwfRowColWidgetClass, WtRowCol,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "TextBox",	OtNonShell, &xfwfLabelWidgetClass, WtTextBox,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "TextButton",	OtNonShell, &xfwfButtonWidgetClass, WtTextButton,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "TextToggle",	OtNonShell, &xfwfToggleWidgetClass, WtTextToggle,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Scrollbar2",	OtNonShell, &xfwfScrollbarWidgetClass, WtScrollbar2,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "Slider2d",	OtNonShell, &xfwfSlider2WidgetClass, WtSlider2d,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },

	{ "Shell", 	OtShell, &shellWidgetClass, WtShell,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "OverrideShell",
			OtShell, &overrideShellWidgetClass, WtShell,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "WMShell",
			OtShell, &wmShellWidgetClass, WtShell,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "TransientShell",
			OtShell, &transientShellWidgetClass, WtShell,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "TopLevelShell",
			OtShell, &topLevelShellWidgetClass, WtShell,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
	{ "ApplicationShell",
			OtShell, &applicationShellWidgetClass, WtShell,
			WidgetClassInit, NULL, NULL, NULL, NULL, NULL },
};
#else
extern objClassRec UiObjects[];
extern baseClassRec baseClasses[];
#endif


/* Generic object instance record, omitting the class part.  The object
 * instance records for the real object classes share the objectCore structure
 * but have a class specific structure and are a different size.
 */
struct obmObject {
	struct obmObjectCore {
	    char name[SZ_NAME];		/* object name */
	    ObjClassRec classrec;	/* class record */
	    ObjClassRec superclass;	/* superclass, if class is subclass */
	    struct obmObject *prevglob;	/* previous object in global list */
	    struct obmObject *nextglob;	/* next object in global list */
	    struct obmObject *nexthash;	/* next object in hash list */
	    struct obmObject *parent;	/* the object's parent */
	    int nchildren;		/* number of child objects */
	    struct obmObject **children;  /* list of child objects */
	    Boolean being_destroyed;	/* set by Destroy method */
	    Boolean mapped;		/* used to recreate shells */
	    char geometry[SZ_GEOMETRY];	/* used to recreate shells */
	} core;
	/* class part omitted */
};

typedef	struct obmObject *ObmObject;
typedef	struct obmObjectCore *ObmObjectCore;

/* Object list. */
struct objList {
	char name[SZ_NAME];
	caddr_t ptr;
	struct objList *next;
};

/* Menu item. */
typedef struct {
	int type;
	XtPointer menu;
	Widget entry;
	char *sbuf;
	char *label;
	char *data;
	char *accelerator;
	char *background;
	char *foreground;
	int justify;
	Pixmap pixmap;
	int flags;
} menuItem, *MenuItem;

/* Menu descriptor. */
typedef struct {
	ObmObject obj;
	Widget menuShell;
	Boolean popped_up;
	XtPointer obm;
	int nitems;
	menuItem items[MAX_MENUITEMS];
} Menu, *MenuPtr;

typedef struct objList *ObjList;

/* Callback type codes. */
#define	Ctcallback			0	/* most widgets */
#define	Ctcharmode			1	/* text widget, char mode */
#define	Ctlinemode			2	/* text widget, char mode */
#define	CtgetValue			3	/* strip chart */
#define	CtjumpProc			4	/* scrollbar */
#define	CtscrollProc			5	/* scrollbar */
#define	CtpopdownCallback		6	/* shell, simpleMenu */
#define	CtpopupCallback			7	/* shell, simpleMenu */
#define	CtreportCallback		8	/* panner, porthole, viewport */
#define	CtstartCallback			9	/* repeater */
#define	CtstopCallback			10	/* repeater */

struct _obmCallback {
	union {
	    ObmObject obj;
	    ObmFunc fcn;
	} u;
	int callback_type;
	XtPointer client_data;
	struct _obmCallback *next;
	char name[SZ_NAME];
};

typedef struct _obmCallback obmCallback;
typedef struct _obmCallback *ObmCallback;

/* Object manager global context. */
struct obmContext {
	char appname[SZ_NAME];		/* application name */
	char appclass[SZ_NAME];		/* application class */
	int argc;			/* args for popup shell */
	char **argv;			/* args for popup shell */
	XtAppContext app_context;	/* application context */
	Display *display;		/* Obm private display connection */
	Screen *screen;			/* Obm private display connection */
	Widget toplevel;		/* toplevel shell of application */
	Tcl_Interp *tcl;		/* global or server interpreter */
	ObmObject head;			/* head of object list */
	ObmObject tail;			/* tail of object list */
	ObmObject objindex[SZ_INDEX];	/* hash index */
	ObjList pixmap_cache;		/* cached pixmaps */
	ObjList cursor_cache;		/* cached cursors */
	ObjList menu_list;		/* list of menu descriptors */
	ObmCallback callback_list;	/* list of callback descriptors */
	Boolean being_destroyed;	/* set by Destroy method */
	Boolean specified;		/* UI has been specified */
	Boolean activated;		/* UI has been activated */
	Boolean mapped;			/* toplevel is mapped */

	int  debug;			/* print debug messages */
	char *debug_objs;		/* debug objects, NULL=>all objs */
};

typedef	struct obmContext *ObmContext;

#ifdef Obm_Main
ObmContext global_obm_handle;	/* only works if single obm */
#else
extern	ObmContext global_obm_handle;
#endif

/* Useful macros. */
#define	abs(a)		(((a)<0)?(-(a)):(a))
#define max(a,b)	((a)>=(b)?(a):(b))
#define min(a,b)	((a)<(b)?(a):(b))
#define ERR		(-1)
#define OK		0
#define SQR(a)		((a)*(a))

#ifndef AIXV3
#ifndef OSF1
typedef unsigned char	uchar;
#endif
#endif

/* The following are the string values returned to Tcl for boolean values. */
#define	TRUESTR		"1"
#define	FALSESTR	"0"

extern	ObmObject obmFindObject();
extern	ObjClassRec obmGetClassrec();
extern	Widget widgetGetPointer();
extern	ObmCallback obmAddCallback();
extern	void widget_setTTName();
extern	char *widget_getTTName();
extern	void obmRemoveCallback();
extern	void obmNewObject();
extern	void obmFreeObject();
extern	void obmDestroyObject();
extern	void obmGenericClassDestroy();
extern	void obmDisplay(), obmUndisplay();
extern  void freeMenu();
extern  void freeIcon();
extern	Pixmap findBitmap();
extern	Pixmap findPixmap();
extern	Cursor findCursor();
extern	Icon *findIcon();
extern	char *get_geometry();

/* Public functions. */
#define Obm_Private
#include "Obm.h"
#undef Obm_Private

extern  char *getenv();
