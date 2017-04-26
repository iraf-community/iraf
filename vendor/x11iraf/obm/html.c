/* Copyright(c) 1993 Association of Universities for Research in Astronomy Inc.
 */

#include <ObmP.h>
#include "widget.h"

/*
 * HTML widget class (a subclass of Widget).
 * -------------------------------------------------------------------------
 * The HTML (hypertext markup language) widget displays a block of HTML
 * formatted text, the "document" to be displayed.  The text consists of a
 * mixture of text to be displayed and embedded formatting directives.  The
 * text may also contain "hot links" pointing to other HTML-formatted
 * documents.
 *
 *  	        setText text [target [header_text [footer_text]]]
 *       text = getText [format [font]]
 *	  retestAnchors
 *
 *    id = positionToId x y
 *         idToPosition id x y
 *     anchorToPosition name x y
 *      id = anchorToId name
 *		 gotoId id
 *
 *         n = getHRefs list
 *     n = getImageSrcs list
 *         n = getLinks list
 *
 *         setSelection start end
 *  text = getSelection start end
 *	 clearSelection
 *
 * 	     searchText pattern start end [direction [search_type]]
 *
 * 	    addCallback procedure-name [callback-type]
 * 	 deleteCallback procedure-name [callback-type]
 *
 * The possible callback types and their callback arguments are as follows.
 * 
 *	anchor		widget cbtype event text href element_id
 *	testAnchor	widget cbtype href
 *	submitForm	widget cbtype event attrs href method enctype encentity
 *	link		widget cbtype href role
 *	pointerMotion	widget cbtype href
 *
 * See the comments below for further details on the callback types and their
 * arguments.
 *
 * All a "hot link" is to the HTML widget is a document object containing a
 * HREF which causes a callback when selected by the user viewing the document.
 * It is up to the application using the HTML widget to define what the meaning 
 * of an HREF is.
 *
 * This version of the HTML widget binding does not yet support inline images.
 */

#define	CB_Anchor		1
#define	CB_TestAnchor		2
#define	CB_PointerMotion	3
#define	CB_SubmitForm		4
#define	CB_Link			5

/* HTML class instance descriptor. */
struct htmlPrivate {
	ObmCallback callback_list;
};

typedef struct htmlPrivate *HTMLPrivate;

struct htmlObject {
	struct obmObjectCore core;
	struct widgetPrivate widget;
	struct htmlPrivate html;
};

typedef struct htmlObject *HTMLObject;

/* HTML class class record private data. */
typedef struct {
	/* standard MsgContext fields. */
	Tcl_Interp *tcl;                /* class interpreter */
	ObmObject object[MAX_LEVELS];   /* object which received last message */
	int level;

	/* HTML specific fields. */
	/* (none) */
} htmlClassData, *HTMLClassData;


void HTMLDestroy();
void HTMLClassDestroy();
ObmObject HTMLCreate();

static int htmlSetText(), htmlGetText(), htmlGetHRefs();
static int htmlGetImageSrcs(), htmlGetLinks();
static int htmlRetestAnchors(), htmlPositionToId(), htmlIdToPosition();
static int htmlAnchorToPosition(), htmlAnchorToId();
static int htmlGotoId(), htmlAddCallback(), htmlDeleteCallback();
static int htmlSetSelection(), htmlGetSelection(), htmlClearSelection();
static int htmlSearchText();

static void anchorCallback(), pointerMotionCallback();
static void submitFormCallback(), linkCallback();
static char *cb_encode(), *makeList();
static int testAnchorCallback();
static void cb_error();
static int cb_decode();
extern long strtol();


/* HTMLClassInit -- Initialize the class record for the HTML widget class.
 */
void
HTMLClassInit (obm, classrec)
ObmContext obm;
register ObjClassRec classrec;
{
	register HTMLClassData gcd;
	register Tcl_Interp *tcl;
	register ClientData c_gcd;

	/* Install the class methods. */
	classrec->ClassDestroy = HTMLClassDestroy;
	classrec->Create = (ObmFunc) HTMLCreate;
	classrec->Destroy = HTMLDestroy;
	classrec->Evaluate = WidgetEvaluate;

	/* The HTML widget subclass has its own command set hence has its
	 * own interpreter.  The widget will respond both to all the commands
	 * defined here, and to all the commands implemented by the base
	 * Widget class.
	 */
	if (!classrec->class_data) {
	    gcd = (HTMLClassData) XtCalloc (1, sizeof (htmlClassData));
	    gcd->tcl = tcl = Tcl_CreateInterp();
	    classrec->class_data = (XtPointer) gcd;
	    c_gcd = (ClientData)gcd;
	    gcd->level = 0;

	    Tcl_CreateCommand (tcl,
		"addCallback", htmlAddCallback, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"deleteCallback", htmlDeleteCallback, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setText", htmlSetText, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"getText", htmlGetText, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"positionToId", htmlPositionToId, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"idToPosition", htmlIdToPosition, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"anchorToPosition", htmlAnchorToPosition, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"anchorToId", htmlAnchorToId, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"gotoId", htmlGotoId, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"getHRefs", htmlGetHRefs, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"getImageSrcs", htmlGetImageSrcs, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"getLinks", htmlGetLinks, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"retestAnchors", htmlRetestAnchors, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setSelection", htmlSetSelection, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"getSelection", htmlGetSelection, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"clearSelection", htmlClearSelection, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"searchText", htmlSearchText, c_gcd, NULL);
	}
}


/* HTMLClassDestroy -- Custom destroy procedure for the HTML class.
 */
void
HTMLClassDestroy (obm, classrec)
ObmContext obm;
register ObjClassRec classrec;
{
	register HTMLClassData gcd = (HTMLClassData) classrec->class_data;

	if (gcd) {
	    if (gcd->tcl)
		Tcl_DeleteInterp (gcd->tcl);
	    XtFree ((char *)gcd);
	    classrec->class_data = NULL;
	}
}


/* HTMLCreate -- Create an instance of a HTML object.
 */
ObmObject
HTMLCreate (obm, name, classrec, parent, a_args, a_nargs)
ObmContext obm;
char *name;
ObjClassRec classrec;
char *parent;
ArgList a_args;
int a_nargs;
{
	register HTMLObject obj;
	register Widget w;
	Arg args[128];
	int nargs = 0;

	for (nargs = 0;  nargs < a_nargs;  nargs++)
	    args[nargs] = a_args[nargs];

	XtSetArg (args[nargs], WbNpreviouslyVisitedTestFunction,
	    (long)testAnchorCallback);		nargs++;
	XtSetArg (args[nargs], WbNpointerMotionCallback,
	    (long)pointerMotionCallback);	nargs++;

	obj = (HTMLObject) WidgetCreate (obm, name,classrec,parent,args,nargs);
	if (obj == NULL)
	    return (NULL);
	obj = (HTMLObject) XtRealloc ((char *)obj, sizeof(struct htmlObject));
	if (obj == NULL)
	    return (NULL);

	w = obj->widget.w;
	/* register_image_resolution_function (w); */
	XtAddCallback (w, WbNanchorCallback, anchorCallback, (XtPointer)obj);
	XtAddCallback (w, WbNlinkCallback, linkCallback, (XtPointer)obj);
	XtAddCallback (w, WbNsubmitFormCallback, submitFormCallback,
	    (XtPointer)obj);

	XtSetArg (args[0], WbNpreviouslyVisitedTestData, obj);
	XtSetArg (args[1], WbNpointerMotionData, obj);
	XtSetValues (w, args, 2);

	/* Initialize HTMLPrivate instance structure. */
	obj->html.callback_list = NULL;

	return ((ObmObject) obj);
}


/* HTMLDestroy -- Destroy an instance of a HTML object.
 */
void
HTMLDestroy (object)
ObmObject object;
{
	HTMLObject obj = (HTMLObject) object;
	ObjClassRec classrec = obj->core.classrec;
	register HTMLClassData gcd = (HTMLClassData) classrec->class_data;
	register ObmCallback cb, cb_next;
	ObmContext obm = obj->widget.obm;
	Widget w = obj->widget.w;

	/* Destroy the object in the second final call to Destroy. */
	if (!obj->core.being_destroyed++)
	    return;

	/* Free any HTML callback descriptors. */
	for (cb = obj->html.callback_list;  cb;  cb = cb_next) {
	    cb_next = cb->next;

	    XtFree ((char *)cb);
	}

	WidgetDestroy (object);
}


/*
 * HTML class functions.
 * -----------------------
 */


/* setText -- Set the text to be displayed in the HTML widget.
 *
 *  Usage:	setText text [target [header_text [footer_text]]]
 *
 * If a target anchor is given the text will be positioned to view the
 * given anchor.  The target anchor may be specified either by name or by
 * its element_id (tag number within the document, e.g. as returned in the
 * anchor callback).  If any HTML-formatted header or footer text is given
 * this will be displayed before or after the document passed in as "text".
 */
static int 
htmlSetText (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	register WidgetPrivate wp = &obj->widget;
	ObmContext obm = wp->obm;
	char *text, *target_anchor;
	char *header_text, *footer_text;
	int element_id;

	text = (argc > 1) ? argv[1] : NULL;
	target_anchor = (argc > 2) ? argv[2] : NULL;
	header_text = (argc > 3) ? argv[3] : NULL;
	footer_text = (argc > 4) ? argv[4] : NULL;
	element_id = target_anchor ? atoi (target_anchor) : 0;

	HTMLSetText (wp->w, text,
	    header_text, footer_text, element_id, target_anchor, NULL);

	return (TCL_OK);
}


/* getText -- Get the text of the document currently being displayed.
 *
 *  Usage:	text = getText [format [font]]
 *
 * The optional format argument determines the type of text to be returned.
 * The possible values are as follows.
 *
 *	simple		No formatting other than indents.
 *	pretty		Simple formatting.
 *	postscript	Return formatted Postscript.	
 *
 * The default output format is simple.  If Postscript output is selected
 * the font can be selected from one of the following:
 *
 *	times		Times
 *	helvetica	Helvetica
 *	schoolbook	New century schoolbook
 *	lucida		Lucida Bright
 *
 * The default Postscript font is Times.
 */
static int 
htmlGetText (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	register WidgetPrivate wp = &obj->widget;
	char *text, *format, *font;
	int pretty = 0;

	format = (argc > 1) ? argv[1] : "simple";
	font   = (argc > 2) ? argv[2] : "times";

	if (strcmp (format, "simple") == 0)
	    pretty = 0;
	else if (strcmp (format, "pretty") == 0)
	    pretty = 1;
	else if (strcmp (format, "postscript") == 0) {
	    if (strcmp (font, "times") == 0)
		pretty = 2;
	    else if (strcmp (font, "helvetica") == 0)
		pretty = 3;
	    else if (strcmp (font, "schoolbook") == 0)
		pretty = 4;
	    else if (strcmp (font, "lucida") == 0)
		pretty = 5;
	    else
		pretty = 2;
	}

	if (text = HTMLGetText (wp->w, pretty)) {
	    Tcl_SetResult (wp->obm->tcl, text, TCL_VOLATILE);
	    free (text);
	}

	return (TCL_OK);
}


/* positionToId -- Return the element id of the HTML element nearest to the
 * given position x,y.
 *
 *  Usage:	id = positionToId x y
 *
 * If there is no element at the given position the first element in the
 * current line is returned.  If we are not positioned to a line, either the
 * beginning or the end of the document is returned.
 */
static int 
htmlPositionToId (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	register WidgetPrivate wp = &obj->widget;
	int element_id, x, y;
	char buf[SZ_NUMBER];

	if (argc < 3)
	    return (TCL_ERROR);

	x = atoi (argv[1]);
	y = atoi (argv[2]);
	element_id = HTMLPositionToId (wp->w, x, y);

	sprintf (buf, "%d", element_id);
	Tcl_SetResult (wp->obm->tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* idToPosition -- Return the position of an HTML element given its
 * element id.
 *
 *  Usage:	idToPosition id x y
 *
 * If there is no element with the given element id false is returned and
 * the coordinates x,y are undefined.
 */
static int 
htmlIdToPosition (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	register WidgetPrivate wp = &obj->widget;
	ObmContext obm = obj->widget.obm;
	int status, element_id, x, y;
	char buf[SZ_NUMBER];
	char *s_x, *s_y;

	if (argc < 2)
	    return (TCL_ERROR);

	element_id = atoi (argv[1]);
	s_x = (argc > 2) ? argv[2] : NULL;
	s_y = (argc > 3) ? argv[3] : NULL;
	status = HTMLIdToPosition (wp->w, element_id, &x, &y);

	if (status < 0)
	    Tcl_SetResult (obm->tcl, FALSESTR, TCL_STATIC);
	else {
	    if (s_x) {
		sprintf (buf, "%d", x);
		if ((Tcl_SetVar (obm->tcl, s_x, buf, 0)) == NULL) /* MF024 */
		    return (TCL_ERROR);
	    }
	    if (s_y) {
		sprintf (buf, "%d", y);
		if ((Tcl_SetVar (obm->tcl, s_y, buf, 0)) == NULL) /* MF024 */
		    return (TCL_ERROR);
	    }
	    Tcl_SetResult (obm->tcl, TRUESTR, TCL_STATIC);
	}

	return (TCL_OK);
}


/* anchorToPosition -- Return the position of the named anchor.
 *
 *  Usage:	bool = anchorToPosition anchor [x y]
 *
 * If there is no anchor with the given name false is returned and the
 * coordinates x,y are undefined.
 */
static int 
htmlAnchorToPosition (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	register WidgetPrivate wp = &obj->widget;
	ObmContext obm = obj->widget.obm;
	char *anchor, *s_x, *s_y;
	char buf[SZ_NUMBER];
	int status, x, y;

	if (argc < 2)
	    return (TCL_ERROR);

	anchor = argv[1];
	s_x = (argc > 2) ? argv[2] : NULL;
	s_y = (argc > 3) ? argv[3] : NULL;
	status = HTMLAnchorToPosition (wp->w, anchor, &x, &y);

	if (status < 0)
	    Tcl_SetResult (obm->tcl, FALSESTR, TCL_STATIC);
	else {
	    if (s_x) {
		sprintf (buf, "%d", x);
		if ((Tcl_SetVar (obm->tcl, s_x, buf, 0)) == NULL) /* MF024 */
		    return (TCL_ERROR);
	    }
	    if (s_y) {
		sprintf (buf, "%d", y);
		if ((Tcl_SetVar (obm->tcl, s_y, buf, 0)) == NULL) /* MF024 */
		    return (TCL_ERROR);
	    }
	    Tcl_SetResult (obm->tcl, TRUESTR, TCL_STATIC);
	}

	return (TCL_OK);
}


/* anchorToId -- Return the element id of the named anchor.
 *
 *  Usage:	id = anchorToId anchor
 *
 * If there is no anchor with the given name false is returned.
 */
static int 
htmlAnchorToId (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	register WidgetPrivate wp = &obj->widget;
	char buf[SZ_NUMBER];
	int element_id;
	char *anchor;

	if (argc < 2)
	    return (TCL_ERROR);

	anchor = argv[1];
	element_id = HTMLAnchorToId (wp->w, anchor);

	sprintf (buf, "%d", element_id);
	Tcl_SetResult (wp->obm->tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* gotoId -- Position to the given element given its element id.
 *
 *  Usage:	gotoId element_id
 *
 * An id of zero means go to the top of the document.
 */
static int 
htmlGotoId (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	register WidgetPrivate wp = &obj->widget;
	int element_id;

	if (argc < 2)
	    return (TCL_ERROR);

	element_id = atoi (argv[1]);
	HTMLGotoId (wp->w, element_id);

	return (TCL_OK);
}


/* getHRefs -- Get a list of the HREFs of all the active anchors in the
 * document being displayed.
 *
 *  Usage:	n = getHRefs list
 *
 * The number of HREFs is returned as the function value; zero is returned
 * if there are no HREFs, in which case "list" is undefined.  If the document
 * has HREFs on output list will contain a list of HREFs in the form { {HREF1}
 * {HREF2} ... {HREFn} }.
 *
 * An HREF is a hypertext reference, i.e. hot-link or hypertext link to
 * some other hypertext document that can be referenced by clicking on an
 * anchor in the document being displayed.
 */
static int 
htmlGetHRefs (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	WidgetPrivate wp = &obj->widget;
	ObmContext obm = obj->widget.obm;
	char *a_list, *lbuf, **list;
	char buf[SZ_NUMBER];
	int n;

	if (argc < 2)
	    return (TCL_ERROR);
	else
	    a_list = argv[1];

	if (list = HTMLGetHRefs (wp->w, &n)) {
	    if (!(lbuf = makeList (list, n))) {
		free ((char *)list);
		return (TCL_ERROR);
	    }

	    if ((Tcl_SetVar (obm->tcl, a_list, lbuf, 0)) == NULL) { /* MF024 */
		free ((char *)list);
		XtFree (lbuf);
		return (TCL_ERROR);
	    }

	    free ((char *)list);
	    XtFree (lbuf);
	} else
	    n = 0;

	sprintf (buf, "%d", n);
	Tcl_SetResult (wp->obm->tcl, buf, TCL_VOLATILE);
	return (TCL_OK);
}


/* getImageSrcs -- Get a list of the image sources (SRC=) for all the 
 * inline images referenced by the document being displayed.
 *
 *  Usage:	n = getImageSrcs list
 *
 * The number of SRCs is returned as the function value; zero is returned
 * if there are no SRCs, in which case "list" is undefined.  If the document
 * has SRCs on output the list will contain a list of SRCs in the form
 * { {SRC1} {SRC2} ... {SRCn} }.
 *
 * A SRC is a HREF pointing to an image file.
 */
static int 
htmlGetImageSrcs (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	WidgetPrivate wp = &obj->widget;
	ObmContext obm = obj->widget.obm;
	char *a_list, *lbuf, **list;
	char buf[SZ_NUMBER];
	int n;

	if (argc < 2)
	    return (TCL_ERROR);
	else
	    a_list = argv[1];

	if (list = HTMLGetImageSrcs (wp->w, &n)) {
	    if (!(lbuf = makeList (list, n))) {
		free ((char *)list);
		return (TCL_ERROR);
	    }

	    if ((Tcl_SetVar (obm->tcl, a_list, lbuf, 0)) == NULL) { /* MF024 */
		free ((char *)list);
		XtFree (lbuf);
		return (TCL_ERROR);
	    }

	    free ((char *)list);
	    XtFree (lbuf);
	} else
	    n = 0;

	sprintf (buf, "%d", n);
	Tcl_SetResult (wp->obm->tcl, buf, TCL_VOLATILE);
	return (TCL_OK);
}


/* getLinks -- Get a list of the link tags (<LINK>) referenced by the
 * document being displayed.
 *
 *  Usage:	n = getLinks list
 *
 * The number of links is returned as the function value; zero is returned
 * if there are no links, in which case "list" is undefined.  If there are
 * any links the returned list will have the format { {{href} {role}} ...}
 * where the structure {{href} {role}} describes each link.
 */
static int 
htmlGetLinks (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	WidgetPrivate wp = &obj->widget;
	ObmContext obm = obj->widget.obm;
	char *a_list, *lbuf;
	char buf[SZ_NUMBER];
	LinkInfo *list;
	int nchars, n;

	if (argc < 2)
	    return (TCL_ERROR);
	else
	    a_list = argv[1];

	if (list = HTMLGetLinks (wp->w, &n)) {
	    register char *ip, *op;
	    register int i;

	    /* Determine how much storage we need for the list. */
	    for (i=0, nchars=0;  i < n;  i++) {
		nchars += strlen (list[i].href) + 4;
		nchars += strlen (list[i].role) + 4;
	    }
	    nchars += 5;

	    /* Get the storage. */
	    if (!(lbuf = op = XtMalloc (nchars))) {
		free ((char *)list);
		return (TCL_ERROR);
	    }

	    /* Encode the list as a Tcl list of lists. */
	    *op++ = '{';  *op++ = ' ';
	    for (i=0;  i < n;  i++) {
		*op++ = '{';
		*op++ = '{';
		for (ip=list[i].href;  ip && *ip;  )
		    *op++ = *ip++;
		*op++ = '}';  *op++ = ' ';
		*op++ = '{';
		for (ip=list[i].role;  ip && *ip;  )
		    *op++ = *ip++;
		*op++ = '}';
		*op++ = '}';  *op++ = ' ';
	    }
	    *op++ = '}';
	    *op++ = '\0';

	    if ((Tcl_SetVar (obm->tcl, a_list, lbuf, 0)) == NULL) { /* MF024 */
		free ((char *)list);
		XtFree (lbuf);
		return (TCL_ERROR);
	    }

	    free ((char *)list);
	    XtFree (lbuf);
	} else
	    n = 0;

	sprintf (buf, "%d", n);
	Tcl_SetResult (wp->obm->tcl, buf, TCL_VOLATILE);
	return (TCL_OK);
}


/* makeList -- Take a list of NULL terminated strings and turn it into a
 * Tcl list of strings.
 */
static char *
makeList (list, n)
char **list;
int n;
{
	register char *ip, *op;
	register int i;
	int nchars;
	char *buf;

	/* Determine how much storage we need for the list. */
	for (i=0, nchars=0;  i < n;  i++)
	    nchars += strlen (list[i]) + 4;
	nchars += 5;

	/* Get the storage. */
	if (!(buf = op = XtMalloc (nchars))) {
	    free ((char *)list);
	    return (NULL);
	}

	/* Encode the list as a Tcl list of strings. */
	*op++ = '{';  *op++ = ' ';
	for (i=0;  i < n;  i++) {
	    *op++ = '{';
	    for (ip=list[i];  ip && *ip;  )
		*op++ = *ip++;
	    *op++ = '}';  *op++ = ' ';
	}
	*op++ = '}';
	*op++ = '\0';

	return (buf);
}


/* retestAnchors -- Test each anchor and update the display to indicate
 * the current status of the anchor.
 *
 *  Usage:	retestAnchors
 *
 * retestAnchors should be called after loading new text into a widget,
 * or when the status of one or more anchors has changed, e.g. after a
 * given URL has been visited.
 */
static int 
htmlRetestAnchors (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	register WidgetPrivate wp = &obj->widget;

	HTMLRetestAnchors (wp->w, NULL, 0);
	return (TCL_OK);
}


/* setSelection -- Set the current text selection to the text bracketed by
 * the input start and end element refs.
 *
 *  Usage:	setSelection start end
 *
 * "start" and "end" are elements refs such as returned by searchText.
 * An element ref is a structure of the form {element_id offset} specifying
 * the element id within the document, and the character offset within
 * that element.
 */
static int 
htmlSetSelection (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	register WidgetPrivate wp = &obj->widget;
	ElementRef start, end;
	char *ip = (char *)NULL;

	if (argc < 3)
	    return (TCL_ERROR);

	ip = (char *)NULL;
	start.id  = strtol (&argv[1][1], &ip, 0);
	start.pos = strtol (ip, &ip, 0);
	if (ip == argv[1])
	    return (TCL_ERROR);

	ip = (char *)NULL;
	end.id  = strtol (&argv[2][1], &ip, 0);
	end.pos = strtol (ip, &ip, 0);
	if (ip == argv[2])
	    return (TCL_ERROR);

	HTMLSetSelection (wp->w, &start, &end);
	return (TCL_OK);
}


/* getSelection -- Get the selected text, if any.
 *
 *  Usage:	text = getSelection
 *
 * An empty string is returned if there is no current text selection.
 */
static int 
htmlGetSelection (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	register WidgetPrivate wp = &obj->widget;
	char *text, *start, *end, *insert;

	text = HTMLGetTextAndSelection (wp->w, &start, &end, &insert);
	if (text && start) {
	    int nchars = end - start + 1;
	    start[nchars] = '\0';
	    Tcl_SetResult (wp->obm->tcl, start, TCL_VOLATILE);
	}

	if (text)
	    free (text);
	return (TCL_OK);
}


/* clearSelection -- Clear the current selection, if any.
 *
 *  Usage:	clearSelection
 */
static int 
htmlClearSelection (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	register WidgetPrivate wp = &obj->widget;

	HTMLClearSelection (wp->w);
	return (TCL_OK);
}


/* searchText -- Search the document for the given pattern.
 *
 *  Usage:	bool = searchText pattern start end [direction [search_type]]
 *
 *	direction	"forward" or "backward"
 *	search_type	"caseSensitive" or "caseless"
 *
 * If the search is successful start and end are set to the element refs
 * of the matched region and the function returns a true (nonzero) value.
 * False is returned if the search fails.  An element ref is a structure of
 * the form {element_id offset} specifying the element id within the document,
 * and the character offset within that element.  The search will automatically
 * wrap around the page if not found initially.
 *
 */
static int 
htmlSearchText (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register HTMLClassData gcd = (HTMLClassData) msg;
	HTMLObject obj = (HTMLObject) gcd->object[gcd->level];
	register WidgetPrivate wp = &obj->widget;
	ObmContext obm = obj->widget.obm;
	char *pattern, *a_start, *a_end;
	int backward = 0, caseless = 1;
	static ElementRef start, end;
  	static char patstr[64];
	int status, again = 1;

	if (argc < 4)
	    return (TCL_ERROR);

	pattern = argv[1];
	a_start = argv[2];
	a_end = argv[3];
	backward = 0;
	if (argc > 4)
	    backward = (strcmp (argv[4], "backward") == 0);
	caseless = 0;
	if (argc > 5)
	    caseless = (strcmp (argv[5], "caseless") == 0);

	/* See whether the pattern has changed and we need to reset the
	 * start and end element refs.
	 */
	if (strcmp (pattern, patstr) != 0) {
retry: 	    start.id = start.pos = 0;
	    end.id = end.pos = 0;
	    strcpy (patstr, "");
	    again = 0;
	}

	/* Do the search. */
	status = HTMLSearchText (wp->w, pattern,
	    &start, &end, backward, caseless);

	if (status == 1) {
	    char buf[SZ_LINE];
	    sprintf (buf, "{%d %d}", start.id, start.pos);
	    if ((Tcl_SetVar (obm->tcl, a_start, buf, 0)) == NULL) /* MF024 */
		return (TCL_ERROR);
	    sprintf (buf, "{%d %d}", end.id, end.pos);
	    if ((Tcl_SetVar (obm->tcl, a_end, buf, 0)) == NULL)   /* MF024 */
		return (TCL_ERROR);
	    Tcl_SetResult (wp->obm->tcl, TRUESTR, TCL_STATIC);
	} else {
	    if (again == 1)
		goto retry;
	    Tcl_SetResult (wp->obm->tcl, FALSESTR, TCL_STATIC);
	}

	/* Save the pattern string so we can reset later if it changes. */
	strcpy (patstr, pattern);

	return (TCL_OK);
}


/* AddCallback -- Post a callback for a HTML widget event.
 *
 * Usage:	addCallback procedure-name [callback-type]
 *
 * The recognized HTML callbacks are
 *
 *	anchor			Called to load a new URL.
 *
 *	testAnchor		Called to test whether a given URL has
 *				been previously visited.
 *
 *	pointerMotion		Called when the pointer enters an object
 *				which has a URL.
 *
 *	submitForm		Called when a form is submitted from within
 *				the document being viewed.
 *
 *	link			Called when a <LINK> tag is encountered
 *				while loading text.
 *
 * The default callback type is "anchor", which is called when the user
 * selects a new URL while viewing a document.
 */
static int 
htmlAddCallback (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	HTMLObject obj = (HTMLObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register HTMLPrivate hp = &obj->html;
	register Widget w = wp->w;
	char *userproc, *callback_type;
	ObmCallback cb, new;
	int type;

	if (argc < 2)
	    return (TCL_ERROR);

	userproc = argv[1];
	callback_type = (argc > 2) ? argv[2] : "anchor";
	if (!(type = cb_decode (callback_type)))
	    return (TCL_ERROR);

	/* Initialize callback descriptor. */
	new = (ObmCallback) XtCalloc (1, sizeof (obmCallback));
	new->u.obj = (ObmObject) obj;
	new->callback_type = type;
	strncpy (new->name, userproc, SZ_NAME);

	/* Append descriptor to callback list for widget. */
	for (cb = hp->callback_list;  cb && cb->next;  cb = cb->next)
	    ;
	if (cb)
	    cb->next = new;
	else
	    hp->callback_list = new;

	return (TCL_OK);
}


/* DeleteCallback -- Delete a HTML callback.
 *
 *  Usage:	deleteCallback procedure [callback_type]
 *
 * If a callback type is specified all entries of type callback_type for
 * the named procedure are deleted, else all entries of any type for the
 * named procedure are deleted.
 */
static int 
htmlDeleteCallback (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	HTMLObject obj = (HTMLObject) msg->object[msg->level];
	register HTMLPrivate hp = &obj->html;
	register ObmCallback cb, prev, next;
	char *procedure, *callback_type;
	int type;

	if (argc < 2)
	    return (TCL_ERROR);

	procedure = argv[1];
	callback_type = (argc > 2) ? argv[2] : NULL;
	type = callback_type ? cb_decode(callback_type) : 0;

	/* Locate and delete procedure entry in callback list. */
	for (prev=NULL, cb=hp->callback_list;  cb;  prev=cb, cb=next) {
	    next = cb->next;
	    if (strcmp (cb->name, procedure) == 0 &&
		    (!type || cb->callback_type == type)) {
		if (prev)
		    prev->next = next;
		else
		    hp->callback_list = next;
		XtFree ((char *)cb);
	    }
	}

	return (TCL_OK);
}


/* anchorCallback -- Callback procedure called by the HTML widget when an
 * anchor (URL) is selected.
 *
 * Calling sequence:
 *
 *	userproc widget cbtype event text href element_id
 *
 * All callbacks registered with the current widget for the anchor callback
 * are called in the order in which they were registered.
 */
static void
anchorCallback (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
	register char *ip, *op;
	register ObmCallback cb;
	register WbAnchorCallbackData *ap = (WbAnchorCallbackData *)call_data;
	HTMLObject obj = (HTMLObject) client_data;
	ObmContext obm = obj->widget.obm;
	char *text, *href, *none = "none";
	char event_type[SZ_LINE];
	char element_id[SZ_NUMBER];
	int status;

	text = ap->text ? ap->text : none;
	href = ap->href ? ap->href : none;
	sprintf (element_id, "%d", ap->element_id);
	op = event_type;

	/* Compose the event type information.  This is the name of the
	 * key typed, or "Button1", "Button2", etc. for the mouse buttons.
	 */
	switch (ap->event->type) {
	case KeyPress:
	case KeyRelease:
	    {	XKeyPressedEvent *ev = (XKeyPressedEvent *) ap->event;
		char buf[20];
		int n;

		if ((n = XLookupString(ev,buf,sizeof(buf),NULL,NULL)) > 0) {
		    for (ip=buf;  --n >= 0;  )
			if (*ip <= ' ') {
			    *op++ = '^';
			    *op++ = *ip++ + 'A' - 1;
			} else if (isprint (*ip)) {
			    *op++ = *ip++;
			} else
			    ip++;
		} else {
		    /* This case occurs when only a modifier is typed. */
		    for (ip = "??";  *op++ = *ip++; )
			;
		}
		*op = '\0';
	    }
	    break;

	case ButtonPress:
	case ButtonRelease:
	    {	XButtonPressedEvent *ev = (XButtonPressedEvent *) ap->event;
		sprintf (op, "Button%d", ev->button);
	    }
	    break;
	default:
	    strcpy (event_type, "unknown");
	}

	/* Call any registered callback functions. */
	for (cb = obj->html.callback_list;  cb;  cb = cb->next) {
	    if (cb->callback_type != CB_Anchor)
		continue;

	    status = Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		cb_encode(cb->callback_type), " ",
		event_type, " ",
		"{", text, "} ",
		"{", href, "} ",
		element_id,
		NULL);

	    if (status != TCL_OK)
		cb_error (obm, cb);
	}
}


/* testAnchorCallback -- Callback procedure called by the HTML widget to
 * test whether a given anchor (URL) has been previously visited.
 *
 * Calling sequence:
 *
 *	userproc widget cbtype href
 *
 * A nonzero value should be returned by the userproc if the given anchor
 * has been visited previously, otherwise a zero should be returned.
 */
static int
testAnchorCallback (w, client_data, href)
Widget w;
XtPointer client_data;
char *href;
{
	register ObmCallback cb;
	register HTMLObject obj = (HTMLObject) client_data;
	register ObmContext obm = obj->widget.obm;
	int status, retval = 0;

	/* Call any registered callback functions. */
	for (cb = obj->html.callback_list;  cb;  cb = cb->next) {
	    if (cb->callback_type != CB_TestAnchor)
		continue;

	    status = Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		cb_encode(cb->callback_type), " ",
		"{", href, "} ",
		NULL);

	    if (status != TCL_OK)
		cb_error (obm, cb);
	    else if (atoi (obm->tcl->result))
		retval = 1;
	}

	return (retval);
}


/* submitFormCallback -- Callback procedure called by the HTML widget when
 * a form is submitted from the document.
 *
 * Calling sequence:
 *
 *	userproc widget cbtype event attrs href method enctype encentity
 *
 * "widget" is the name of the HTML widget which generated the callback.
 * "cbtype" is the type of callback, i.e., "submitForm".  "event" is the
 * key/button event which triggered the callback, e.g. "Button1".
 *
 * "attrs" is a list of attribute-value pairs defining the contents of the
 * form.  That is, a list of the form { {attr1 value1} {attr2 value2} ... }.
 * 
 * The final block of arguments deal with how to process or deliver the form.
 * "href", "method", "enctype", and "encentity" are strings defined by the
 * HTML form.  The application is free to use these as it wishes, except for
 * an HTML form query where the meaning of these fields is well defined.
 * The "href" field is normally the URL to which the form is to be submitted,
 * while "method" is the method to be used to submit the form.
 */
static void
submitFormCallback (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
	register char *ip, *op;
	register ObmCallback cb;
	register WbFormCallbackData *fp = (WbFormCallbackData *)call_data;
	HTMLObject obj = (HTMLObject) client_data;
	ObmContext obm = obj->widget.obm;

	char *href, *method, *enctype, *encentity;
	char *abuf, event_type[SZ_LINE];
	char *none = "none";
	int status, i, n;

	href = fp->href ? fp->href : none;
	method = fp->method ? fp->method : none;
	enctype = fp->enctype ? fp->enctype : none;
	encentity = fp->enc_entity ? fp->enc_entity : none;
	op = event_type;

	/* Compose the event type information.  This is the name of the
	 * key typed, or "Button1", "Button2", etc. for the mouse buttons.
	 */
	switch (fp->event->type) {
	case KeyPress:
	case KeyRelease:
	    {	XKeyPressedEvent *ev = (XKeyPressedEvent *) fp->event;
		char buf[20];
		int n;

		if ((n = XLookupString(ev,buf,sizeof(buf),NULL,NULL)) > 0) {
		    for (ip=buf;  --n >= 0;  )
			if (*ip <= ' ') {
			    *op++ = '^';
			    *op++ = *ip++ + 'A' - 1;
			} else if (isprint (*ip)) {
			    *op++ = *ip++;
			} else
			    ip++;
		} else {
		    /* This case occurs when only a modifier is typed. */
		    for (ip = "??";  *op++ = *ip++; )
			;
		}
		*op = '\0';
	    }
	    break;

	case ButtonPress:
	case ButtonRelease:
	    {	XButtonPressedEvent *ev = (XButtonPressedEvent *) fp->event;
		sprintf (op, "Button%d", ev->button);
	    }
	    break;
	default:
	    strcpy (event_type, "unknown");
	}

	/* Get storage for the attribute list. */
	for (i=0, n=0;  i < fp->attribute_count;  i++) {
	    n += strlen (fp->attribute_names[i]);
	    n += strlen (fp->attribute_values[i]);
	    n += 10;
	}
	if (!(abuf = XtMalloc (n)))
	    return;

	/* Construct the attribute list.
	 */
	for (i=0, op=abuf;  i < fp->attribute_count;  i++) {
	    *op++ = '{';

	    *op++ = '{';
	    for (ip = fp->attribute_names[i];  ip && *ip;  )
		*op++ = *ip++;
	    *op++ = '}';  *op++ = ' ';

	    *op++ = '{';
	    for (ip = fp->attribute_values[i];  ip && *ip;  )
		*op++ = *ip++;
	    *op++ = '}';

	    *op++ = '}';  *op++ = ' ';
	}
	*op = '\0';

	/* Call any registered callback functions. */
	for (cb = obj->html.callback_list;  cb;  cb = cb->next) {
	    if (cb->callback_type != CB_SubmitForm)
		continue;

	    status = Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		cb_encode(cb->callback_type), " ",
		event_type, " ",
		"{ ", abuf, "} ",
		href, " ",
		method, " ",
		enctype, " ",
		encentity, " ",
		NULL);

	    if (status != TCL_OK)
		cb_error (obm, cb);
	}

	XtFree (abuf);
}


/* linkCallback -- Callback procedure called by the HTML widget when
 * a <LINK> directive is encountered while loading text into the widget.
 *
 * Calling sequence:
 *
 *	userproc widget cbtype href role
 *
 * All callbacks registered with the current widget for the callback
 * are called in the order in which they were registered.
 */
static void
linkCallback (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
	register char *ip, *op;
	register ObmCallback cb;
	LinkInfo *l_info = (LinkInfo *) call_data;
	HTMLObject obj = (HTMLObject) client_data;
	ObmContext obm = obj->widget.obm;
	char *href, *role, *none = "none";
	int status;

	href = l_info->href ? l_info->href : none;
	role = l_info->role ? l_info->role : none;

	/* Call any registered callback functions. */
	for (cb = obj->html.callback_list;  cb;  cb = cb->next) {
	    if (cb->callback_type != CB_Link)
		continue;

	    status = Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		cb_encode(cb->callback_type), " ",
		href, " ",
		role, " ",
		NULL);

	    if (status != TCL_OK)
		cb_error (obm, cb);
	}
}


/* pointerMotionCallback -- Callback procedure called by the HTML widget when
 * the pointer enters an anchor.
 *
 * Calling sequence:
 *
 *	userproc widget cbtype href
 */
static void
pointerMotionCallback (w, client_data, href)
Widget w;
XtPointer client_data;
char *href;
{
	register ObmCallback cb;
	register HTMLObject obj = (HTMLObject) client_data;
	register ObmContext obm = obj->widget.obm;
	int status;

	/* Call any registered callback functions. */
	for (cb = obj->html.callback_list;  cb;  cb = cb->next) {
	    if (cb->callback_type != CB_PointerMotion)
		continue;

	    status = Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		cb_encode(cb->callback_type), " ",
		"{", href, "} ",
		NULL);

	    if (status != TCL_OK)
		cb_error (obm, cb);
	}
}


/* cb_error -- Convenience routine to return an error from a callback.
 */
static void
cb_error (obm, cb)
register ObmContext obm;
register ObmCallback cb;
{
	register Tcl_Interp *tcl = obm->tcl;
	char *errstr = Tcl_GetVar (tcl, "errorInfo", 0);
	fprintf (stderr, "Error on line %d in %s: %s\n",
	    tcl->errorLine, cb->name,
	    errstr ? errstr : tcl->result);
}

/* cb_decode -- Convert a callback_type string to a callback type code.
 */
static int
cb_decode (callback_type)
register char *callback_type;
{
	register int type = 0;

	if (strcmp (callback_type, "anchor") == 0)
	    type = CB_Anchor;
	else if (strcmp (callback_type, "testAnchor") == 0)
	    type = CB_TestAnchor;
	else if (strcmp (callback_type, "pointerMotion") == 0)
	    type = CB_PointerMotion;
	else if (strcmp (callback_type, "submitForm") == 0)
	    type = CB_SubmitForm;
	else if (strcmp (callback_type, "link") == 0)
	    type = CB_Link;

	return (type);
}

/* cb_encode -- Convert a callback_type string to a callback type code.
 */
static char *
cb_encode (callback_type)
int callback_type;
{
	register char *type = "unknown";

	switch (callback_type) {
	case CB_Anchor:
	    type = "anchor";
	    break;
	case CB_TestAnchor:
	    type = "testAnchor";
	    break;
	case CB_PointerMotion:
	    type = "pointerMotion";
	    break;
	case CB_SubmitForm:
	    type = "submitForm";
	    break;
	case CB_Link:
	    type = "link";
	    break;
	}

	return (type);
}
