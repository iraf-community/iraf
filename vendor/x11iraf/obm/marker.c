/* Copyright(c) 1993 Association of Universities for Research in Astronomy Inc.
 */

#include <ctype.h>
#include <ObmP.h>
#include "widget.h"

/*
 * Graphics Marker class (a base class, requires a parent Gterm widget).
 * --------------------------------------------------------------------------
 * A marker is a graphics object implemented by the Gterm-Image widget.
 * Markers are not real toolkit widgets, but they act much like widgets and
 * are interfaced as an object class under the Object Manager.  The Marker
 * class is not a subclass, it is a base class like Widget, but Marker objects
 * can exist only as children of Gterm widgets.
 *
 * Since markers are not independent widgets but rather part of a Gterm widget
 * instance, the parent Gterm widget is partially responsible for managing
 * markers.  The Gterm widget implements the following commands for dealing
 * with markers.
 *
 * 	       createMarker name [attribute-list]
 *               markerInit
 *
 * A new marker is created by sending the createMarker message to the parent
 * gterm widget.  This creates a marker of the given name and type.
 * The markerInit command, if sent to a gterm widget, destroys any markers
 * defined for that widget and reinitializes the marker facility.  Markers
 * may also be created by action procedures in response to user input events.
 *
 * A marker may be destroyed by itself in response to an input event (e.g. the
 * user presses the delete key), by sending the marker the destroy message
 * to tell it to destroy itself, by sending a markerInit to the parent gterm
 * widget, or by destroying the marker object (or any parent) with the server
 * command destroyObject.
 *
 * Once a marker has been created it behaves as an independent object and
 * receives and executes messages, responds to events, generates callbacks,
 * and so on.  The marker class defines the following commands.
 *
 *                 makeCopy name
 *              addCallback procedure [event [event ...]]
 *           deleteCallback procedure
 *                   notify [event-type [param [param ...]]]
 *                  destroy [nocallback]
 *
 *		    markpos
 *		     redraw [function] [markpos|nomarkpos] [erase|noerase]
 *
 *                    raise [reference-marker]
 *                    lower [reference-marker]
 *
 *                     move x y
 *                   resize width height
 *                   rotate angle		# radians
 *
 *			set attribute value	# alias for setAttribute
 *		value = get attribute
 *
 *             setAttribute attribute value
 *     value = getAttribute attribute
 *	      setAttributes attribute-list
 *	      getAttributes attribute-list
 *              setVertices points first npts
 *              getVertices points first npts
 *
 *	 region = getRegion [unmap] [coord-type]
 *	            getRect type dx dy dnx dny
 *
 * Marker positions and dimensions are given in window (raster 0) pixel
 * coordinates.   NDC (raster normalized) coordinates are not supported for
 * markers, except in the unmap option of getRegion which can return a marker
 * description in the raster coordinate system of the raster the marker is in.
 *
 * The operators raise, lower, move, resize, and rotate erase the marker,
 * modify it as indicated, and redraw it with the new attributes.  For finer
 * control over marker attributes one can use [get|set]Attribute[s] and
 * [get|set]Vertices to edit the markers directly.  In this case an auto
 * redraw is not performed (unless the autoRedraw marker attribute is set).
 * The usual sequence is a markpos to record the marker position, one or more
 * setAttribute calls to change marker attributes, then a redraw to erase
 * the old marker and redraw the new one.  Markers have many attributes which
 * can be set to control things like the position and size, colors, line
 * widths, fill type and style, font, rubber-band technique, and so on.
 * Refer to <ObmW/Gterm.h> for a list of marker types and attributes.
 *
 * The marker type may be changed at runtime without destroying the marker.
 * For example a circle can be changed to an ellipse or a rectangle.  This
 * also works for polygons (the vertex list is preserved and restored when
 * the marker is changed back to a polygon).
 *
 * The current shape of a marker may be queried with getVertices, which
 * returns the polygon or polyline vertex list in window coordinates.  A more
 * powerful routine which does something similar is getRegion.  This routine
 * returns a high level description of the region outlined by the marker,
 * giving the marker type (rectangle, circle, ellipse etc.), center, width
 * and height, and so on.  Any position or dimension information may
 * optionally be transformed back to the original source raster, if the marker
 * center is in a region of the window which is the destination of an active
 * mapping.  The unmap option will follow multiple mappings back to the
 * original mapped source raster.
 *
 * The getRect function returns a rect (x y width height) describing either
 * the rectangular region enclosed by a marker, or the rect defining the
 * marker boundary.  This works for any marker but is not likely to be useful
 * for other than nonrotated rectangular markers.  For example, the enclosed
 * rect returned may be used in a setMapping call to map a raster into the
 * interior of a marker.
 */

#define	MAX_POLYPTS	4096

typedef struct {
	ObmContext obm;			/* object manager */
	ObmObject pobj;			/* parent object */
	Widget gt;			/* gterm widget */
	XtPointer gm;			/* marker */
	ObmCallback callback;		/* callback list */
} markerPrivate, *MarkerPrivate;

typedef struct {
	struct obmObjectCore core;
	markerPrivate marker;
} markerObject, *MarkerObject;

static void MarkerDestroy();
static void MarkerClassDestroy();
static ObmObject MarkerCreate();
static int MarkerEvaluate();
static int markerDestroyCallback();
static int markerFocusCallback();

static	int markerMakeCopy(), markerAddCallback();
static	int markerDeleteCallback(), markerCallbackProc();
static	int markerNotify(), markerDestroy(), markerMarkpos(), markerRedraw();
static	int markerRaise(), markerLower(), markerMove(), markerResize();
static	int markerRotate(), markerGetAttribute(), markerSetAttribute();
static	int markerGetAttributes(), markerSetAttributes();
static	int markerGetVertices(), markerSetVertices();
static	int markerGetRegion(), markerGetRect();

extern	XtPointer GmCreate(), GmCopy();
extern	double strtod();


/* MarkerClassInit -- Initialize the class record for the marker widget class.
 */
void
MarkerClassInit (obm, classrec)
ObmContext obm;
register ObjClassRec classrec;
{
	register MsgContext msg;
	register Tcl_Interp *tcl;

	/* Install the class methods. */
	classrec->ClassDestroy = MarkerClassDestroy;
	classrec->Create = (ObmFunc) MarkerCreate;
	classrec->Destroy = MarkerDestroy;
	classrec->Evaluate = MarkerEvaluate;

	/* The marker widget subclass has its own command set hence has its
	 * own interpreter.  The widget will respond both to all the commands
	 * defined here, and to all the commands implemented by the base
	 * Widget class.
	 */
	if (!classrec->class_data) {
	    msg = (MsgContext) XtMalloc (sizeof (struct msgContext));
	    msg->tcl = tcl = Tcl_CreateInterp();
	    classrec->class_data = (XtPointer) msg;
	    msg->level = 0;

	    Tcl_CreateCommand (tcl,
		"makeCopy", markerMakeCopy, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"addCallback", markerAddCallback, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"deleteCallback", markerDeleteCallback, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"callbackProc", markerCallbackProc, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"notify", markerNotify, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"destroy", markerDestroy, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"markpos", markerMarkpos, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"redraw", markerRedraw, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"raise", markerRaise, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"lower", markerLower, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"move", markerMove, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"resize", markerResize, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"rotate", markerRotate, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"set", markerSetAttribute, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"get", markerGetAttribute, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"setAttribute", markerSetAttribute, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"getAttribute", markerGetAttribute, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"setAttributes", markerSetAttributes, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"getAttributes", markerGetAttributes, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"setVertices", markerSetVertices, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"getVertices", markerGetVertices, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"getRegion", markerGetRegion, (ClientData)msg, NULL);
	    Tcl_CreateCommand (tcl,
		"getRect", markerGetRect, (ClientData)msg, NULL);
	}
}


/* MarkerClassDestroy -- Custom destroy procedure for the widget class.
 */
static void
MarkerClassDestroy (obm, classrec)
ObmContext obm;
register ObjClassRec classrec;
{
	register MsgContext msg = (MsgContext) classrec->class_data;

	if (msg) {
	    if (msg->tcl)
		Tcl_DeleteInterp (msg->tcl);
	    XtFree ((char *)msg);
	    classrec->class_data = NULL;
	}
}


/* MarkerCreate -- Create a new instance of a marker object.
 */
static ObmObject
MarkerCreate (obm, name, classrec, parent, args, nargs)
ObmContext obm;
char *name;
ObjClassRec classrec;
char *parent;
ArgList args;
int nargs;
{
	register MarkerObject obj;
	register Widget gt;
	ObmObject gterm_obj;
	int type, interactive;
	int erase, visible, i;
	XtPointer marker, gm;

	/* Get descriptor. */
	if ((gterm_obj = obmFindObject (obm, parent)) == NULL)
	    return (NULL);
	obj = (MarkerObject) XtCalloc (1, sizeof(markerObject));
	if (obj == NULL)
	    return (NULL);

	/* Examine the marker attribute list to determine if we are
	 * building an object around a marker that has already been created,
	 * or if we are creating a new marker, the marker type and create mode.
	 */
	type = Gm_Rectangle;
	interactive = 0;
	visible = 0;
	marker = NULL;

	for (i=0;  i < nargs;  i++) {
	    Arg *ap = &args[i];
	    int ch;

	    if (strcmp (ap->name, "marker") == 0)
		marker = (XtPointer)ap->value;
	    else if (strcmp (ap->name, "createMode") == 0)
		interactive = (strcmp ((char *)ap->value, "interactive") == 0);
	    else if (strcmp (ap->name, "visible") == 0)
		visible = ((ch = *(char *)ap->value) == 'T' || ch == 't');
	    else if (strcmp (ap->name, "type") == 0) {
		if (!(type = GmStrToType ((char *)ap->value)))
		    type = Gm_Rectangle;
	    }
	}

	/* Create the marker. */
	if (marker)
	    gm = marker;
	else {
	    gt = widgetGetPointer (gterm_obj);
	    if ((gm = GmCreate (gt, type, interactive)) == NULL) {
		XtFree ((char *)obj);
		return (NULL);
	    }
	}

	/* Set any marker attributes. */
	for (i=0;  i < nargs;  i++) {
	    Arg *ap = &args[i];
	    if (strcmp (ap->name, "marker") != 0 &&
		strcmp (ap->name, "createMode") != 0 &&
		strcmp (ap->name, "type") != 0)
		GmSetAttribute (gm, ap->name, ap->value, XtRString);
	}

	/* Initialize descriptor. */
	obj->marker.obm = obm;
	obj->marker.pobj = gterm_obj;
	obj->marker.gt = gt;
	obj->marker.gm = gm;

	/* Define focusin and focusout callbacks to keep track of the
	 * marker state.
	 */
	GmAddCallback (gm, GmEvFocusIn|GmEvFocusOut,
	    markerFocusCallback, (XtPointer) obj);

	/* Define a destroy callback to automatically free the Marker object
	 * when a marker is destroyed at the Gterm widget level.
	 */
	GmAddCallback (gm, GmEvDestroy,
	    markerDestroyCallback, (XtPointer) obj);

	if (!interactive && visible)
	    GmRedraw (gm, GXcopy, erase=True);

	return ((ObmObject) obj);
}


/* MarkerDestroy -- Destroy an instance of a marker object.
 */
static void
MarkerDestroy (object)
ObmObject object;
{
	MarkerObject obj = (MarkerObject) object;
	register ObmContext obm = obj->marker.obm;
        register ObmCallback cb, next;

	/* Destroy the object in the second final call to Destroy. */
	if (!obj->core.being_destroyed++)
	    return;

	/* When a marker is destroyed at the widget level or by sending a
	 * marker object the destroy message, GmDestroy is called.
	 * This in turn calls the Marker destroy callback which does an OBM
	 * Object destroy which calls the class procedure MarkerDestroy 
	 * (this procedure) which calls GmDestroy again below.  The second
	 * GmDestroy will find that a destroy operation is already in progress
	 * and return immediately.  Hence, no matter how a Gterm marker is
	 * destroyed we end up destroying the associated OBM Marker as well.
	 */
	if (obj->marker.gm)
	    GmDestroy (obj->marker.gm);

        /* Free any callback descriptors. */
        for (cb = obj->marker.callback;  cb;  cb = next) {
            next = cb->next;
            XtFree ((char *)cb);
        }
}


/* MarkerEvaluate -- Evaluate a marker command or message.
 */
static int
MarkerEvaluate (object, command)
ObmObject object;
char *command;
{
	register MarkerObject obj = (MarkerObject) object;
	register MsgContext msg = (MsgContext) obj->core.classrec->class_data;
	register ObmContext obm = obj->marker.obm;
	int status;

	/* Since the class wide interpreter is used to evaluate the message
	 * we can't pass the object descriptor directly to the class procedure
	 * referenced in the message.  Instead we pass the object reference
	 * in the message descriptor.
	 */
	Tcl_SetResult (obm->tcl, "", TCL_VOLATILE);
	msg->object[++msg->level] = object;

	if (!obmClientCommand (msg->tcl, command)) {
	    Tcl_SetResult (obm->tcl, "invalid command", TCL_VOLATILE);
	    status = TCL_ERROR;
	} else {
	    status = Tcl_Eval (msg->tcl, command);
	    if (status == TCL_ERROR) {
		if (*msg->tcl->result)
		    Tcl_SetResult (obm->tcl, msg->tcl->result, TCL_VOLATILE);
		else {
		    /* Supply a default error message if none was returned. */
		    Tcl_SetResult (obm->tcl, "evaluation error", TCL_VOLATILE);
		}
		obm->tcl->errorLine = msg->tcl->errorLine;

	    } else if (*msg->tcl->result)
		Tcl_SetResult (obm->tcl, msg->tcl->result, TCL_VOLATILE);
	}

	msg->level--;
	return (status);
}


/*
 * MARKER class commands.
 * -----------------------
 */


/* makeCopy -- Copy a marker.  The new marker is initially identical to the
 * old one, and will not be distinct until, e.g., moved to a new center.
 *
 *  Usage:	makeCopy name
 */
static int 
markerMakeCopy (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmContext obm = mp->obm;
	XtPointer gm;
	Arg args[10];
	char *name;

	if (argc < 2)
	    return (TCL_ERROR);

	name = argv[1];
	gm = GmCopy (mp->gm);

	XtSetArg (args[0], "marker", gm);
	obmNewObject (obm, name, "Marker", mp->pobj->core.name, args, 1);

	return (TCL_OK);
}


/* addCallback -- Post a marker callback to be called when the specified
 * event or events occurs.  If no events are listed a Notify callback will
 * be posted.
 *
 *  Usage:	addCallback procedure [event [event ...]]
 */
static int 
markerAddCallback (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	ObmContext obm = mp->obm;
	register ObmCallback cb, new_cb;
	char *procedure;
	int events=0, i;

	if (argc < 2)
	    return (TCL_ERROR);

	procedure = argv[1];
	if (argc > 2) {
	    for (i=2;  i < argc;  i++)
		events |= GmStrToEvent(argv[i]);
	} else
	    events = GmEvNotify;

        /* Create callback record. */
        new_cb = (ObmCallback) XtCalloc (1, sizeof (obmCallback));
	new_cb->u.obj = (ObmObject) obj;
        new_cb->callback_type = events;
        strncpy (new_cb->name, procedure, SZ_NAME);

        /* Add new callback to the tail of callback list minus one, with the
	 * markerDestroyCallback at the very tail of this list.  This is
	 * necessary because we don't want to physically destroy the marker
	 * object until all marker object Destroy callbacks have been
	 * processed.
	 */
        if (mp->callback) {
            for (cb = mp->callback;  cb->next;  cb = cb->next)
                ;
            cb->next = new_cb;
        } else
            mp->callback = new_cb;

	GmDeleteCallback (mp->gm, markerDestroyCallback, (XtPointer) obj);
	GmAddCallback (mp->gm, events, markerCallbackProc, (XtPointer) new_cb);
	GmAddCallback (mp->gm, GmEvDestroy, markerDestroyCallback,
	    (XtPointer) obj);

	return (TCL_OK);
}


/* deleteCallback -- Delete a marker callback.
 *
 *  Usage:	deleteCallback procedure
 */
static int 
markerDeleteCallback (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmCallback cb, prev;
	char *procedure;

	if (argc < 2)
	    return (TCL_ERROR);
	procedure = argv[1];

	/* Locate and delete procedure entry in callback list. */
	for (prev=NULL, cb=mp->callback;  cb;  prev=cb, cb=cb->next)
	    if (strcmp (cb->name, procedure) == 0) {
		if (prev)
		    prev->next = cb->next;
		else
		    mp->callback = cb->next;
		GmDeleteCallback (mp->gm, markerCallbackProc, cb);
		XtFree ((char *)cb);
		break;
	    }

	return (TCL_OK);
}


/* markerCallbackProc -- Low level callback procedure, called by the Gterm
 * marker code when a marker event occurs.
 */
static int
markerCallbackProc (cb, gm, events, event, params, nparams)
register ObmCallback cb;
XtPointer gm;
int events;
XEvent *event;
String *params;
Cardinal nparams;
{
	MarkerObject obj = (MarkerObject) cb->u.obj;
	MarkerPrivate mp = &obj->marker;
	ObmContext obm = mp->obm;
	char message_data[SZ_MESSAGE];
	register char *op;
	int status = 0;

	/* Call the user callback procedure.  The callback procedure is called
	 * with the arguments "marker-name event-type event-data", where
	 * event-data is a Tcl list (delimited by braces) of data strings
	 * describing the event.
	 */
	
	if (events & GmEvNotify) {
	    /* Callback: marker-name notify {}
	     */
	    status |= Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		"notify", " ",
		"{}",
		NULL); 
	}

	if (events & GmEvMoveResize) {
	    /* Callback: marker-name moveResize {x y width height}
	     */
	    if (nparams >= 4) {
		sprintf (message_data, "%s %s %s %s",
		    params[0], params[1], params[2], params[3]);
	    } else
		strcpy (message_data, "0 0 0 0");

	    status |= Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		"moveResize", " ",
		"{", message_data, "}",
		NULL); 
	}

	if (events & GmEvModify) {
	    /* Callback: marker-name modify {attribute-name}
	     */
	    status |= Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		"modify", " ",
		"{", nparams ? params[0] : "unknown", "}",
		NULL); 
	}

	if (events & GmEvRedraw) {
	    /* Callback: marker-name redraw {}
	     */
	    status |= Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		"redraw", " ",
		"{}",
		NULL); 
	}

	if (events & GmEvDestroy) {
	    /* Callback: marker-name destroy {}
	     */
	    status |= Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		"destroy", " ",
		"{}",
		NULL); 
	}

	if (events & GmEvInput) {
	    /* Callback: marker-name input {type data modifiers}
	     *
	     * where type is keyPress, keyRelease, buttonPress, or
	     * buttonRelease, data is the character typed or a digit 1-5
	     * identifying the button, and modifier is "shift", "control",
	     * and so on.  Multiple modifiers may be given.
	     */
	    switch (event->type) {
	    case KeyPress:
	    case KeyRelease:
		{   XKeyPressedEvent *ev = (XKeyPressedEvent *) event;
		    register char *ip, *op = message_data;
		    char buf[SZ_MESSAGE];
		    int n;

		    if (event->type == KeyPress)
			strcpy (op, "keyPress ");
		    else
			strcpy (op, "keyRelease ");
		    while (op)
			op++;

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
		    *op++ = ' ';
		    op = widgetEventState (op, ev->state);
		    while (op > message_data && isspace (*(op-1)))
			--op;
		    *op = '\0';
		}
		break;

	    case ButtonPress:
	    case ButtonRelease:
		{   XButtonPressedEvent *ev = (XButtonPressedEvent *) event;
		    register char *op = message_data;

		    if (event->type == ButtonPress)
			strcpy (op, "buttonPress ");
		    else
			strcpy (op, "buttonRelease ");
		    while (op)
			op++;

		    sprintf (op, "%d ", ev->button);
		    while (*op)
			op++;
		    op = widgetEventState (op, ev->state);
		    while (op > message_data && isspace (*(op-1)))
			--op;
		    *op = '\0';
		}
		break;

	    default:
		strcpy (message_data, "unknown none");
	    }

	    status |= Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		"input", " ",
		"{", message_data, "}",
		NULL); 
	}

	if (events & GmEvFocusIn) {
	    /* Callback: marker-name focusIn {}
	     */
	    status |= Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		"focusIn", " ",
		"{}",
		NULL); 
	}

	if (events & GmEvFocusOut) {
	    /* Callback: marker-name focusOut {}
	     */
	    status |= Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		"focusOut", " ",
		"{}",
		NULL); 
	}

	/* The constraint callback is a special case because it can modify its
	 * arguments.  The called procedure returns as the function value a
	 * list of marker attributes with new values for those attributes.
	 * These are used to modify in place the param strings passed in by
	 * the widget.
	 *
	 * Callback: marker-name constraint { {name oldval newval} ... }
	 *
	 * The data field of this callback is a list of marker attributes,
	 * giving the attribute name, old value, and new value for each
	 * attribute.  The function value returned, if any, should be a list
	 * of marker attributes giving the attribute name and new value for
	 * each attribute, e.g.  { {name newval} ... }.  Only modified new
	 * values need be returned; if no values are returned, no constraints
	 * are applied.
	 */
	if (events & GmEvConstraint) {
	    Tcl_Interp *tcl = obm->tcl;
	    char **items, **fields;
	    int nitems;
	    int n, i, j;

	    op = message_data;
	    *op++ = '{';
	    *op++ = ' ';

	    for (i=0;  i < nparams;  i += 3) {
		sprintf (op, "{%s %s %s}",
		    params[i+0], params[i+1], params[i+2]);
		while (*op)
		    op++;
		*op++ = ' ';
	    }

	    *op++ = '}';
	    *op++ = '\0';

	    /* Call the client constraint procedure. */
	    status |= Tcl_VarEval (tcl,
		cb->name, " ",
		obj->core.name, " ",
		"constraint", " ",
		message_data,
		NULL); 

	    /* Process the list of modified values returned by the client.
	     * This is a list of lists, one for each modified value.
	     */
	    if (*tcl->result && strcmp(tcl->result,"{}") != 0) {
		if (Tcl_SplitList(tcl,tcl->result,&nitems,&items) != TCL_OK)
		    status = ERR;
		else {
		    for (i=0;  i < nitems;  i++) {
			if (Tcl_SplitList(tcl,items[i],&n,&fields) != TCL_OK) {
			    status = ERR;
			    continue;
			}
			if (n > 0)
			    for (j=0;  j < nparams;  j += 3)
				if (strcmp (fields[0], params[j]) == 0) {
				    strcpy (params[j+2], fields[1]);
				    break;
				}
			free ((char *) fields);
		    }
		}

		free ((char *) items);
	    }
	}

	if (status != TCL_OK) {
	    char *errstr = Tcl_GetVar (obm->tcl, "errorInfo", 0);
	    fprintf (stderr, "Error on line %d in %s: %s\n",
		obm->tcl->errorLine, cb->name,
		errstr ? errstr : obm->tcl->result);
	}

	return (status ? ERR : OK);
}


/* notify -- Generate a Marker pseudo-event, causing any posted client
 * callback procedures to be called.
 *
 * Usage:	notify [event-type [param [param ...]]]
 */
static int
markerNotify (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmContext obm = mp->obm;
	char *event_type;
	int first_param;
	XEvent event;

	event_type = (argc > 1) ? argv[1] : "notify";
	first_param = (argc > 1) ? 2 : 1;

	GmNotify (mp->gm, GmStrToEvent(event_type), &event,
	    argv[first_param], max (0, argc - first_param));

	return (TCL_OK);
}


/* destroy -- Destroy a marker.  Just tell the marker to destroy itself.
 * All cleanup outside the marker facility relies upon the use of callbacks.
 * This includes our callback markerDestroyCallback below.
 *
 * Usage:	destroy
 */
static int
markerDestroy (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;

	GmDestroy (mp->gm);
	return (TCL_OK);
}


/* markerDestroyCallback -- Low level callback procedure, called by the
 * Gterm widget code when a marker is about to be destroyed.
 */
static int
markerDestroyCallback (obj, gm, events, event, params, nparams)
MarkerObject obj;
XtPointer gm;
int events;
XEvent *event;
String *params;
Cardinal nparams;
{
	MarkerPrivate mp = &obj->marker;
	ObmContext obm = mp->obm;

	obmDestroyObject (obm, (ObmObject) obj);
	return (0);
}


/* markerFocusCallback -- Marker callback procedure, called when a marker
 * gets or loses the focus.
 */
static int
markerFocusCallback (obj, gm, events, event, params, nparams)
MarkerObject obj;
XtPointer gm;
int events;
XEvent *event;
String *params;
Cardinal nparams;
{
	MarkerPrivate mp = &obj->marker;
	ObmObject gtobj = mp->pobj;
	ObmContext obm = mp->obm;

	if (events & GmEvFocusIn)
	    widget_setTTName (gtobj, obj->core.name);
	else if (events & GmEvFocusOut)
	    widget_setTTName (gtobj, mp->pobj->core.name);

	return (0);
}


/* markpos -- Mark the current position of a marker for a later redraw.
 *
 * Usage:	markpos
 *
 * Markpos is used to mark the position of a marker before changing any
 * marker attributes, so that a later "redraw marked" will erase the old
 * marker rather than the new one.  This is necessary, for example, if any
 * marker attributes are changed which affect the size or position of the
 * marker.
 */
static int
markerMarkpos (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmContext obm = mp->obm;
	int erase;

	GmMarkpos (mp->gm);

	return (TCL_OK);
}


/* redraw -- Redraw a marker.
 *
 * Usage:	redraw [function] [erase|noerase] [markpos|nomarkpos]
 *
 * By default redraw will erase the old marker at the position indicated by
 * a previous call to markpos, and redraw the marker with the current
 * attributes using the drawing function copy (copy source to destination).
 * Hence the usual usage is "markpos ... change marker attributes ... redraw".
 * Optional arguments may be given to change the drawing function, enable or
 * disable erase, or force redraw to do a markpos.  These arguments may be
 * given in any order.
 *
 * The drawing functions are as given in the XLIB documentation, minus the
 * "GX" prefix.  The most commonly used functions are "copy" and "xor".
 * A normal marker redraw uses function=copy.
 */
static int
markerRedraw (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmContext obm = mp->obm;
	Boolean markpos = False;
	Boolean erase = True;
	int function = GXcopy;
	int i, v;
	char *ap;

	/* Process any optional arguments. */
	for (i=1;  i < argc;  i++) {
	    ap = argv[i];
	    if (strcmp (ap, "erase") == 0)
		erase = True;
	    else if (strcmp (ap, "noerase") == 0)
		erase = False;
	    else if (strcmp (ap, "markpos") == 0)
		markpos = True;
	    else if (strcmp (ap, "nomarkpos") == 0)
		markpos = False;
	    else if ((v = GmStrToFunction (ap)) > 0)
		function = v;
	}

	if (markpos)
	    GmMarkpos (mp->gm);
	GmRedraw (mp->gm, function, erase);

	return (TCL_OK);
}


/* raise -- Raise a marker, i.e., cause it to be drawn on top of other
 * markers when overlapping markers are drawn.
 *
 * Usage:	raise [reference-marker]
 *
 * In a reference marker is named the marker will raise itself above this
 * marker, otherwise the raised marker becomes the topmost marker.
 */
static int
markerRaise (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmContext obm = mp->obm;
	XtPointer ref_gm = NULL;
	MarkerObject ref;

	if (argc > 1) {
	    if (ref = (MarkerObject) obmFindObject (obm, argv[1]))
		ref_gm = ref->marker.gm;
	    else
		return (TCL_ERROR);
	}

	/* This implies an automatic erase and redraw. */
	GmRaise (mp->gm, ref_gm);

	return (TCL_OK);
}


/* lower -- Lower a marker, i.e., cause it to be drawn beneath other
 * markers when overlapping markers are drawn.
 *
 * Usage:	lower [reference-marker]
 *
 * In a reference marker is named the marker will lower itself beneath this
 * marker, otherwise the lowered marker becomes the lowest marker.
 */
static int
markerLower (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmContext obm = mp->obm;
	XtPointer ref_gm = NULL;
	MarkerObject ref;

	if (argc > 1) {
	    if (ref = (MarkerObject) obmFindObject (obm, argv[1]))
		ref_gm = ref->marker.gm;
	    else
		return (TCL_ERROR);
	}

	/* This implies an automatic erase and redraw. */
	GmLower (mp->gm, ref_gm);

	return (TCL_OK);
}


/* move -- Move a marker.
 *
 * Usage:	move x y
 *
 * Move the marker center to the indicated coordinates in the display window.
 */
static int
markerMove (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmContext obm = mp->obm;
	Arg args[10];
	int erase;

	if (argc < 3)
	    return (TCL_ERROR);

	XtSetArg (args[0], GmX, argv[1]);
	XtSetArg (args[1], GmY, argv[2]);

	GmMarkpos (mp->gm);
	GmSetAttributes (mp->gm, args, 2, XtRString);
	GmRedraw (mp->gm, GXcopy, erase=True);

	return (TCL_OK);
}


/* resize -- Resize a marker.
 *
 * Usage:	 resize width height
 *
 * Resize the marker to the indicated size.  By default width and height are
 * given in pixels.  For a text marker one can append "ch" to indicate that
 * the units are chars in whatever font is in use, e.g., "40ch" or "40 chars"
 * is an acceptable value for a text marker dimension.
 */
static int
markerResize (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmContext obm = mp->obm;
	Arg args[10];
	int erase;

	if (argc < 3)
	    return (TCL_ERROR);

	XtSetArg (args[0], GmWidth, argv[1]);
	XtSetArg (args[1], GmHeight, argv[2]);

	GmMarkpos (mp->gm);
	GmSetAttributes (mp->gm, args, 2, XtRString);
	GmRedraw (mp->gm, GXcopy, erase=True);

	return (TCL_OK);
}


/* rotate -- Rotate a marker.
 *
 * Usage:	 rotate angle
 *
 * Redraw a marker oriented to the given rotation angle.  The angle is
 * given in radians.
 */
static int
markerRotate (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmContext obm = mp->obm;
	Arg args[10];
	int erase;

	if (argc < 3)
	    return (TCL_ERROR);

	XtSetArg (args[0], GmRotangle, argv[1]);

	GmMarkpos (mp->gm);
	GmSetAttributes (mp->gm, args, 1, XtRString);
	GmRedraw (mp->gm, GXcopy, erase=True);

	return (TCL_OK);
}


/* getAttribute -- Return the value of a marker attribute.
 *
 * Usage:	value = getAttribute attribute-name
 */
static int
markerGetAttribute (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmContext obm = mp->obm;
	char *name, value[SZ_COMMAND];

	if (argc < 2)
	    return (TCL_ERROR);

	name = argv[1];

	if (GmGetAttribute (mp->gm, name, (XtArgVal)value, XtRString) < 0)
	    return (TCL_ERROR);
	else {
	    Tcl_SetResult (obm->tcl, value, TCL_VOLATILE);
	    return (TCL_OK);
	}
}


/* setAttribute -- Set the value of a marker attribute.
 *
 * Usage:	setAttribute attribute-name value
 */
static int
markerSetAttribute (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmContext obm = mp->obm;
	char *name, *value;

	if (argc < 3)
	    return (TCL_ERROR);

	name = argv[1];
	value = argv[2];

	if (GmSetAttribute (mp->gm, name, (XtArgVal)value, XtRString) < 0)
	    return (TCL_ERROR);
	else
	    return (TCL_OK);
}


/* getAttributes -- Return the values of a list of marker attributes.
 *
 * Usage:	getAttributes attribute-list
 *   i.e.	getAttributes {name value [name value ...]}
 *     or	getAttributes name value [name value ...]
 *
 * where "value" is the name of the variable in which the attribute value 
 * is to be stored.
 */
static int
markerGetAttributes (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmContext obm = mp->obm;
	char *name, *variable, value[SZ_COMMAND];
	int i, status = 0;
	char **items;
	int nitems;

	if (argc < 2)
	    return (TCL_ERROR);

	if (argc == 2) {
	    /* Attribute list passed as a list argument. */
	    if (Tcl_SplitList (tcl, argv[1], &nitems, &items) != TCL_OK)
		return (TCL_ERROR);
	} else if (argc > 2) {
	    /* Attribute list passed as separate arguments. */
	    nitems = argc - 1;
	    items = (char **) XtMalloc (nitems * sizeof(char *));
	    if (items == NULL)
		return (TCL_ERROR);
	    for (i=0;  i < nitems;  i++)
		items[i] = argv[i+1];
	} else
	    return (TCL_ERROR);

	for (i=0;  i < nitems;  i += 2) {
	    name = items[i];
	    variable = items[i+1];

	    if (GmGetAttribute (mp->gm, name, (XtArgVal)value, XtRString) < 0)
		status++;
	    if ((Tcl_SetVar (obm->tcl, variable, value, 0)) == NULL)
		status++;
	}

	free ((char *) items);
	return (status ? TCL_ERROR : TCL_OK);
}


/* setAttributes -- Set the values of a list of marker attributes.
 *
 * Usage:	setAttributes attribute-list
 *   i.e.	setAttributes {name value [name value ...]}
 *
 * where "value" is the new value of the associated marker attribute.
 */
static int
markerSetAttributes (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmContext obm = mp->obm;
	char *name, *value;
	Arg args[MAX_ARGS];
	int status, argno, i;
	char **items;
	int nitems;

	if (argc < 2)
	    return (TCL_ERROR);

	if (argc == 2) {
	    /* Attribute list passed as a list argument. */
	    if (Tcl_SplitList (tcl, argv[1], &nitems, &items) != TCL_OK)
		return (TCL_ERROR);
	} else if (argc > 2) {
	    /* Attribute list passed as separate arguments. */
	    nitems = argc - 1;
	    items = (char **) XtMalloc (nitems * sizeof(char *));
	    if (items == NULL)
		return (TCL_ERROR);
	    for (i=0;  i < nitems;  i++)
		items[i] = argv[i+1];
	} else
	    return (TCL_ERROR);

	for (i=0, argno=0;  i < nitems && argno < MAX_ARGS;  i += 2, argno++) {
	    name = items[i];
	    value = items[i+1];
	    XtSetArg (args[argno], name, value);
	}

	if (GmSetAttributes (mp->gm, args, argno, XtRString) < 0)
	    status = TCL_ERROR;
	else
	    status = TCL_OK;

	free ((char *) items);
	return (status);
}


/* getVertices -- Get some or all of the vertices making up the polygon or
 * polyline representation of a marker.
 *
 * Usage:	getVertices points [first npts]
 *
 * The polygon or polyline representation of a marker is returned in the
 * variable "points", as a string of the form { {x y} {x y} ...}.  The first
 * point is number zero.  Coordinates are specified in raster zero pixel
 * coordinates.
 */
static int
markerGetVertices (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	MarkerPrivate mp = &obj->marker;
	ObmContext obm = mp->obm;

	register int i;
	register char *op;
	register DPoint *pv;
	int first, maxpts, npts, ngot;
	int nchars, status, buflen;
	char *points, *buf;

	if (argc < 2)
	    return (TCL_ERROR);

	points = argv[1];
	first = (argc > 2) ? atoi(argv[2]) : 0;
	npts = (argc > 3) ? atoi(argv[3]) : 0;
	maxpts = max (npts, MAX_POLYPTS);
again:
	if ((pv = (DPoint *) XtMalloc (maxpts * sizeof(DPoint))) == NULL)
	    return (TCL_ERROR);

	ngot = GmGetVertices (mp->gm, pv, first, npts ? npts : maxpts);
	if (ngot >= maxpts) {
	    /* This is pretty unlikely. */
	    XtFree ((char *)pv);
	    maxpts *= 2;
	    goto again;
	}

	buflen = SZ_COMMAND;
	buf = XtMalloc (buflen);
	if (buf == NULL) {
	    XtFree ((char *)pv);
	    return (TCL_ERROR);
	}

	op = buf;
	*op++ = '{';
	*op++ = ' ';

	for (i=0;  i < ngot;  i++) {				/* MF001 */
	    sprintf (op, "{%d %d} ", (int)pv[i].x, (int)pv[i].y);
	    while (*op)						/* MF002 */
		op++;

	    if (op - buf + SZ_NUMBER > buflen) {
		buflen += SZ_COMMAND;
		nchars = op - buf;
		if ((buf = XtRealloc (buf, buflen)) == NULL) {
		    XtFree ((char *)pv);
		    return (TCL_ERROR);
		}
		op = buf + nchars;
	    }
	}

	*op++ = '}';
	*op++ = '\0';

	if ((Tcl_SetVar (obm->tcl, points, buf, 0)) == NULL)
	    status = TCL_ERROR;
	else
	    status = TCL_OK;

	XtFree ((char *)pv);
	XtFree (buf);

	return (status);
}


/* setVertices -- Set some or all of the vertices making up the polygon or
 * polyline representation of a marker.
 *
 * Usage:	setVertices points [first npts]
 *
 * The polygon or polyline representation of a marker is set using the points
 * passed in the "points" variable as a string of the form { {x y} {x y} ...}.
 * If FIRST and NPTS are not specified first is assumed to be zero (the first
 * point) and NPTS is the length of the points array.  Coordinates are 
 * specified in raster zero pixel coordinates.  In the case of 'poly' type
 * markers the procedure will close the polygon by adding a copy of the first
 * point to the list.
 */
static int
markerSetVertices (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	MarkerPrivate mp = &obj->marker;
	ObmContext obm = mp->obm;

	register int i;
	register char *ip;
	register DPoint *pv;
	int first, maxpts, npts, ngot;
	char *ipp, *points, *ip_save;

	if (argc < 2)
	    return (TCL_ERROR);

	points = argv[1];
	first = (argc > 2) ? atoi(argv[2]) : 0;
	npts = (argc > 3) ? atoi(argv[3]) : 0;
	maxpts = max (npts, MAX_POLYPTS);
	
	if ((pv = (DPoint *) XtMalloc (maxpts * sizeof(DPoint))) == NULL)
	    return (TCL_ERROR);

	/* Get the points array. */
	for (ngot=0, ip=points;  *ip;  ) {
	    while (isspace(*ip) || *ip == '{')
		ip++;

	    ip_save = ip;
	    pv[ngot].x = strtod (ip, &ipp);  ip = ipp;
	    pv[ngot].y = strtod (ip, &ipp);  ip = ipp;
	    if (ip == ip_save) {
		XtFree ((char *) pv);
		return (TCL_ERROR);
	    }

	    while (isspace(*ip) || *ip == '}')
		ip++;

	    if (++ngot >= maxpts) {
		maxpts *= 2;
		if ((pv = (DPoint *) XtRealloc ((char *)pv,
			maxpts * sizeof(DPoint))) == NULL)
		    return (TCL_ERROR);
	    }
	}

	GmSetVertices (mp->gm, pv, first, npts ? npts : ngot);

	XtFree ((char *)pv);
	return (TCL_OK);
}


/* getRegion -- Return as a text string a high level description of the
 * region defined by a marker.
 *
 * Usage:	region = getRegion [unmap] [coord-type]
 *
 * The output string defines the marker type and the major marker positional
 * attributes.  The region description formats for the various marker types
 * follow.
 *
 *	text raster x y width height
 *	line raster x y x y
 *	polyline raster npts { {x y} {x y} ...}
 *	rectangle raster x y width height rotangle
 *	circle raster x y radius
 *	ellipse raster x y width height rotangle
 *	polygon raster npts { {x y} {x y} ...}
 *
 * Here, width and height refer to the distance from the marker center to an
 * edge, not to the width or height of the whole marker.  This avoids
 * ambiguities about where the edge of a marker is if the width is even or
 * odd.  Using the center to edge measurement, the edge is at x +/- width,
 * y +/- height.
 *
 * If the "unmap" flag is given getRegion will attempt to associate the
 * marker with a mapped raster, reversing any mappings from the screen back
 * to the original source raster, and returning the raster number and raster
 * coordinates and marker sizes.  If "unmap" is not given the marker
 * coordinates will refer to raster 0.  Either raster pixel ("pixel" or
 * "Pixel") or raster NDC ("ndc" or "NDC") coordinates may be returned, pixel
 * coordinates being the default.
 */
static int
markerGetRegion (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	MarkerPrivate mp = &obj->marker;
	ObmContext obm = mp->obm;
	register DPoint *pv, *vv;
	register char *op;
	register int i;

	Boolean unmap = False;
	int ctype_out = GtPixel;
	int x, y, width, height;
	int maxpts, npts, src, dst, mapping;
	int ctype, pvlen, buflen, nchars;
	double rx, ry, rwidth, rheight;
	Boolean output_points;
	char *buf, *ap;
	int marker_type;
	double rotangle;
	int status, v;
	Arg args[10];

	/* Process any optional arguments. */
	for (i=1;  i < argc;  i++) {
	    ap = argv[i];
	    if (strcmp (ap, "unmap") == 0)
		unmap = True;
	    else if ((v = coordType (ap)) > 0)
		ctype_out = v;
	}

	/* First get the basic attributes like x, y, width, height, which
	 * we need for most markers.
	 */
	XtSetArg (args[0], GmType, &marker_type);
	XtSetArg (args[1], GmX, &x);
	XtSetArg (args[2], GmY, &y);
	XtSetArg (args[3], GmWidth, &width);
	XtSetArg (args[4], GmHeight, &height);
	if (GmGetAttributes (mp->gm, args, 5, XtRInt) < 0)
	    return (TCL_ERROR);

	if (GmGetAttribute (mp->gm, GmRotangle, &rotangle, XtRFloat) < 0)
	    return (TCL_ERROR);

	/* Get the points array, needed for some markers. */
	maxpts = MAX_POLYPTS;
again:
	if ((pv = (DPoint *) XtMalloc (maxpts * sizeof(DPoint))) == NULL)
	    return (TCL_ERROR);

	/* To ease the coordinate conversions we store the marker center and
	 * width information as the first two elements in the points array.
	 * These are not part of the marker polygon or polyline.  Putting them
	 * in the points vector allows us to transform everything in one
	 * operation.
	 */
	pv[0].x = x;
	pv[0].y = y;
	pv[1].x = x + width;
	pv[1].y = y + height;
	vv = pv + 2;

	/* Now read the points array into pv[2]. */
	npts = GmGetVertices (mp->gm, vv, 0, maxpts - 2);
	if (npts == maxpts-2) {
	    /* This is pretty unlikely. */
	    XtFree ((char *)pv);
	    maxpts *= 2;
	    goto again;
	}

	/* If the unmap option is specified select the mapping, if any, which
	 * contains the marker center.  Transform the coordinate vector
	 * backwards to the source raster.  This process is repeated until we
	 * get back to a raster pixel which is not the destination of any
	 * mapping.
	 */
	src = 0;
	pvlen = npts + 2;

	if (unmap) {
	    do {
		src = GtSelectRaster (mp->gt, dst=src,
		    GtPixel, (int)(pv[0].x + 0.5), (int)(pv[0].y + 0.5),
		    GtPixel, &x, &y, &mapping);
		if (src != dst)
		    GtMapVector (mp->gt, mapping, GtUnmap, pv, pv, pvlen);
	    } while (dst != src);
	}

	/* Convert the point data to NDC coordinates if indicated.  NDC
	 * coordinates are scaled to the integer range 0:MAXNDC at the widget
	 * level, which we scale to the range 0-1 floating at the GUI level.
	 */
	if (ctype_out == GtNDC) {
	    GtPixelToNDC (mp->gt, src, pv, pv, pvlen);
	    for (i=0;  i < pvlen;  i++) {
		pv[i].x /= (double)MAXNDC;
		pv[i].y /= (double)MAXNDC;
	    }
	}

	rx = pv[0].x;
	ry = pv[0].y;
	rwidth = abs (pv[1].x - rx);
	rheight = abs (pv[1].y - ry);

	buflen = SZ_COMMAND;
	output_points = False;
	op = buf = XtMalloc (buflen);
	if (buf == NULL) {
	    XtFree ((char *)pv);
	    return (TCL_ERROR);
	}

	/* Generate the marker description. */
	switch (marker_type) {
	case Gm_Text:
	    /* text raster x y width height */
	    if (ctype_out == GtNDC) {
		sprintf (op, "%s %d %0.5f %0.5f %0.5f %0.5f",
		    GmText, src, rx, ry, rwidth, rheight);
	    } else {
		sprintf (op, "%s %d %0.2f %0.2f %0.2f %0.2f",
		    GmText, src, rx, ry, rwidth, rheight);
	    }
	    break;

	case Gm_Line:
	    /* line raster x y x y */
	    if (ctype_out == GtNDC) {
		sprintf (op, "%s %d %0.5f %0.5f %0.5f %0.5f", GmLine, src,
		    vv[0].x, vv[0].y, vv[1].x, vv[1].y);
	    } else {
		sprintf (op, "%s %d %0.2f %0.2f %0.2f %0.2f", GmLine, src,
		    vv[0].x, vv[0].y, vv[1].x, vv[1].y);
	    }
	    break;

	case Gm_Polyline:
	    /* polyline raster npts { {x y} {x y} ...} */
	    sprintf (op, "%s %d %d ", GmPolyline, src, npts);
	    output_points = True;
	    break;

	case Gm_Rectangle:
	    /* rectangle raster x y width height rotangle */
	    if (ctype_out == GtNDC) {
		sprintf (op, "%s %d %0.5f %0.5f %0.5f %0.5f %0.5f",
		    GmRectangle, src, rx, ry, rwidth, rheight, rotangle);
	    } else {
		sprintf (op, "%s %d %0.2f %0.2f %0.2f %0.2f %0.4f",
		    GmRectangle, src, rx, ry, rwidth, rheight, rotangle);
	    }
	    break;

	case Gm_Box:
	    /* box raster x y width height rotangle */
	    if (ctype_out == GtNDC) {
		sprintf (op, "%s %d %0.5f %0.5f %0.5f %0.5f %0.5f",
		    GmBox, src, rx, ry, rwidth, rheight, rotangle);
	    } else {
		sprintf (op, "%s %d %0.2f %0.2f %0.2f %0.2f %0.4f",
		    GmBox, src, rx, ry, rwidth, rheight, rotangle);
	    }
	    break;

	case Gm_Circle:
	    /* circle raster x y radius */
	    if (ctype_out == GtNDC) {
		sprintf (op, "%s %d %0.5f %0.5f %0.5f", GmCircle, src,
		    rx, ry, rwidth);
	    } else {
		sprintf (op, "%s %d %0.2f %0.2f %0.2f", GmCircle, src,
		    rx, ry, rwidth);
	    }
	    break;

	case Gm_Ellipse:
	    /* ellipse raster x y width height rotangle */
	    if (ctype_out == GtNDC) {
		sprintf (op, "%s %d %0.5f %0.5f %0.5f %0.5f %0.5f",
		    GmEllipse, src, rx, ry, rwidth, rheight, rotangle);
	    } else {
		sprintf (op, "%s %d %0.2f %0.2f %0.2f %0.2f %0.4f",
		    GmEllipse, src, rx, ry, rwidth, rheight, rotangle);
	    }
	    break;

	case Gm_Polygon:
	    /* polygon raster npts { {x y} {x y} ...} */
	    sprintf (op, "%s %d %d ", GmPolygon, src, npts);
	    output_points = True;
	    break;

	default:
	    XtFree (buf);
	    XtFree ((char *)pv);
	    return (TCL_ERROR);
	}

	/* If the marker description includes a point vector output this
	 * too.
	 */
	if (output_points) {
	    while (*op)
		op++;

	    *op++ = '{';
	    *op++ = ' ';

	    for (i=0;  i < npts;  i++) {
		if (ctype_out == GtNDC)
		    sprintf (op, "{%0.5f %0.5f} ", vv[i].x, vv[i].y);
		else
		    sprintf (op, "{%0.2f %0.2f} ", vv[i].x, vv[i].y);
		while (*op)					/* MF003 */
		    op++;

		if (op - buf + SZ_NUMBER > buflen) {
		    buflen += SZ_COMMAND;
		    nchars = op - buf;
		    if ((buf = XtRealloc (buf, buflen)) == NULL) {
			XtFree ((char *)pv);
			return (TCL_ERROR);
		    }
		    op = buf + nchars;
		}
	    }

	    *op++ = '}';
	    *op++ = '\0';
	}

	Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);
	XtFree ((char *)pv);
	XtFree (buf);

	return (TCL_OK);
}


/* getRect -- Return the region defined by a rectangle marker.  The rect is
 * returned in a form convenient for use as the destination rect in a gterm
 * widget raster mapping.
 *
 * Usage:	getRect type dx dy dnx dny
 *
 * The rect is stored in the output arguments.  The rect coordinates are
 * integer pixel coordinates (raster 0 pixel coordinates), as with all marker
 * level coords.  If the rect type is "interior" the rect defining the region
 * enclosed by the marker is returned.  If the rect type is "boundary' the
 * rect returned refers to the location of the marker itself.  If the rect
 * type is "boundingBox" the rect returned is one which is large enough to
 * completely enclose the marker.  getRect may be used with any marker, but 
 * the interior and boundary options are probably not useful except for
 * nonrotated rectangular markers.
 */
static int
markerGetRect (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	MarkerObject obj = (MarkerObject) msg->object[msg->level];
	register MarkerPrivate mp = &obj->marker;
	register ObmContext obm = mp->obm;
	char *dx_out, *dy_out, *dnx_out, *dny_out;
	int marker_type, x, y, width, height, status;
	int dx, dy, dnx, dny;
	int x1, x2, y1, y2;
	char *type, buf[32];
	Arg args[10];

	if (argc < 6)
	    return (TCL_ERROR);

	type    = argv[1];
	dx_out  = argv[2];
	dy_out  = argv[3];
	dnx_out = argv[4];
	dny_out = argv[5];

	XtSetArg (args[0], GmType, &marker_type);
	XtSetArg (args[1], GmX, &x);
	XtSetArg (args[2], GmY, &y);
	XtSetArg (args[3], GmWidth, &width);
	XtSetArg (args[4], GmHeight, &height);
	if (GmGetAttributes (mp->gm, args, 5, XtRInt) < 0)
	    return (TCL_ERROR);

	/* Get the coordinates of the marker boundary. */
	if (marker_type == Gm_Text) {
	    x1 = x;  x2 = x + width - 1;
	    y1 = y;  y2 = y + height - 1;
	} else {
	    x1 = x - width;   x2 = x + width;
	    y1 = y - height;  y2 = y + height;
	}

	if (strcmp (type, "boundary") == 0) {
	    /* Return the rect defining the marker itself. */
	    dx = x1;  dnx = max(0, x2 - x1 + 1);
	    dy = y1;  dny = max(0, y2 - y1 + 1);

	} else if (strcmp (type, "boundingBox") == 0) {
	    /* Return a rect large enough to enclose the entire marker.
	     */
	    GmGetBoundingBox (mp->gm, &dx, &dy, &dnx, &dny);

	} else {
	    /* Compute the enclosed region, leaving a little space between
	     * the rect and the marker boundary.  This is the default.
	     */
	    dx = x1 + 2;  dnx = max(0, (x2 - 2) - (x1 + 2) + 1);
	    dy = y1 + 2;  dny = max(0, (y2 - 2) - (y1 + 2) + 1);
	}

	status = 0;
	sprintf (buf, "%d", dx);
	if ((Tcl_SetVar (obm->tcl, dx_out, buf, 0)) == NULL)
	    status++;
	sprintf (buf, "%d", dy);
	if ((Tcl_SetVar (obm->tcl, dy_out, buf, 0)) == NULL)
	    status++;
	sprintf (buf, "%d", dnx);
	if ((Tcl_SetVar (obm->tcl, dnx_out, buf, 0)) == NULL)
	    status++;
	sprintf (buf, "%d", dny);
	if ((Tcl_SetVar (obm->tcl, dny_out, buf, 0)) == NULL)
	    status++;

	return (TCL_OK);
}
