/* Copyright(c) 1993 Association of Universities for Research in Astronomy Inc.
 */

#include <ObmP.h>


/*
 * UI PARAMETER class.
 * --------------------------
 * The UI parameter class is used for client-UI communications.  The client
 * does not control the user interface directly, rather the UI defines a set
 * of abstract UI parameters, and during execution the client application
 * assigns values to these parameters.  These UI parameters should be thought
 * of as describing the runtime state of the client as viewed by the GUI.
 * The GUI is free to interpret this state information in any way, including
 * ignoring it.  Many GUIs can be written which use the same client state
 * as described by the UI parameters.
 *
 * Assigning a value to a UI parameter causes the new value to be stored, and
 * any parameter action procedures registered by the UI to be called.
 * The action or actions (if any) taken when a parameter value changes are
 * arbitrary, e.g. the action might be something as simple as changing a
 * displayed value of a UI widget, or something more complex like displaying
 * a popup.
 *
 * UI Parameter class commands:
 *
 *             getValue
 *             setValue <new-value>
 *          addCallback <procedure-name>
 *       deleteCallback <procedure-name>
 *               notify
 *
 * The most common usage is for the GUI to post one or more callbacks for
 * each UI parameter.  When the UI parameter value is changed (with setValue,
 * e.g. by the client) the GUI callback procedures are called with the old
 * and new UI parameter values on the command line.  addCallback is used to
 * add a callback procedure, and deleteCallback to delete one.  Multiple
 * callbacks may be registered for a single UI parameter.  notify is used
 * to simulate a parameter value change, causing any callback procedures to
 * be invoked.
 *
 * The callback procedure is called as follows:
 *
 *	user-procedure param-name {old-value} {new-value}
 *
 * The important thing to note here is that the old and new value strings
 * are quoted with braces.  This prevents any interpretation of the string
 * by Tcl when the callback is executed, which is necessary because the
 * strings can contain arbitrary data.  When Tcl calls the callback the
 * first level of braces will be stripped off, leaving old-value and new-value
 * each as a single string argument.
 */

struct parameterPrivate {
	ObmContext obm;
	char *value;
	int len_value;
	ObmCallback callback;
};

typedef	struct parameterPrivate *ParameterPrivate;

struct parameterObject {
	struct obmObjectCore core;
	struct parameterPrivate parameter;
};

typedef	struct parameterObject *ParameterObject;

/* Object message context. */
struct msgContext {
	Tcl_Interp *tcl;		/* class interpreter */
	ObmObject object[MAX_LEVELS];	/* object which received last message */
	int level;
};
typedef struct msgContext *MsgContext;

static	void ParameterDestroy();
static	int ParameterEvaluate();
static	ObmObject ParameterCreate();
static	void ParameterClassDestroy();
static	int parameterSetValue(), parameterGetValue(), parameterNotify();
static	int parameterAddCallback(), parameterDeleteCallback();


/* ParameterClassInit -- Initialize the class record for the parameter class.
 */
void
ParameterClassInit (obm, classrec)
ObmContext obm;
register ObjClassRec classrec;
{
	register Tcl_Interp *tcl;
	register MsgContext msg;

	/* Install the class methods. */
	classrec->ClassDestroy = ParameterClassDestroy;
	classrec->Create = (ObmFunc) ParameterCreate;
	classrec->Destroy = ParameterDestroy;
	classrec->Evaluate = ParameterEvaluate;

	/* Since there can be many instances of the parameter object and
	 * they all respond to the same class messages, a single interpreter
	 * is used for all objects.
	 */
	msg = (MsgContext) XtMalloc (sizeof (struct msgContext));
	classrec->class_data = (XtPointer) msg;
	msg->tcl = tcl = Tcl_CreateInterp();
	msg->level = 0;

	/* Register parameter-object actions.  */
	Tcl_CreateCommand (tcl, "setValue",
	    parameterSetValue, (ClientData)msg, NULL);
	Tcl_CreateCommand (tcl, "getValue",
	    parameterGetValue, (ClientData)msg, NULL);
	Tcl_CreateCommand (tcl, "addCallback",
	    parameterAddCallback, (ClientData)msg, NULL);
	Tcl_CreateCommand (tcl, "deleteCallback",
	    parameterDeleteCallback, (ClientData)msg, NULL);
	Tcl_CreateCommand (tcl, "notify",
	    parameterNotify, (ClientData)msg, NULL);
}


/* ParameterClassDestroy -- Custom destroy procedure for the parameter
 * class.
 */
static void
ParameterClassDestroy (obm, classrec)
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


/* ParameterCreate -- Create an instance of a parameter object.
 */
static ObmObject
ParameterCreate (obm, name, classrec, parent, args, nargs)
ObmContext obm;
char *name;
ObjClassRec classrec;
char *parent;
ArgList args;
int nargs;
{
	register ParameterObject obj;

	obj = (ParameterObject) XtCalloc (1, sizeof (struct parameterObject));
	obj->parameter.obm = obm;

	return ((ObmObject) obj);
}


/* ParameterDestroy -- Destroy an instance of a parameter object.
 */
static void
ParameterDestroy (object)
ObmObject object;
{
	register ParameterObject obj = (ParameterObject) object;
	register ObmCallback cb, next;

	/* Destroy the object in the second final call to Destroy. */
	if (!obj->core.being_destroyed++)
	    return;

	XtFree ((char *)obj->parameter.value);
	for (cb = obj->parameter.callback;  cb;  cb = next) {
	    next = cb->next;
	    XtFree ((char *)cb);
	}
}


/* ParameterEvaluate -- Evaluate a parameter command or message.
 */
static int
ParameterEvaluate (object, command)
ObmObject object;
char *command;
{
	register ParameterObject obj = (ParameterObject) object;
	register MsgContext msg = (MsgContext) obj->core.classrec->class_data;
	register ObmContext obm = obj->parameter.obm;
	int status;

	/* Since the class wide interpreter is used to evaluate the message
	 * we can't pass the object descriptor directly to the class procedure
	 * referenced in the message.  Instead we pass the object reference
	 * in the message descriptor.
	 */
	msg->object[++msg->level] = object;
	Tcl_SetResult (obm->tcl, "", TCL_VOLATILE);

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


/* parameterSetValue -- Set the value of a parameter, and notify all clients
 * via the posted callback procedures that the parameter value has changed.
 *
 *  Usage:	setValue <new-value>
 */
static int 
parameterSetValue (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	ParameterObject obj = (ParameterObject) msg->object[msg->level];
	register ObmContext obm = obj->parameter.obm;
	register ParameterPrivate pp = &obj->parameter;
	char *new_value, *old_value;
	ObmCallback cb, cbl[128];
	int ncb, status, i;

	/* Assign new value. */
	old_value = pp->value;
	pp->len_value = strlen (argv[1]);
	pp->value = new_value = XtMalloc (pp->len_value + 1);
	memmove (pp->value, argv[1], pp->len_value + 1);

	/* Safeguard callback list against changes by callback procs. */
	for (cb = pp->callback, ncb=0;  cb;  cb = cb->next)
	    cbl[ncb++] = cb;

	/* Notify clients that value has changed. */
	for (i=0;  i < ncb && (cb = cbl[i]) != NULL;  i++) {
/*printf ("setValue: i=%d  obj='%s' cb='%s' new='%s'\n", 
i, obj->core.name, cb->name, new_value);*/
	    status = Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		"{", old_value ? old_value : "", "} ",
		"{", new_value, "} ",
		NULL);
	    if (status != TCL_OK) {
		char *errstr = Tcl_GetVar (obm->tcl, "errorInfo", 0);
		fprintf (stderr, "Error on line %d in %s: %s\n",
		    obm->tcl->errorLine, cb->name,
		    errstr ? errstr : obm->tcl->result);
	    }
	}

	XtFree ((char *)old_value);
	Tcl_SetResult (obm->tcl, "", TCL_STATIC);
	return (TCL_OK);
}


/* parameterGetValue -- Get the value of a parameter.
 *
 *  Usage:	getValue
 */
static int 
parameterGetValue (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	ParameterObject obj = (ParameterObject) msg->object[msg->level];
	register ObmContext obm = obj->parameter.obm;
	register ParameterPrivate pp = &obj->parameter;

	Tcl_SetResult (obm->tcl, pp->value, TCL_STATIC);
	return (TCL_OK);
}


/* parameterNotify -- Notify the registered clients of a parameter as if the
 * value had changed.
 *
 *  Usage:	notify
 */
static int 
parameterNotify (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	ParameterObject obj = (ParameterObject) msg->object[msg->level];
	register ObmContext obm = obj->parameter.obm;
	register ParameterPrivate pp = &obj->parameter;
	ObmCallback cb;
	int status;

	/* Notify clients. */
	for (cb = pp->callback;  cb;  cb = cb->next) {
	    status = Tcl_VarEval (obm->tcl,
		cb->name, " ",
		obj->core.name, " ",
		"{", pp->value, "} ",
		"{", pp->value, "} ",
		NULL);
	    if (status != TCL_OK) {
		char *errstr = Tcl_GetVar (obm->tcl, "errorInfo", 0);
		fprintf (stderr, "Error on line %d in %s: %s\n",
		    obm->tcl->errorLine, cb->name,
		    errstr ? errstr : obm->tcl->result);
	    }
	}

	Tcl_SetResult (obm->tcl, "", TCL_STATIC);
	return (TCL_OK);
}


/* parameterAddCallback -- Add a callback procedure to the callback list for
 * a parameter.
 *
 *  Usage:	addCallback <procedure-name>
 */
static int 
parameterAddCallback (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	ParameterObject obj = (ParameterObject) msg->object[msg->level];
	register ParameterPrivate pp = &obj->parameter;
	ObmCallback cb, new_cb;

	/* Create callback record. */
	new_cb = (ObmCallback) XtCalloc (1, sizeof (obmCallback));
	strcpy (new_cb->name, argv[1]);

	/* Add callback to tail of callback list. */
	if (pp->callback) {
	    for (cb = pp->callback;  cb->next;  cb = cb->next)
		;
	    cb->next = new_cb;
	} else
	    pp->callback = new_cb;

	return (TCL_OK);
}


/* parameterDeleteCallback -- Delete a callback procedure previously registered
 * for a parameter.
 *
 *  Usage:	deleteCallback <procedure-name>
 */
static int 
parameterDeleteCallback (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	ParameterObject obj = (ParameterObject) msg->object[msg->level];
	register ParameterPrivate pp = &obj->parameter;
	ObmCallback cb, prev;

	/* Locate and delete procedure entry in callback list. */
	for (prev=NULL, cb=pp->callback;  cb;  prev=cb, cb=cb->next)
	    if (strcmp (cb->name, argv[1]) == 0) {
		if (prev)
		    prev->next = cb->next;
		else
		    pp->callback = cb->next;
		XtFree ((char *)cb);
		break;
	    }

	return (TCL_OK);
}
