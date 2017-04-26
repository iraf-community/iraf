/* Copyright(c) 1993 Association of Universities for Research in Astronomy Inc.
 */

#include <ObmP.h>

/* The following internal files are needed for some widget level SmeBSB code
 * included in this file.
 */
#include <X11/IntrinsicP.h>
#include <X11/Xaw/SmeBSBP.h>

/*
 * SERVER class.
 * --------------------------
 * The server, or object manager, is the control center of the user interface.
 * The server object provides a Tcl interpreter calling custom object manager
 * commands.  These are used to define and initialize the user interface, and
 * execute UI action procedures at runtime.
 *
 *           reset-server
 *          appInitialize appname, appclass, resources
 *              appExtend new-resources [overwrite]
 *          createObjects [resource-name]
 *	    destroyObject object
 *	      queryObject object [class [subclass]]
 *               activate
 *             deactivate [unmap]
 *	      synchronize
 *		    flush
 *
 *    value = getResource resource-name [default-value [class]]
 *	     getResources resource-list
 *
 *             createMenu menu-name parent item-list
 *               editMenu menu-name parent item-list
 *            destroyMenu menu-name
 *
 *           createBitmap name width height data
 *           createCursor name source mask fg_color bg_color x_hot y_hot
 *           createPixmap name width height depth fg_color bg_color data
 *          createXPixmap name widget description
 *
 *                  print arg [arg ...]		# debug messages
 *                   send object message
 *
 *   postActivateCallback procedure
 * postDeactivateCallback procedure
 *
 * id = postTimedCallback procedure msec [client-data]
 *    deleteTimedCallback id
 *      id = postWorkProc procedure [client-data]
 *         deleteWorkProc id
 */

#define	CB_WORKPROC	1
#define	CB_TIMER	2

/* Callback structure for timer, workproc callbacks. */
struct _serverCallback {
	XtPointer obj;
	int callback_type;
	char *userproc;
	char *client_data;
	union {
	    XtIntervalId intervalId;
	    XtWorkProcId workProcId;
	} id;
	struct _serverCallback *next;
};
typedef struct _serverCallback serverCallback;
typedef struct _serverCallback *ServerCallback;

struct serverPrivate {
	ObmContext obm;
	ServerCallback cb_head;
	ServerCallback cb_tail;
};

typedef	struct serverPrivate *ServerPrivate;

struct serverObject {
	struct obmObjectCore core;
	struct serverPrivate server;
};

typedef	struct serverObject *ServerObject;

static	ObmObject ServerCreate();
static	void ServerDestroy();
static	int ServerEvaluate(), serverQueryObject();
static	int serverCreateMenu(), serverDestroyMenu();
static	int serverAppInitialize(), serverAppExtend(), serverCreateObjects();
static	int serverSend(), serverPrint(), serverDestroyObject();
static	int serverReset(), serverActivate(), serverDeactivate();
static	int serverCreateBitmap(), serverCreatePixmap(), serverCreateCursor();
static	int serverPostActivateCallback(), serverPostDeactivateCallback();
static	int serverPostTimedCallback(), serverPostWorkProc();
static	int serverDeleteTimedCallback(), serverDeleteWorkProc();
static	int serverCreateXPixmap(), serverSynchronize(), serverFlush();
static	int serverGetResource(), serverGetResources();
static	void link_callback(), unlink_callback();
static	void serverTimedProc();
static	Boolean serverWorkProc();

static	int editMenu();
static	void menu_popup(), menu_popdown(), menu_popdown_msgHandler();
static	void createMenu(), menuSelect(), build_colorlist();
static	void menu_classInit(), menu_addEntry(), menu_delEntry();
static	void menu_highlight(), menu_unhighlight();
static	Pixmap menu_pullrightBitmap();
static	MenuPtr findMenu();
extern	long strtol();

/* The pull-right bitmap for menus. */
#define MB_WIDTH 16
#define MB_HEIGHT 16
#define MB1_PIXELS \
  "0x00, 0x00, 0x30, 0x00, 0xf0, 0x00, 0xf0, 0x03, 0xf0, 0x0f, 0xf0, 0x0f,\
   0xf0, 0x03, 0xf0, 0x00, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,\
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00"
#define MB2_PIXELS \
  "0x00, 0x00, 0x30, 0x00, 0xd0, 0x00, 0x10, 0x03, 0x10, 0x0c, 0x10, 0x0c,\
   0x10, 0x03, 0xd0, 0x00, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,\
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00"


/* ServerClassInit -- Initialize the class record for the server class.
 */
void
ServerClassInit (obm, classrec)
ObmContext obm;
register ObjClassRec classrec;
{
	classrec->ClassDestroy = obmGenericClassDestroy;
	classrec->Create = (ObmFunc) ServerCreate;
	classrec->Destroy = ServerDestroy;
	classrec->Evaluate = ServerEvaluate;
}


/* ServerCreate -- Create an instance of a server object.
 */
static ObmObject
ServerCreate (obm, name, classrec, parent, args, nargs)
ObmContext obm;
char *name;
ObjClassRec classrec;
char *parent;
ArgList args;
int nargs;
{
	register ServerObject obj;
	register Tcl_Interp *tcl;

	obj = (ServerObject) XtCalloc (1, sizeof (struct serverObject));
	obm->tcl = tcl = Tcl_CreateInterp();
	obj->server.obm = obm;

	/* Register server actions.  */
	Tcl_CreateCommand (tcl,
	    "reset-server", serverReset, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "activate", serverActivate, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "deactivate", serverDeactivate, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "synchronize", serverSynchronize, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "flush", serverFlush, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "postActivateCallback", serverPostActivateCallback,
	    (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "postDeactivateCallback", serverPostDeactivateCallback,
	    (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "appInitialize", serverAppInitialize, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "appExtend", serverAppExtend, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "createObjects", serverCreateObjects, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "destroyObject", serverDestroyObject, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "queryObject", serverQueryObject, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "send", serverSend, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "print", serverPrint, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "getResource", serverGetResource, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "getResources", serverGetResources, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "postTimedCallback", serverPostTimedCallback,
	    (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "deleteTimedCallback", serverDeleteTimedCallback,
	    (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "postWorkProc", serverPostWorkProc, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "deleteWorkProc", serverDeleteWorkProc, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "createBitmap", serverCreateBitmap, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "createPixmap", serverCreatePixmap, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "createXPixmap", serverCreateXPixmap, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "createCursor", serverCreateCursor, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "createMenu", serverCreateMenu, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "editMenu", serverCreateMenu, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "destroyMenu", serverDestroyMenu, (ClientData)obj, NULL);

	return ((ObmObject) obj);
}


/* ServerDestroy -- Destroy an instance of a server object.
 */
static void
ServerDestroy (object)
ObmObject object;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;
	register ServerCallback cb, next;

	/* Destroy the object in the second final call to Destroy. */
	if (!obj->core.being_destroyed++)
	    return;

	/* Delete any pending timers or work procs. */
	for (cb = obj->server.cb_head;  cb;  cb = next) {
	    next = cb->next;
	    switch (cb->callback_type) {
	    case CB_TIMER:
		XtRemoveTimeOut (cb->id.intervalId);
		break;
	    case CB_WORKPROC:
		XtRemoveWorkProc (cb->id.workProcId);
		break;
	    }
	    XtFree ((char *)cb);
	}

	obj->server.cb_head = NULL;
	obj->server.cb_tail = NULL;

	/* Destroy the server interpreter. */
	if (obm->tcl) {
	    Tcl_DeleteInterp (obm->tcl);
	    obm->tcl = NULL;
	}
}


/* ServerEvaluate -- Evaluate a server command or message.
 */
static int
ServerEvaluate (object, command)
ObmObject object;
char *command;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;
	static char reset[] = "reset-server";
	register char *ip;

	/* The command "reset-server" is a special case.  This destroys the
	 * current user interface including all objects and widgets.  One
	 * of the objects destroyed is the server object including the tcl
	 * interpreter.  We can't use a normal Tcl command to implement this
	 * as the server and Tcl data structures will be freed and the rest
	 * of the input command would be lost.  Instead, we check for the
	 * reset-server command, which must be the first command in the input
	 * command string, and manually reset things then call Tcl to process
	 * the remainder of the input command.  The reset-server command should
	 * be the first command in the server config file used to define the
	 * user interface for a new application.  Comments and blank lines
	 * at the head of the file are ignored.
	 */
	for (ip=command;  *ip;  ) {
	    while (isspace (*ip))
		ip++;
	    if (*ip == '#')
		while (*ip && *ip != '\n')
		    ip++;
	    if (isspace (*ip))
		ip++;
	    else
		break;
	}
	if (strncmp (ip, reset, strlen(reset)) == 0) {
	    ObmInitialize (obm);
	    obj = (ServerObject) obmFindObject (obm, "Server");
	}

	/* Now interpret the full message using Tcl.  This re-executes the
	 * reset-server command, which will be ignored since this is a no-op
	 * when it occurs within a script.  We want to leave the command in
	 * the script as otherwise the line numbers won't be correct.
	 */
	return (Tcl_Eval (obm->tcl, command));
}


/* serverAppInitialize -- TCL command to initialize the server for a new
 * application, setting the application name and loading the application
 * resources.
 *
 *   Usage:	appInitialize appname, appclass, resources
 */
static int
serverAppInitialize (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;
	register ObmCallback cb;

	char *resource_buf, *resource_list[MAX_RESOURCES];
	char *appname, *appclass, *resources;
	int sv_argc, nresources;
	char **sv_argv;
	char *ip, *op;

	/* Get arguments. */
	if (argc >= 2) {
	    strcpy (obm->appname, appname = argv[1]);
	    strcpy (obm->appclass, appclass = argv[2]);
	} else {
	    appname = "gterm-iraf";
	    appclass = "Xgterm";
	}

	if (argc >= 3)
	    resources = argv[3];
	else
	    resources = "";

	/* Get fallback resources. */
	resource_buf = op = XtMalloc (strlen(resources) + MAX_RESOURCES);
	resource_list[0] = op;
	nresources = 0;

	for (ip=resources;  *ip;  ip++) {
	    while (*ip && (*ip == ' ' || *ip == '\t'))
		ip++;
	    if (*ip == '\n') {
		;
	    } else if (*ip == '!') {
		while (*ip && *ip != '\n')
		    ip++;
	    } else {
		while (*ip && *ip != '\n')
		    *op++ = *ip++;
		*op++ = '\0';
		nresources++;
		resource_list[nresources] = op;
	    }
	}
	*op++ = '\0';
	resource_list[nresources] = NULL;

	/* Set fallback resources. */
	XtAppSetFallbackResources (obm->app_context, resource_list);

	/* Get local copy of argc and argv. */
	if ((sv_argc = obm->argc) > 0) {
	    sv_argv = (char **) XtMalloc (obm->argc * sizeof(char *));
	    memmove (sv_argv, obm->argv, obm->argc * sizeof(char *));
	} else
	    sv_argv = obm->argv;

	/* Open the display (initializes the resource database).  A separate
	 * display descriptor is used so that we can specify the application
	 * name, class, and resources independently from those of the
	 * application using the object manager.
	 */
	obm->display = XtOpenDisplay (obm->app_context, (String)NULL,
	    appname, appclass, NULL, 0, &sv_argc, sv_argv);
	if (obm->display == (Display *)NULL)
	    XtAppError (obm->app_context, "appInitialize: Can't open display.");

	if (obm->debug > 1)
	    XSynchronize (obm->display, True);

	/* Create the top level shell. */
	obm->toplevel = XtAppCreateShell (appname, appclass,
	    applicationShellWidgetClass, obm->display, (ArgList)NULL, 0);
	obm->screen = XtScreen (obm->toplevel);

	/* Call the client's display connection callback if any. */
	for (cb = obm->callback_list;  cb;  cb = cb->next)
	    if ((cb->callback_type & OBMCB_connect) && cb->u.fcn)
		(*cb->u.fcn) (cb->client_data, obm->display, obm->toplevel, 1);

	/* Add the toplevel shell to the application's object list. */
	obmNewObject (obm, "toplevel", "TopLevelShell", NULL, NULL, 0);

	if (obm->argc > 0)
	    XtFree ((char *)sv_argv);
	XtFree ((char *)resource_buf);
	XtAppSetFallbackResources (obm->app_context, NULL);

	obm->specified++;
	return (TCL_OK);
}


/* serverAppExtend -- TCL command to extend the application resource database
 * to allow for the creation of new widgets loaded since the application was
 * first started.  The 'overwrite' option, if present, says to allow the new
 * resource strings to overwrite the existing resources, otherwise the older
 * ones will not be changed.
 *
 *   Usage:	appExtend new-resources [overwrite]
 */
static int
serverAppExtend (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;
	XrmDatabase old_db, extended_db;
	Boolean overwrite = False;
	char *resources;


	if (!obm->specified || !obm->display || argc < 2)
	    return (TCL_ERROR);

	/* Get arguments. */
	resources = argv[1];
        overwrite = (argc > 2) ? (strcmp (argv[2], "overwrite") == 0) : False;

	/* Get the current fallback resource database. */
	old_db = XrmGetDatabase (obm->display);
	if (old_db == (XrmDatabase) NULL)
	    return (TCL_ERROR);

	/* Create a database structure from the resource string. */
	extended_db = XrmGetStringDatabase (resources);
	if (extended_db == (XrmDatabase) NULL)
	    return (TCL_ERROR);

	/* Combine the old an new databases. */
	XrmCombineDatabase (extended_db, &old_db, overwrite);

	/* Update the application resource database. */
	XrmSetDatabase (obm->display, old_db);

	return (TCL_OK);
}


/* serverCreateObjects -- TCL command to create the tree of UI objects
 * comprising the user interface.  The object tree is defined by a string
 * valued resource.  If no resource is named the default "objects" resource
 * will be used.
 *
 *   Usage:	createObjects [resource-name]
 */
static int
serverCreateObjects (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register char *ip, *op;
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;
	char name[SZ_NAME], class[SZ_NAME], parent[SZ_NAME];
	char *objects = NULL;
	XtResource r;

	r.resource_name   = (argc >= 2) ? argv[1] : "objects";
	r.resource_class  = "Objects";
	r.resource_type   = XtRString;
	r.resource_size   = sizeof (char *);
	r.resource_offset = 0;
	r.default_type    = XtRString;
	r.default_addr    = (caddr_t) NULL;

	/* Get the UI object list. */
	XtGetApplicationResources (obm->toplevel, &objects, &r, 1, NULL, 0);
	/* XrmPutFileDatabase (obm->display->db, "zz.list"); */

	/* Parse the objects list and create the objects.  Each entry has
	 * the form "parent object-class object-name" with a newline (or
	 * other whitespace) terminating each entry.
	 */
	for (ip = objects;  ip && *ip;  ) {
	    /* Get name of parent object. */
	    while (isspace (*ip))
		ip++;
	    for (op=parent;  *ip && !isspace(*ip);  )
		*op++ = *ip++;
	    *op = '\0';

	    /* Get object class. */
	    while (isspace (*ip))
		ip++;
	    for (op=class;  *ip && !isspace(*ip);  )
		*op++ = *ip++;
	    *op = '\0';

	    /* Get object name. */
	    while (isspace (*ip))
		ip++;
	    for (op=name;  *ip && !isspace(*ip);  )
		*op++ = *ip++;
	    *op = '\0';

	    /* Create the new object. */
	    if (*name && *class && *parent)
		obmNewObject (obm, name, class, parent, NULL, 0);

	    while (isspace (*ip))
		ip++;
	}

	return (TCL_OK);
}


/* serverDestroyObject -- Destroy an object and all of its children.
 *
 *   Usage:	destroyObject object-name
 */
static int
serverDestroyObject (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;
	char *object_name;
	ObmObject killobj;

	if (argc < 2)
	    return (TCL_ERROR);

	object_name = argv[1];
	if ((killobj = obmFindObject (obm, object_name)) == NULL)
	    return (TCL_ERROR);
	obmDestroyObject (obm, killobj);

	return (TCL_OK);
}


/* serverQueryObject -- Test if the named object exists.
 *
 *   Usage:	queryObject object-name [class [subclass]]
 *
 * A nonzero function value is returned if the named object exists.  The
 * class and subclass of the object are optionally returned in the output
 * variables given on the command line.
 */
static int
serverQueryObject (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;
	char *object_name, *s_class, *s_subclass;
	ObjClassRec classrec;
	ObmObject o;

	if (argc < 2)
	    return (TCL_ERROR);

	object_name = argv[1];
	s_class = (argc > 2) ? argv[2] : NULL;
	s_subclass = (argc > 3) ? argv[3] : NULL;

	if (o = obmFindObject (obm, object_name)) {
	    classrec = o->core.classrec;
	    if (s_class) {
		BaseClassRec bp;
		int i;

		for (i=0;  i < OtNClasses;  i++) {
		    bp = &baseClasses[i];
		    if (bp->class == classrec->object_type) {
			Tcl_SetVar (obm->tcl, s_class, bp->name, 0);
			break;
		    }
		}
	    }
	    if (s_subclass)
		Tcl_SetVar (obm->tcl, s_subclass,
		    (classrec->object_type == OtShell) ?
		    "Shell" : classrec->name, 0);

	    Tcl_SetResult (obm->tcl, TRUESTR, TCL_STATIC);
	} else
	    Tcl_SetResult (obm->tcl, FALSESTR, TCL_STATIC);

	return (TCL_OK);
}


/* serverActivate -- Activate the user interface.  When called the first
 * time the user interface is created and activated, thereafter the UI is
 * merely reactivated (e.g. mapped if unmapped).
 *
 *  Usage:	activate
 */
static int 
serverActivate (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm; 
	XWMHints hints;  
	register int i;
	ObmObject child; 
	Widget    w;

 
	/* Activate the interface. */
        ObmActivate (obm);


	/* Now set the WM hints for the toplevel shell and any subwindows
	 * of the UI.  Certain ICCCM-compliant window managers make assumptions
	 * about how the client windows will handle input focus if it's not
	 * set explicitly, and often the UI is not given the focus.
	 */
	hints.flags = InputHint | StateHint;
	hints.input = True;
	hints.initial_state = NormalState;
	hints.icon_pixmap = None;
	hints.icon_window = None;
	hints.icon_x = hints.icon_y = 0;
	hints.icon_mask = None;
	hints.window_group = None;
 
	XSetWMHints(obm->display, XtWindow(obm->toplevel), &hints);
 
	obj = (ServerObject) obmFindObject (obm, "toplevel");
	for (i=0;  i < obj->core.nchildren;  i++) {
	    child = obj->core.children[i];
	    if (child->core.classrec->object_type == OtShell) {
		w = widgetGetPointer (child);
		XSetWMHints(obm->display, XtWindow(w), &hints);
	    }
	}
 
	return (TCL_OK);
}


/* serverDeactivate -- Deactivate the user interface.  Optionally unmaps the
 * UI and calls the Obm client back to let it know that the UI has been
 * deactivated.
 *
 *  Usage:	deactivate [unmap]
 */
static int 
serverDeactivate (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;

	ObmDeactivate (obm, argc >=2 && strcmp(argv[1],"unmap") == 0);
	return (TCL_OK);
}


/* serverSynchronize -- Synchronize the user interface.
 *
 *  Usage:	synchronize
 *
 * Any buffered output to the display is flushed and execution pauses until
 * the display has caught up.  It is rarely necessary to sychronize the
 * display with the client and this defeats the purpose of command buffering,
 * hence should be done only when necessary.  Try "flush" below first.
 */
static int 
serverSynchronize (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;

	XSync (obm->display, False);
	while (XtAppPending (obm->app_context))
	    XtAppProcessEvent (obm->app_context, XtIMAll);

	return (TCL_OK);
}


/* serverFlush -- Flush output to the user interface.
 *
 *  Usage:	flush
 *
 * Any buffered output to the display is flushed to the display and
 * execution continues.
 */
static int 
serverFlush (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;

	XFlush (obm->display);

	return (TCL_OK);
}


/* serverPostActivateCallback -- Post a callback procedure to be called
 * when the UI is activated.  The UI is activated when it is first
 * downloaded to server, but it may also be activated (reactivated) after
 * the application has exited and is later restarted, or when the UI
 * is deactivated and reactivated.  Note that the UI state vis-a-vis the
 * external world (client application) may no longer be accurate after
 * it has been idle for a time and then reactivated.
 *
 *  Usage:	postActivateCallback <procedure>
 */
static int 
serverPostActivateCallback (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;
	register ObmCallback cb;

	if (argc < 2)
	    return (TCL_ERROR);
	if (!(cb = obmAddCallback (&obm->callback_list)))
	    return (TCL_ERROR);

	cb->callback_type = OBMUI_activate;
	strncpy (cb->name, argv[1], SZ_NAME);

	return (TCL_OK);
}


/* serverPostDeactivateCallback -- Post a callback procedure to be called
 * when the UI is deactivated.
 *
 *  Usage:	postDeactivateCallback <procedure>
 */
static int 
serverPostDeactivateCallback (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;
	register ObmCallback cb;

	if (argc < 2)
	    return (TCL_ERROR);
	if (!(cb = obmAddCallback (&obm->callback_list)))
	    return (TCL_ERROR);

	cb->callback_type = OBMUI_deactivate;
	strncpy (cb->name, argv[1], SZ_NAME);

	return (TCL_OK);
}


/* serverSend -- Send a message to an object.  The object interprets the 
 * message and returns a function value as the string result of the TCL
 * command.
 *
 *  Usage:	send <object> <message>
 */
static int 
serverSend (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;
	int status;

	/* The object which interprets the message leaves the string result,
	 * if any, directly in the server tcl result string.
	 */
	if (argc == 3)
	    status = ObmDeliverMsg (obm, argv[1], argv[2]);
	else {
	    char *message = Tcl_Merge (argc-2, &argv[2]);
	    status = ObmDeliverMsg (obm, argv[1], message);
	    free ((char *)message);
	}
	
	return (status);
}


/* serverPrint -- Print a string on the standard output.  This is used mainly
 * for debugging user interfaces.
 *
 *  Usage:	print arg [arg ...]
 */
static int 
serverPrint (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;

	if (argc >= 2) {
	    char *message = Tcl_Concat (argc-1, &argv[1]);
	    printf ("%s\n", message);
	    fflush (stdout);
	    free ((char *)message);
	}
	
	return (TCL_OK);
}


/* serverReset -- The "reset-server" command is implemented as a special
 * case in ServerEvaluate.  After doing a true reset ServerEvaluate calls
 * Tcl_Eval to evaluate the full message which still contains the reset-server
 * command.  We want to ignore this the second time, so we treat the command
 * here as a no-op.
 *
 *  Usage:	reset-server
 *
 * Note: for reset-server to be recognized by ServerEvaluate and really reset
 * things, it must be the first command in a message to the server.
 */
static int 
serverReset (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	return (TCL_OK);
}


/* serverGetResource -- Get the string value of the specified application
 * resource (window system parameter).  This allows use of the resource
 * mechanism to supply default values for GUI parameters.
 *
 * Usage:	value = getResource resource-name [class [default-value]]
 *
 * In the simplest case one merely requests a resource by name and the
 * string value is returned as the function value.  If the resource has
 * an entry in the fallback resources for the application (appInitialize
 * resource list) then a value is guaranteed to be returned.
 *
 * If the Class name for the resource is given then a class default value
 * will be returned if no entry is found for the name resource instance.
 * This is useful when there are a number of resources of the same type
 * (same class).  If most or all resources in the same class have the same
 * default value one need only make one entry for the Class in the application
 * defaults resource list.  It is up to the application developer to define
 * the class name of a resource - the class name can be any string.  Examples
 * are "Font", "Cursor", etc.  By convention the first character of a class
 * name is capitalized, while instance names begin with a lower case letter.
 *
 * If there is an entry for the named resource in the resource list passed
 * to appInitialize then a value string is guaranteed to be returned.  This
 * will be either the appInitialize default, or a value specified by the
 * system or the user in an X resources file.  If one is not certain a
 * default value is defined somewhere, a default value should be specified
 * in the getResource call as shown above.
 *
 * See also getResources, used to get multiple resources in one call.
 */
static int 
serverGetResource (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;
	char *resource_name, *class_name, *default_value;
        char *value = NULL;
        XtResource r;

	if (argc < 2)
	    return (TCL_ERROR);

	resource_name = argv[1];
	class_name = (argc > 2) ? argv[2] : XtCString;
	default_value = (argc > 3) ? argv[3] : "";

        r.resource_name   = resource_name;
        r.resource_class  = class_name;
        r.resource_type   = XtRString;
        r.resource_size   = sizeof (char *);
        r.resource_offset = 0;
        r.default_type    = XtRString;
        r.default_addr    = (caddr_t) default_value;

        XtGetApplicationResources (obm->toplevel, &value, &r, 1, NULL, 0);
	Tcl_SetResult (tcl, value, TCL_VOLATILE);

	return (TCL_OK);
}


/* serverGetResources -- Get the string values of a list of resources.
 *
 * Usage:	getResources resource-list
 *   e.g.
 *		getResources {
 *		    { resource   [variable   class  [default-value]]] }
 *		    { resource   [variable   class  [default-value]]] }
 *			(etc.)
 *		}
 *
 * The resource list is a list of resource descriptions.  Each resource
 * entry must give at least the resource name.  If no Tcl variable is named
 * the resource name will be used and this variable will be set to the
 * resource value.  The class name and default value fields are optional.
 */
static int 
serverGetResources (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;
	register XtResource *r;
        XtResource resources[MAX_RESOURCES];
	char *resource_name, *class_name, *default_value;
	char *resource_list, *variable;
	char **items, **fields;
	int nitems, nfields;
	char buf[SZ_NUMBER];
	int item, i;

	typedef struct {
	    char *variable;
	    char *value;
	    char *item_list;
	} Value;
	Value values[MAX_RESOURCES];

	if (argc < 2) {
	    tcl->result = "missing resource-list argument";
	    return (TCL_ERROR);
	} else
	    resource_list = argv[1];

	if (Tcl_SplitList (tcl, resource_list, &nitems, &items) != TCL_OK) {
	    tcl->result = "could not parse resource list";
	    return (TCL_ERROR);
	} else if (nitems > MAX_MENUITEMS)
	    nitems = MAX_MENUITEMS;

	for (item=0;  item < nitems;  item++) {
	    if (Tcl_SplitList (tcl, items[item], &nfields, &fields) != TCL_OK) {
err:		sprintf (buf, "bad item '%d' in resource list", item + 1);
		Tcl_AppendResult (tcl, buf, NULL);
		for (i=0;  i < item;  i++)
		    free (values[item].item_list);
		return (TCL_ERROR);
	    }

	    if (nfields < 1)
		goto err;

	    resource_name = fields[0];
	    variable = (nfields > 1) ? fields[1] : fields[0];
	    class_name = (nfields > 2) ? fields[2] : XtCString;
	    default_value = (nfields > 3) ? fields[3] : "";

	    r = &resources[item];
	    r->resource_name   = resource_name;
	    r->resource_class  = class_name;
	    r->resource_type   = XtRString;
	    r->resource_size   = sizeof (char *);
	    r->resource_offset = (unsigned int) &(((Value *)NULL)[item].value);
	    r->default_type    = XtRString;
	    r->default_addr    = (caddr_t) default_value;

	    values[item].variable = variable;
	    values[item].item_list = (char *) fields;
	}

        XtGetApplicationResources (obm->toplevel,
	    (XtPointer) values, resources, nitems, NULL, 0);

	for (item=0;  item < nitems;  item++) {
	    if (Tcl_SetVar (tcl,
		    values[item].variable, values[item].value, 0) == NULL) {
		fprintf (stderr,
		    "Warning (getResources): cannot set value of %s\n",
		    values[item].variable);
	    }
	    free (values[item].item_list);
	}
	free ((char *) items);

	return (TCL_OK);
}


/* serverPostTimedCallback -- Post a callback to call the named procedure
 * back after a specified delay in milliseconds.
 *
 * Usage:	id = postTimedCallback procedure msec [client-data]
 *
 * After the specified delay the user callback procedure will be called
 * with client_data (if given) as the single argument.  Only one call will
 * be made; the client must repost the callback in each call if the procedure
 * is to be repeatedly executed.
 *
 * An ID value is returned which may be passed to deleteTimedCallback to
 * delete the timer.  If a zero or negative time interval is requested no
 * timer will be set and zero will be returned as the timer ID.
 *
 */
static int
serverPostTimedCallback (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;
	char *userproc, *client_data;
	unsigned long interval;
	char buf[SZ_NUMBER];
	ServerCallback cb;
	int nchars;

	if (argc < 3)
	    return (TCL_ERROR);

	/* Get arguments. */
	userproc = argv[1];
	interval = atoi (argv[2]);
	client_data = (argc > 3) ? argv[3] : NULL;

	if (interval > 0) {
	    /* Allocate and initialize the callback structure. */
	    nchars = sizeof(serverCallback) + strlen(userproc)+1 +
		(client_data ? strlen(client_data)+1 : 0);
	    if (!(cb = (ServerCallback) XtCalloc (nchars,1)))
		return (TCL_ERROR);

	    cb->obj = (XtPointer) obj;
	    cb->userproc = (char *)cb + sizeof(serverCallback);
	    cb->client_data = client_data ?
		cb->userproc+strlen(userproc)+1 : NULL;
	    cb->callback_type = CB_TIMER;
	    cb->next = NULL;

	    strcpy (cb->userproc, userproc);
	    if (client_data)
		strcpy (cb->client_data, client_data);

	    cb->id.intervalId = XtAppAddTimeOut (obm->app_context,
		interval, serverTimedProc, (XtPointer)cb);
	    link_callback (&obj->server, cb);
	} else
	    cb = NULL;

	sprintf (buf, "0x%lx", cb);
	Tcl_SetResult (tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* serverTimedProc -- Xt callback procedure for interval timers.
 */
static void
serverTimedProc (cb_ptr, id)
XtPointer cb_ptr;
XtIntervalId *id;
{
	register ServerCallback cb = (ServerCallback) cb_ptr;
	register ServerObject obj = (ServerObject) cb->obj;
	ObmContext obm = obj->server.obm;
	int status;

	status = Tcl_VarEval (obm->tcl, 
	    cb->userproc, " ",
	    cb->client_data ? cb->client_data : " ",
	    NULL);
	if (status != TCL_OK) {
	    char *errstr = Tcl_GetVar (obm->tcl, "errorInfo", 0);
	    fprintf (stderr, "Error on line %d in %s: %s\n",
		obm->tcl->errorLine, cb->userproc,
		errstr ? errstr : obm->tcl->result);
	}

	unlink_callback (&obj->server, cb);
/*	XtFree ((char *)cb);*/
}


/* serverDeleteTimedCallback -- Delete a timer callback procedure.  This
 * procedure is typically used to break a timer loop, where the timer
 * procedure repeatedly reposts itself at the end of each interval.
 *
 * Usage:	deleteTimedCallback id
 *
 * The ID string is returned by postTimedCallback when a timer is posted.
 */
static int
serverDeleteTimedCallback (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	ServerCallback cb;
	XtIntervalId id;

	if (argc < 2)
	    return (TCL_ERROR);
	
	if (cb = (ServerCallback) strtol (argv[1], (char **)NULL, 16)) {
	    XtRemoveTimeOut (cb->id.intervalId);
	    unlink_callback (&obj->server, cb);
	    XtFree ((char *)cb);
	}
	return (TCL_OK);
}


/* serverPostWorkProc -- Post a callback for a procedure to be called when
 * the server is idle.  Work procedures are used to perform computations in
 * the background while the user interface remains active and able to respond
 * to input events.  This works only if the user work procedure does its job
 * in small increments, doing only a small amount of processing in each call.
 * The work procedure will be called repeatedly until it returns a status
 * indicating that it has finished its task.
 *
 * Usage:	id = postWorkProc procedure [client-data]
 *
 * When the server has nothing else to do the user work procedure will be
 * called with client_data (if given) as the single argument.  The work
 * procedure should return the string "done" when all processing is finished,
 * or any other string if the procedure is to be called again.
 *
 * An ID value is returned which may be passed to deleteWorkProc to
 * delete the work procedure.
 */
static int
serverPostWorkProc (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	register ObmContext obm = obj->server.obm;
	char *userproc, *client_data;
	char buf[SZ_NUMBER];
	ServerCallback cb;
	int nchars;

	if (argc < 2)
	    return (TCL_ERROR);

	/* Get arguments. */
	userproc = argv[1];
	client_data = (argc > 2) ? argv[2] : NULL;

	/* Allocate and initialize the callback structure. */
	nchars = sizeof(serverCallback) + strlen(userproc)+1 +
	    (client_data ? strlen(client_data)+1 : 0);
	if (!(cb = (ServerCallback) XtMalloc (nchars)))
	    return (TCL_ERROR);

	cb->obj = (XtPointer) obj;
	cb->userproc = (char *)cb + sizeof(serverCallback);
	cb->client_data = client_data ? cb->userproc+strlen(userproc)+1 : NULL;
	cb->callback_type = CB_WORKPROC;
	cb->next = NULL;

	strcpy (cb->userproc, userproc);
	if (client_data)
	    strcpy (cb->client_data, client_data);

	cb->id.workProcId = XtAppAddWorkProc (obm->app_context,
	    serverWorkProc, (XtPointer)cb);
	link_callback (&obj->server, cb);

	sprintf (buf, "0x%lx", cb);
	Tcl_SetResult (tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* serverWorkProc -- Xt callback procedure for work procedures.
 */
static Boolean
serverWorkProc (cb_ptr)
XtPointer cb_ptr;
{
	register ServerCallback cb = (ServerCallback) cb_ptr;
	register ServerObject obj = (ServerObject) cb->obj;
	register ObmContext obm = obj->server.obm;
	Boolean done;
	int status;

	status = Tcl_VarEval (obm->tcl, 
	    cb->userproc, " ",
	    cb->client_data ? cb->client_data : " ",
	    NULL);

	if (status != TCL_OK) {
	    char *errstr = Tcl_GetVar (obm->tcl, "errorInfo", 0);
	    fprintf (stderr, "Error on line %d in %s: %s\n",
		obm->tcl->errorLine, cb->userproc,
		errstr ? errstr : obm->tcl->result);
	    done = True;
	} else
	    done = (strcmp (obm->tcl->result, "done") == 0) ? True : False;

	if (done) {
	    unlink_callback (&obj->server, cb);
	    XtFree ((char *)cb);
	}

	return (done);
}


/* serverDeleteWorkProc -- Delete a work callback procedure.
 *
 * Usage:	deleteWorkProc id
 *
 * The ID string is returned by postWorkProc when a work procedure is
 * posted.
 */
static int
serverDeleteWorkProc (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ServerObject obj = (ServerObject) object;
	ServerCallback cb;
	XtIntervalId id;

	if (argc < 2)
	    return (TCL_ERROR);

	cb = (ServerCallback) strtol (argv[1], (char **)NULL, 16);
	XtRemoveWorkProc (cb->id.workProcId);
	unlink_callback (&obj->server, cb);
	XtFree ((char *)cb);
	return (TCL_OK);
}


/* link_callback -- Link a callback descriptor into the global server
 * callback list.
 */
static void
link_callback (server, cb)
register ServerPrivate server;
register ServerCallback cb;
{
	if (!server->cb_head) {
	    server->cb_head = cb;
	    server->cb_tail = cb;
	} else {
	    server->cb_tail->next = cb;
	    server->cb_tail = cb;
	}
}


/* unlink_callback -- Unlink a callback descriptor from the global server
 * callback list.
 */
static void
unlink_callback (server, cb)
register ServerPrivate server;
register ServerCallback cb;
{
	register ServerCallback cp;

	if (cb == server->cb_head) {
	    if (!(server->cb_head = cb->next))
		server->cb_tail = NULL;
	} else {
	    for (cp = server->cb_head;  cp && cp->next != cb;  cp = cp->next)
		;
	    if (cp) {
		cp->next = cb->next;
		if (cb == server->cb_tail)
		    server->cb_tail = cp;
	    }
	}
}


/* serverCreateBitmap -- Create a named bitmap.  This replaces any old bitmap
 * of the same name.  The new bitmap is cached in server memory; when a widget
 * bitmap resource is set, the bitmap cache will be searched for the named
 * bitmap before asking Xlib to find the bitmap.
 *
 *  Usage:	createBitmap name width height data
 *
 * e.g., 
 *	createBitmap foo 16 16 {
 *	    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0x01,
 *	    0x60,0x03,0x20,0x02,0x60,0x03,0xc0,0x01,0x00,0x00,0x00,0x00,
 *	    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00 }
 */
static int 
serverCreateBitmap (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	ServerObject obj = (ServerObject) object;
	ObmContext obm = (ObmContext) obj->server.obm;
	char *name, *pixels;
	int width, height;
	int status;

	if (argc < 5)
	    return (TCL_ERROR);

	name = argv[1];
	width = atoi (argv[2]);
	height = atoi (argv[3]);
	pixels = argv[4];

	status = createBitmap (obm, name, width, height, pixels);
	return (status == OK ? TCL_OK : TCL_ERROR);
}


/* createBitmap -- Create a bitmap of the indicated size and add it to the
 * pixmap cache.
 */
createBitmap (obm, name, width, height, pixels)
ObmContext obm;
char *name;
int width, height;
char *pixels;
{
	register char *ip, *op;
	register ObjList lp, last_lp;
	char numbuf[32], *data, *dp;
	Icon *icon;
	int nchars;

	if (!obm->specified || !obm->display)
	    return (TCL_ERROR);

	/* Check if bitmap is already in cache. */
	for (last_lp = lp = obm->pixmap_cache;  lp;  lp = lp->next) {
	    if (strcmp (name, lp->name) == 0)
		break;
	    last_lp = lp;
	}

	/* Get an empty bitmap descriptor. */
	if (lp) {
	    if (lp->ptr)
		freeIcon (obm, (Icon *) lp->ptr);
	} else {
	    lp = (ObjList) XtMalloc (sizeof (struct objList));
	    if (last_lp)
		last_lp->next = lp;
	    else
		obm->pixmap_cache = lp;
	    strcpy (lp->name, name);
	    lp->next = NULL;
	}

	/* Get bitmap data. */
	data = (char *) XtCalloc (nchars = (width * height), 1);
	for (dp=data, ip=pixels;  *ip;  ) {
	    while (isspace(*ip) || *ip == ',')
		ip++;
	    for (op=numbuf;  *ip && !(isspace(*ip) || *ip == ',');  )
		*op++ = *ip++;
	    *op++ = '\0';
	    if (--nchars >= 0)
		*dp++ = strtol (numbuf, NULL, 0);
	}

	/* Create the bitmap. */
	if (!(icon = (Icon *) XtCalloc (1, sizeof (*icon))))
	    return (TCL_ERROR);
	icon->pixmap = XCreateBitmapFromData (obm->display,
	    RootWindowOfScreen (obm->screen), data, width, height);
	lp->ptr = (caddr_t) icon;

	XtFree ((char *)data);
	return (OK);
}


/* findBitmap -- Search the bitmap cache for the named bitmap.  Note that
 * a bitmap is a pixmap of depth one, hence bitmaps are stored in the pixmap
 * cache.
 */
Pixmap
findBitmap (obm, name)
ObmContext obm;
char *name;
{
	return (findPixmap (obm, name));
}


/* serverCreatePixmap -- Create a named pixmap.  This replaces any old pixmap
 * of the same name.  The new pixmap is cached in server memory; when a widget
 * pixmap resource is set, the pixmap cache will be searched for the named
 * pixmap before asking Xlib to find the pixmap.
 *
 *  Usage:	createPixmap name width height depth fg_color bg_color data
 *
 * e.g., 
 *	createPixmap foo 16 16 8 black white {
 *	    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xc0,0x01,
 *	    0x60,0x03,0x20,0x02,0x60,0x03,0xc0,0x01,0x00,0x00,0x00,0x00,
 *	    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00 }
 */
static int 
serverCreatePixmap (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	ServerObject obj = (ServerObject) object;
	ObmContext obm = (ObmContext) obj->server.obm;
	int width, height, depth;
	char *name, *pixels;
	unsigned long fg, bg;
	int status;

	if (argc < 8)
	    return (TCL_ERROR);

	name = argv[1];
	width = atoi (argv[2]);
	height = atoi (argv[3]);
	depth = atoi (argv[4]);
	pixels = argv[7];

	/* Get fg_color pixel value. */
	if (isdigit (*argv[5]))
	    fg = strtol (argv[5], NULL, 0);
	else {
	    XrmValue from, to;
	    from.size = strlen (argv[5]) + 1;
	    from.addr = argv[5];
	    to.addr = (caddr_t) &fg;
	    to.size = sizeof(fg);

	    if (!XtConvertAndStore (obm->toplevel,
		XtRString, &from, XtRPixel, &to))
		fg = BlackPixelOfScreen (obm->screen);
	}

	/* Get bg_color pixel value. */
	if (isdigit (*argv[6]))
	    bg = strtol (argv[6], NULL, 0);
	else {
	    XrmValue from, to;
	    from.size = strlen (argv[6]) + 1;
	    from.addr = argv[6];
	    to.addr = (caddr_t) &bg;
	    to.size = sizeof(bg);

	    if (!XtConvertAndStore (obm->toplevel,
		XtRString, &from, XtRPixel, &to))
		bg = WhitePixelOfScreen (obm->screen);
	}

	status = createPixmap (obm, name, width,height,8, NULL, pixels, fg,bg);
	return (status == OK ? TCL_OK : TCL_ERROR);
}


/* createPixmap -- Create a pixmap of the indicated size and add it to the
 * pixmap cache.  If PIXMAP is non-null the existing pixmap is merely entered
 * into the pixmap cache.  Otherwise, if PIXELS is NULL an empty pixmap is
 * created, otherwise  PIXELS points to a character string containing the
 * pixmap data in bitmap format, and BG and FG give the background and
 * foreground colors.
 */
createPixmap (obm, name, width, height, depth, pixmap, pixels, bg, fg)
ObmContext obm;
char *name;
int width, height, depth;
Pixmap pixmap;
char *pixels;
unsigned long fg, bg;
{
	register char *ip, *op;
	register ObjList lp, last_lp;
	char numbuf[32], *data, *dp;
	Icon *icon;
	int nchars;

	if (!obm->specified || !obm->display)
	    return (TCL_ERROR);

	/* Check if pixmap is already in cache. */
	for (last_lp = lp = obm->pixmap_cache;  lp;  lp = lp->next) {
	    if (strcmp (name, lp->name) == 0)
		break;
	    last_lp = lp;
	}

	/* Get an empty pixmap descriptor. */
	if (lp) {
	    if (lp->ptr)
		freeIcon (obm, (Icon *) lp->ptr);
	} else {
	    lp = (ObjList) XtMalloc (sizeof (struct objList));
	    if (last_lp)
		last_lp->next = lp;
	    else
		obm->pixmap_cache = lp;
	    strcpy (lp->name, name);
	    lp->next = NULL;
	}

	if (!(icon = (Icon *) XtCalloc (1, sizeof (*icon))))
	    return (TCL_ERROR);

	/* Get pixmap data. */
	if (pixmap) {
	    icon->pixmap = pixmap;
	} else {
	    if (pixels) {
		data = (char *) XtCalloc (nchars = (width * height), 1);
		for (dp=data, ip=pixels;  *ip;  ) {
		    while (isspace(*ip) || *ip == ',')
			ip++;
		    for (op=numbuf;  *ip && !(isspace(*ip) || *ip == ',');  )
			*op++ = *ip++;
		    *op++ = '\0';
		    if (--nchars >= 0)
			*dp++ = strtol (numbuf, NULL, 0);
		}

		/* Create the pixmap. */
		icon->pixmap = XCreatePixmapFromBitmapData (obm->display,
		    RootWindowOfScreen(obm->screen), data, width,height, fg,bg,
		    depth);

	    } else {
		/* Create the pixmap. */
		icon->pixmap = XCreatePixmap (obm->display,
		    RootWindowOfScreen(obm->screen), width, height, depth);
	    }
	}

	lp->ptr = (caddr_t) icon;
	XtFree ((char *)data);

	return (OK);
}


/* serverCreateXPixmap -- Create a pixmap of the given name.  The pixmap is
 * specified in XPM format which provides much better support for color than
 * the simpler format used by createPixmap.
 *
 * The new pixmap replaces any old pixmap of the same name.  The new pixmap
 * is cached in server memory; when a widget pixmap resource is set, the
 * pixmap cache will be searched for the named pixmap before asking Xlib to
 * find the pixmap.
 *
 *  Usage:	createXPixmap name widget description
 *
 * where "name" is the name of the pixmap to be created, "widget" is the
 * name of a widget object to be used to search for pixel resources (to color
 * the pixmap), and "description" is the XPM format description of the pixmap.
 *
 * For example:
 *
 * createXPixmap empty_diamond font1 {
 *	[* XPM *]
 *	static char * diamond0c [] = {
 *	[* width height ncolors cpp [x_hot y_hot] *]
 *	"17 17 3 1 0 0",
 *	[* colors *]
 *	" 	s none	m none	c none",
 *	".	s topShadowColor	m white	c #c8c8c8c8c8c8",
 *	"X	s bottomShadowColor	m black	c #646464646464",
 *	[* pixels *]
 *	"                 ",
 *	"                 ",
 *	"       ...       ",
 *	"      .. ..      ",
 *	"     ..   ..     ",
 *	"    ..     ..    ",
 *	"   ..       ..   ",
 *	"  ..         ..  ",
 *	"  .           .  ",
 *	"  XX         XX  ",
 *	"   XX       XX   ",
 *	"    XX     XX    ",
 *	"     XX   XX     ",
 *	"      XX XX      ",
 *	"       XXX       ",
 *	"                 ",
 *	"                 "};
 * }
 *
 * In the above the C style comments have been replaced by [* ... *] to avoid
 * prematurely terminating the C comment you are reading.  The actual text
 * input to createXPixmap should use C style comments, exactly as in the XPM
 * file.
 *
 * The pixmap is specified in XPM format as an array of strings.  C style
 * coments, commas, and whitespace are ignored (but are permitted, to allow
 * XPM files to be directly included).  The XPM format is fully defined in
 * the XPM documentation, but there is not really that much to it, so we
 * summarize it here.  The advantage of the XPM format is that it uses a
 * visual ascii representation of the pixmap, and it provides good support
 * for colored pixmaps, using characters to indicate the color.  The fields
 * of the color description are as follows:
 *
 *	character	character used in "pixels" to signify color (arbitrary)
 *	s		resource name used in application to override color
 *	m		default color on monochrome screens
 *	g4		for 4-level grayscale screens
 *	g		for grayscale with more than 4 levels
 *	c		for color screens
 *
 * An important feature of createXPixmap is the reference widget.  When the
 * pixmap is created the resource list of the named reference widget will be
 * searched for any resources that specify colors.  If the resource name for
 * a color resource matches the "s" name given for a color in the XPM pixmap
 * description, then the widget-specific color will be used.  This allows
 * resources to be used to specify the colors for a pixmap on a per-widget
 * basis.  If a dummy widget-object name is given (e.g. "none") or no matching
 * resources are found, the default colors will be used.  Any widget object
 * that has color resources may be used for the reference widget (it doesn't
 * have to be the widget which will later use the pixmap).
 *
 * A pixmap created with createXPixmap may be used with the widget-class "set"
 * command to set the value of any Pixmap or Icon class widget resource.
 */
static int 
serverCreateXPixmap (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	ServerObject obj = (ServerObject) object;
	ObmContext obm = (ObmContext) obj->server.obm;
	char *name, *widget, *description;
	int status;

	if (argc < 4)
	    return (TCL_ERROR);

	name = argv[1];
	widget = argv[2];
	description = argv[3];
	while (isspace (*description))
	    description++;

	status = createXPixmap (obm, name, widget, description);
	return (status == OK ? TCL_OK : TCL_ERROR);
}


/* createXPixmap -- Create a pixmap defined in an XPM format description and
 * add it to the pixmap cache.  DESCRIPTION points to a character string
 * containing the pixmap description in XPM format.  A reference widget object
 * may be given to associate color resources with the pixmap.
 */
createXPixmap (obm, name, widget, description)
ObmContext obm;
char *name;
char *widget;
char *description;
{
	register char *ip, *op;
	register ObjList lp, last_lp;
	XpmImage image;
	ObmObject obj;
	String *data;
	Icon *icon;
	int status;
	Widget w;

	if (!obm->specified || !obm->display)
	    return (TCL_ERROR);

	/* Get reference widget if any. */
	w = NULL;
	if (obj = obmFindObject (obm, widget, obm->toplevel))
	    w = widgetGetPointer (obj);

	/* Create the pixmap (actually icon).
	 */
	status = XpmCreateXpmImageFromBuffer (description, &image, NULL, NULL);
	if (status != XpmSuccess)
	    return (TCL_ERROR);
	XpmCreateDataFromXpmImage (&data, &image, NULL);

	if (data) {
	    static XpmColorSymbol table[MAXCOLORSYM];
	    Cardinal n;

	    if (!(icon = (Icon *) XtCalloc (1, sizeof(*icon))))
		return (TCL_ERROR);

	    build_colorlist (w, table, XtNumber(table), &n);
	    icon->attributes.colorsymbols = table;
	    icon->attributes.numsymbols = n;
	    icon->attributes.valuemask = XpmColorSymbols;

	    XpmCreatePixmapFromData (obm->display,
		RootWindowOfScreen(obm->screen), data,
	        &icon->pixmap, &icon->mask, &icon->attributes);

	    XtFree ((String) data);
/*	    XtFree ((String) table);*/
	    XpmFreeXpmImage (&image);

	} else {
	    XpmFreeXpmImage (&image);
	    return (TCL_ERROR);
	}

	/* Check if pixmap is already in cache. */
	for (last_lp = lp = obm->pixmap_cache;  lp;  lp = lp->next) {
	    if (strcmp (name, lp->name) == 0)
		break;
	    last_lp = lp;
	}

	/* Get an empty pixmap descriptor. */
	if (lp) {
	    if (lp->ptr)
		freeIcon (obm, (Icon *) lp->ptr);
	} else {
	    lp = (ObjList) XtMalloc (sizeof (struct objList));
	    if (last_lp)
		last_lp->next = lp;
	    else
		obm->pixmap_cache = lp;
	    strcpy (lp->name, name);
	    lp->next = NULL;
	}

	lp->ptr = (caddr_t) icon;
	return (OK);
}


/* build_colorlist -- Get a list of all the color resources defined by a
 * widget.  This looks through all the resources for resources that specify
 * a color (Pixel). All such resources and their values are entered in the
 * output table.
 *
 * To get at the resource value, the resource_offset (an unsigned int) must be
 * added to the base address of the widget.  The widget pointer is first
 * converted to an unsigned long, tehn the offset is added to it and the result
 * is converted back to a pointer, in this case a pointer to a Pixel.
 *
 * This code is based on build_colortable from icon.c in the FWF sources.
 */
static void
build_colorlist (w, table, size, n)
Widget w;
register XpmColorSymbol *table;
Cardinal size;
Cardinal *n;
{
    Cardinal nres, i;
    XtResourceList res;

    *n = 0;
    XtGetResourceList (XtClass(w), &res, &nres);
    for (i=0;  i < nres;  i++)
        if (strcmp(res[i].resource_type, XtRPixel) == 0 && *n < size) {
            table[*n].name = res[i].resource_name;
            table[*n].value = NULL;
            table[*n].pixel =
                * (Pixel*) ((unsigned long) w + res[i].resource_offset);
            (*n)++;
        }
    if (res)
        XtFree ((char *)res);					/* MF037 */
}


/* findPixmap -- Search the pixmap cache for the named pixmap.
 */
Pixmap
findPixmap (obm, name)
ObmContext obm;
char *name;
{
	register ObjList lp;

	for (lp = obm->pixmap_cache;  lp;  lp = lp->next)
	    if (lp->ptr && strcmp (name, lp->name) == 0)
		return (((Icon *)lp->ptr)->pixmap);

	return ((Pixmap) NULL);
}


/* findIcon -- Search the pixmap cache for the named icon.
 */
Icon *
findIcon (obm, name)
ObmContext obm;
char *name;
{
	register ObjList lp;

	for (lp = obm->pixmap_cache;  lp;  lp = lp->next)
	    if (lp->ptr && strcmp (name, lp->name) == 0)
		return ((Icon *) lp->ptr);

	return ((Icon *) NULL);
}


/* freeIcon -- Free an icon descriptor (pixmap list).
 */
void
freeIcon (obm, icon)
register ObmContext obm;
register Icon *icon;
{
	if (icon->pixmap)
	    XFreePixmap (obm->display, icon->pixmap);
	if (icon->mask)
	    XFreePixmap (obm->display, icon->mask);
	XtFree ((char *) icon);
}


/* serverCreateCursor -- Create a cursor from bitmap data.  The cursor is
 * entered into the server's cursor cache and will override any existing
 * entry of the same name.
 *
 *  Usage:	createCursor name source mask fg_color bg_color x_hot y_hot
 * e.g., 
 *	createCursor foo bitmap1 bitmap2 black white 8 8
 *
 * The named bitmaps must be created first with createBitmap.
 */
static int 
serverCreateCursor (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	ServerObject obj = (ServerObject) object;
	ObmContext obm = (ObmContext) obj->server.obm;
	register ObjList lp, last_lp;
	XColor fg_color, bg_color;
	unsigned long fg, bg;
	Pixmap source, mask;
	Colormap colormap;
	int x_hot, y_hot;
	char *name;

	if (!obm->specified || !obm->display)
	    return (TCL_ERROR);

	if (argc < 8)
	    return (TCL_ERROR);

	name = argv[1];
	source = findPixmap (obm, argv[2]);
	mask = findPixmap (obm, argv[3]);
	x_hot = atoi (argv[6]);
	y_hot = atoi (argv[7]);

	if (!source)
	    return (TCL_ERROR);

	colormap = XDefaultColormapOfScreen (obm->screen);

	/* Get fg_color pixel value. */
	if (isdigit (*argv[4]))
	    fg = strtol (argv[4], NULL, 0);
	else {
	    XrmValue from, to;
	    from.size = strlen (argv[4]) + 1;
	    from.addr = argv[4];
	    to.addr = (caddr_t) &fg;
	    to.size = sizeof(fg);

	    if (!XtConvertAndStore (obm->toplevel,
		XtRString, &from, XtRPixel, &to))
		fg = BlackPixelOfScreen (obm->screen);

	    fg_color.pixel = fg;
	    XQueryColor (obm->display, colormap, &fg_color);
	}

	/* Get bg_color pixel value. */
	if (isdigit (*argv[5]))
	    bg = strtol (argv[5], NULL, 0);
	else {
	    XrmValue from, to;
	    from.size = strlen (argv[5]) + 1;
	    from.addr = argv[5];
	    to.addr = (caddr_t) &bg;
	    to.size = sizeof(bg);

	    if (!XtConvertAndStore (obm->toplevel,
		XtRString, &from, XtRPixel, &to))
		bg = WhitePixelOfScreen (obm->screen);

	    bg_color.pixel = bg;
	    XQueryColor (obm->display, colormap, &bg_color);
	}

	/* Check if cursor is already in cache. */
	for (last_lp = lp = obm->cursor_cache;  lp;  lp = lp->next) {
	    if (strcmp (name, lp->name) == 0)
		break;
	    last_lp = lp;
	}

	/* Get an empty cursor descriptor. */
	if (lp) {
	    if (lp->ptr)
		XFreeCursor (obm->display, (Cursor)lp->ptr);
	} else {
	    lp = (ObjList) XtMalloc (sizeof (struct objList));
	    if (last_lp)
		last_lp->next = lp;
	    else
		obm->cursor_cache = lp;
	    strcpy (lp->name, name);
	    lp->next = NULL;
	}

	/* Create the cursor. */
	lp->ptr = (caddr_t) XCreatePixmapCursor (obm->display,
	    source, mask, &fg_color, &bg_color, x_hot, y_hot);

	return (TCL_OK);
}


/* findCursor -- Search the cursor cache for the named cursor.
 */
Cursor
findCursor (obm, name)
ObmContext obm;
char *name;
{
	register ObjList lp;

	for (lp = obm->cursor_cache;  lp;  lp = lp->next)
	    if (lp->ptr && strcmp (name, lp->name) == 0)
		return ((Cursor) lp->ptr);

	return ((Cursor) NULL);
}


/* serverCreateMenu, serverEditMenu -- Create or modify a menu.
 * The editMenu function is an alias for createMenu.
 *
 * Usage:	createMenu menu-name parent item-list
 *
 *  e.g.,	createMenu menu-name parent {
 *		    { label function data [options...] }
 *		    { label function data [options...] }
 *				(etc.)
 *		}
 * where
 *
 *	menu-name	is the object name for the menu popup shell
 *	parent		is the parent widget of the menu shell
 *
 *	label		is a menu item label
 *
 *	function	is the function to be performed when the menu
 *			item is selected, e.g., f.exec, f.data, f.space,
 *			or f.line.
 *
 *	data		is function dependent data
 *
 *	options		are option-name option-value pairs, as specified
 *			below.
 *
 * In the item list the fields label and option-value may be any Tcl
 * expression.  Expressions are evaluated in the server context.   The data
 * field is a Tcl script to be executed when the menu item is selected.
 *
 * Options are specified as "option option-value".  The menu item options
 * are as follows.
 *
 *	foreground	Foreground color.
 *
 *	background	Background color.
 *
 *	bitmap		A bitmap to be displayed left justified in the
 *			label field (e.g. to indicate a parameter setting).
 *
 *	justify		Type of text alignment: left, center, right.
 *
 *	sensitive	Specifies whether the menu item is active (sensitive=
 *			true) or inactive (sensitive=false, item grayed out).
 *
 *	accelerator	Specifies an input translation (accelerator, e.g.,
 *			keyboard event) which can be used to execute the
 *			menu item.
 *
 * The option-value field may be any Tcl expression.
 *
 * Example:	createMenu fileMenu toplevel {
 *		    { "File Menu"  f.title		}
 *		    { Open         f.exec  openFile	}
 *		    { Save         f.exec  saveFile	}
 *		    { Load         f.menu  loadMenu	}
 *		    { no-label     f.line          	}
 *		    { Quit         f.exec  "send client Quit" }
 *		}
 *
 * The first createMenu is called for a given menu the menu is created,
 * added to the menu list, and all window system widgets are created for
 * the menu.  Subsequent calls will result in only the changed parts of the
 * menu being altered provided the changes are not great.  Hence this routine
 * can be called to efficiently modify a menu when minor runtime changes
 * occur, e.g., an item label or action changes, the item value changes state,
 * and so on, without need for the GUI code to know how to make the necessary
 * detailed changes to the widgets used to implement the menu.
 */
static int 
serverCreateMenu (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	ServerObject obj = (ServerObject) object;
	ObmContext obm = (ObmContext) obj->server.obm;
	register MenuPtr mp, o_mp;
	register MenuItem ip;
	register ObjList lp, newobj;
	char *menu_name, *menu_label;
	char *parent, *item_list;
	char **items, **fields;
	int nitems, nfields;
	int field, item;
	ObmObject pobj;
	Widget w, pw;

	if (obm->being_destroyed)
	    return (TCL_OK);
	if (!obm->specified || !obm->display)
	    return (TCL_ERROR);
	if (argc < 4)
	    return (TCL_ERROR);

	menu_name  = argv[1];
	parent     = argv[2];
	item_list  = argv[3];

	/* Locate the parent widget. */
	if ((pobj = (ObmObject) obmFindObject (obm, parent)) == NULL) {
	    fprintf (stderr, "obm: cannot find parent widget %s for menu %s\n",
		parent, menu_name);
	    return (TCL_ERROR);
	} else
	    pw = widgetGetPointer (pobj);

	/* Ignore request if parent is being destroyed. */
	if (pobj->core.being_destroyed)
	    return (TCL_OK);

	/* Get the list of menu item specifier strings. */
	if (Tcl_SplitList (tcl, item_list, &nitems, &items) != TCL_OK) {
	    fprintf (stderr, "obm: error parsing menu for %s\n", menu_name);
	    return (TCL_ERROR);
	} else if (nitems > MAX_MENUITEMS)
	    nitems = MAX_MENUITEMS;

	/* Allocate a new, empty menu descriptor. */
	mp = (MenuPtr) XtCalloc (1, sizeof (Menu));

	/* Process each item and add it to the menu descriptor. */
	for (item=0;  item < nitems;  item++) {
	    if (Tcl_SplitList (tcl, items[item], &nfields, &fields) != TCL_OK) {
		fprintf (stderr, "obm: error parsing menu item %d of %s\n",
		    item + 1, menu_name);
		continue;
	    }

	    ip = &mp->items[mp->nitems++];

	    /* The first three fields label,type,data have a fairly strict
	     * syntax and must be in order.  Try to interpret the label field
	     * as a string expression; if this fails, assume it is a literal
	     * label string.
	     */
	    field = 0;
	    if (strncmp (fields[field], "f.", 2) == 0)
		ip->label = NULL;
	    else {
		char *cp = fields[field++];

		if (Tcl_ExprString (tcl, cp) != TCL_OK)
		    ip->label = cp;
		else {
		    ip->label = XtMalloc (strlen(tcl->result) + 1);
		    strcpy (ip->label, tcl->result);
		    ip->flags |= M_FreeLabel;
		}
	    }

	    /* Determine menu item type. */
	    if (strcmp (fields[field], "f.exec") == 0) {
		ip->type = MI_EXEC;
		ip->data = fields[++field];
	    } else if (strcmp (fields[field], "f.line") == 0) {
		ip->type = MI_LINE;
		ip->data = NULL;
	    } else if (strcmp (fields[field], "f.dblline") == 0) {
		ip->type = MI_DBLLINE;
		ip->data = NULL;
	    } else if (strcmp (fields[field], "f.menu") == 0) {
		ip->type = MI_MENU;
		ip->data = fields[++field];
	    } else if (strcmp (fields[field], "f.space") == 0) {
		ip->type = MI_SPACE;
		ip->data = fields[++field];
	    } else if (strcmp (fields[field], "f.title") == 0) {
		ip->type = MI_TITLE;
		ip->data = NULL;
	    } else {
		fprintf (stderr, "obm: bad menu item type `%s'\n",
		    fields[field]);
		ip->type = MI_IGNORE;
		ip->data = NULL;
	    }
	    field++;

	    /* Process any optional menu item attributes. */
	    for (  ;  field < nfields;  field++) {
		if (strcmp (fields[field], "background") == 0) {
		    ip->background = fields[++field];

		} else if (strcmp (fields[field], "foreground") == 0) {
		    ip->foreground = fields[++field];

		} else if (strcmp (fields[field], "bitmap") == 0) {
		    char *cp = fields[++field];

		    if (Tcl_ExprString (tcl, cp) != TCL_OK)
			ip->pixmap = findBitmap (obm, cp);
		    else
			ip->pixmap = findBitmap (obm, tcl->result);

		} else if (strcmp (fields[field], "justify") == 0) {
		    char *justify = fields[++field];
		    if (strcmp (justify, "left") == 0)
			ip->justify = XtJustifyLeft;
		    else if (strcmp (justify, "center") == 0)
			ip->justify = XtJustifyCenter;
		    else if (strcmp (justify, "right") == 0)
			ip->justify = XtJustifyRight;

		} else if (strcmp (fields[field], "sensitive") == 0) {
		    int ch = fields[++field][0];
		    int bval;

		    if (ch == 'f' || ch == 'F')
			ip->flags |= M_Insensitive;
		    else if (ch == 't' || ch == 'T')
			ip->flags &= ~M_Insensitive;
		    else {
			if (Tcl_ExprBoolean (tcl,
				fields[field], &bval) != TCL_OK) {
			    fprintf (stderr,
				"menu %s.%d sensitive option: %s\n",
				menu_name, item, tcl->result);
			} else if (!bval)
			    ip->flags |= M_Insensitive;
		    }

		} else if (strncmp (fields[field], "accelerator", 5) == 0) {
		    ip->accelerator = fields[++field];

		} else {
		    fprintf (stderr, "obm: bad menu item parameter `%s'\n",
			fields[field]);
		}
	    }

	    /* Save the string buffer pointer to be freed by menuFree. */
	    ip->sbuf = (char *) fields;
	}

	/* Free list of menu item specification strings. */
	free ((char *) items);

	/* Search the menu list and see if there is already a menu with
	 * the given menu name.
	 */
	for (lp = obm->menu_list, o_mp=NULL;  lp;  lp = lp->next)
	    if (strcmp (lp->name, menu_name) == 0) {
		o_mp = (MenuPtr) lp->ptr;
		break;
	    }

	/* If the menu already exists try to edit it, otherwise delete any
	 * existing menu and create a new one from scratch.
	 */
	if (o_mp && editMenu (o_mp, mp) == 0) {
	    /* The edit succeeded.  Discard the request descriptor. */
	    freeMenu (mp);

	} else if (o_mp) {
	    /* Replace an existing menu. */
	    obmDestroyObject (obm, o_mp->obj);
	    freeMenu (o_mp);
	    createMenu (obm, mp, menu_name, parent, pw);
	    lp->ptr = (caddr_t) mp;

	} else {
	    /* Create a new menu. */
	    createMenu (obm, mp, menu_name, parent, pw);

	    newobj = (ObjList) XtMalloc (sizeof (struct objList));
	    strcpy (newobj->name, menu_name);
	    newobj->ptr = (caddr_t) mp;
	    newobj->next = NULL;

	    if (obm->menu_list == NULL)
		obm->menu_list = newobj;
	    else {
		for (lp = obm->menu_list;  lp->next;  lp = lp->next)
		    ;
		lp->next = newobj;
	    }
	}

	return (TCL_OK);
}


/* serverDestroyMenu -- Destroy a menu.  This can be used to free up the
 * resources used by a menu, e.g., if the menu is not expected to be needed
 * again for a while.
 *
 *  Usage:	destroyMenu menu-name
 */
static int 
serverDestroyMenu (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	ServerObject obj = (ServerObject) object;
	ObmContext obm = (ObmContext) obj->server.obm;
	register ObjList lp, lpp;
	register MenuPtr mp;
	char *menu_name;

	if (argc < 2)
	    return (TCL_ERROR);
	else
	    menu_name = argv[1];

	/* Locate the menu descriptor. */
        for (lp = obm->menu_list, lpp=NULL, mp=NULL;  lp;  lpp=lp, lp=lp->next)
            if (strcmp (lp->name, menu_name) == 0) {
                mp = (MenuPtr) lp->ptr;
                break;
            }
	if (mp == NULL)
	    return (TCL_OK);

	/* Unlink the menu from the menu list. */
	if (lpp == NULL)
	    obm->menu_list = lp->next;
	else
	    lpp->next = lp->next;
	    
	/* Destroy the menu object and any descendents. */
	obmDestroyObject (obm, mp->obj);
	freeMenu (mp);

	return (TCL_OK);
}


/* createMenu -- Create a new menu from a menu descriptor.
 */
static void
createMenu (obm, mp, menu_name, parent, pw)
ObmContext obm;
register MenuPtr mp;
char *menu_name;
char *parent;
Widget pw;
{
	register MenuItem ip;
	int itemno, menuno, lineno, spaceno;
	Widget menu, entry;
	char name[SZ_NAME];
	XrmValue from, to[2];
	Pixel value[2];
	Arg args[10];
	int nargs, i;

	menu_classInit();

	/* The following resources are statically defined for all menus. */
	nargs = 0;
	if (mp->items[0].type == MI_TITLE) {
	    XtSetArg (args[nargs], XtNlabel, mp->items[0].label);
	    nargs++;
	}

	/* Create the menu shell. */
	obmNewObject (obm, menu_name, "SimpleMenu", parent, args, nargs);
	mp->obj = obmFindObject (obm, menu_name);
	mp->menuShell = menu = widgetGetPointer (mp->obj);
	mp->obm = (XtPointer) obm;

	XtAddCallback (menu, XtNpopupCallback, menu_popup, (XtPointer)mp);
	XtAddCallback (menu, XtNpopdownCallback, menu_popdown, (XtPointer)mp);

	ip = &mp->items[0];
	itemno = menuno = lineno = spaceno = 1;

	/* Create each menu item. */
	for (i=0;  i < mp->nitems;  i++) {
	    ip->menu = (XtPointer)mp;

	    /* Create the menu item widget. */
	    switch (ip->type) {
	    case MI_EXEC:
		sprintf (name, "item%d", itemno++);
		obmNewObject (obm, name, "SmeBSB", menu_name, NULL, 0);
		entry = XtNameToWidget (menu, name);
		XtAddCallback (entry, XtNcallback, menuSelect, (XtPointer)mp);
		break;

	    case MI_LINE:
		sprintf (name, "line%d", lineno++);
		obmNewObject (obm, name, "SmeLine", menu_name, NULL, 0);
		entry = XtNameToWidget (menu, name);
		break;

	    case MI_DBLLINE:
		nargs = 0;
		XtSetArg (args[nargs], XtNheight, 2);
		    nargs++;
		sprintf (name, "line%d", lineno++);
		obmNewObject (obm, name, "SmeLine", menu_name, args, nargs);
		sprintf (name, "line%d", lineno++);
		obmNewObject (obm, name, "SmeLine", menu_name, args, nargs);
		entry = XtNameToWidget (menu, name);
		break;

	    case MI_MENU:
		sprintf (name, "menu%d", menuno++);
		obmNewObject (obm, name, "SmeBSB", menu_name, NULL, 0);
		entry = XtNameToWidget (menu, name);
		XtAddCallback (entry, XtNcallback, menuSelect, (XtPointer)mp);

		menu_addEntry (entry, menu_name, ip->data, obm);
		XtAddCallback (entry, XtNdestroyCallback, menu_delEntry,
		    (XtPointer)NULL);
		break;

	    case MI_SPACE:
		nargs = 0;
		XtSetArg (args[nargs], XtNheight, atoi(ip->data));
		    nargs++;
		sprintf (name, "line%d", lineno++);
		obmNewObject (obm, name, "Sme", menu_name, args, nargs);
		entry = XtNameToWidget (menu, name);
		break;

	    case MI_TITLE:
		if (i > 0)
		    fprintf (stderr,
			"obm: menu title must be first item in menu\n");
		ip++;
		continue;

	    default:
		/* ignore */
		fprintf (stderr, "obm: unknown menu item type %s[%d]\n",
		    menu_name, i + 1);
		ip++;
		continue;
	    }

	    /* Set the item specific resources. */
	    nargs = 0;
	    if (ip->label) {
		XtSetArg (args[nargs], XtNlabel, ip->label);
		nargs++;
	    }
	    if (ip->background || ip->foreground) {
		char *s[3];
		int i=0;

		if (ip->background)
		    s[i++] = ip->background;
		if (ip->foreground)
		    s[i++] = ip->foreground;
		s[i++] = NULL;

		for (i=0;  s[i];  i++) {
		    from.size = strlen(s[i]) + 1;
		    from.addr = s[i];
		    to[i].addr = (caddr_t) &value[i];
		    to[i].size = sizeof(value[i]);

		    if (XtConvertAndStore (entry,
			    XtRString, &from, XtRPixel, &to[i])) {
			XtSetArg (args[nargs], s[i] == ip->background ? 
			    XtNbackground : XtNforeground, value[i]);
			nargs++;
		    }
		}
	    }
	    if (ip->justify) {
		XtSetArg (args[nargs], XtNjustify, ip->justify);
		nargs++;
	    }
	    if (ip->pixmap) {
		XtSetArg (args[nargs], XtNleftBitmap, ip->pixmap);
		nargs++;
	    }
	    if (ip->type == MI_MENU) {
		XtSetArg (args[nargs], XtNrightBitmap,
		    menu_pullrightBitmap (obm, 0));
		nargs++;
		XtSetArg (args[nargs], XtNrightMargin, MB_WIDTH);
		nargs++;
	    }
	    if (ip->flags & M_Insensitive) {
		XtSetArg (args[nargs], XtNsensitive, False);
		nargs++;
	    }
	    if (ip->accelerator) {
		char buf[SZ_MESSAGE];
		sprintf (buf, "%s: notify()", ip->accelerator);
		XtSetArg (args[nargs], XtNaccelerators, buf);
		nargs++;
	    }

	    if (nargs)
		XtSetValues (entry, args, nargs);
	    ip->entry = entry;
	    ip++;
	}
}


/* editMenu -- Edit a menu given descriptors for the current menu and the
 * new version.  Zero is returned if the edit succeeds.  If the menus are
 * too different editMenu will play it safe and return nonzero, and the
 * caller should delete the old one and create a new menu from scratch.
 */
static int
editMenu (mp, request)
register MenuPtr mp;			/* existing menu */
MenuPtr request;			/* requested values */
{
	register MenuItem old, new;
	register int i;
	int ncolors=0, nargs=0;
	XrmValue from, to[2];
	Pixel value[2];
	Arg args[10];

	/* Make a quick comparision of the old and new menu descriptors to
	 * see if they are similar enough for the edit to make sense.
	 */
	if (mp->nitems != request->nitems)
	    return (-1);
	for (i=0;  i < mp->nitems;  i++) {
	    if (mp->items[i].type != request->items[i].type)
		return (-1);
	}

	/* Edit each menu item. */
	for (i=0;  i < mp->nitems;  i++) {
	    old = &mp->items[i];
	    new = &request->items[i];

	    nargs = 0;
	    if (new->label &&
		    (!old->label || strcmp (old->label, new->label))) {
		if (old->flags & M_FreeLabel)
		    old->label = XtRealloc (old->label, strlen(new->label)+1);
		else {
		    old->label = XtMalloc (strlen(new->label) + 1);
		    old->flags |= M_FreeLabel;
		}
		strcpy (old->label, new->label);
		XtSetArg (args[nargs], XtNlabel, old->label);
		nargs++;
	    }

	    if (new->data && (!old->data || strcmp (old->data, new->data))) {
		if (old->flags & M_FreeData)
		    old->data = XtRealloc (old->data, strlen(new->data)+1);
		else {
		    old->data = XtMalloc (strlen(new->data) + 1);
		    old->flags |= M_FreeData;
		}
		strcpy (old->data, new->data);
	    }

	    if (new->background && (!old->background ||
		    strcmp (old->background, new->background))) {

		int nchars = strlen (new->background) + 1;
		char *s;

		if (old->flags & M_FreeBackground)
		    old->background = XtRealloc (old->background, nchars);
		else {
		    old->background = XtMalloc (nchars);
		    old->flags |= M_FreeBackground;
		}
		strcpy (s = old->background, new->background);

		from.size = strlen(s) + 1;
		from.addr = s;
		to[ncolors].addr = (caddr_t) &value[ncolors];
		to[ncolors].size = sizeof(value[ncolors]);

		if (XtConvertAndStore (old->entry,
			XtRString, &from, XtRPixel, &to[ncolors])) {
		    XtSetArg (args[nargs], XtNbackground, value[ncolors]);
		    nargs++;
		    ncolors++;
		}
	    }

	    if (new->foreground && (!old->foreground ||
		    strcmp (old->foreground, new->foreground))) {

		int nchars = strlen (new->foreground) + 1;
		char *s;

		if (old->flags & M_FreeForeground)
		    old->foreground = XtRealloc (old->foreground, nchars);
		else {
		    old->foreground = XtMalloc (nchars);
		    old->flags |= M_FreeForeground;
		}
		strcpy (s = old->foreground, new->foreground);

		from.size = strlen(s) + 1;
		from.addr = s;
		to[ncolors].addr = (caddr_t) &value[ncolors];
		to[ncolors].size = sizeof(value[ncolors]);

		if (XtConvertAndStore (old->entry,
			XtRString, &from, XtRPixel, &to[ncolors])) {
		    XtSetArg (args[nargs], XtNforeground, value[ncolors]);
		    nargs++;
		    ncolors++;
		}
	    }

	    if (old->justify != new->justify) {
		old->justify = new->justify;
		XtSetArg (args[nargs], XtNjustify, old->justify);
		nargs++;
	    }

	    if (new->accelerator && (!old->accelerator ||
		    strcmp (old->accelerator, new->accelerator))) {
		char buf[SZ_MESSAGE];
		int nchars = strlen (new->accelerator) + 1;
		sprintf (buf, "%s: notify()", new->accelerator);

		if (old->flags & M_FreeAccel)
		    old->accelerator = XtRealloc (old->accelerator, nchars);
		else {
		    old->accelerator = XtMalloc (nchars);
		    old->flags |= M_FreeAccel;
		}
		strcpy (old->accelerator, new->accelerator);
		XtSetArg (args[nargs], XtNaccelerators, buf);
		nargs++;
	    }

	    if (old->pixmap != new->pixmap) {
		old->pixmap = new->pixmap;
		XtSetArg (args[nargs], XtNleftBitmap, new->pixmap);
		nargs++;
	    }

	    if ((old->flags & M_Insensitive) != (new->flags & M_Insensitive)) {
		if (new->flags & M_Insensitive) {
		    old->flags |= M_Insensitive;
		    XtSetArg (args[nargs], XtNsensitive, False);
		} else {
		    old->flags &= ~M_Insensitive;
		    XtSetArg (args[nargs], XtNsensitive, True);
		}
		nargs++;
	    }

	    if (old->entry && nargs > 0)
		XtSetValues (old->entry, args, nargs);
	}

	return (0);
}


/* freeMenu -- Free a menu descriptor.
 */
void
freeMenu (mp)
register MenuPtr mp;
{
	register MenuItem ip;
	register int i;

	for (i=0;  i < mp->nitems;  i++) {
	    ip = &mp->items[i];
	    if (ip->type == MI_MENU && ip->entry)
		menu_delEntry (ip->entry);

	    if ((ip->flags & M_FreeBackground) && ip->background)
		XtFree (ip->background);
	    if ((ip->flags & M_FreeForeground) && ip->foreground)
		XtFree (ip->foreground);
	    if ((ip->flags & M_FreeAccel) && ip->accelerator)
		XtFree (ip->accelerator);
	    if ((ip->flags & M_FreeLabel) && ip->label)
		XtFree (ip->label);
	    if ((ip->flags & M_FreeData) && ip->data)
		XtFree (ip->data);

	    XtFree (ip->sbuf);
	}

	XtFree ((char *)mp);
}


/* menuSelect -- Callback routine, called when a menu item is selected.
 */
static void
menuSelect (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
	register MenuPtr mp = (MenuPtr) client_data;
	register MenuItem ip;
	register int i;
	ObmContext obm = (ObmContext) mp->obm;

	/* Locate the menu item to which the callback refers. */
	for (i=0, ip=NULL;  i < mp->nitems;  i++) {
	    ip = &mp->items[i];
	    if (ip->entry == w)
		break;
	}

	/* Ignore callbacks other than for command type menu entries (MI_EXEC).
	 * In the case of a command entry the data field of the menu item
	 * descriptor contains the server command to be executed.
	 */

	if (ip && ip->type == MI_EXEC)
	    if (Tcl_Eval (obm->tcl, ip->data) != TCL_OK) {
		char *errstr = Tcl_GetVar (obm->tcl, "errorInfo", 0);
		fprintf (stderr, "Error %s.%s line %d: %s\n",
		    mp->obj->core.name, XtName(ip->entry), obm->tcl->errorLine,
		    errstr ? errstr : obm->tcl->result);
	    }
}


/*
 * The following code is used to interpose special highlight() and
 * unhighlight() SmeBSB class procedures, to allow menu entries which point to
 * submenus to automatically popup and popdown the submenu when the menu entry
 * which points to it is highlighted or unhighlighted.  It is a bit of a trick
 * to replace the standard class procedure as we do here, but this is a lot
 * simpler than subclassing the entire SmeBSB widget, and works for both the
 * standard Athena and Xaw3D versions of the widget.  Tying the submenu
 * popup/popdown to the simpleMenu highlight/unhighlight actions allows us
 * to let the simpleMenu widget track the pointer and determine when the
 * submenu should be displayed.
 *
 * [Note added later].  Having now implemented this technique for cascaded
 * menus I am not sure it was the best way to do this - it might have been
 * better to just use motion events.  There were a number of subtle problems
 * to be solved to get this to work.  It is true though, that most of these
 * don't have much to do with the interpose-highlight technique, so maybe
 * I would have run into the problems anyway.  The problems were things like
 * state changes when the pointer crosses from a menu pane into a submenu,
 * the need to explicitly popdown submenus when the main window is popped
 * down or when a nonmenu item in the main menu is entered, getting the
 * grabs right, an annoying warning message about trying to remove a grab
 * which no longer existed, and so on.  Getting the pull-right bitmap to
 * look right took a lot of fiddling and it appeared that there might be a
 * positioning bug in the Xaw3d code related to this.  The one problem that
 * was clearly specific to the highlight/unhighlight technique was that
 * SmeBSB uses a toggle type function for highlight/unhighlight.  This
 * assumes that the toggle is always in a known state.  This works for a
 * single menu, but it appears that there is a bug in the SmeBSB code when
 * a pull-right menu obscures the parent menu, causing the toggle to get
 * out of phase when moving into the submenu band back out (out of phase
 * means that when one calls the unhighlight class procedure, the toggle
 * actually highlights, and vice versa).  I had to disable highlighting of
 * submenu items in a menu to avoid this problem.
 */

/* The following describes a menu entry widget which calls a submenu. */
struct _menuEntry {
	Widget w;			/* this widget */
	char name[SZ_NAME];		/* name of menu containing widget */
	char child[SZ_NAME];		/* submenu name */
	Widget menu;			/* shell widget of submenu */
	ObmContext obm;			/* obm context */
	struct _menuEntry *next;	/* next menuEntry on list */
};
typedef struct _menuEntry menuEntry, *MenuEntry;

MenuEntry menuWidgetList;
static char menu_bitmap1[] = "BSB_pullright1";
static char menu_bitmap2[] = "BSB_pullright2";
static void (*BSB_highlight)();
static void (*BSB_unhighlight)();


/* menu_classInit -- Edit the SME class record to interpose our custom
 * highlight/unhighlight class procedures.
 */
static void
menu_classInit()
{
	register SmeClassPart *sme = &smeBSBClassRec.sme_class;

	if (sme->highlight != menu_highlight) {
	    BSB_highlight = sme->highlight;
	    sme->highlight = menu_highlight;
	}
	if (sme->unhighlight != menu_unhighlight) {
	    BSB_unhighlight = sme->unhighlight;
	    sme->unhighlight = menu_unhighlight;
	}
}


/* menu_pullrightBitmap -- Return the bitmap id of the pullright bitmap
 * displayed on the right side of a menu entry that brings up a submenu.
 */
static Pixmap
menu_pullrightBitmap (obm, state)
ObmContext obm;
int state;
{
	Pixmap bitmap;

	if (bitmap = findBitmap (obm, state ? menu_bitmap2 : menu_bitmap1))
	    return (bitmap);

	createBitmap (obm, menu_bitmap1, MB_WIDTH, MB_HEIGHT, MB1_PIXELS);
	createBitmap (obm, menu_bitmap2, MB_WIDTH, MB_HEIGHT, MB2_PIXELS);

	if (bitmap = findBitmap (obm, state ? menu_bitmap2 : menu_bitmap1))
	    return (bitmap);
	else
	    return ((Pixmap)NULL);
}


/* menu_addEntry -- Add a widget to the menuWidgetList list.
 */
static void
menu_addEntry (w, name, child, obm)
Widget w;			/* menu entry which calls submenu */
char *name;			/* name of menu containing this widget */
char *child;			/* name of submenu shell widget */
ObmContext obm;
{
	register MenuEntry mw, new;

	for (mw=menuWidgetList;  mw && mw->next;  mw = mw->next)
	    if (mw->w == w)
		return;

	if ((new = (MenuEntry) XtCalloc (1, sizeof (menuEntry))) == NULL)
	    return;

	new->w = w;
	strcpy (new->name, name);
	strcpy (new->child, child);
	new->obm = obm;

	if (mw)
	    mw->next = new;
	else
	    menuWidgetList = new;
}


/* menu_delEntry -- Delete a widget from the menuWidgetList list.
 */
static void
menu_delEntry (w, client_data, call_data)
register Widget w;
XtPointer client_data;			/* not used */
XtPointer call_data;			/* not used */
{
	register MenuEntry mw, prev_mw;

	for (mw=menuWidgetList, prev_mw=NULL;  mw;  prev_mw=mw, mw=mw->next)
	    if (mw->w == w)
		break;

	if (mw) {
	    if (prev_mw)
		prev_mw->next = mw->next;
	    else
		menuWidgetList = NULL;
	    XtFree ((char *)mw);
	}
}


/* menu_popup -- Called when a menu is popped up.
 */
static void
menu_popup (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;			/* not used */
{
	register MenuPtr mp = (MenuPtr) client_data;
	mp->popped_up = True;
}


/* menu_popdown -- Called when a menu is popped down.  Make sure any
 * child menus are popped down before popping down the parent.
 */
static void
menu_popdown (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;			/* not used */
{
	register MenuPtr mp = (MenuPtr) client_data;
	ObmContext obm = (ObmContext) mp->obm;
	register MenuEntry mw;
	register MenuPtr mm;
	MenuItem mi;
	int i;

	/* This routine is called with w=NULL to popdown any child menus.
	 * If w is not NULL that means we are being called as a
	 * popdownCallback for the menu shell.
	 */
	if (w)
	    mp->popped_up = False;

	for (i=0;  i < mp->nitems;  i++) {
	    mi = &mp->items[i];
	    if (mi->type == MI_MENU) {
		/* Locate menu descriptor for named menu. */
		mm = findMenu (obm, mi->data);

		/* Popdown the child menu (if necessary).  The temporary dummy
		 * warning message handler appears to be the only way to avoid
		 * a warning messages about an attempt to remove a grab for a
		 * widget on the grab list.  What happens is that when
		 * XtPopdown is called on the main menu this calls
		 * XtRemoveGrab on the window, which removes any grabs for the
		 * main window _and any later grabs_, i.e., for the submenus.
		 * XtPopdown then calls the menu_popdown popdown callback,
		 * which is necessary to popdown any child menus as XtPopdown
		 * does not do this.  The XtPopdown called in this routine
		 * tries to remove a grab which has already been removed,
		 * causing the warning message.  The situation itself appears
		 * to be harmless, so the best thing to do is just disable the
		 * warning message.
		 */
		if (mm && mm->menuShell) {
		    XtErrorMsgHandler old_handler;

		    old_handler = XtAppSetWarningMsgHandler (obm->app_context,
			(XtErrorMsgHandler) menu_popdown_msgHandler);
		    XtPopdown (mm->menuShell);
		    XtAppSetWarningMsgHandler (obm->app_context, old_handler);
		}
	    }
	}
}


/* menu_popdown_msgHandler -- Dummy warning message handler used in menu
 * popdown above.
 */
static void
menu_popdown_msgHandler (name,type,class,defaultp,params,num_params)
String name,type,class,defaultp;
String* params;
Cardinal* num_params;
{
}


/* menu_highlight -- Custom version of the simpleMenu class action highlight
 * procedure, called when a menu entry is highlighted.  This is identical to
 * the standard class procedure (in fact we call the standard class highlight
 * procedure) except that we check to see if the widget being highlighted
 * is a submenu, and if so, popup the submenu.
 */
static void
menu_highlight (w)
register Widget w;
{
	register MenuEntry mw, sm;
	ObmContext obm = global_obm_handle;
	MenuPtr mp;

	/* If we are highlighting an entry in a menu then any pull-right
	 * submenus which are still up should not be, so get rid of them.
	 */
	if (mp = findMenu (obm, XtName(w->core.parent)))
	    menu_popdown ((Widget)NULL, (XtPointer)mp, (XtPointer)NULL);

	/* Is the menu entry being highlighted on our list of call-submenu
	 * widgets?
	 */
	for (mw=menuWidgetList;  mw;  mw = mw->next)
	    if (mw->w == w)
		break;

	/* If the menu item is for a submenu, popup the submenu. */
	if (mw) {
	    Position x, y;
	    Position menu_x, menu_y;
	    Dimension parent_width;
	    Dimension menu_width, menu_height;
	    char target[SZ_NAME];
	    ObmContext obm = mw->obm;
	    Widget menu;

	    /* If the parent menu is not popped up, do not pop up the
	     * child.  This doesn't sound likely but it can happen when
	     * menu_highlight is called after a button-up event which
	     * pops down the main menu.
	     */
	    if (mp && mp->popped_up == False)
		return;

	    /* Get shell widget of submenu. */
	    if (!(mp && (mw->menu = XtNameToWidget (mp->menuShell, target)))) {
		sprintf (target, "*%s", mw->child);
		if (!(mw->menu = XtNameToWidget (obm->toplevel, target)))
		    return;
	    }

	    menu = mw->menu;
	    XtTranslateCoords (w, 0, 0, &x, &y);
	    XtVaGetValues (w, XtNwidth,	&parent_width, NULL);

	    menu_width = menu->core.width + 2 * menu->core.border_width;
	    menu_height = menu->core.height + 2 * menu->core.border_width;
	    menu_x = x + parent_width - 5;
	    menu_y = y - 5;

	    if (menu_x >= 0) {
		int scr_width = WidthOfScreen(XtScreen(menu));
		if ((int)(menu_x + menu_width) > (int)scr_width)
		    menu_x = scr_width - menu_width;
	    }
	    if (menu_x < 0)
		menu_x = 0;

	    if (menu_y >= 0) {
		int scr_height = HeightOfScreen(XtScreen(menu));
		if ((int)(menu_y + menu_height) > (int)scr_height)
		    menu_y = scr_height - menu_height;
	    }
	    if (menu_y < 0)
		menu_y = 0;

	    XtVaSetValues (menu,
		XtNx,		menu_x,
		XtNy,		menu_y,
		NULL);
	    /*
	     * This appears to bring out a bug in the Xaw|Xaw3d SmeBSB.
	     *
	    XtVaSetValues (w,
		XtNrightBitmap,	 menu_pullrightBitmap (obm, 1),
		NULL);
	    */

	    /* Popup the pull-right menu. */
	    XtPopup (menu, XtGrabNonexclusive);

	} else {
	    /* Call the standard simplemenu highlight method. */
	    BSB_highlight (w);
	}
}


/* menu_unhighlight -- Custom unhighlight class procedure, interposed in
 * front of the standard class procedure.
 */
static void
menu_unhighlight (w)
register Widget w;
{
	register MenuEntry mw;

	/* Is the menu entry being unhighlighted on our list of call-submenu
	 * widgets?
	 */
	for (mw=menuWidgetList;  mw;  mw = mw->next)
	    if (mw->w == w)
		break;

	if (mw == NULL) {
	    /* Now call the standard class unhighlight procedure. */
	    BSB_unhighlight (w);

	} else if (mw->menu) {
	    /* Popdown the submenu.
	     */
	    ObmContext obm = mw->obm;
	    Dimension width, height;
	    int in_window, i;
	    XMotionEvent *ev;
	    XEvent event;
	    Position x, y;
	    Widget wl[2];

	    /* Get the next window event.  All we are looking for here is
	     * the pointer coordinates.
	     */
	    ev = (XMotionEvent *) &event;
	    while (!XtAppPeekEvent (obm->app_context, &event))
		;

	    wl[0] = w;
	    wl[1] = mw->menu;
	    in_window = 0;

	    /* Check if the pointer is in either the pull-right pane of
	     * the parent menu, or the pull-right menu itself.
	     */
	    for (i=0;  i < 2;  i++) {
		XtTranslateCoords (wl[i], 0, 0, &x, &y);
		XtVaGetValues (wl[i],
		    XtNwidth,	&width,
		    XtNheight,	&height,
		    NULL);

		if (ev->x_root >= x && ev->x_root < x + (Position)width &&
		    ev->y_root >= y && ev->y_root < y + (Position)height) {
		    in_window++;
		    break;
		}
	    }

	    /* If it is not in either window then go ahead and popdown the
	     * child menu, otherwise ignore the request.  Erroneous requests
	     * can occur when the pointer is in crossing from one window
	     * to the other.
	     */
	    if (!in_window) {
		/*
		 * This appears to bring out a bug in the Xaw3d SmeBSB.
		XtVaSetValues (w,
		    XtNrightBitmap,	 menu_pullrightBitmap (obm, 0),
		    NULL);
		 */

		XtPopdown (mw->menu);
	    }
	}
}


/* findMenu -- Return the menu descriptor of a menu given its name.
 */
static MenuPtr
findMenu (obm, name)
register ObmContext obm;
char *name;
{
	register ObjList lp;

	for (lp = obm->menu_list;  lp;  lp = lp->next)
	    if (strcmp (lp->name, name) == 0)
		return ((MenuPtr) lp->ptr);

	return (NULL);
}
