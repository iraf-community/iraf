/* Copyright(c) 1993 Association of Universities for Research in Astronomy Inc.
 */

#define Obm_Main
#include <ObmP.h>

/*
 * OBM.C -- Object manager for the graphics subsystem.
 *
 *               obm = ObmOpen (app_context, argc, argv)
 *                    ObmClose (obm)
 *               ObmInitialize (obm)
 *                 ObmActivate (obm)
 *               ObmDeactivate (obm, unmap)
 *       status = ObmActivated (obm)
 *	    status = ObmStatus (obm, app_name, app_class)
 *      status = ObmDeliverMsg (obm, object, message)
 *  st = ObmDeliverMsgFromFile (obm, object, fname)
 *	 interp = ObmGetInterp (obm)
 *
 * Callbacks:
 *
 *	   id = ObmAddCallback (obm, callback_type, fcn, client_data)
 *           ObmRemoveCallback (obm, id)
 *
 *	      connect_callback (client_data, display, toplevel, state)
 *	     activate_callback (client_data, toplevel)
 *	   deactivate_callback (client_data, toplevel)
 *	     setGterm_callback (client_data, gterm_widget)
 *	 clientOutput_callback (client_data, tcl, key, string)
 *
 * The callback type bitflags are defined in <Obm.h> as OBMCB_connect,
 * OBMCB_activate and so on.
 *
 * An xgterm graphics user interface (UI) consists of one or more windows
 * containing an arbitrary hierarchy of widgets.  These widgets and their
 * runtime actions are defined by an interpreted text program downloaded
 * by the client application, which does not itself deal directly with
 * the window user interface.
 *
 * The object manager provides a higher level of abstraction for dealing
 * with widgets and other UI objects.  The main function of the object
 * manager is to deliver messages to UI objects.  Each instance of a widget
 * is an object in the UI.  The UI contains other types of objects however,
 * including the client object (client application), the server object
 * (the object manager itself), and the application specific UI parameters,
 * each of which is an object with a callback list of UI procedures to be
 * called when the parameter value changes.  All of these UI objects can
 * receive messages and take actions as a result.  Messages may come from the
 * client application, or as a result of actions executed by the interpreted
 * UI code in response to graphics events.
 *
 * Object classes:
 *
 *	Client				the client application
 *	Server				the object manager itself
 *	Parameter			UI control parameter
 *	Gterm widget			graphics terminal widget
 *	Vterm widget			vt100 terminal widget
 *	various Xt and Athena widgets	box, shell, label, text, list, etc.
 *
 * In addition to delivering messages to objects (ObmDeliverMsg), one of the
 * functions of the object manager is to manage the hierarchy of widgets
 * comprising the UI.  ObmOpen opens the object manager.  ObmInitialize
 * initializes the object manager, destroying any existing widget hierarchy.
 * ObmActivate readies the UI to receive graphics output and graphics input
 * events, creating a default UI if none has been downloaded by the client.
 *
 * Sophisticated graphics applications will download a UI during initialization
 * to define a custom graphics user interface.  This is done by sending a
 * message to the object manager.  Naive applications assume a simple graphics
 * terminal and do not download a UI; in this case, a default UI is created
 * for the application when the UI is enabled with ObmEnable.  The default
 * UI is a single top level shell window containing a single gterm (graphics
 * terminal) widget.
 *
 *	reset-server
 *	appInitialize appname classname {resources}
 *	createObjects
 *		(UI specific code)
 *	activate
 *
 * A UI specification consists of a sequence of commands to be executed by
 * the server object.  This is downloaded by the client as a message for the
 * server object.  The commands should include "reset-server" (this must be
 * the first executable command), "appInitialize" (defines the UI objects and
 * their resources), and "createObjects" (creates the objects and the widget
 * tree), followed by any UI specific commands to define and register UI
 * callback procedures.  Finally, "activate" is executed to activate the new
 * user interface.
 */

#define	MAXOBJ	512
static void obm_call_activate_callbacks();


/* ObmOpen -- Open the object manager.
 */
ObmContext
ObmOpen (app_context, argc, argv)
XtAppContext app_context;
int argc;
char *argv[];
{
	register ObmContext obm;
	register ObjClassRec classrec;
	register int i;
	char *s;

	/* Initialize object manager global context. */
	obm = (ObmContext) XtCalloc (1, sizeof (struct obmContext));
	strcpy (obm->appname, "gterm-iraf");
	strcpy (obm->appclass, "Xgterm");
	obm->app_context = app_context;
	obm->argc = argc;
	obm->argv = argv;
	obm->debug = ((s = getenv("OBMDEBUG")) != NULL);
	if (s && (i = atoi(s)))
	    obm->debug = i;
	if (s = getenv("OBMOBJECTS")) {
	    obm->debug_objs = (char *) XtCalloc (1, strlen(s)+1);
	    strcpy (obm->debug_objs, s);
	} else
	    obm->debug_objs = (char *)NULL;
	    

	/* Initialize object classes. */
	for (i=0;  i < XtNumber(UiObjects);  i++) {
	    classrec = &UiObjects[i];
	    if (classrec->ClassInit)
		(*(classrec->ClassInit)) (obm, classrec);
	}

	/* Create new server and client objects. */
	ObmInitialize (obm);

	return (global_obm_handle = obm);
}


/* ObmClose -- Close the object manager.
 */
void
ObmClose (obm)
ObmContext obm;
{
	register ObjClassRec classrec;
	register int i;

	/* Get rid of any current UI. */
	ObmInitialize (obm);
	while (obm->head)
	    obmDestroyObject (obm, obm->head);

	/* Delete any callback descriptors. */
	while (obm->callback_list)
	    obmRemoveCallback (&obm->callback_list, obm->callback_list);

	/* Free any resources associated with the class descriptors. */
	for (i=0;  i < XtNumber(UiObjects);  i++) {
	    classrec = &UiObjects[i];
	    (*(classrec->ClassDestroy)) (obm, classrec);
	}

	if (obm->debug_objs)
	    XtFree ((char *)obm->debug_objs);
	XtFree ((char *)obm);
}


/* ObmInitialize -- Initialize the object manager, i.e., destroy any existing
 * user interface.
 */
void
ObmInitialize (obm)
register ObmContext obm;
{
	register ObmObject obj, nextobj;
	register ObmCallback cb;
	register MenuItem ip;
	ObjList lp, lp_next;
	Menu mp;
	int i;

        /* Destroy the UI object tree.  We need to be a little careful how
         * we do this as objects like to destroy child objects, execute
         * destroy callbacks, unmap windows, perform geometry requests, etc.
         * during destruction.  The widget class code can most efficiently
         * destroy the widget tree if destroy the toplevel widget first,
         * so we unmap the GUI and destroy the toplevel widget-class object
         * first.  Then we destroy whatever else is left.
         */
	obm->being_destroyed++;
	if (obj = obmFindObject (obm, "toplevel")) {
	    obmUndisplay (obm, obj);
	    obmDestroyObject (obm, obj);
	}
	while (obm->head)
	    obmDestroyObject (obm, obm->head);

	/* Free any cached pixmaps. */
	for (lp = obm->pixmap_cache;  lp;  lp = lp_next) {
	    lp_next = lp->next;
	    freeIcon (obm, (Icon *) lp->ptr);
	    XtFree ((char *)lp);
	}
	obm->pixmap_cache = NULL;

	/* Free any cached cursors. */
	for (lp = obm->cursor_cache;  lp;  lp = lp_next) {
	    lp_next = lp->next;
	    XFreeCursor (obm->display, (Cursor)lp->ptr);
	    XtFree ((char *)lp);
	}
	obm->cursor_cache = NULL;

	/* Free any menu lists. */
	for (lp = obm->menu_list;  lp;  lp = lp_next) {
	    lp_next = lp->next;
	    freeMenu ((MenuPtr) lp->ptr);
	}
	obm->menu_list = NULL;

	/* Close the application specific display connection. */
	if (obm->display) {
	    /* Call the client's display connection callbacks if any to
	     * inform the client that we are about to close the display.
	     */
	    for (cb = obm->callback_list;  cb;  cb = cb->next)
		if ((cb->callback_type & OBMCB_connect) && cb->u.fcn)
		    (*cb->u.fcn) (cb->client_data, obm->display, NULL, 0);

	    XFlush (obm->display);
	    XtCloseDisplay (obm->display);
	    obm->display = NULL;
	}

	/* Reinitialize the global context. */
	memset ((void *)obm->objindex, 0, sizeof(obm->objindex));
	strcpy (obm->appname, "gterm-iraf");
	strcpy (obm->appclass, "Xgterm");
	obm->head = obm->tail = NULL;
	obm->being_destroyed = 0;
	obm->toplevel = NULL;
	obm->specified = 0;
	obm->activated = 0;
	obm->mapped = 0;

	/* Free any callbacks that don't have the OBMCB_preserve flag set. */
	for (cb = obm->callback_list;  cb;  cb = cb->next)
	    if (!(cb->callback_type & OBMCB_preserve))
		obmRemoveCallback (&obm->callback_list, cb);

	/* Create new server and client objects. */
	obmNewObject (obm, "server", "Server", NULL, NULL, 0);
	obmNewObject (obm, "client", "Client", NULL, NULL, 0);
}


/* ObmActivate -- Activate the UI, i.e., ensure that it is active and in
 * such a state as to be ready for client i/o.
 */
void
ObmActivate (obm)
register ObmContext obm;
{
	register ObmObject obj;
	char defaultUI[SZ_MESSAGE];

	/* UI has already been activated? */
	if (obm->activated) {
	    if (!obm->mapped) {
		if (obj = obmFindObject (obm, "toplevel"))
		    obmDisplay (obm, obj);
		obm->mapped++;

		/* Call activate callbacks after UI has been realized. */
		obm_call_activate_callbacks (obm, 1);
	    }
	    return;
	}

	if (!obm->specified) {
	    /* Construct a UI specification for the default UI. */
	    sprintf (defaultUI, "%s %s %s {%s.objects: %s%s%s%s%s}; %s; %s\n",
		"appInitialize", obm->appname, obm->appclass,
		 obm->appclass, "toplevel  Gterm  gterm\n",
		".geometry: 640x480\n",
		"*gterm.warpCursor: True\n",
		"*gterm.raiseWindow: True\n",
		"*gterm.deiconifyWindow: True\n",
		"createObjects",
		"send gterm setGterm\n");

	    /* Call the server to configure the default UI. */
	    ObmDeliverMsg (obm, "server", defaultUI);
	}

	/* Realize the toplevel widgets. */
	if (obj = obmFindObject (obm, "toplevel"))
	    obmDisplay (obm, obj);

	obm->activated++;
	obm->mapped++;

	XFlush (obm->display);
	obm_call_activate_callbacks (obm, 1);
}


/* ObmDeactivate -- Deactivate the UI.  Optionally unmap the UI widget
 * tree and execute any deactivate callback registered by the Obm client.
 * Deactivation does not affect the state of the UI, i.e. a reactivate
 * will cause the UI to resume execution in the same state at which it
 * was deactivated.
 */
void
ObmDeactivate (obm, unmap)
register ObmContext obm;
Boolean unmap;
{
	register ObmObject obj;
	register ObmCallback cb;
	ObmFunc deactivate;

	/* The Obm "activated" flag is not affected by deactivation.
	 * Deactivation merely means that control has temporarily been
	 * returned to the caller.  If the activate flag has not been set
	 * that means we do not yet have a UI to deactivate.
	 */
	if (!obm->activated)
	    return;

	/* Call any client deactivate callbacks before unrealizing the UI. */
	obm_call_activate_callbacks (obm, 0);

	if (unmap) {
	    if (obj = obmFindObject (obm, "toplevel"))
		obmUndisplay (obm, obj);
	    obm->mapped = 0;
	}

	XFlush (obm->display);
}


/* obm_call_activate_callbacks -- Internal procedure to search the callback
 * lists and call any activate/deactivate callbacks.
 */
static void
obm_call_activate_callbacks (obm, state)
register ObmContext obm;
int state;
{
	register ObmCallback cb;
	register int type;

	/* Call any client activate callbacks. */
	type = state ? OBMCB_activate : OBMCB_deactivate;
	for (cb = obm->callback_list;  cb;  cb = cb->next)
	    if ((cb->callback_type & type) && cb->u.fcn)
		(*cb->u.fcn) (cb->client_data, obm->toplevel, state);

	/* Call any GUI activate callbacks. */
	type = state ? OBMUI_activate : OBMUI_deactivate;
	for (cb = obm->callback_list;  cb;  cb = cb->next)
	    if (cb->callback_type & type) {
		char message[SZ_NUMBER];
		int status;

		sprintf (message, "%d", state);
		status = Tcl_VarEval (obm->tcl,
		    cb->name, " ",
		    message, " ",
		    NULL); 

		if (status != TCL_OK) {
		    char *errstr = Tcl_GetVar (obm->tcl, "errorInfo", 0);
		    fprintf (stderr, "Error on line %d in activate: %s\n",
			obm->tcl->errorLine,
			errstr ? errstr : obm->tcl->result);
		}
	    }
}


/* ObmActivated -- Test whether the GUI is activated, i.e., both defined
 * and mapped.
 */
ObmActivated (obm)
register ObmContext obm;
{
	return (obm->activated && obm->mapped);
}


/* ObmStatus -- Get the Object Manager status.
 */
ObmStatus (obm, app_name, app_class)
register ObmContext obm;
char *app_name;
char *app_class;
{
	if (obm->specified) {
	    if (app_name)
		strcpy (app_name, obm->appname);
	    if (app_class)
		strcpy (app_class, obm->appclass);

	    if (obm->activated)
		return (obm->mapped ? OBM_ACTIVE : OBM_IDLE);
	}

	return (OBM_INITIALIZED);
}


/* ObmGetInterp -- Get the main OBM (server object) interpreter.  This can
 * be used, e.g., by the calling program to extend the Tcl environment seen
 * by the GUI code.
 */
XtPointer
ObmGetInterp (obm)
register ObmContext obm;
{
	return ((XtPointer)obm->tcl);
}


/* ObmDeliverMsg -- Deliver a message to a UI object.
 */
ObmDeliverMsg (obm, object, message)
register ObmContext obm;
char *object;
char *message;
{
	register ObmFunc evaluate;
	register ObmObject obj;
	int status = TCL_ERROR;

	if (obm->debug) {
	    if (!obm->debug_objs || strstr (obm->debug_objs, object) != NULL) {
	        printf ("%s: %s\n", object, message);
	        fflush (stdout);
	    }
	}
	    

        /* Note -- the following can execute appInitialize, which initializes
         * the server and creates a new Tcl, so no server context (such as
         * obm->tcl) should be stored in an internal variable here unless
	 * updated or not used again after the evaluate call.
         */
	Tcl_SetResult (obm->tcl, "", TCL_STATIC);
	if (obj = obmFindObject (obm, object)) {
	    if (evaluate = obj->core.classrec->Evaluate) {
		status = (*evaluate)(obj, message);
		if (status != TCL_OK) {
		    char *errstr = Tcl_GetVar (obm->tcl, "errorInfo", 0);
		    fprintf (stderr, "Error in message to %s, line %d: %s\n",
			object, obm->tcl->errorLine,
			errstr ? errstr : obm->tcl->result);
		}
	    } else
		status = TCL_OK;
	} else {
	    Tcl_AppendResult (obm->tcl,
		"send: could not find object ", object, NULL);
	    status = TCL_ERROR;
	}
	    
	return (status);
}


/* ObmDeliverMsgFromFile -- Deliver a message to a UI object, taking the
 * message from the named text file.
 */
ObmDeliverMsgFromFile (obm, object, fname)
register ObmContext obm;
char *object;
char *fname;
{
	struct stat fs;
	char *message = NULL;
	int status, nchars;
	int fd = -1;

	if (stat (fname, &fs) >= 0) {
	    nchars = fs.st_size;
	    if ((message = (char *) XtMalloc (nchars + 1)) == NULL)
		goto err;
	    if ((fd = open (fname, 0)) < 0)
		goto err;
	    if (read (fd, message, nchars) != nchars)
		goto err;

	    message[nchars] = '\0';
	    status = ObmDeliverMsg (obm, object, message);

	    close (fd);
	    XtFree ((char *)message);
	    return (status);
	}
err:
	printf ("cannot access file %s\n", fname);
	if (fd >= 0)
	    close (fd);
	if (message)
	    XtFree ((char *)message);
	return (TCL_ERROR);
}


/* ObmAddCallback -- Add a callback of the given type to the OBM global
 * callback list.
 */
XtPointer
ObmAddCallback (obm, callback_type, fcn, client_data)
register ObmContext obm;
int callback_type;
ObmFunc fcn;
XtPointer client_data;
{
	register ObmCallback cb;

	if (!(cb = obmAddCallback (&obm->callback_list)))
	    return (NULL);

	cb->u.fcn = fcn;
	cb->callback_type = callback_type;
	cb->client_data = client_data;
	cb->name[0] = '\0';
}


/* ObmRemoveCallback -- Remove a callback from the OBM global callback list.
 */
void
ObmRemoveCallback (obm, callback)
register ObmContext obm;
ObmCallback callback;
{
	obmRemoveCallback (&obm->callback_list, callback);
}


/*
 * Object manager internal routines.
 * ----------------------------------
 */

/* obmFindObject -- Lookup an object by name and return a pointer to the
 * associated object descriptor.  If the object name is unique only the object
 * name need be given, otherwise a name such as "parent1.parent2...object"
 * may be given to specify which object to use.
 */
ObmObject
obmFindObject (obm, object)
ObmContext obm;
char *object;
{
	register int hashval, n;
	register char *ip, *op;
	ObmObject objlist1[MAXOBJ], objlist2[MAXOBJ];
	ObmObject obj, *otemp, *objs, *pobjs;
	char name[SZ_NAME];
	int nobjs;

	if (object == NULL)
	    return (NULL);

	objs = objlist1;
	pobjs = objlist2;
	pobjs[0] = objs[0] = NULL;
	nobjs = 0; 

	for (ip=object;  *ip;  ) {
	    /* List of objects from last run becomes the list of parent objects
	     * for the current run.
	     */
	    otemp = pobjs;
	    pobjs = objs;
	    objs = otemp;

	    /* Get next object name. */
	    for (op=name;  *ip;  ) {
		if (*ip == '.') {
		    ip++;
		    break;
		} else
		    *op++ = *ip++;
	    }
	    *op = '\0';

	    /* Get list of candidate objects. */
	    if (obm_nameToObjectList (obm, name, pobjs, &nobjs, objs) == 0)
		return (NULL);
	}

	if (nobjs <= 0)
	    return (NULL);
	else if (nobjs > 1)
	    fprintf (stderr, "ambiguous object name: %s\n", object);

	return (objs[0]);
}


/* obm_nameToObjectList -- Return a list of objects with the given name which
 * are children of one of the (possibly empty) list of parent objects.  If the
 * parent object list is empty the list of all objects with the given name
 * is returned.
 */
obm_nameToObjectList (obm, object, pobjs, nobjs, objs)
ObmContext obm;
char *object;			/* object name */
ObmObject *pobjs;		/* list of parent objects */
int *nobjs;			/* number of objects found (output) */
ObmObject *objs;		/* list of objects (output) */
{
	register char *ip;
	register int hashval, n;
	register ObmObject obj;
	int accept, i;

	if (object == NULL)
	    return (0);

	/* Compute hash value. */
	for (hashval=0, ip=object, n=MAX_HASHCHARS;  --n >= 0 && *ip;  ip++)
	    hashval += (hashval + *ip);
	if (hashval < 0)
	    return (0);

	/* Examine any objects on the hash thread. */
	obj = obm->objindex[hashval%SZ_INDEX];
	for (n=0;  obj;  obj=obj->core.nexthash) {
	    if (strcmp (object, obj->core.name) == 0) {
		accept = 0;
		if (pobjs && pobjs[0]) {
		    for (i=0;  pobjs[i];  i++)
			if (obj->core.parent == pobjs[i]) {
			    accept++;
			    break;
			}
		} else
		    accept++;

		if (accept)
		    objs[n++] = obj;
	    }
	}

	objs[n] = NULL;
	return (*nobjs = n);
}


/* obmNewObject -- Create a new object of the given type.
 */
void
obmNewObject (obm, name, class, parent, args, nargs)
register ObmContext obm;
char *name;			/* name of new object */
char *class;			/* name of class to which object belongs */
char *parent;			/* name of parent widget, for widget objects */
ArgList args;			/* optional argument list */
int nargs;			/* optional argument list */
{
	register char *ip;
	register int hashval, n;
	ObmObject newobj, obj, pobj;
	ObjClassRec classrec;

	if (obm->being_destroyed)
	    return;

	/* Ignore the request if parent is being destroyed. */
	pobj = obmFindObject (obm, parent);
	if (parent && (!pobj || pobj->core.being_destroyed))
	    return;

	/* Get class record. */
	if (!(classrec = obmGetClassrec (class))) {
	    fprintf (stderr, "obm: object %s has unknown class %s\n",
		name, class);
	    return;
	}

	/* Create the object. */
	    
	newobj = (ObmObject) (*(classrec->Create)) (obm,
	    name, classrec, parent, args, nargs);
	if (!newobj) {
	    fprintf (stderr, "obm: could not create object %s class %s\n",
		name, class);
	    return;
	}

	strcpy (newobj->core.name, name);
	newobj->core.classrec = classrec;
	newobj->core.parent = pobj;

	/* Link the object into the object list. */
	if (!obm->head)
	    obm->head = newobj;
	if (newobj->core.prevglob = obm->tail)
	    obm->tail->core.nextglob = newobj;
	obm->tail = newobj;

	/* Compute hash value. */
	for (hashval=0, ip=name, n=MAX_HASHCHARS;  --n >= 0 && *ip;  ip++)
	    hashval += (hashval + *ip);

	/* Enter the object into the hash table. */
	if (obj = obm->objindex[hashval%SZ_INDEX]) {
	    while (obj->core.nexthash)
		obj = obj->core.nexthash;
	    obj->core.nexthash = newobj;
	} else
	    obm->objindex[hashval%SZ_INDEX] = newobj;

	/* Add the object to the parent's list of children. */
	if (parent) {
	    ObmObject pobj;
	    ObmObjectCore cp;
	    
	    if ((pobj = obmFindObject (obm, parent)) == NULL) {
		fprintf (stderr, "obm: object %s has unknown parent %s\n",
		    name, parent);
		return;
	    } else
		cp = &pobj->core;

	    if (cp->nchildren) {
		cp->children = (ObmObject *) XtRealloc ((char *)cp->children,
		    (cp->nchildren + 1) * sizeof(ObmObject));
	    } else
		cp->children = (ObmObject *) XtMalloc (sizeof(ObmObject));

	    cp->children[cp->nchildren++] = newobj;
	}
}


/* obmDestroyObject -- Destroy an object and all its descendents.
 */
void
obmDestroyObject (obm, object)
ObmContext obm;
ObmObject object;
{
	register ObmObjectCore cp = &object->core;
	register ObmObject obj;
	register int i;
	int hashval, n;
	char *ip;

	if (!object)
	    return;
	if (object->core.being_destroyed)
	    return;

	/* Destroy the object instance itself.  The object destroy class
	 * method is called twice when an object is destroyed, once at the
	 * beginning before any children are destroyed and once at the end
	 * after all children have been destroyed and the object descriptor
	 * has been unlinked from all Obm object data structures.  It is
	 * up to the Destroy class method for the object to decide how much
	 * of the object to destroy in each call.  Most objects just set a
	 * being_destroyed flag in the first call and wait until the end
	 * to destroy the object, in case the object descriptor is referenced
	 * during the destroy process.  An example of a class which does
	 * things differently is the widget class.  In the case of a widget
	 * the first call destroys the widget and all of its children, even
	 * though the object descriptors are not freed until the remainder of
	 * the code below is executed.  This allows the window system toolkit
	 * code to determine the best way to destroy a widget hierarchy,
	 * rather than having us destroy each widget one by one from the
	 * bottom up.
	 */
	(*(object->core.classrec->Destroy)) (object);

	/* Destroy any children.  Note that each time a child is destroyed the
	 * child list in the object descriptor is modified, so we merely loop
	 * until nchildren goes to zero.
	 */
	if (cp->nchildren) {
	    while (cp->nchildren > 0)
		obmDestroyObject (obm, cp->children[0]);
	}

	/* Remove this object from the child list of the parent. */
	if (obj = object->core.parent) {
	    if (obj->core.nchildren == 1) {
		XtFree ((char *)obj->core.children);
		obj->core.nchildren = 0;

	    } else {
		int nchild = obj->core.nchildren;
		ObmObject *new_list, *ip, *op;

		new_list = (ObmObject *) XtMalloc (nchild * sizeof(ObmObject));
		op = new_list;

		for (ip = obj->core.children;  --nchild >= 0;  ip++)
		    if (*ip != object)
			*op++ = *ip;

		XtFree ((char *)obj->core.children);
		obj->core.children = new_list;
		obj->core.nchildren--;
	    }
	}

	/* Unlink the object from the global object list. */
	if (obj = object->core.prevglob) {
	    if (!(obj->core.nextglob = object->core.nextglob))
		obm->tail = obj;
	} else
	    obm->head = object->core.nextglob;

	if (obj = object->core.nextglob) {
	    if (!(obj->core.prevglob = object->core.prevglob))
		obm->head = obj;
	} else
	    obm->tail = object->core.prevglob;

	/* Unlink the object from the hash list. */
	ip = object->core.name;
	for (hashval=0, n=MAX_HASHCHARS;  --n >= 0 && *ip;  ip++)
	    hashval += (hashval + *ip);
	if (obj = obm->objindex[hashval%SZ_INDEX]) {
	    while (obj && obj->core.nexthash != object)
		obj = obj->core.nexthash;
	    if (obj)
		obj->core.nexthash = object->core.nexthash;
	    else
		obm->objindex[hashval%SZ_INDEX] = NULL;
	}

	/* Free the object descriptor. */
	(*(object->core.classrec->Destroy)) (object);
	XtFree ((char *)object);
}


/* obmDisplay -- Display an entire user interface, including all top level
 * shells.
 */
void
obmDisplay (obm, obj)
ObmContext obm;
ObmObject obj;
{
	register Widget w = widgetGetPointer (obj);
	register ObmObject child;
	register int i;
	char buf[SZ_NAME];

	for (i=0;  i < obj->core.nchildren;  i++) {
	    child = obj->core.children[i];
	    if (child->core.classrec->object_type == OtShell)
		obmDisplay (obm, child);
	}

	/* The following isn't used anymore. */
	if (obj->core.geometry[0])
	    XtVaSetValues (w, XtNgeometry, obj->core.geometry, NULL);

	XtRealizeWidget (w);
	if (obj->core.mapped)
	    XtMapWidget (w);
}


/* obmUndisplay -- Undisplay an entire user interface, including all top level
 * shells.
 */
void
obmUndisplay (obm, obj)
ObmContext obm;
ObmObject obj;
{
	register int i;
	register ObmObject child;
	register Widget w = widgetGetPointer (obj);
	XWindowAttributes wa;
	char *s;

	for (i=0;  i < obj->core.nchildren;  i++) {
	    child = obj->core.children[i];
	    if (child->core.classrec->object_type == OtShell)
		obmUndisplay (obm, child);
	}

	if (XtWindow(w)) {
	    /* The following isn't used anymore. */
	    if (s = get_geometry (obm->display, obm->screen, XtWindow(w), 1))
		strcpy (obj->core.geometry, s);
	    if (XGetWindowAttributes (obm->display, XtWindow(w), &wa))
		obj->core.mapped = (wa.map_state != IsUnmapped);
	    /* XtUnrealizeWidget (w); */

	    /* Unrealizing the widgets is too drastic, the following merely
	     * makes the window and icon disappear.
	     */
	    XmuUpdateMapHints (obm->display, XtWindow(w), NULL);
	    XWithdrawWindow (obm->display, XtWindow(w),
		XScreenNumberOfScreen(obm->screen));
	}
}


/* obmGetClassrec -- Get the class record for the named class.
 */
ObjClassRec
obmGetClassrec (classname)
char *classname;
{
	register ObjClassRec classrec;
	register int i;

	for (i=0;  i < XtNumber(UiObjects);  i++) {
	    classrec = &UiObjects[i];
	    if (strcmp (classname, classrec->name) == 0)
		return (classrec);
	}

	return (NULL);
}


/* obmGenericClassDestroy - Free any resources associated with a class record.
 */
void
obmGenericClassDestroy (obm, classrec)
ObmContext obm;
register ObjClassRec classrec;
{
}


/* obmClass -- Test if a class record belongs to the given object class.
 */
obmClass (classrec, flag1, flag2)
register ObjClassRec classrec;
unsigned long flag1, flag2;
{
	return ((classrec->flag1 & flag1) || (classrec->flag2 & flag2));
}


/* obmAddCallback -- Add a callback descriptor to a callback list.
 */
ObmCallback
obmAddCallback (callback_list)
ObmCallback *callback_list;
{
	register ObmCallback cb, last_cb;

	/* Find tail of list. */
	for (cb = last_cb = *callback_list;  cb;  cb = cb->next)
	    last_cb = cb;

	if (!(cb = (ObmCallback) XtCalloc (1, sizeof (obmCallback))))
	    return (NULL);

	if (!last_cb)
	    *callback_list = cb;
	else
	    last_cb->next = cb;

	return (cb);
}


/* obmRemoveCallback -- Remove a callback descriptor from a callback list.
 */
void
obmRemoveCallback (callback_list, callback)
ObmCallback *callback_list;
ObmCallback callback;
{
	register ObmCallback cb, last_cb;

	if (!callback)
	    return;

	/* Search for named callback descriptor. */
	for (cb = *callback_list, last_cb = NULL;  cb;  cb = cb->next) {
	    if (cb == callback)
		break;
	    else
		last_cb = cb;
	}

	if (last_cb)
	    last_cb->next = cb->next;
	else
	    *callback_list = cb->next;

	XtFree ((char *) cb);
}


/* obmDefined -- Test if the named function is a defined client function in
 * the given Tcl interepter.
 */
obmClientCommand (tcl, commmand)
Tcl_Interp *tcl;
char *commmand;
{
	register char *ip, *op;
	char name[SZ_NAME];
	Tcl_CmdInfo info;

	/* Get command name.  This works even if we are passed a command
	 * line including arguments.
	 */
	for (ip=commmand;  *ip && isspace(*ip);  ip++)
	    ;
	for (op=name;  *ip && !isspace(*ip) && *ip != ';';  )
	    *op++ = *ip++;
	*op = '\0';
	if (op - name == 0)
	    return (False);

	/* Test if the named client command exists.  This assumes that
	 * client commands always have the clientData field set to a
	 * non-NULL value.  Tcl (at least in the current version) sets
	 * this field to NULL for the builtin commands.
	 */
	if (Tcl_GetCommandInfo (tcl, name, &info))
	    return (info.clientData != NULL);
	else
	    return (False);
}
