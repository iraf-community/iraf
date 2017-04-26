/* Copyright(c) 1993 Association of Universities for Research in Astronomy Inc.
 */

#include <ObmP.h>


/*
 * CLIENT class.
 * --------------------------
 * The client is the client application, which provides the functionality
 * underlying the UI.  When a message is sent to the client object it usually
 * results in a message being sent to the client *application*, usually an
 * external program communicating via IPC, which has little or no knowledge
 * of the UI.  The client application receives and executes commands delivered
 * by the UI via the client object.  Output from the client may or may not
 * come back to the object manager.  That portion of the output which comes
 * back to the object manager is in the form of assignments of string values
 * to UI parameter-class objects (another way of thinking of this is that
 * messages or events are sent to and acted upon by the parameter objects).
 * Hence, the client object is output only so far as the client application
 * is concerned.
 *
 * The Client-class commands are used to send a message to the client.
 *
 *                 gkey <key>
 *                 gcmd <command-string>
 *              literal <command>
 *
 * or just <command>, e.g., "send client <command>" will work in most cases.
 *
 * GKEY sends and IRAF graphics keystroke.  GCMD sends and IRAF graphics
 * colon command.  LITERAL sends a literal command string to the client.
 * The keyword "literal" may optionally be omitted, i.e., "send client foo"
 * and "send client literal foo" are the same.  The keyword "literal" may
 * be used to ensure that the client command string which follows will not
 * be interpreted as a Client-class command (such as gkey, gcmd, or literal).
 */

struct clientPrivate {
	ObmContext obm;
	Tcl_Interp *tcl;
};

typedef	struct clientPrivate *ClientPrivate;

struct clientObject  {
	struct obmObjectCore core;
	struct clientPrivate client;
};

typedef	struct clientObject *ClientObject;

static	void ClientDestroy();
static	int ClientEvaluate();
static	ObmObject ClientCreate();
static	int clientGcmd(), clientGkey(), clientLiteral();
static	int client_output();


/* ClientClassInit -- Initialize the class record for the client class.
 */
void
ClientClassInit (obm, classrec)
ObmContext obm;
register ObjClassRec classrec;
{
	classrec->ClassDestroy = obmGenericClassDestroy;
	classrec->Create = (ObmFunc) ClientCreate;
	classrec->Destroy = ClientDestroy;
	classrec->Evaluate = ClientEvaluate;
}


/* ClientCreate -- Create an instance of a client object.
 */
static ObmObject
ClientCreate (obm, name, classrec, parent, args, nargs)
ObmContext obm;
char *name;
ObjClassRec classrec;
char *parent;
ArgList args;
int nargs;
{
	register ClientObject obj;
	register Tcl_Interp *tcl;

	obj = (ClientObject) XtCalloc (1, sizeof (struct clientObject));
	obj->client.tcl = tcl = Tcl_CreateInterp();
	obj->client.obm = obm;

	/* Register client-object actions.  */
	Tcl_CreateCommand (tcl,
	    "gcmd", clientGcmd, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "gkey", clientGkey, (ClientData)obj, NULL);
	Tcl_CreateCommand (tcl,
	    "literal", clientLiteral, (ClientData)obj, NULL);

	return ((ObmObject) obj);
}


/* ClientDestroy -- Destroy an instance of a client object.
 */
static void
ClientDestroy (object)
ObmObject object;
{
	register ClientObject obj = (ClientObject) object;

	if (obj->core.being_destroyed++)
	    Tcl_DeleteInterp (obj->client.tcl);
}


/* ClientEvaluate -- Evaluate a client command or message.
 */
static int
ClientEvaluate (object, command)
ObmObject object;
char *command;
{
	register ClientObject obj = (ClientObject) object;
	register Tcl_Interp *tcl = obj->client.tcl;
	int status, argc, i;
	char *argv[MAX_ARGS];
	char **argvp;

	if (!obmClientCommand (tcl, command))
	    goto literal;

	/* If the command is unrecognized pass it on to the client as a
	 * literal to be processed by the client.
	 */
	if ((status = Tcl_Eval (tcl, command)) != TCL_OK) {
literal:    if (Tcl_SplitList (tcl, command, &argc, &argvp) == TCL_OK) {
		argv[0] = "literal";
		if (argc > MAX_ARGS)
		    argc = MAX_ARGS;
		for (i=0;  i <= argc;  i++)
		    argv[i+1] = argvp[i];

		status = clientLiteral (object, tcl, argc + 1, argv);
		free ((char *) argvp);
	    }
	}

	return (status);
}


/* clientGcmd -- Send a graphics command string to the client application.
 * A graphics command string is a graphics cursor value with the key set
 * to `:' and the command string given as the string part of the cursor
 * value.  The protocol module which posted the client output procedure is
 * responsible for encoding and sending the cursor command.
 *
 *  Usage:	gcmd <command-string>
 */
static int 
clientGcmd (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ClientObject obj = (ClientObject) object;
	register ObmContext obm = obj->client.obm;
	int stat;

	if (argc >= 2) {
	    char *message = Tcl_Concat (argc-1, &argv[1]);
	    stat = client_output (obm, obj->core.name, ':', message);
	    free ((char *)message);
	} else
	    stat = -1;

	return (stat < 0 ? TCL_ERROR : TCL_OK);
}


/* clientGkey -- Send a graphics key event to the client application.
 * A graphics key event is a graphics cursor value with the key set to some
 * integer value and a null string part.
 *
 *  Usage:	gkey <key>
 */
static int 
clientGkey (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ClientObject obj = (ClientObject) object;
	register ObmContext obm = obj->client.obm;
	int stat;

	if (argc >= 2)
	    stat = client_output (obm, obj->core.name, *argv[1], "");
	else
	    stat = -1;

	return (stat < 0 ? TCL_ERROR : TCL_OK);
}


/* clientLiteral -- Send a literal command to the client application.
 *
 *  Usage:	literal <command>
 */
static int 
clientLiteral (object, tcl, argc, argv)
ObmObject object;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register ClientObject obj = (ClientObject) object;
	register ObmContext obm = obj->client.obm;
	int stat;

	if (argc >= 2) {
	    char *message = Tcl_Concat (argc-1, &argv[1]);
	    stat = client_output (obm, obj->core.name, 0, message);
	    free ((char *)message);
	} else
	    stat = -1;

	return (stat < 0 ? TCL_ERROR : TCL_OK);
}


/* client_output -- Call the client output callbacks if any.
 */
static int
client_output (obm, objname, key, strval)
register ObmContext obm;
char *objname;
int key;
char *strval;
{
	register ObmCallback cb;
	register int stat = 0;

	for (cb = obm->callback_list;  cb;  cb = cb->next)
	    if ((cb->callback_type & OBMCB_clientOutput) && cb->u.fcn)
		stat |= ((*cb->u.fcn) (cb->client_data, obm->tcl,
			     objname, key, strval));

	return (stat != 0);
}
