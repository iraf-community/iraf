/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

/**
 *  SAMP.C -- Interface routines for the client and server side of the 
 *  SAMP messaging commands.
 */

#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <signal.h>
#include <pthread.h>
#include <stdio.h>
#include <readline/readline.h>		/* to install rl_event_hook	*/

#define import_spp
#define import_libc
#define import_stdio
#define import_prstat
#define import_xwhen
#include <iraf.h>

#include "config.h"			/* CL declarations		*/
#include "clmodes.h"
#include "operand.h"
#include "mem.h"
#include "grammar.h"
#include "opcodes.h"
#include "param.h"
#include "task.h"
#include "errs.h"
#include "clsamp.h"


extern pid_t cl_pid;
extern int   samp_registered;
extern char  samp_cmd[SZ_CMDBLK];

extern int   optbl[];
extern char *ifnames[];

extern Handler userHandlers[];
extern int     numHandlers;

extern	XINT	samp;				/* SAMP handle 		*/
extern  pthread_mutex_t samp_mutex;		/* global data mutex	*/

extern	char   *voGetStrArg ();




/*****************************************************************************/



/* SAMPDBG -- Toggle the XML-RPC tracing flag.
 *
 * Usage:   cl> =sampDbg (ival)			# explicitly set debug value
 *          cl> =sampDbg ()			# toggle debug value
 */
void
func_sampDbg (void)
{
    register struct pfile *pfp;
    struct operand o;
    static int sampDebug = 0;
    extern int nargs();


    pfp = newtask->t_pfp;
    if (nargs (pfp) > 0) {
        pushbparams (pfp->pf_pp);
        popop();                            	/* discard the $1       */
        o = popop();
        if (o.o_type != OT_INT)
            cl_error (E_UERR, "samp trace arg should be an integer");
        sampDebug = o.o_val.v_i;

    } else					/* toggle it		*/
        sampDebug = (sampDebug+1) % 2;

    unsetenv ("XMLRPC_TRACE_XML");
    if (sampDebug)
	  setenv ("XMLRPC_TRACE_XML", "1", 1);

    o.o_type = OT_BOOL;
    o.o_val.v_i = sampDebug;
    pushop (&o);
}


/* SAMPSTATUS -- Print or set the status of the samp interface.
 */
void
func_sampStatus (int nargs)
{
    char   *arg = NULL;
    int     i, stat = samp_registered, len = 0;
    struct  operand o;


    if (nargs > 0) {

        arg = voGetStrArg ();

	if (strcasecmp ("on", arg) == 0 || 
	    strcasecmp ("start", arg) == 0) {
                if (!samp_hubActive (samp))
                    cmd_sampStart ();
                if (samp_registered && samp_hubActive (samp))
                    stat = 1;

	} else if (strcasecmp ("off", arg) == 0 || 
	    strcasecmp ("stop", arg) == 0) {
                if (samp_registered)
                    cmd_sampStop ();
		stat = samp_registered;

	} else if (strcasecmp ("restart", arg) == 0) {
	    cmd_sampRestart ();
	    stat = samp_registered;
	}

	if (arg)  free ((void *) arg);
    }

    o.o_type = OT_STRING;
    o.o_val.v_s = (stat ? "on" : "off");
    pushop (&o);
}


/* SAMPHUBACCESS -- Check to see if a Hub is running and available.
 */
void
func_sampHubAccess (int nargs)
{
    struct  operand o;
    int     found = 0, verb;
    char   *home = envget ("HOME");
    char   path[SZ_LINE];

    if (samp >= 0) {
        verb = sampVerbose (samp, -1);
        sampVerbose (samp, 0);
	memset (path, 0, SZ_LINE);
	sprintf (path, "%s/.samp", home);
	found = (c_access (path, 0, 0) == YES);
        sampVerbose (samp, verb);
    }

    o.o_type = OT_BOOL;
    o.o_val.v_i = found;
    pushop (&o);
}


/* SAMPACCESS -- Require an external application to be registered.  We don't
 * attempt to start the application ourselves, we simply report on whether
 * it is currently available.
 */
void
func_sampAccess (int nargs)
{
    char   *appName = NULL, *app = NULL;
    int     i, found = 0, len = 0;
    struct  operand o;

    if (nargs > 0) {
        XINT clients;
	char   *pubId = NULL;
	extern char *samp_getStringFromList(), *samp_app2id();

        appName = voGetStrArg ();
	if (strcasecmp ("hub", appName) == 0) 
	    pubId = "hub";
	else 
	    pubId =  samp_app2id (samp, appName);

	/*  Search the list for a matching appName
	 */
        clients = samp_GetRegisteredClients (samp);
        for (i=0; i < samp_listLen (clients); i++) {
	    app = samp_getStringFromList (clients, i);
	    len = min (strlen (pubId), strlen (app));
            if (strncasecmp (pubId, app, len) == 0) {
		found = 1;
		break;
	    }
	}
        samp_freeList (clients);

    } else 
        cl_error (E_UERR, "Application name must be specifified");

    o.o_type = OT_BOOL;
    o.o_val.v_i = found;
    pushop (&o);
}


/* SAMPNAME -- Set (or print) a current SAMP application name.
 */
void
func_sampName (int nargs)
{
    char   *name = NULL, res[SZ_LINE];
    struct operand o;

    if (nargs == 1) {
        name  = voGetStrArg ();

        samp_Metadata (samp, "samp.name", name);
	samp_DeclareMetadata (samp);

        /* Push the result operand on the stack.
         */
	strcpy (res, name);
        o.o_type = OT_STRING;
        o.o_val.v_s = res;
        pushop (&o);

	if (name)  free ((void *) name);

    } else if (nargs == 0) {	/*  list currently defined metadata  	*/
	extern  XINT  samp;
	char  *v = NULL;

        /* Push the result operand on the stack.
         */
        v = samp_getMetadata(samp, "samp.name");
        o.o_type = OT_STRING;
        o.o_val.v_s = v;
        pushop (&o);

    } else
        cl_error (E_UERR, "usage: sampName ([name])");
}


/* SAMPMETADATA -- Set (or print) a current SAMP metadata parameters.
 */
void
func_sampMetadata (int nargs)
{
    char   *param = NULL, *value = NULL;
    struct operand o;

    if (nargs == 2) {
        value  = voGetStrArg ();	/* ars on stack in reverse order */
        param  = voGetStrArg ();

        samp_Metadata (samp, param, value);
	samp_DeclareMetadata (samp);

	if (param)  free ((void *) param);
	if (value)  free ((void *) value);

        /* Push the result operand on the stack.
         */
        o.o_type = OT_STRING;
        o.o_val.v_s = "ok";
        pushop (&o);

    } else if (nargs == 1) {	/*  list currently defined metadata  	*/
        param  = voGetStrArg ();
	samp_printMetadata (samp, param);

    } else
        cl_error (E_UERR, "usage: sampMetadata ([ [name], [value] ])");
}


/* CL_SAMPRESTART -- Restart the SAMP connection.
 */
void
func_sampRestart (void)
{
    cmd_sampStop ();
    cmd_sampStart ();
}


/* SAMPSTART -- Register with the SAMP Hub and begin messaging.
 */
void
func_sampStart (void)
{
    if (!samp_registered) {
        cmd_sampStart ();
        samp_registered = 1;
    } else
	cmd_sampRestart();			/*  disconnect and restart  */
}


/* SAMPSTOP -- UnRegister from the SAMP Hub and stop messaging.
 */
void
func_sampStop (void)
{
    if (samp_registered) {
        samp_registered = 0;
	cmd_sampStop ();
    }
}


/*  SAMPSEND -- Send a generic message.  The format of a message on the 
 *  cmdline must be:
 *
 *	cl> sampSend <mtype> [to=<appName>] [<param>=<value> .....]
 *
 *  If there is no 'to' argument the message will be broadcast to all
 *  recipients, otherwise this is expected to be the name of the receiving
 *  application.  The mtype and parameters may correspond to well-known
 *  message type, in which case this is a low-level interface to sending
 *  those messages.  However, this method can also be used to send arbitrary
 *  messages using private mtypes.
 */
#define	MAX_ARGS	32

void
func_sampSend (void)
{
    register struct pfile *pfp;
    char   *mtype = NULL, *to = "all", *nam = NULL, *val = NULL;
    char   *args[MAX_ARGS];
    int    i, stat, numArgs;
    struct operand o;
    extern int nargs();


    pfp = newtask->t_pfp;
    numArgs = nargs (pfp);
    if (numArgs > 0) {
        pushbparams (pfp->pf_pp);

	/*  Simple, first argument has to be the mtype.
	 */
        o = popop();                           	/* discard the $1       */
        mtype = voGetStrArg ();

	memset (args, 0, (MAX_ARGS * sizeof(char *)));
	for (i=1; i < numArgs; i++) {
            o = popop();                       	/* param nam  or $N	*/
	    nam = o.o_val.v_s;

            o = popop();                       	/* param nam  or $N	*/
            val = o.o_val.v_s;

	    if (strcmp (nam, "to") == 0)
		to = val;
	    else {
		args[i-1] = calloc (1, SZ_LINE);;
		sprintf (args[i-1], "%s=%s", nam, val);
	    }
	}

	stat = samp_sendGeneric (samp, to, mtype, args);

	for (i=0; i < (numArgs - 1); i++) {
	    if (args[i]) 
		free ((void *) args[i]);
	}

    } else
        cl_error (E_UERR, "sampSend: No message specified\n");

    /*  Push the result on the stack.
     */
    o.o_type = OT_BOOL;
    o.o_val.v_i = stat;
    pushop (&o);
}



/*****************************************************************************
 *  SAMP builtin function definitions.  These are client-side commands.
 ****************************************************************************/

/**
 *  SAMP_HANDLER -- Add a user-defined handler to a specific mtype.
 *
 *  Usage:   sampHandler (mtype, cmd)
 *
 *  Examples:
 */

void
func_sampAddHandler (int nargs)
{
    char   *mtype, *cmd, *arg1, *arg2;
    int    i, res = -1;
    struct operand o;


    o.o_type = OT_STRING;

    if (nargs == 2) {
        arg2 = voGetStrArg ();		/* args on stack in reverse order */
        arg1 = voGetStrArg ();

	if (strncasecmp (arg1, "del", 3) == 0) {
	    /*  samp handler delete foo.bar
	     */
            mtype = arg2;
            res   = cl_delUserHandler (mtype); /*  add the handler	  */

	} else {
	    /* samp handler foo.bar "cmd $arg"
	     */
            cmd   = arg2;		/* args on stack in reverse order */
            mtype = arg1;
            res   = cl_addUserHandler (mtype, cmd); /*  add the handler	  */
	}
	if (arg1)  free ((void *) arg1);
	if (arg2)  free ((void *) arg2);

        o.o_val.v_s = (res < 0 ? "error" : "ok");

    } else if (nargs == 1) {
        arg1 = mtype = voGetStrArg ();

	if (strncasecmp (arg1, "del", 3) == 0) {
            int  i;

            for (i=0; i < numHandlers; i++) {
                memset (userHandlers[i].mtype, 0, SZ_FNAME);
                memset (userHandlers[i].cmd, 0, SZ_FNAME);
            }
            numHandlers = 0;

	} else {
            for (i=0; i < numHandlers; i++) {
	        /*  See if a handler is already defined for the given mtype.
	         */
	        if (strcmp (userHandlers[i].mtype, mtype) == 0) {
		    res = OK;
		    break;
	        }
	    }
	}
        o.o_val.v_s = (res < 0 ? "" : userHandlers[i].cmd);

    } else {                    /*  list currently defined handlers     */
	if (numHandlers == 0)
	    oprintf ("No SAMP handlers defined\n");
	else {
            for (i=0; i < numHandlers; i++)
                oprintf ("%-20.20s %s\n", 
		    userHandlers[i].mtype, userHandlers[i].cmd);
	}
        o.o_val.v_s = "ok";
    }
    
    pushop (&o); 		/* push the result operand on the stack   */
}


/**
 *  SAMP_LOADIMAGE -- Send a 'image.load.fits' message to other clients.
 *
 *  Usage:   sampLoadImage (file|url [, to [, name [, tag ]]])
 *
 *  Examples:
 *
 *  1) Broadcast the message to all subscribed clients:
 * 
 *	cl> = sampLoadImage ("http://iraf.noao.edu/votest/sif.fits")
 *	ok
 *
 *  2) Send the message to a named client:
 * 
 *	cl> = sampLoadImage ("data$foo.fits", "aladin")
 *	ok
 *
 *  3) Load the image with a given name:
 * 
 *	cl> = sampLoadImage ("/data/image001.fits", "aladin", "image1")
 *	ok
 *
 *  If a message is sent to a named client that either isn't connected or
 *  returns an error, our result is the error string.  On success, the string
 *  "ok" will be returned.  It is not considered an error if a broadcast 
 *  results in no clients actually receiving the message.
 */

void
func_sampLoadImage (int nargs)
{
    char      *what=NULL, *name=NULL, *tag=NULL, *to=NULL;
    char      url[SZ_PATHNAME+1], osfn[SZ_PATHNAME+1];
    struct    operand o;


    /* Parse any remaining (optional) arguments.  Remember that the args are
     * on the stack in the reverse order!  The 1st arg is required and will 
     * be either an ivo: identifier or is presumed to be the ShortName.
     */
    switch (nargs) {
    case 4: 
        tag = voGetStrArg ();
	/*  fall thru */
    case 3: 
        name = voGetStrArg ();
	/*  fall thru */
    case 2: 
        to = voGetStrArg ();
	/*  fall thru */
    case 1: 
        what = voGetStrArg ();
        if (strncmp(what, "http:", 5) == 0 || strncmp(what, "file:", 5) == 0) {
	    strcpy (url, what);
	} else {
	    c_fmapfn (what, osfn, SZ_PATHNAME);
	    if (c_access (osfn, READ_ONLY, 0) == NO) {
        	cl_error (E_UERR, "Cannot access image '%s'", what);
		return;
	    }
	    sprintf (url, "file://%s", osfn);
	}
	break;

    default:
        cl_error (E_UERR, "sampImLoad: invalid number of arguments\n");
	return;
    }

    if (name == NULL) name = strdup (what);
    if (tag  == NULL) tag  = strdup ("foo");
    if (to   == NULL) to   = strdup ("all");


    /*  Send the message.
     */
    o.o_type = OT_STRING;
    if (samp_imageLoadFITS (samp, to, url, tag, name) != 0)
        o.o_val.v_s = "ok";
    else
        o.o_val.v_s = samp_getErr (samp);
 
    /* Push the result operand on the stack.
     */
    pushop (&o);

    /* Clean up and return.				FIXME
     */
    if (tag)  free ((char *) tag);
    if (name) free ((char *) name);
    if (what) free ((char *) what);
    if (to)   free ((char *) to);
}


/**
 *  SAMP_LOADFITS -- Send a 'table.load.fits' message to other clients.
 *
 *  Usage:   sampLoadFITS (file|url [, to [, name [, tag ]]])
 *
 *  Examples:
 *
 *  1) Broadcast the message to all subscribed clients:
 * 
 *	cl> = sampLoadFITS ("http://iraf.noao.edu/votest/sif.fits")
 *	ok
 *
 *  2) Send the message to a named client:
 * 
 *	cl> = sampLoadFITS ("data$foo.fits", "aladin")
 *	ok
 *
 *  3) Load the image with a given name:
 * 
 *	cl> = sampLoadFITS ("/data/image001.fits", "aladin", "image1")
 *	ok
 *
 *  If a message is sent to a named client that either isn't connected or
 *  returns an error, our result is the error string.  On success, the string
 *  "ok" will be returned.  It is not considered an error if a broadcast 
 *  results in no clients actually receiving the message.
 */

void
func_sampLoadFITS (int nargs)
{
    char      *what=NULL, *name=NULL, *tag=NULL, *to=NULL;
    char      url[SZ_PATHNAME+1], osfn[SZ_PATHNAME+1];
    struct    operand o;


    /* Parse any remaining (optional) arguments.  Remember that the args are
     * on the stack in the reverse order!  The 1st arg is required and will 
     * be either an ivo: identifier or is presumed to be the ShortName.
     */
    switch (nargs) {
    case 4: 
        tag = voGetStrArg ();
	/*  fall thru */
    case 3: 
        name = voGetStrArg ();
	/*  fall thru */
    case 2: 
        to = voGetStrArg ();
	/*  fall thru */
    case 1: 
        what = voGetStrArg ();
        if (strncmp(what, "http:", 5) == 0 || strncmp(what, "file:", 5) == 0) {
	    strcpy (url, what);
	} else {
	    c_fmapfn (what, osfn, SZ_PATHNAME);
	    if (c_access (osfn, READ_ONLY, 0) == NO) {
        	cl_error (E_UERR, "Cannot access image '%s'", what);
		return;
	    }
	    sprintf (url, "file://%s", osfn);
	}
	break;

    default:
        cl_error (E_UERR, "sampImLoad: invalid number of arguments\n");
	return;
    }

    if (name == NULL) name = strdup (what);
    if (tag  == NULL) tag  = strdup ("foo");
    if (to   == NULL) to   = strdup ("all");


    /*  Send the message.
     */
    o.o_type = OT_STRING;
    if (samp_tableLoadFITS (samp, to, url, tag, name) != 0)
        o.o_val.v_s = "ok";
    else
        o.o_val.v_s = samp_getErr (samp);
 
    /* Push the result operand on the stack.
     */
    pushop (&o);

    /* Clean up and return.
     */
    if (tag)  free ((char *) tag);
    if (name) free ((char *) name);
    if (what) free ((char *) what);
    if (to)   free ((char *) to);
}


/**
 *  SAMP_LOADVOTABLE -- Send a 'table.load.votable' message to other clients.
 *
 *  Usage:   sampLoadVOTable (file|url [, to [, name [, tag ]]])
 *
 *  Examples:
 *
 *  1) Broadcast the message to all subscribed clients:
 * 
 *	cl> = sampLoadFITS ("http://iraf.noao.edu/votest/sif.fits")
 *	ok
 *
 *  2) Send the message to a named client:
 * 
 *	cl> = sampLoadFITS ("data$foo.fits", "aladin")
 *	ok
 *
 *  3) Load the image with a given name:
 * 
 *	cl> = sampLoadFITS ("/data/image001.fits", "aladin", "image1")
 *	ok
 *
 *  If a message is sent to a named client that either isn't connected or
 *  returns an error, our result is the error string.  On success, the string
 *  "ok" will be returned.  It is not considered an error if a broadcast 
 *  results in no clients actually receiving the message.
 */

void
func_sampLoadVOTable (int nargs)
{
    char      *what=NULL, *name=NULL, *tag=NULL, *to=NULL;
    char      url[SZ_PATHNAME+1], osfn[SZ_PATHNAME+1];
    struct    operand o;


    /* Parse any remaining (optional) arguments.  Remember that the args are
     * on the stack in the reverse order!  The 1st arg is required and will 
     * be either an ivo: identifier or is presumed to be the ShortName.
     */
    switch (nargs) {
    case 4: 
        tag = voGetStrArg ();
	/*  fall thru */
    case 3: 
        name = voGetStrArg ();
	/*  fall thru */
    case 2: 
        to = voGetStrArg ();
	/*  fall thru */
    case 1: 
        what = voGetStrArg ();
        if (strncmp(what, "http:", 5) == 0 || strncmp(what, "file:", 5) == 0) {
	    strcpy (url, what);
	} else {
	    c_fmapfn (what, osfn, SZ_PATHNAME);
	    if (c_access (osfn, READ_ONLY, 0) == NO) {
        	cl_error (E_UERR, "Cannot access file '%s'", what);
		return;
	    }
	    sprintf (url, "file://%s", osfn);
	}
	break;

    default:
        cl_error (E_UERR, "sampLoadVOTable: invalid number of arguments\n");
	return;
    }

    if (name == NULL) name = strdup (what);
    if (tag  == NULL) tag  = strdup ("foo");
    if (to   == NULL) to   = strdup ("all");


    /*  Send the message.
     */
    o.o_type = OT_STRING;
    if (samp_tableLoadVOTable (samp, to, url, tag, name) != 0)
        o.o_val.v_s = "ok";
    else
        o.o_val.v_s = samp_getErr (samp);
 
    /* Push the result operand on the stack.
     */
    pushop (&o);

    /* Clean up and return.
     */
    if (tag)  free ((char *) tag);
    if (name) free ((char *) name);
    if (what) free ((char *) what);
    if (to)   free ((char *) to);
}


/**
 *  SAMP_SHOWROW -- Send a 'table.highlight.row' message to other clients.
 *
 *  Usage:   sampShowRow (url, id, row [, to])
 */

void
func_sampShowRow (int nargs)
{
    register int i, row=0, stat = 0;
    char   *to=NULL, *srow=NULL, *what=NULL, *tblId=NULL;
    char   osfn[SZ_PATHNAME],  url[SZ_URL];
    struct operand o;


    memset (url, 0, SZ_URL);
    switch (nargs) {
    case 4: 
        to = voGetStrArg ();
	/*  fall thru */
    case 3: 
	row = atoi ((srow = voGetStrArg ()));
	/*  fall thru */
    case 2: 
        tblId = voGetStrArg ();
	/*  fall thru */
    case 1: 
        what = voGetStrArg ();
        if (strncmp(what, "http:", 5) == 0 || strncmp(what, "file:", 5) == 0) {
	    strcpy (url, what);
	} else {
	    c_fmapfn (what, osfn, SZ_PATHNAME);
	    if (c_access (osfn, READ_ONLY, 0) == NO) {
        	cl_error (E_UERR, "Cannot access file '%s'", what);
		return;
	    }
	    sprintf (url, "file://%s", osfn);
	}
	break;
    default:
        cl_error (E_UERR, "usage: sampShowRow (url, tblId, row[, to])");
	return;
    }

    if (!to)  to = strdup ("all");

    stat = samp_tableHighlightRow (samp, to, tblId, url, row);

    if (to)    free ((void *) to);
    if (srow)  free ((void *) srow);
    if (tblId) free ((void *) tblId);
    if (what)  free ((void *) what);

    /* Push the result operand on the stack.
     */
    o.o_type = OT_STRING;
    o.o_val.v_s = (stat != 0 ? "ok" : samp_getErr(samp));
    pushop (&o);
}


/**
 *  SAMP_SELECTROWLIST -- Send a 'table.select.rowList' message to other 
 *  clients.
 *
 *  Usage:   sampSelectRowList (url, id, row [, to])
 */

#define MAX_ROWSELECT           1024

void
func_sampSelectRowList (int nargs)
{
    int    i, nrows = 0, stat = 0, rows[MAX_ROWSELECT];
    char   *to=NULL, *srow=NULL, *what=NULL, *tblId=NULL;
    char   osfn[SZ_PATHNAME], url[SZ_URL], *ip, *n;
    struct operand o;


    memset (url, 0, SZ_URL);
    switch (nargs) {
    case 4: 
        to = voGetStrArg ();
	/*  fall thru */
    case 3: 
	srow = voGetStrArg ();
	/*  fall thru */
    case 2: 
        tblId = voGetStrArg ();
	/*  fall thru */
    case 1: 
        what = voGetStrArg ();
        if (strncmp(what, "http:", 5) == 0 || strncmp(what, "file:", 5) == 0) {
	    strcpy (url, what);
	} else {
	    c_fmapfn (what, osfn, SZ_PATHNAME);
	    if (c_access (osfn, READ_ONLY, 0) == NO) {
        	cl_error (E_UERR, "Cannot access file '%s'", what);
		return;
	    }
	    sprintf (url, "file://%s", osfn);
	}
	break;
    default:
        cl_error (E_UERR, "usage: sampShowRow (url, tblId, row[, to])");
	return;
    }

    if (!to)  to = strdup ("all");

    /* Convert the row list string into an int array.
     */
    for (ip=srow, n=srow, nrows=0; n; ip=n ) {
        if ( (n = strchr (ip, (int) ',')) )
            *n++ = '\0';
        rows[nrows++] = atoi (ip);
    }

    stat = samp_tableSelectRowList (samp, to, tblId, url, rows, nrows);

    if (to)    free ((void *) to);
    if (srow)  free ((void *) srow);
    if (tblId) free ((void *) tblId);
    if (what)  free ((void *) what);

    /* Push the result operand on the stack.
     */
    o.o_type = OT_STRING;
    o.o_val.v_s = (stat != 0 ? "ok" : samp_getErr(samp));
    pushop (&o);
}


/**
 *  SAMP_POINTAT -- Send a 'coords.pointAt.sky' message to other clients.
 *
 *  Usage:   sampPointAt (ra, dec[, to])
 */

void
func_sampPointAt (int nargs)
{
    register  int i, stat;
    char   *arg = NULL, to[SZ_LINE];
    float  ra = -1.0, dec = 0.0;
    struct operand o;

    if (nargs > 0) {
	strcpy (to, "all");
	for (i=0; i < nargs; i++) {
            arg = voGetStrArg ();	/* args on stack in reverse order */

            if (isalpha ((int) arg[0]))         /* recipient arg          */
                strcpy (to, arg);
            else if (ra < 0)                    /* not initialized yet    */
                dec = atof (arg);
            else
                ra = atof (arg);

            if (arg)  free ((char *) arg);
	}

        stat = samp_coordPointAtSky (samp, to, ra, dec);

        /* Push the result operand on the stack.
         */
        o.o_type = OT_STRING;
        o.o_val.v_s = (stat < 0 ? "error" : "ok");
        pushop (&o);

    } else
        cl_error (E_UERR, "usage: sampPointAt (ra_deg, dec_deg)");
}


/**
 *  SAMP_SPECLOAD -- Send a 'spec.load.ssa-generic' message to other clients.
 *
 *  Usage:   sampSpecLoad ()
 */

void
func_sampSpecLoad (int nargs)
{
}


/**
 *  SAMP_BIBCODELOAD -- Send a 'bibcode.load' message to other clients.
 *
 *  Usage:   sampBibcodeLoad (bibcode[, to])
 */

void
func_sampBibcodeLoad (int nargs)
{
    register int stat = 0;
    char   *to=NULL, *bibcode=NULL;
    struct operand o;


    switch (nargs) {
    case 2: 
        to = voGetStrArg ();
	/*  fall thru  */
    case 1: 
        bibcode = voGetStrArg ();
	break;
    default:
        cl_error (E_UERR, "usage: sampShowRow (tblId, url, row[, to])");
	return;
    }

    if (!to)  to = strdup ("all");

    stat = samp_bibLoad (samp, to, bibcode);

    if (to)      free ((void *) to);
    if (bibcode) free ((void *) bibcode);

    /* Push the result operand on the stack.
     */
    o.o_type = OT_STRING;
    o.o_val.v_s = (stat != 0 ? "ok" : samp_getErr(samp));
    pushop (&o);
}


/**
 *  SAMP_CMDEXEC -- Send a 'client.cmd.exec' message to other clients.
 *
 *  Usage:   sampCmdExec (cmd[, to]))
 */

void
func_sampCmdExec (int nargs)
{
    register int stat = 0;
    char   *to=NULL, *cmd=NULL;
    struct operand o;


    switch (nargs) {
    case 2: 
        to = voGetStrArg ();
	/*  fall thru  */
    case 1: 
        cmd = voGetStrArg ();
	break;
    default:
        cl_error (E_UERR, "usage: sampCmdExec (cmd[, to])");
	return;
    }

    if (!to)  to = strdup ("all");

    stat = samp_cmdExec (samp, to, cmd);

    if (to)   free ((void *) to);
    if (cmd)  free ((void *) cmd);

    /* Push the result operand on the stack.
     */
    o.o_type = OT_STRING;
    o.o_val.v_s = (stat != 0 ? "ok" : samp_getErr(samp));
    pushop (&o);
}


/**
 *  SAMP_ENVGET -- Send a 'client.env.get' message to other clients.
 *
 *  Usage:   sampEnvGet (param[, to]))
 */

void
func_sampEnvGet (int nargs)
{
    char   *res=NULL, *to=NULL, *param=NULL;
    struct operand o;


    switch (nargs) {
    case 2: 
        to = voGetStrArg ();
	/*  fall thru  */
    case 1: 
        param = voGetStrArg ();
	break;
    default:
        cl_error (E_UERR, "usage: sampEnvGet (param[, to])");
	return;
    }

    if (!to)  to = strdup ("all");

    res = samp_envGet (samp, to, param);

    if (to)    free ((void *) to);
    if (param) free ((void *) param);

    /* Push the result operand on the stack.
     */
    o.o_type = OT_STRING;
    o.o_val.v_s = res;
    pushop (&o);
}


/**
 *  SAMP_ENVSET -- Send a 'client.env.set' message to other clients.
 *
 *  Usage:   sampEnvSet (param, val[, to]))
 */

void
func_sampEnvSet (int nargs)
{
    register int stat = 0;
    char   *res=NULL, *to=NULL, *param=NULL, *val=NULL;
    struct operand o;


    switch (nargs) {
    case 3: 
        to = voGetStrArg ();
	/*  fall thru  */
    case 2: 
        val = voGetStrArg ();
	/*  fall thru  */
    case 1: 
        param = voGetStrArg ();
	break;
    default:
        cl_error (E_UERR, "usage: sampEnvSet (param, val[, to])");
	return;
    }

    if (!to)  to = strdup ("all");

    stat = samp_envSet (samp, to, param, val);

    if (to)    free ((void *) to);
    if (param) free ((void *) param);
    if (val)   free ((void *) val);

    /* Push the result operand on the stack.
     */
    o.o_type = OT_STRING;
    o.o_val.v_s = (stat != 0 ? "ok" : samp_getErr(samp));
    pushop (&o);
}


/**
 *  SAMP_PARAMGET -- Send a 'client.param.get' message to other clients.
 *
 *  Usage:   sampParamGet (param[, to]))
 */

void
func_sampParamGet (int nargs)
{
    char   *res=NULL, *to=NULL, *param=NULL;
    struct operand o;


    switch (nargs) {
    case 2: 
        to = voGetStrArg ();
	/*  fall thru  */
    case 1: 
        param = voGetStrArg ();
	break;
    default:
        cl_error (E_UERR, "usage: sampParamGet (param[, to])");
	return;
    }

    if (!to)  to = strdup ("all");

    res = samp_paramGet (samp, to, param);

    if (to)    free ((void *) to);
    if (param) free ((void *) param);

    /* Push the result operand on the stack.
     */
    o.o_type = OT_STRING;
    o.o_val.v_s = res;
    pushop (&o);
}


/**
 *  SAMP_PARAMSET -- Send a 'client.param.set' message to other clients.
 *
 *  Usage:   sampParamSet (param, val[, to]))
 */

void
func_sampParamSet (int nargs)
{
    register int stat = 0;
    char   *res=NULL, *to=NULL, *param=NULL, *val=NULL;
    struct operand o;


    switch (nargs) {
    case 3: 
        to = voGetStrArg ();
	/*  fall thru  */
    case 2: 
        val = voGetStrArg ();
	/*  fall thru  */
    case 1: 
        param = voGetStrArg ();
	break;
    default:
        cl_error (E_UERR, "usage: sampParamSet (param, val[, to])");
	return;
    }

    if (!to)  to = strdup ("all");

    stat = samp_paramSet (samp, to, param, val);

    if (to)    free ((void *) to);
    if (param) free ((void *) param);
    if (val)   free ((void *) val);

    /* Push the result operand on the stack.
     */
    o.o_type = OT_STRING;
    o.o_val.v_s = (stat != 0 ? "ok" : samp_getErr(samp));
    pushop (&o);
}
