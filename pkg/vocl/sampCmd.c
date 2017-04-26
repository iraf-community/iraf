/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

/**
 *  SAMPCMD.C -- Procedures for the 'samp' user command.
 */

#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <ctype.h>
#include <signal.h>
#include <pthread.h>
#include <stdio.h>
#include <readline/readline.h>		/* to install rl_event_hook	*/

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

#include "config.h"			/* CL declarations		*/
#include "operand.h"
#include "task.h"
#include "errs.h"
#include "param.h"
#include "clsamp.h"


extern pid_t   cl_pid;
extern char    samp_cmd[SZ_CMDBLK];
extern char   *ifnames[];
extern Handler userHandlers[];
extern int     numHandlers;
extern int     samp_registered;
extern int     optbl[];

extern	XINT	samp;				/* SAMP handle 		*/
extern  pthread_mutex_t samp_mutex;		/* global data mutex	*/

extern	char   *voGetStrArg ();

int 	cl_sampStart (void);
int 	cl_sampStop (void);




/*****************************************************************************/



/* SAMPDBG -- Toggle the XML-RPC tracing flag.
 */
int
cmd_sampDbg (int nargs)
{
    struct operand o;
    static int sampDebug = 0;
    extern int samp_trace;


    if (nargs  > 0) {
        popop();                            	/* discard the $1       */
        o = popop();
        if (o.o_type != OT_INT)
            cl_error (E_UERR, "samp trace arg should be an integer");
        sampDebug = o.o_val.v_i;

    } else					/* toggle it		*/
        sampDebug = (sampDebug+1) % 2;

    unsetenv ("XMLRPC_TRACE_XML");
    if (sampDebug) {
	  setenv ("XMLRPC_TRACE_XML", "1", 1);
    }
    samp_trace = sampDebug;

    return (sampDebug);
}


/* CMD_SAMPADDHANDLER -- Set (or print) the user-defined handlers.
 */
int
cmd_sampAddHandler (int nargs)
{
    if (nargs > 0) {
	static char m[SZ_LINE], c[SZ_LINE];
        char  *mtype = m, *cmd = c, *arg1 = NULL, *arg2 = NULL;
        int    i, res = -1;

	memset (mtype, 0, SZ_LINE);
	memset (cmd, 0, SZ_LINE);

        popop();                            	/* discard the $1       */
        arg1 = mtype = voGetStrArg ();
	if (nargs > 1) {
            popop();                            /* discard the $2       */
            arg2 = cmd   = voGetStrArg ();
	}

	if (strncasecmp (arg1, "del", 3) == 0) {
            /*  Delete the handler, return (0 = OK, -1 = Error)
	     */
            return ((res = cl_delUserHandler (arg2)));

	} else {
            /*  Add the handler, return (0 = OK, -1 = Error)
	     */
            return ((res = cl_addUserHandler (mtype, cmd)));
	}

    } else { 			/*  list currently defined handlers  	*/
        int    i;

	if (numHandlers == 0)
	    oprintf ("No SAMP handlers defined\n");
	else {
	    for (i=0; i < numHandlers; i++)
	        oprintf ("%-20.20s %s\n", 
		    userHandlers[i].mtype, userHandlers[i].cmd);
	}
        return (0);				/* 0 = OK, -1 = Error	*/
    }
}


/* CMD_SAMPDELHANDLER -- Delete a user-defined handler.
 */
int
cmd_sampDelHandler (int nargs)
{
    if (nargs > 0) {
	static char m[SZ_LINE];
        char  *mtype = m;
        int    res = -1;

	memset (mtype, 0, SZ_LINE);

        popop();                            	/* discard the $1       */
        mtype = voGetStrArg ();

        /*  Delete the handler.
	 */
        return ((res = cl_delUserHandler (mtype)));

    } else { 			/*  list currently defined handlers  	*/
	int  i;

	for (i=0; i < numHandlers; i++) {
            memset (userHandlers[i].mtype, 0, SZ_FNAME);
            memset (userHandlers[i].cmd, 0, SZ_FNAME);
	}
	numHandlers = 0;

        return (0);				/* 0 = OK, -1 = Error	*/
    }
}


/* CMD_SAMPACCESS -- Require an external application to be registered.  We 
 * don't attempt to start the application ourselves, we simply report on 
 * whether it is currently available.
 */
int
cmd_sampAccess (int nargs)
{
    char   *appName = NULL, *app = NULL;
    int     i, found = 0, len = 0;


    if (nargs > 0) {
	extern XINT  samp;
        XINT clients;
	char   *pubId = NULL;
	extern char *samp_getStringFromList(), *samp_app2id();

        popop();                            	/* discard the $1       */
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

	if (appName)  
	    free ((void *) appName);
    } else 
	samp_listClients (samp);

    return (found);
}


/* CMD_SAMPNAME -- Set (or print) a current SAMP application name.
 */
int
cmd_sampName (int nargs)
{
    char   *name = NULL;
    extern  XINT  samp;


    if (nargs > 0) {
        popop();                            	/* discard the $1       */
        name = voGetStrArg ();

        samp_Metadata (samp, "samp.name", name);
	samp_DeclareMetadata (samp);

	if (name)  
	    free ((void *) name);
	return (1);

    } else {			/*  list currently defined metadata  	*/
	samp_printMetadata (samp, "samp.name");
	oprintf ("\n");
	return (0);
    }
}


/* CMD_SAMPMETADATA -- Set (or print) a current SAMP metadata parameters.
 */
int
cmd_sampMetadata (int nargs)
{
    char   *param = NULL, *value = NULL;
    extern  XINT  samp;


    if (nargs > 0) {
        popop();                            	/* discard the $1       */
        param = voGetStrArg ();
        popop();                            	/* discard the $2       */
        value   = voGetStrArg ();

        samp_Metadata (samp, param, value);
	samp_DeclareMetadata (samp);

	if (param)  
	    free ((void *) param);
	if (value)  
	    free ((void *) value);
	return (1);

    } else {			/*  list currently defined metadata  	*/
	samp_printMetadata (samp, NULL);
	return (0);
    }
}


/* CMD_SAMPRESTART -- Restart the SAMP connection.
 */
void
cmd_sampRestart (void)
{
    cmd_sampStop ();
    cmd_sampStart ();
}


/* CMD_SAMPSTART -- Register with the SAMP Hub and begin messaging.
 */
void
cmd_sampStart (void)
{
    extern XINT  samp;

    if (samp >= 0 && !samp_registered)
	cl_sampStart ();
    else
	cmd_sampRestart();			/*  disconnect and restart  */
}


/* CMD_SAMPSTOP -- UnRegister from the SAMP Hub and stop messaging.
 */
void
cmd_sampStop (void)
{
    extern  XINT  samp;

    if (samp >= 0 && samp_registered) {
        samp_registered = 0;
	cl_sampStop ();
    }
}


/*  CMD_SAMPSEND -- Send a generic message.  The format of a message on the 
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

int
cmd_sampSend (int nargs)
{
    char   *mtype = NULL, *to = "all", *nam = NULL, *val = NULL;
    char   *args[MAX_ARGS];
    int    i, stat = -1;
    struct operand o;


    if (nargs > 0) {
	/*  Simple, first argument has to be the mtype.
	 */
        o = popop();                           	/* discard the $1           */
        mtype = voGetStrArg ();

	memset (args, 0, (MAX_ARGS * sizeof(char *)));
	for (i=1; i < nargs; i++) {
            o = popop();                       	/* param nam  or $N	    */
	    nam = o.o_val.v_s;

            o = popop();                       	/* param nam  or $N	    */
            val = o.o_val.v_s;

	    if (strcmp (nam, "to") == 0)
		to = val;		/* special case for recipient       */
	    else {
		args[i-1] = calloc (1, SZ_LINE);;
		sprintf (args[i-1], "%s=%s", nam, val);
	    }
	}

	stat = samp_sendGeneric (samp, to, mtype, args);

	for (i=0; i < (nargs - 1); i++)
	    if (args[i]) 
		free ((void *) args[i]);

	if (stat < 0)				/* error return		 */
            cl_error (E_UERR, samp_getErr (samp));

    } else
        cl_error (E_UERR, "sampSend: No message specified\n");

    return (stat);
}


/*****************************************************************************
 *  SAMP builtin function definitions.  These are client-side commands.
 ****************************************************************************/

/**
 *  SAMP_LOADIMAGE -- Send a 'image.load.fits' message to other clients.
 */

int
cmd_sampLoadImage (int nargs)
{
    char   to[SZ_LINE], url[SZ_LINE], name[SZ_LINE], id[SZ_LINE], *arg;
    char   what[SZ_LINE], osfn[SZ_PATHNAME], *ip, *op;
    int    i, stat = ERR;
    struct operand o;
    extern XINT samp;


    if (nargs > 0) {
	strcpy (to, "all");
	memset (url,  0, SZ_LINE);
	memset (name, 0, SZ_LINE);
	memset (id,   0, SZ_LINE);

	for (i=0; i < nargs; i++) {
            o = popop();                      	/* discard the $N       */
            arg = voGetStrArg ();

	    if (strcmp (o.o_val.v_s, "to") == 0)
		strcpy (to, arg);
	    else if (strcmp (o.o_val.v_s, "id") == 0)
		strcpy (id, arg);
	    else if (strcmp (o.o_val.v_s, "name") == 0)
		strcpy (name, arg);
	    else
		strcpy (what, arg);

	    if (arg) free ((void *) arg);
	}

	/*  Convert logical paths (e.g. dev$foo.fits) to host file URIs.
	 */
        if (strncmp(what, "http:", 5) == 0 || strncmp(what, "file:", 5) == 0) {
            strcpy (url, what);
        } else {
	    /* Need to construct a complete path name here.
	     */
            c_fpathname (what, osfn, SZ_PATHNAME);
	    if ((ip = strchr (osfn, (int) '!'))) {
		for (op = osfn, ip++; *ip; ) 	/* skip 'node!' prefix	*/
		    *op++ = *ip++;
		*op = '\0';
	    }
            sprintf (url, "file://%s", osfn);
        }

        stat = samp_imageLoadFITS (samp, to, url, id, name);
	if (stat < 0)				/* error return		 */
            cl_error (E_UERR, samp_getErr (samp));

    } else
        cl_error (E_UERR, "imageLoad: invalid number of arguments\n");

    return (stat);
}


/**
 *  SAMP_LOADFITS -- Send a 'table.load.fits' message to other clients.
 */

int
cmd_sampLoadFITS (int nargs)
{
    char   to[SZ_LINE], url[SZ_LINE], name[SZ_LINE], id[SZ_LINE], *arg;
    char   what[SZ_LINE], osfn[SZ_PATHNAME], *ip, *op;
    int    i, stat = ERR;
    struct operand o;
    extern XINT samp;


    if (nargs > 0) {
	strcpy (to, "all");
	memset (url,  0, SZ_LINE);
	memset (name, 0, SZ_LINE);
	memset (id,   0, SZ_LINE);

	for (i=0; i < nargs; i++) {
            o = popop();                      	/* discard the $N       */
            arg = voGetStrArg ();

	    if (strcmp (o.o_val.v_s, "to") == 0)
		strcpy (to, arg);
	    else if (strcmp (o.o_val.v_s, "id") == 0)
		strcpy (id, arg);
	    else if (strcmp (o.o_val.v_s, "name") == 0)
		strcpy (name, arg);
	    else
		strcpy (what, arg);

	    if (arg) free ((void *) arg);
	}

	/*  Convert logical paths (e.g. dev$foo.fits) to host file URIs.
	 */
        if (strncmp(what, "http:", 5) == 0 || strncmp(what, "file:", 5) == 0) {
            strcpy (url, what);
        } else {
	    /* Need to construct a complete path name here.
	     */
            c_fpathname (what, osfn, SZ_PATHNAME);
	    if ((ip = strchr (osfn, (int) '!'))) {
		for (op = osfn, ip++; *ip; ) 	/* skip 'node!' prefix	*/
		    *op++ = *ip++;
		*op = '\0';
	    }
            sprintf (url, "file://%s", osfn);
        }

        stat = samp_tableLoadFITS (samp, to, url, id, name);
	if (stat < 0)				/* error return		 */
            cl_error (E_UERR, samp_getErr (samp));

    } else
        cl_error (E_UERR, "loadFITS: invalid number of arguments\n");

    return (stat);
}


/**
 *  SAMP_LOADVOTABLE -- Send a 'table.load.votable' message to other clients.
 */

int
cmd_sampLoadVOTable (int nargs)
{
    char   to[SZ_LINE], url[SZ_LINE], name[SZ_LINE], id[SZ_LINE], *arg;
    char   what[SZ_LINE], osfn[SZ_PATHNAME], *ip, *op;
    int    i, stat = ERR;
    struct operand o;
    extern XINT samp;


    if (nargs > 0) {
	strcpy (to, "all");
	memset (url,  0, SZ_LINE);
	memset (name, 0, SZ_LINE);
	memset (id,   0, SZ_LINE);

	for (i=0; i < nargs; i++) {
            o = popop();                      	/* discard the $N       */
            arg = voGetStrArg ();

	    if (strcmp (o.o_val.v_s, "to") == 0)
		strcpy (to, arg);
	    else if (strcmp (o.o_val.v_s, "id") == 0)
		strcpy (id, arg);
	    else if (strcmp (o.o_val.v_s, "name") == 0)
		strcpy (name, arg);
	    else
		strcpy (what, arg);

	    if (arg) free ((void *) arg);
	}

	/*  Convert logical paths (e.g. dev$foo.fits) to host file URIs.
	 */
        if (strncmp(what, "http:", 5) == 0 || strncmp(what, "file:", 5) == 0) {
            strcpy (url, what);
        } else {
	    /* Need to construct a complete path name here.
	     */
            c_fpathname (what, osfn, SZ_PATHNAME);
	    if ((ip = strchr (osfn, (int) '!'))) {
		for (op = osfn, ip++; *ip; ) 	/* skip 'node!' prefix	*/
		    *op++ = *ip++;
		*op = '\0';
	    }
            sprintf (url, "file://%s", osfn);
        }

        stat = samp_tableLoadVOTable (samp, to, url, id, name);
	if (stat < 0)				/* error return		 */
            cl_error (E_UERR, samp_getErr (samp));

    } else
        cl_error (E_UERR, "loadVOTable: invalid number of arguments\n");

    return (stat);
}


/**
 *  SAMP_EXEC -- Send a 'client.cmd.exec' message to other clients.
 *
 *  Usage:   sampExec ()
 *
 *  Examples:
 */

int
cmd_sampExec (int nargs)
{
    char   *nam=NULL, *val=NULL, cmd[SZ_CMDBLK], to[SZ_LINE];
    int    i, stat = OK;
    struct operand o;
    extern XINT  samp;

    if (nargs > 0) {
	strcpy (to, "all");
	for (i=0; i < nargs; i++) {
            o = popop ();
            val = voGetStrArg ();

	    if (o.o_val.v_s[0] == '$')
	        strcpy (cmd, val);
	    else
		strcpy (to, val);

	    if (val) free ((void *) val);
	}

	stat = samp_cmdExec (samp, to, cmd);

    } else {			/*  list currently defined metadata  	*/
        cl_error (E_UERR, "sampExec: no command specified\n");
	stat = ERR;
    }

    return (stat);
}


/**
 *  SAMP_ENVSET -- Send a 'client.env.set' message to other clients.
 *
 *  Usage:   sampEnvSet ()
 *
 *  Examples:
 */

int
cmd_sampEnvSet (int nargs)
{
    char   name[SZ_LINE], val[SZ_LINE], to[SZ_LINE], *arg = NULL;
    int    i, stat = OK;
    struct operand o;
    extern XINT  samp;

    if (nargs > 0) {
	strcpy (to, "all");
	memset (name, 0, SZ_LINE);
	memset (val, 0, SZ_LINE);

	for (i=0; i < nargs; i++) {
            o = popop();                            /* discard the $1       */
            arg = voGetStrArg ();

	    if (strcmp (o.o_val.v_s, "to") == 0)
                strcpy (to, arg);
	    else if (name[0] == NULL)
                strcpy (name, arg);
	    else {
		if (val[0]) 
		    strcat (val, "\\ ");
                strcat (val, arg);
	    }

	    if (arg)  free ((void *) arg);
	}

	if (!name[0] || !val[0]) {
            cl_error (E_UERR, "sampEnvSet: no variable or value specified\n");
	    stat = ERR;
	} else 
	    stat = samp_envSet (samp, to, name, val);

    } else {			/*  list currently defined metadata  	*/
        cl_error (E_UERR, "sampEnvSet: no param or value specified\n");
	stat = ERR;
    }

    return (stat);
}


/**
 *  SAMP_ENVGET -- Send a 'client.env.get' message to other clients.
 *
 *  Usage:   sampEnvGet ()
 *
 *  Examples:
 */

char *
cmd_sampEnvGet (int nargs)
{
    char   name[SZ_LINE], to[SZ_LINE], *val = NULL, *arg = NULL;
    int    i;
    struct operand o;
    extern XINT  samp;

    if (nargs > 0) {
	strcpy (to, "all");
	memset (name, 0, SZ_LINE);

	for (i=0; i < nargs; i++) {
            o = popop();                            /* discard the $1       */
            arg = voGetStrArg ();

	    if (strcmp (o.o_val.v_s, "to") == 0)
                strcpy (to, arg);
	    else if (name[0] == NULL)
                strcpy (name, arg);

	    if (arg)  free ((void *) arg);
	}

	val = samp_envGet (samp, to, name);

    } else 			/*  list currently defined metadata  	*/
        cl_error (E_UERR, "sampEnvGet: no name specified\n");

    return (val);
}


/**
 *  SAMP_PARAMSET -- Send a 'client.param.set' message to other clients.
 *
 *  Usage:   sampParamSet ()
 *
 *  Examples:
 */

int
cmd_sampParamSet (int nargs)
{
    char   name[SZ_LINE], val[SZ_LINE], to[SZ_LINE], *arg = NULL;
    int    i, stat = OK;
    struct operand o;
    extern XINT  samp;

    if (nargs > 0) {
	strcpy (to, "all");
	memset (name, 0, SZ_LINE);
	memset (val, 0, SZ_LINE);

	for (i=0; i < nargs; i++) {
            o = popop();                            /* discard the $1       */
            arg = voGetStrArg ();

	    if (strcmp (o.o_val.v_s, "to") == 0)
                strcpy (to, arg);
	    else if (name[0] == NULL)
                strcpy (name, arg);
	    else {
		if (val[0]) 
		    strcat (val, "\\ ");
                strcat (val, arg);
	    }

	    if (arg)  free ((void *) arg);
	}

	if (!name[0] || !val[0]) {
            cl_error (E_UERR, "sampParamSet: no param or value specified\n");
	    stat = ERR;
	} else 
	    stat = samp_paramSet (samp, to, name, val);

    } else {			/*  list currently defined metadata  	*/
        cl_error (E_UERR, "sampParamSet: no param or value specified\n");
	stat = ERR;
    }

    return (stat);
}


/**
 *  SAMP_PARAMGET -- Send a 'client.param.get' message to other clients.
 *
 *  Usage:   sampParamGet ()
 *
 *  Examples:
 */

char *
cmd_sampParamGet (int nargs)
{
    char   name[SZ_LINE], to[SZ_LINE], *val = NULL, *arg = NULL;
    int    i;
    struct operand o;
    extern XINT  samp;

    if (nargs > 0) {
	strcpy (to, "all");
	memset (name, 0, SZ_LINE);

	for (i=0; i < nargs; i++) {
            o = popop();                            /* discard the $1       */
            arg = voGetStrArg ();

	    if (strcmp (o.o_val.v_s, "to") == 0)
                strcpy (to, arg);
	    else if (name[0] == NULL)
                strcpy (name, arg);

	    if (arg)  free ((void *) arg);
	}

	val = samp_paramGet (samp, to, name);

    } else 			/*  list currently defined metadata  	*/
        cl_error (E_UERR, "sampParamGet: no name specified\n");

    return (val);
}


/**
 *  SAMP_SHOWROW -- Send a 'table.highlight.row' message to other clients.
 *
 *  Usage:   sampShowRow ()
 *
 *  Examples:
 */

int
cmd_sampShowRow (int nargs)
{
    char   tblid[SZ_LINE], url[SZ_LINE], to[SZ_LINE], *arg = NULL;
    int    i, stat = OK, row = -1;
    struct operand o;
    extern XINT  samp;

    if (nargs > 0) {
	strcpy (to, "all");
	memset (tblid, 0, SZ_LINE);
	memset (url, 0, SZ_LINE);
	for (i=0; i < nargs; i++) {
            o = popop();                       /* discard the $1        */
            arg = voGetStrArg ();

	    if (strcmp (o.o_val.v_s, "to") == 0)
                strcpy (to, arg);
	    else if (isdigit(arg[0]))
		row = atoi (arg);
	    else if (strstr (arg, "://"))		/* url		*/
                strcpy (url, arg);
	    else if (tblid[0] == NULL && url[0])
                strcpy (tblid, arg);
	    else
                strcpy (url, arg);

	    if (arg) free ((void *) arg);
	}

	stat = samp_tableHighlightRow (samp, to, tblid, url, row);

    } else {			/*  list currently defined metadata  	*/
        cl_error (E_UERR, "sampShowRow: no command specified\n");
	stat = ERR;
    }

    return (stat);
}


/**
 *  SAMP_SELECTROWLIST -- Send a 'table.select.rowList' message to other 
 *  clients.
 *
 *  Usage:   sampSelectRowList ()
 *
 *  Examples:
 */

#define	MAX_ROWSELECT		1024

int
cmd_sampSelectRowList (int nargs)
{
    char   tblid[SZ_LINE], url[SZ_LINE], to[SZ_LINE], *arg = NULL;
    int    i, nrows = 0, stat = OK, rows[MAX_ROWSELECT];
    struct operand o;
    extern XINT  samp;

    if (nargs > 0) {
	strcpy (to, "all");
	memset (tblid, 0, SZ_LINE);
	memset (url, 0, SZ_LINE);
	for (i=0; i < nargs; i++) {
            o = popop();                       /* discard the $1        */
            arg = voGetStrArg ();

	    if (strcmp (o.o_val.v_s, "to") == 0)
                strcpy (to, arg);
	    else if (isdigit(arg[0])) {
		/*  Need to decode array/range strings here.     -- FIXME --
		 */
		char *ip, *n;

		for (ip=arg, n=arg, nrows=0; n; ip=n ) {
		    if ( (n = strchr (ip, (int) ',')) )
			*n++ = '\0';
		    rows[nrows++] = atoi (ip);
		}

	    } else if (strstr (arg, "://"))		/* url		*/
                strcpy (url, arg);
	    else if (tblid[0] == NULL && url[0])
                strcpy (tblid, arg);
	    else
                strcpy (url, arg);

	    if (arg) free ((void *) arg);
	}

	stat = samp_tableSelectRowList (samp, to, tblid, url, rows, nrows);

    } else {			/*  list currently defined metadata  	*/
        cl_error (E_UERR, "sampShowRow: no command specified\n");
	stat = ERR;
    }

    return (stat);
}


/**
 *  SAMP_POINTAT -- Send a 'coords.pointAt.sky' message to other clients.
 *
 *  Usage:   sampPointAt ()
 *
 *  Examples:
 *    samp pointat 187.5 33.4				# args in degrees
 *    samp pointat (12:30:00 * 15) 33:24		# sexagesimal hms
 *    samp pointat 187.5 33.4 to=aladin			# directed msg
 *
 */

int
cmd_sampPointAt (int nargs)
{
    char   *arg = NULL, to[SZ_LINE];
    int    i, stat = OK;
    float  ra=-1.0, dec=0.0;
    extern XINT  samp;

    if (nargs > 0) {
        strcpy (to, "all");
	for (i=0; i < nargs; i++) {
            popop();                            /* discard the $N       */
            arg = voGetStrArg ();

	    if (isalpha ((int) arg[0]))		/* recipient arg	*/
        	strcpy (to, arg);
	    else if (ra < 0)			/* not initialized yts	*/
	        ra = atof (arg);
	    else
	        dec = atof (arg);

	    if (arg) free ((char *) arg);
	}

	stat = samp_coordPointAtSky (samp, to, ra, dec);

    } else {			/*  list currently defined metadata  	*/
        cl_error (E_UERR, "sampPointAt: no command specified\n");
	stat = ERR;
    }

    return (stat);
}




/******************************************************************************
 **
 **  			    Not Yet Implemented
 **
 *****************************************************************************/
/**
 *  SAMP_SPECLOAD -- Send a 'spec.load.ssa-generic' message to other clients.
 */

int
cmd_sampSpecLoad (int nargs)
{
    return (0);
}


/**
 *  SAMP_BIBCODELOAD -- Send a 'bibcode.load' message to other clients.
 */

int
cmd_sampBibcodeLoad (int nargs)
{
    return (0);
}

