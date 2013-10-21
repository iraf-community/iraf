/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

/**
 *  SAMPHANDLERS.C -- Mtype message handlers.
 */

#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
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



extern Handler	userHandlers[];
extern int	numHandlers;

extern int   samp_registered;
extern char  samp_cmd[SZ_CMDBLK];
extern pid_t cl_pid;

extern int   optbl[];
extern char *ifnames[];



/**
 *  MType Handler Declarations	
 */
int 	cl_genericHandler (char *sender, char *mtype, char *msg_id, Map map);
int 	cl_cmdExecHandler (char *cmd);
int 	cl_envSetHandler (char *name, char *value);
int 	cl_envGetHandler (char *name, char *value, int maxch);
int 	cl_paramSetHandler (char *name, char *value);
int 	cl_paramGetHandler (char *name, char *value, int maxch);
int 	cl_pingHandler (char *sender);
int 	cl_imgLoadHandler (char *url, char *imgId, char *name);
int 	cl_tblLoadHandler (char *url, char *tblId, char *name);

int     cl_addUserHandler (char *mtype, char *cmd);
char   *cl_getUserHandler (char *mtype);

void    str_replace (char **string, char *substr, char *replacement);
int 	is_stdMType (char *mtype);

extern	XINT	samp;				/* SAMP handle 		*/
extern  pthread_mutex_t samp_mutex;		/* global data mutex	*/

extern	char   *voGetStrArg ();





/*****************************************************************************
 *  Utility procedures.
 ****************************************************************************/

/**
 *  CL_ADDUSERHANDLER -- Associate an mtype with a user-defined handler.
 */
int
cl_addUserHandler (char *mtype, char *cmd)
{
    register int i = 0;
    int   len = strlen (mtype);


    /*  See if it's a generic message.
     */
    if (!is_stdMType (mtype)) {
	extern XINT  samp;

        strcpy (userHandlers[numHandlers].mtype, mtype);
        strcpy (userHandlers[numHandlers].cmd, cmd);
        numHandlers++;

	samp_Subscribe (samp, mtype, cl_genericHandler);
	samp_DeclareSubscriptions (samp);

        return ( ((numHandlers < MAX_HANDLERS) ? 0 : -1) );
    }

    /* Check for an existing definition, if found, overwrite it.
     */
    for (i=0; i < numHandlers; i++) {
	if (strncmp (mtype, userHandlers[i].mtype, len) == 0) {
	    memset (userHandlers[i].cmd, 0, SZ_FNAME);
	    strcpy (userHandlers[i].cmd, cmd);
	    return (0);
	}
    }

    /*  No handler found, so add it to the list.
     */
    strcpy (userHandlers[numHandlers].mtype, mtype);
    strcpy (userHandlers[numHandlers].cmd, cmd);
    numHandlers++;

    return ( ((numHandlers < MAX_HANDLERS) ? 0 : -1) );
}


/**
 *  CL_DELUSERHANDLER -- Delete a user-defined handler for the named mtype.
 */
int
cl_delUserHandler (char *mtype)
{
    register int i = 0, j = 0;


    if (mtype == NULL) {
	/*  Delete all handlers.
	 */
        for (i=0; i < numHandlers; i++) {
	    memset (userHandlers[i].mtype, 0, SZ_FNAME);
	    memset (userHandlers[i].cmd, 0, SZ_FNAME);
	}
	numHandlers = 0;

    } else {
        int   len = strlen (mtype);

        /* Check for an existing definition, if found, delete it.
         */
        for (i=0; i < numHandlers; i++) {
	    if (strncmp (mtype, userHandlers[i].mtype, len) == 0) {
	        /*  Found a match.
	         */
	        memset (userHandlers[i].mtype, 0, SZ_FNAME);
	        memset (userHandlers[i].cmd, 0, SZ_FNAME);
    	        for (j=i; j < numHandlers; j++) {
		    /*  Shift remaining list.
		     */
	            strcpy (userHandlers[j].cmd, userHandlers[j+1].cmd);
	            strcpy (userHandlers[j].mtype,userHandlers[j+1].mtype);
    	        }

	        numHandlers--;
	    }
        }
    }

    return (0);
}


/**
 *  CL_GETUSERHANDLER -- Get any user-defined handler command for the given
 *  mtype, or NULL of not found.  Match up to the length of the input mtype
 *  to allow general matches, e.g. "image.load" instead of requiring the
 *  full "image.load.fits".
 */
char *
cl_getUserHandler (char *mtype)
{
    register int i = 0;
    int   len = strlen (mtype);
  
    for (i=0; i < numHandlers; i++) {
	if (strncmp (mtype, userHandlers[i].mtype, len) == 0) {
	    return (userHandlers[i].cmd);
	}
    }

    return (NULL);
}




/*****************************************************************************
 *  SAMP message handlers, these are server-side commands.
 ****************************************************************************/

int
cl_genericHandler (char *sender, char *mtype, char *msg_id, Map map)
{
    register int i, npars = 0;
    char  *cmd, *key, *val;


    /*  Check for a user-defined handler command.
     */
    if ( (cmd = cl_getUserHandler (mtype)) ) {
        char   *newstr = calloc (1, SZ_CMDBLK);
	char    arg[SZ_FNAME];

        /*  Do any command string replacements.
         */
        npars = samp_getMapSize (map);
        strcpy (newstr, cmd);
        for (i=0; i < npars; i++ ) {
	    key = (char *) samp_getMapKey (map, i);
	    val = (char *) samp_getMapVal (map, i);

	    memset (arg, 0, SZ_FNAME);
	    sprintf (arg, "$%s", key);
            str_replace (&newstr, arg, val);
        }

        /*  Execute as if it were sent as an exec message.
         */
        cl_cmdExecHandler (newstr);

        free (newstr);

    } else {
        /* Nothing to do..... */
    }

    return (0);
}


int
cl_cmdExecHandler (char *cmd)
{
    /*  Save the command to the buffer.
     */
    pthread_mutex_lock (&samp_mutex);
    strcat (samp_cmd, cmd);
    strcat (samp_cmd, "\n");
    pthread_mutex_unlock (&samp_mutex);

    /* Send the signal to the parent CL thread to notify it of the command.
     */
    kill (cl_pid, SIGIO);

    return (0);
}

int
cl_envSetHandler (char *name, char *value)
{
    c_envputs (name, value);
    pr_envset (0, name, value);
    if (strcmp ("erract", name) == 0)
        erract_init();

    return (0);
}

int
cl_envGetHandler (char *name, char *value, int maxch)
{
    char *s = NULL;
    char  val[SZ_LINE];

    memset (val, 0, SZ_LINE);
    if ((s = envget (name)))
        strncpy (value, s, maxch);
    else {
        if (c_envfind (name, val, SZ_LINE) < 0)
            strncpy (value, "INDEF", maxch);
        else
            strncpy (value, val, maxch);
    }

    return (0);
}

int
cl_paramSetHandler (char *name, char *value)
{
    char *pk, *t, *p, *f;
    struct param *pp;
    struct operand o;
    char  cmd[SZ_LINE], val[SZ_LINE];

    breakout (name, &pk, &t, &p, &f);
    strcpy (val, value);

    /* We can't use paramsrch here because the string we are looking
     * for might be a builtin or not exist, and paramsrch would fail
     * to return a reply.
     */
    if (t[0] && deftask (t) == YES) {
        if ((pp = lookup_param (pk, t, p))) {

            if (*f == FN_NULL && (pp->p_type & PT_LIST)) {
                /* Hitting EOF from a list is ok during an inspect stmt so avoid
                 * so avoid using paramget() with its EOF error. readlist() may
                 * set P_LEOF.
                 */
                o = readlist (pp);
                if ((pp->p_flags & P_LEOF) || inrange (pp, &o))
                    pushop (&o);
                else
                    query (pp);
            } else
                paramget (pp, FN_VALUE);/* get the parameter value field    */
            o = popop();

            /* Quote string params.
             */
            if ((o.o_type & OT_BASIC) == OT_STRING)
	        sprintf (val, "\"%s\"", value);
        }
    }


    /*  Cheat and make this a command string to be executed.
     */
    memset (cmd, 0, SZ_LINE);
    sprintf (cmd, "%s = %s\n", name, val);

    return ( cl_cmdExecHandler (cmd) );
}

int
cl_paramGetHandler (char *name, char *value, int maxch)
{
    char *pk, *t, *p, *f;
    struct param *pp;
    struct operand o;

    breakout (name, &pk, &t, &p, &f);
    strncpy (value, "INDEF", maxch);

    /* We can't use paramsrch here because the string we are looking
     * for might be a builtin or not exist, and paramsrch would fail
     * to return a reply.
     */
    if (t[0] && deftask (t) == NO)
	return (0);
    if (! (pp = lookup_param (pk, t, p)))
	return (0);

    if (*f == FN_NULL && (pp->p_type & PT_LIST)) {
        /*  Hitting EOF from a list is ok during an inspect stmt so avoid
         *  using paramget() with its EOF error. readlist() may set P_LEOF.
         */
        o = readlist (pp);
        if ((pp->p_flags & P_LEOF) || inrange (pp, &o))
            pushop (&o);
        else
            query (pp);
    } else {
        paramget (pp, FN_VALUE);	/* get the parameter value field    */
        opcast (OT_STRING);		/* cast as a string		    */
    }
    o = popop();

    memset (value, 0, maxch);
    if ((o.o_type & OT_BASIC) == OT_STRING)
        strncpy (value, o.o_val.v_s, maxch);
    else
        strncpy (value, "INDEF", maxch);

    return (0);
}

int
cl_pingHandler (char *sender)
{
    /*  no-op  */

    return (0);
}

int
cl_imgLoadHandler (char *url, char *imgId, char *name)
{
    char  *cmd = (char *) NULL;


    /*  Check for a user-defined handler command.
     */
    if ( (cmd = cl_getUserHandler ("image.load.")) ) {
 	char   *newstr = calloc (1, SZ_CMDBLK);

	/*  Do any command string replacements.
	 */
	strcpy (newstr, cmd);
	str_replace (&newstr, "$url", url);
	str_replace (&newstr, "$imageId", imgId);
	str_replace (&newstr, "$name", name);

	/*  Execute as if it were sent as an exec message. 
	 */
	cl_cmdExecHandler (newstr);

 	free (newstr);

    } else {
	/* Nothing to do..... */
    }

    return (0);
}

int
cl_tblLoadHandler (char *url, char *tblId, char *name)
{
    char  *cmd = (char *) NULL;


if (!url)
    return (1);
    /*  Check for a user-defined handler command.
     */
    if ( (cmd = cl_getUserHandler ("table.load.")) ) {
 	char   *newstr = calloc (1, SZ_CMDBLK);

	/*  Do any command string replacements.
	 */
	strcpy (newstr, cmd);
	str_replace (&newstr, "$url", url);
	str_replace (&newstr, "$imageId", tblId);
	str_replace (&newstr, "$name", name);

	/*  Execute as if it were sent as an exec message. 
	 */
	cl_cmdExecHandler (newstr);

 	free (newstr);

    } else {
	/* Nothing to do..... */
    }

    return (0);
}



/******************************************************************************
**  Local Utilities
******************************************************************************/

/**
 *  STR_REPLACE --  Replace the input string, substituting 'replacement' for
 *  all occurrances of 'substr'.  The input string is modified and is assumed
 *  to be at least SZ_CMDBLK long.
 */

void
str_replace (char **string, char *substr, char *replacement )
{
    char  *tok = (char *) NULL, *newstr = (char *) NULL, *str = *string;
    int    n = 0;


    for (n=0; n < MAX_SUBS; n++) {
        tok = strstr (str, substr);		/* check for no subs	*/
        if (tok == NULL) 
	    break;

        if ((newstr = calloc (1, SZ_CMDBLK)) == NULL)
	    break;

	/*  Do the replacements.
	 */
        memcpy (newstr, str, tok - str);	
        memcpy (newstr + (tok - str), replacement, strlen(replacement));
        memcpy (newstr + (tok - str) + strlen(replacement), 
	    tok + strlen(substr), strlen(str) - strlen(substr) - 
	    (tok - str));
        memset (newstr + strlen(str) - strlen(substr) + strlen(replacement), 
	    0, 1);

        strcpy (*string, newstr);
        free (newstr);
    }
}


/**
 * IS_STDMTYPE - See if the mtype is one of the well-known message type.
 */
int
is_stdMType (char *mtype)
{
    int   i = 0, len = 0;
    char *stdMTypes[] = {
	  "samp.app.ping", 	"samp.app.status", 	"samp.hub.event.*",
	  "table.load.fits", 	"table.load.votable", 	"table.highlight.row",
	  "image.load.fits", 	"coord.pointAt.sky", 	"client.cmd.exec",
	  "client.env.get", 	"client.env.set", 	"client.param.get",
	  "client.param.set", 	"bibcode.load", 	"table.select.rowList",
	  "voresource.loadlist",
	  "spectrum.load.ssa-generic",
	  NULL
	};

    for (i=0; stdMTypes[i]; i++) {
	len = min (strlen (mtype), strlen (stdMTypes[i]));
	if (strncasecmp (mtype, stdMTypes[i], len) == 0)
	    return (1);
    }
    return (0);
}
