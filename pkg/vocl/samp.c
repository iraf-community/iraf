/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

/**
 *  SAMP.C -- Interface routines for the client and server side of the 
 *  SAMP messaging commands.
 */

#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
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


/* To disable all messaging
#define	SAMP_DISABLE	1
*/


XINT    samp            	= -1;   /* samp handle                  */

pid_t	cl_pid			= 0;	/* local interface variables	*/
int	samp_registered		= 0;
int	samp_trace		= 0;
int	samp_quiet		= 0;
char    samp_cmd[SZ_CMDBLK];

extern int     optbl[];
extern char   *ifnames[];


/**
 *  MType Handler Declarations	
 */
int 	cl_genericHandler ();
int 	cl_cmdExecHandler (char *cmd);
int 	cl_envSetHandler (char *name, char *value);
int 	cl_envGetHandler (char *name, char *value, int maxch);
int 	cl_paramSetHandler (char *name, char *value);
int 	cl_paramGetHandler (char *name, char *value, int maxch);
int 	cl_pingHandler (char *sender);
int 	cl_imgLoadHandler (char *url, char *imgId, char *name);
int 	cl_tblLoadHandler (char *url, char *tblId, char *name);

int 	cl_sampInit (void);
int 	cl_sampStart (void);
int 	cl_sampStop (void);

void    sampio_handler (int signum);		/* I/O handlers		*/
int	samp_rl_hook (void);


Handler userHandlers[MAX_HANDLERS];     /* user-defined handlers        */
int     numHandlers             = 0;    /* Num user-defined handlers    */


extern	XINT	samp;				/* SAMP handle 		*/
extern  pthread_mutex_t samp_mutex;		/* global data mutex	*/
extern  int     rl_done;

extern	char   *voGetStrArg ();




/**
 *  CL_SAMPINIT -- Initialize the SAMP interface.
 */
int
cl_sampInit ()
{
    cl_pid = getpid ();				/* initialize		*/
    memset (samp_cmd, 0, SZ_CMDBLK);	
    memset (userHandlers, 0, (MAX_HANDLERS * sizeof(Handler)));

#ifdef SAMP_DISABLE
return 0;
#endif

    /*  Install a SIGIO handler so the samp handler can notify the main
     *  CL thread of pending input from the samp clients.
     */
    signal (SIGIO, &sampio_handler);
    rl_event_hook = samp_rl_hook;


    /*  Initialize the SAMP interface.
    */
    samp = sampInit ("IRAF", "VO/IRAF v2.16 CL");

    /*  Set up some local metadata values.
     */
    samp_Metadata (samp, "author.name",   "Mike Fitzpatrick, NOAO"); 
    samp_Metadata (samp, "author.email",  "fitz@noao.edu"); 
    samp_Metadata (samp, "samp.icon.url", 
	"http://iraf.noao.edu/images/iraf-icon.gif");
    samp_Metadata (samp, "samp.description.html", 
	"http://iraf.noao.edu/voiraf/clsamp.html");


    /*  Subscribe to various message types.
     */
    samp_Subscribe (samp, "samp.app.ping",        cl_pingHandler);
    samp_Subscribe (samp, "samp.app.event.*",     NULL);
    samp_Subscribe (samp, "samp.hub.event.*",     NULL);

    samp_Subscribe (samp, "image.load.fits",      cl_imgLoadHandler);
    samp_Subscribe (samp, "table.load.fits",      cl_tblLoadHandler);
    samp_Subscribe (samp, "table.load.votable",   cl_tblLoadHandler);

    samp_Subscribe (samp, "client.cmd.exec",      cl_cmdExecHandler);
    samp_Subscribe (samp, "client.param.set",     cl_paramSetHandler);
    samp_Subscribe (samp, "client.param.get",     cl_paramGetHandler);
    samp_Subscribe (samp, "client.env.set",       cl_envSetHandler);
    samp_Subscribe (samp, "client.env.get",       cl_envGetHandler);

    return (0);
}


/**
 *  CL_SAMPSTART -- Start up the samp messaging.
 */
int
cl_sampStart (void)
{
#ifdef SAMP_DISABLE
return 0;
#endif

    if (samp > 0) {
	sampVerbose (samp, 0);
        sampStartup (samp);
	sampVerbose (samp, 1);
        samp_registered = samp_hubActive (samp);
    }

    return (0);
}


/**
 *  CL_SAMPSTOP - Stop SAMP communications, but we'll reinitialize the
 *  the interface.
 */
int
cl_sampStop ()
{
    samp_registered = 0;
    if (samp >= 0) {
 	if (sampShutdown (samp) < 0)
            cl_error (E_UERR, "Error shutting down SAMP messaging\n");
	/* 
	sampClose (samp);
	cl_sampInit (); 
	*/
    }

    return (0);
}


/*****************************************************************************/


/* CL_SAMP -- SAMP master command.
 *
 *	samp [on|off|restart]			start/stop SAMP messaging
 *	samp status
 *
 *	samp handler  [ <mtype> <cmd> ]
 *	samp access   [ <appName> ]
 *	samp meta     [ <param> <value> ]
 *	samp trace    [ <state> ]
 *
 *	samp send         <mtype> [<arg>] ...
 *	samp exec         <cmd>
 *	samp setparam     <param> <val>
 *	samp getparam     <param>
 *	samp setenv       <envvar> <val>
 *	samp getenv       <envvar>
 *
 *	samp loadImage    <file>|<url> [id] [name]
 *	samp loadVOTable  <file>|<url> [id] [name]
 *	samp loadFITS     <file>|<url> [id] [name]
 *	samp pointAt      <ra> <dec>
 *	samp showRow	  <tblId> <url> <row>
 *	samp selectRow    <tblId> <url> <row>,<row>,....
 */
void
cl_Samp (void)
{
    register struct pfile *pfp;
    char  *cmd = NULL;
    int    numargs = 0;
    extern int nargs ();

#ifdef SAMP_DISABLE
return;
#endif

    pfp = newtask->t_pfp;
    numargs = nargs (pfp);


    if (numargs > 0) {
        pushbparams (pfp->pf_pp);

        popop();                        /* discard the $1       	    */
        cmd = voGetStrArg ();		/* first arg is the subcommand	    */


	if (samp_trace)
	    fprintf (stderr, "cl_Samp: numargs=%d  samp=%d  cmd='%s'\n", 
		numargs, samp, cmd);

	    
	/*  If the samp interface isn't enabled, and we want something more
	 *  than the status, start the samp interface.
	 */
	if (!samp_registered && strncasecmp (cmd, "off", 3) == 0)
	     return;
	if (!samp_registered && 
	    (strncasecmp (cmd, "status", 4) != 0 &&
	     strncasecmp (cmd, "hubaccess", 7) != 0 &&
	     strncasecmp (cmd, "quiet", 4) != 0 &&
	     strncasecmp (cmd, "noquiet", 4) != 0 &&
	     strncasecmp (cmd, "handler", 4) != 0 &&
	     strncasecmp (cmd, "trace", 4) != 0)) {
	    	cmd_sampStart ();

		if (! samp_hubActive (samp)) {
		    oprintf ("Error: Hub is not active.\n");
		    return;
		}
	}


	if (strncasecmp ("status", cmd, 4) == 0) {		/* STATUS     */
            oprintf ("%s\n", ((samp >= 0 && samp_registered) ? "on" : "off"));

	} else if (strncasecmp ("help", cmd, 4) == 0) {		/* HELP       */
	    goto help_;

	} else if (strncasecmp ("on", cmd, 2) == 0 || 		/* ON/START   */
	    strncasecmp ("start", cmd, 5) == 0) {
	        if (!samp_hubActive (samp))
		    cmd_sampStart ();

                oprintf ("%s\n", (samp_hubActive (samp) ? "on" : "off"));

	} else if (strncasecmp ("off", cmd, 3) == 0 || 		/* OFF/STOP   */
	    strncasecmp ("stop", cmd, 5) == 0) {
	        if (samp_registered)
	            cmd_sampStop ();
                oprintf ("%s\n", (samp_hubActive (samp) ? "on" : "off"));

	} else if (strncasecmp ("restart", cmd, 4) == 0) {	/* RESTART    */
	    cmd_sampRestart ();
	    if (samp_registered && !samp_quiet)
                oprintf ("ok\n");

	} else if (strncasecmp ("quiet", cmd, 5) == 0) {	/* QUIET      */
	    samp_quiet = 1;
	} else if (strncasecmp ("noquiet", cmd, 7) == 0) {	/* NOQUIET    */
	    samp_quiet = 0;

	} else if (strncasecmp ("hubaccess", cmd, 7) == 0) {	/* HUBACCESS  */
            int res, verb = sampVerbose (samp, -1);
	    sampVerbose (samp, 0);
            res = samp_Ping (samp, "Hub");
	    if (!samp_quiet)
                oprintf ("%s\n", ((res > 0) ? "yes" : "no"));
	    sampVerbose (samp, verb);

	} else if (strncasecmp ("access", cmd, 4) == 0) {	/* ACCESS     */
            int res = cmd_sampAccess(numargs - 1);
	    if (numargs - 1 && !samp_quiet) 
                oprintf ("%s\n", (res ? "yes" : "no"));

	} else if (strncasecmp ("handler", cmd, 4) == 0) {	/* HANDLER    */
 	    int stat = cmd_sampAddHandler (numargs - 1);
	    if (!samp_quiet && stat < 0)
                oprintf ("err\n");

	} else if (strncasecmp ("trace", cmd, 4) == 0) {	/* TRACE      */
 	    int stat = cmd_sampDbg (numargs - 1);
	    if (!samp_quiet)
                oprintf ("%s\n", (stat ? "on" : "off"));

	} else if (strncasecmp ("name", cmd, 4) == 0) {		/* NAME       */
	    if (cmd_sampName (numargs - 1) && !samp_quiet)
		oprintf ("ok\n");

	} else if (strncasecmp ("meta", cmd, 4) == 0) {		/* META       */
	    if (cmd_sampMetadata (numargs - 1) && !samp_quiet)
		oprintf ("ok\n");

	} else if (strncasecmp ("send", cmd, 4) == 0) {		/* SEND       */
	    if (cmd_sampSend (numargs - 1) >= 0 && !samp_quiet)
		oprintf ("ok\n");

	} else if (strncasecmp ("exec", cmd, 4) == 0) { 	/* EXEC       */
	    if (cmd_sampExec (numargs - 1) >= 0 && !samp_quiet)
		oprintf ("ok\n");

	} else if (strncasecmp ("pointAt", cmd, 4) == 0) { 	/* POINTAT    */
	    if (cmd_sampPointAt (numargs - 1) >= 0 && !samp_quiet)
		oprintf ("ok\n");

	} else if (strncasecmp ("setenv", cmd, 4) == 0) {	/* SETENV     */
	    if (cmd_sampEnvSet (numargs - 1) >= 0 && !samp_quiet)
		oprintf ("ok\n");

	} else if (strncasecmp ("getenv", cmd, 4) == 0) {	/* GETENV     */
	    char *val = cmd_sampEnvGet (numargs - 1);
	    if (val && !samp_quiet)
		oprintf ("%s\n", val);

	} else if (strncasecmp ("setparam", cmd, 4) == 0) {	/* SETPARAM   */
	    if (cmd_sampParamSet (numargs - 1) >= 0 && !samp_quiet)
		oprintf ("ok\n");

	} else if (strncasecmp ("getparam", cmd, 4) == 0) {	/* GETPARAM   */
	    char *val = cmd_sampParamGet (numargs - 1);
	    if (val && !samp_quiet)
		oprintf ("%s\n", val);

	} else if (strncasecmp ("showRow", cmd, 5) == 0) {	/* SHOWROW    */
	    if (cmd_sampShowRow (numargs - 1) >= 0 && !samp_quiet)
		oprintf ("ok\n");

	} else if (strncasecmp ("loadImage", cmd, 6) == 0) {	/* IMAGE      */
	    if (cmd_sampLoadImage (numargs - 1) >= 0 && !samp_quiet)
		oprintf ("ok\n");

	} else if (strncasecmp ("loadVOTable", cmd, 6) == 0) {	/* VOTABLE    */
	    if (cmd_sampLoadVOTable (numargs - 1) >= 0 && !samp_quiet)
		oprintf ("ok\n");

	} else if (strncasecmp ("loadFITS", cmd, 6) == 0) {	/* FITS       */
	    if (cmd_sampLoadFITS (numargs - 1) >= 0 && !samp_quiet)
		oprintf ("ok\n");

	} else if (strncasecmp ("selectRows", cmd, 6) == 0) {	/* SELECT     */
	    if (cmd_sampSelectRowList (numargs - 1) >= 0 && !samp_quiet)
		oprintf ("ok\n");
	} else {
	    oprintf ("Unknown command '%s'\n", cmd);
	}

    } else {
help_: 								/*  USAGE     */
	oprintf ("Samp Command Help:\n\n"
    	    "    samp status\n"
    	    "    samp help\n"
    	    "    samp on|start\n"
    	    "    samp off|stop\n"
    	    "    samp restart\n"
    	    "    samp trace 		[<value>]\n"
    	    "    samp access 	[<appName>]\n"
    	    "    samp hubaccess\n"
    	    "    samp quiet\n"
    	    "    samp noquiet\n"
    	    "    samp handler	[<mtype> <cmd>]\n"
    	    "    samp meta		[<param> <value>]\n"
	    "    \n"
	    "    The following commands will be broadcast to all clients\n"
	    "    unless an argument of the form 'to=<appName>' is present.\n"
	    "    \n"
    	    "    samp send		<mtype> [<args> ....]\n"
    	    "    samp exec		<cmd>\n"
    	    "    samp pointAt	<ra> <dec>\n"
    	    "    samp setenv		<name> <value>\n"
    	    "    samp getenv		<name>\n"
    	    "    samp setparam	<name> <value>\n"
    	    "    samp getparam	<name>\n"
    	    "    samp loadImage	<url> [id=<id>] [name=<name>]\n"
    	    "    samp loadVOTable	<url> [id=<id>] [name=<name>]\n"
    	    "    samp loadFITS	<url> [id=<id>] [name=<name>]\n"
    	    "    samp showRow	[<tblID>] [<url>] <row>\n"
    	    "    samp selectRows	[<tblID>] [<url>] <row>,<row>,....\n"
	    "    \n"
	);
    }
}



/*****************************************************************************
 *  Utility procedures.
 ****************************************************************************/

/**
 *  SAMPIO_HANDLER -- Signal handler for the SIGIO signal sent by the 
 *  remote application.
 */
void
sampio_handler (int signum)
{
    int  len = strlen (samp_cmd);

    if (signum == SIGIO) {
        if (len) {
	    char  *ip, *op, buf[SZ_CMDBLK];

	    /* Extract the first line for printing.
	     */
	    memset (buf, 0, SZ_CMDBLK);
	    for (ip=samp_cmd, op=buf; *ip && *ip != '\n'; ip++, op++) {
		*op = *ip;
		if (*ip == '%') { 	    /* escape for printing	*/
		    op++, *op = '%';
		}
	    }

            oprintf (buf);		    /* write it to the cmdline 	*/
            fflush (stdout);
        }
    }
}


/**
 *  SAMP_RL_HOOK -- Event hook function for the readline() interface.  This
 *  procedure is installed when we open the SAMP interface and is called
 *  occassionally from readline(), allowing us to use alternate input for
 *  the command line.
 */
int
samp_rl_hook (void)
{
    /* rl_set_keyboard_input_timeout (333333); */
    rl_done = (samp_cmd[0] ? 1 : 0);

    return ( rl_done );
}


/**
 *  GET_SAMP_COMMAND -- Get the next line from the SAMP command buffer.
 */
int
get_samp_command (char *cmdbuf, int maxch)
{
    char *nl;
    int   stat = 0;


    memset (cmdbuf, 0, maxch);
    strcpy (cmdbuf, samp_cmd);
    nl = strchr (cmdbuf, (int)'\n');
    if ( nl ) {
	*nl = '\0';				/* kill the newline	    */

	/*  cmdbuf now contains the next line of input.  Shift the rest
	 *  of samp_cmd to the beginning of the array.
	 */
        memset (samp_cmd, 0, SZ_CMDBLK);
        strcpy (samp_cmd, ++nl);
	stat = 1;
    } else
        memset (samp_cmd, 0, SZ_CMDBLK);

    return (stat);
}



/**
 *  SAMPOP --  Process a SAMP request as a builtin CL function.
 */
int
sampop (int opcode, int op_index, int nargs)
{
    int     op = optbl[op_index];	
    int	    debug = 0;


    if (debug)
	printf ("sampop:  opcode=%d  index=%d  nargs=%d\n", 
	    opcode, op_index, nargs);
	

    /*  If the samp interface isn't enabled, and we want something more
     *  than the status, start the samp interface.
     */
    if (!samp_registered && 
	opcode != OP_SAMP && opcode != OP_SAMPSTATUS &&
	opcode != OP_SAMPHUBACC && opcode != OP_SAMPACCESS) {
            cmd_sampStart ();

	    if (! samp_hubActive (samp)) {
		oprintf ("Error: Hub is not active.\n");
		return 0;
	    }
	}


    switch (opcode) {
    case OP_SAMP: 	
/*  FIXME
	cl_Samp (); 
*/
	break;

    case OP_SAMPSTATUS:
	if (nargs > 1)
            cl_error (E_UERR, "usage: sampStatus ([on|off|restart])");
	else
	    func_sampStatus (nargs);
	break;

    case OP_SAMPNAME:
	if (nargs > 1)
            cl_error (E_UERR, "usage: sampName ([name])");
	else
	    func_sampName (nargs);
	break;

    case OP_SAMPMETA:
	if (nargs > 2)
            cl_error (E_UERR, "usage: sampMeta ([[param [, value]]])");
	else
	    func_sampMetadata (nargs);
	break;

    case OP_SAMPHANDLER:              	/* add/print user handlers   	*/
	if (nargs > 2)
            cl_error (E_UERR, "usage: sampHandler ( [mtype, cmd])");
	else
	    func_sampAddHandler (nargs);
	break;

    case OP_SAMPHUBACC:              	/* check on a running Hub       */
	if (nargs > 0)
            cl_error (E_UERR, "usage: sampHubAccess ()");
	else
	    func_sampHubAccess (nargs);
	break;

    case OP_SAMPACCESS:              	/* check on an external app     */
	if (nargs < 1 || nargs > 1)
            cl_error (E_UERR, "usage: sampAccess (appName)");
	else
	    func_sampAccess (nargs);
	break;

    case OP_SAMPIMLOAD:               	/* image.load.fits              */
	if (nargs < 1 || nargs > 3)
            cl_error (E_UERR, 
		"usage: sampLoadImage (file|url [, recip [, tag ] ])\n");
	else
	    func_sampLoadImage (nargs);
	break;

    case OP_SAMPTBLVOT:               	/* table.load.votable           */
	if (nargs < 1 || nargs > 3)
            cl_error (E_UERR, 
		"usage: sampLoadImage (file|url [, recip [, tag ] ])\n");
	else
	    func_sampLoadVOTable (nargs);
	break;

    case OP_SAMPTBLFITS:              	/* table.load.fits              */
	if (nargs < 1 || nargs > 3)
            cl_error (E_UERR, 
		"usage: sampLoadFITS (file|url [, recip [, tag ] ])\n");
	else
	    func_sampLoadFITS (nargs);
	break;

    case OP_SAMPTBLROW:               	/* table.highlight.row          */
	if (nargs < 3)
            cl_error (E_UERR, "usage: sampShowRow (tblId, url, row[, to])");
	else
	    func_sampShowRow (nargs);
	break;
    case OP_SAMPSELRLIST:             	/* table.select.rowList         */
	if (nargs < 3)
            cl_error (E_UERR, 
		"usage: sampSelectRowList (tblId, url, rowlist[, to])");
	else
	    func_sampSelectRowList (nargs);
	break;
    case OP_SAMPPOINTAT:              	/* coord.pointAt.sky            */
	if (nargs < 1 || nargs > 3)
            cl_error (E_UERR, "usage: sampPointAt (ra, dec)");
	else
	    func_sampPointAt (nargs);
	break;

    case OP_SAMPCMDEXEC:              	/* client.cmd.exec              */
	if (nargs < 1 || nargs > 3)
            cl_error (E_UERR, "usage: sampCmdExec (cmd[, to])");
	else
	    func_sampCmdExec (nargs);
	break;
    case OP_SAMPENVGET:              	/* client.env.get               */
	if (nargs < 1 || nargs > 2)
            cl_error (E_UERR, "usage: sampEnvGet (param[, to])");
	else
	    func_sampEnvGet (nargs);
	break;
    case OP_SAMPENVSET:              	/* client.env.set               */
	if (nargs < 2 || nargs > 3)
            cl_error (E_UERR, "usage: sampEnvGet (param, val[, to])");
	else
	    func_sampEnvSet (nargs);
	break;
    case OP_SAMPPARAMGET:              	/* client.param.get             */
	if (nargs < 1 || nargs > 2)
            cl_error (E_UERR, "usage: sampParamGet (param[, to])");
	else
	    func_sampParamGet (nargs);
	break;
    case OP_SAMPPARAMSET:              	/* client.param.set             */
	if (nargs < 2 || nargs > 3)
            cl_error (E_UERR, "usage: sampParamGet (param, val[, to])");
	else
	    func_sampParamSet (nargs);
	break;

    case OP_SAMPSPECLOAD:              	/* spectrum.load.ssa-generic    */
        cl_error (E_UERR, "specLoad: not yet implemented");
	break;
    case OP_SAMPRESLOAD:               	/* voresource.loadlist.*        */
        cl_error (E_UERR, "specResourceLoad: not yet implemented");
	break;
    case OP_SAMPBIBLOAD:               	/* bibcode.load                 */
	if (nargs < 1 || nargs > 2)
            cl_error (E_UERR, "usage: sampBibcodeLoad (bibcode[, to])");
	else
	    func_sampBibcodeLoad (nargs);
	break;

    default:
	/*  Should never get here .....
	 */
    	cl_error (E_IERR, e_badsw, op, "sampop has invalid intrfunc()");
        break;
    }

    return (OK);
}
