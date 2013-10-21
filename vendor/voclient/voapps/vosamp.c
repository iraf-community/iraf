/**
 *  VOSAMP - Example task to send a single SAMP message for the cmdline.
 *
 *  Usage:
 *
 *	vosamp [<opts>] <cmd> [args ...]
 *
 *  Where	
 *	<cmd>				command to process
 *  	-%,--test			run unit tests
 *  	-h,--help			print help summary
 *  	-d,--debug			debug output
 *  	-v,--verbose			verbose output
 *  	-q,--quiet			suppress all output
 *
 *  	-i,--interactive		interactive mode
 *  	-m,--many			handle multiple messages
 *  	-s,--sender <sender>		handle only messages from <sender>
 *
 *  	-t,--to <to>			send to specified application (or all)
 *  	-p,--pattern <pattern>		message pattern:  sync|async|notify
 *  	-f,--file <file>		send all commands in the file
 *  	-n,--nokeepalive		disable keep_alive feature
 *
 *  	-P,--proxy <pattern>		proxy process connection
 *  	-T,--timeout <N>		keepalive timeout
 *  	-S,--session <name>		session name
 *
 *	-r,--return			return result to API
 *
 *
 *  Subcommands:
 *
 *    snoop 				    print all received messages
 *
 *    session list 			    list all nodes in current session
 *    session leave|exit|logout 	    leave the current session
 *    session <name> 			    join the named session
 *
 *    status 				    print Hub availability
 *    list 				    list all registered clients
 *    access <appName>			    print <appName> availability
 *    handle <mtype>			    wait for <mtype> message
 *
 *    send <mtype> [<args> ...]		    generalized <mtype> message send
 *    exec <cmd>			    execute a client command
 *    pointAt <ra> <dec>		    point at given coords
 *    setenv  <name> <value>		    set an environment value
 *    getenv  <name>			    get an environment value
 *    setparam <name> <value>		    set a parameter value
 *    getparam <name>			    get a parameter value
 *
 *    load <url>			    load the named image/table file
 *    loadImage <url>			    load the named image
 *    loadVOTable <url>			    load the named VOTable
 *    loadFITS <url>			    load the named FITS bintable
 *    showRow [<tblId>] [<url>] <row>	    highlight specified row
 *    selectRows [<tblId>] [<url>] <rows>   select specified rows
 *    bibcode <bibcode>			    load the named bibcode
 *
 *
 *  @file       vosamp.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/12
 *
 *  @brief      Desktop SAMP messaging utility.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <time.h>

#include <netdb.h>
#include <sys/errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>


#include "samp.h"				/* LIBSAMP interface	    */
#include "voApps.h"				/* voApps interface	    */


#define	SZ_MTYPE	64
#define	SZ_BUF		128
#define	SZ_HOSTIP	16
#define	SZ_APPNAME	16
#define	SZ_BLOCK	4096

#define HUB_TIMEOUT				/* allow timeout	    */

#define	ENV_PROXY	"VOSAMP_PROXY"


#define	MAX_ARGS	8
#define	VOS_TIMEOUT	600			/* 10-min proxy timeout     */
#define	VOS_DEFPORT	3999
#define	VOS_HUBWAIT	3

#define MATCH(s)        (strcasecmp(cmd,s)==0)


char     senderIP[SZ_HOSTIP]; 
char     fname[SZ_FNAME]; 
char    *to			= NULL;

static int	sampH		= 0;		/* samp struct handle	    */

						/* task options		    */
static int	verbose		= 0;		/* verbose output	    */
static int	quiet		= 0;		/* suppress all output	    */
static int	debug		= 0;		/* verbose output	    */
static int	interact	= 0;		/* interactive shell	    */
static int	multiple	= 0;		/* accept multiple msgs	    */
static int 	xml_trace 	= 0;		/* trace XML_RPC	    */
static int 	keep_alive 	= 1;		/* lingering connection	    */
static int 	proxy_pid	= 0;		/* proxy vosamp pid	    */
static int 	proxy_port	= VOS_DEFPORT;	/* proxy vosamp port	    */

static int	svr_sock	= 0;		/* child server socket	    */
static int	ipc_sock	= 0;		/* IPC sock (parent/child)  */
static int	session_sock	= 0;		/* session mgr socket       */
static int	input_sock	= 0;		/* cmd input socket         */
static int	use_ipc		= 1;		/* use IPC		    */
static int      have_dotfile 	= 0;		/* have a valid .vosamp?    */
static int	in_parent 	= 1;		/* are we parent process?   */
static int	do_return 	= 1;		/* return result to API     */
static int	return_stat 	= OK;		/* return status  	    */
static int  	numargs 	= 0;		/* number of cmdline args   */ 
static int	timeout 	= VOS_TIMEOUT;	/* child timeout	    */

static int	hub_connected	= 0;		/* connected to SAMP hub    */
static int	sess_connected	= 0;		/* connected to session mgr */

char  *session_host = SESS_DEFHOST;  		/* session manager host IP  */
int    session_port = SESS_DEFPORT;         	/* session manager port     */
    
static char    *proxy		= NULL;		/* proxy connection string  */
static char    *session		= NULL;		/* session name		    */
static char    *pattern		= NULL;		/* SAMP msg pattern	    */
static char    *cmdfile		= NULL;		/* input command file	    */
static char    *filt_mtype      = NULL;		/* snoop filter mtype	    */
static char    *filt_sender     = NULL;		/* snoop sender name	    */
static char     cmd[SZ_CMD];			/* command name	    	    */
static char     cmdline[SZ_CMD];		/* full command line	    */
static char     proxy_host[SZ_FNAME];		/* proxy vosamp host name   */
static char     dotfile[SZ_FNAME];		/* path to local dotfile    */
static char     psession[SZ_FNAME];		/* proxy file session name  */
static char    *args[MAX_ARGS];			/* command args buffer      */

static FILE   *fd		= (FILE *) NULL;
static fd_set allset, fds;
static struct timeval tm;


static int   vos_sendToProxy (char *cmdline);
static int   vos_startProxy (void);
static int   vos_sampInit (void);
static int   vos_sendCmd (int sock, char *cmdline);
static int   vos_invalidArg (int argc, char *argv[], int nargs, int exist);
static void  vos_sampShutdown (void);
static void  vos_cmdHelp (char *cmd);
static void  vos_procCmd(int sampH, char *to, char *cmd, char **argv, int argc);
static void  vos_handleCmdInput (void);
static void  vos_msgHandler (char *sender, char *mtype, char *msg_id, Map map);
static void  vos_sessionHandler (char *sender, char *mtype, char *id, Map map);
static void  vos_forwardCmd (char *sess_cmd);
static void  vos_printMessage (char *mtype, char *sender, Map map);

static void  vos_rewriteCmd (char *line, char *fname);
static void  vosDebug (char *format, ...);
static char *vos_rewriteURL (char *in);
static char *vos_dotFile ();

int   vos_uploadFiles (int fd, char *cmdline);
int   vos_recvFile (int sock, int size, char *fname);
int   vos_sendFile (int sock, char *path, char *fname);
int   vos_openSession (char *host, int port, char *session_name);
int   vos_closeSession (int sock);


extern char *vos_toURL (char *arg);
extern char *vos_optArg (char *arg);
extern char *vos_getLocalIP (void);
extern char *vos_getFName (char *url);
extern char *vos_typeName (int type);
extern int  *vos_toIntArray (char *arg, int *nrows);
extern int   vot_atoi (char *v);


/*  Utility socket routines.
 */
extern int  vos_fileRead (int fd, void *vptr, int nbytes);
extern int  vos_fileWrite (int fd, void *vptr, int nbytes);
extern int  vos_openServerSocket (int port);
extern int  vos_openClientSocket (char *host, int port, int retry);
extern int  vos_testClientSocket (char *host, int port);
extern int  vos_sockRead (int fd, void *vptr, int nbytes);
extern int  vos_sockWrite (int fd, void *vptr, int nbytes);
extern int  vos_sockWriteHdr (int fd, int len, char *name, int type, 
				int mode, char *to);
extern int  vos_sockReadHdr (int fd, int *len, char *name, int *type, 
				int *mode);
extern int  vos_urlType (char *url);
extern int  vos_getURL (char *url, char *fname);

extern double vot_atof (char *v);

extern void vos_setNonBlock (int sock);
extern struct hostent *vos_getHostByName (char *lhost);
extern struct hostent *vos_dupHostent (struct hostent *hentry);



/*  Task specific option declarations.
 */
int  vosamp (int argc, char **argv, size_t *len, void **result);

static Task  self       = {  "vosamp",  vosamp,  0,  0,  0  };
static char  *opts      = "%:hnrdif:mps:t:vP:T:S:";
static struct option long_opts[] = {
        { "return",       0, 0,   'r'},         /* task option          */
        { "help",         2, 0,   'h'},         /* required             */
        { "test",         1, 0,   '%'},         /* required             */
        { "debug",        2, 0,   'd'},         /* debug		*/
        { "verbose",      2, 0,   'v'},         /* verbose output	*/
        { "quiet",        2, 0,   'q'},         /* suppress output	*/
        { "many",         2, 0,   'm'},         /* multiple msgs	*/
        { "sender",       1, 0,   's'},         /* sender filter	*/
        { "interactive",  2, 0,   'i'},         /* interactive mode	*/
        { "file",         1, 0,   'f'},         /* cmd file		*/
        { "pattern",      1, 0,   'p'},         /* msg pattern		*/
        { "proxy",        1, 0,   'P'},         /* proxy connection	*/
        { "session",      1, 0,   'S'},         /* session name		*/
        { "to",           1, 0,   't'},         /* recipient name	*/
        { "nokeepalive",  2, 0,   'n'},         /* keep connection 	*/
        { "timeout",      1, 0,   'T'},         /* connection timeout  	*/
        { NULL,           0, 0,    0 }
};

static void Usage (void);
static void Tests (char *input);



/****************************************************************************
 *  Program entry point.
 */
int
vosamp (int argc, char **argv, size_t *reslen, void **result)
{
    char **pargv, optval[SZ_FNAME], ch, *env_proxy = NULL;
    int	   i, pos = 0, len = 0;
    time_t expiry;


    /*  Initialize.
     */
    memset (cmd, 0, SZ_CMD);
    memset (cmdline, 0, SZ_CMD);
    memset (dotfile, 0, SZ_FNAME);
    memset (psession, 0, SZ_FNAME);
    memset (proxy_host, 0, SZ_FNAME);

    strcpy (proxy_host, vos_getLocalIP ());
    strcpy (dotfile, vos_dotFile ());
    for (i=0; i < MAX_ARGS; i++) 
	args[i] = calloc (1, SZ_LINE);

    *reslen     = 0;
    *result     = NULL;
    return_stat = OK;


    /*  Parse the argument list.
     */
    pargv = vo_paramInit (argc, argv, opts, long_opts);
    i = 0;
    while ((ch = vo_paramNext (opts,long_opts,argc,pargv,optval,&pos)) != 0) {
	i++;
        if (ch > 0) {
            switch (ch) {
            case '%':  Tests (optval);                  return (self.nfail);
            case 'h':  Usage ();                        return (OK);

            case 'd':  debug++;   				break;
            case 'v':  verbose++;				break;
            case 'q':  quiet++;					break;
            case 'n':  keep_alive = 0;				break;
            case 'm':  multiple++;     				break;
            case 'i':  interact++;  				break;

            case 's':  filt_sender = strdup (optval);  	i++; 	break;
            case 'f':  cmdfile     = strdup (optval);  	i++; 	break;
            case 't':  to          = strdup (optval);	i++; 	break;
            case 'p':  pattern     = strdup (optval);  	i++; 	break;
            case 'P':  proxy       = strdup (optval);  	i++; 	break;
            case 'S':  session     = strdup (optval);  	i++; 	break;
            case 'T':  timeout 	   = vot_atoi (optval);	i++; 	break;

            case 'r':  do_return=1;                     	break;
            default:
                fprintf (stderr, "Invalid option '%s'\n", optval);
                return (1);
            }

        } else if (ch == PARG_ERR) {
            return (ERR);

        } else {
	    /*  Remainder of argv is the subcommand and its arguments.
	     */
	    if (pargv[i] == NULL)
		break;
	    if (!cmd[0]) {
                strcpy (cmd, optval);
	    } else {
		if (strcasecmp (cmd, "exec") == 0 || 
		    strcasecmp (cmd, "echo") == 0) {
		        strcat (args[0], optval);
		        strcat (args[0], " ");
		} else {
		    char *ip;

		    while (pargv[i]) {
			if (strstr (pargv[i], "://") == NULL) {
			    /* replace ':' w/ '=' */
			    for (ip=pargv[i]; *ip; ip++)  
			        *ip = ((*ip == ':') ? '=' : *ip);
			}
                        strcpy (args[numargs++], pargv[i++]);
		    }
		    break;
		}
	    }
        }
    }


    /*  Print command help if that's all we're asked.
     */
    if (strncmp (cmd, "?", 1) == 0 || strncasecmp (cmd, "help", 4) == 0) {
	Usage ();
	return (OK);
    }
    if (strncmp(args[0], "?", 1) == 0 || strncasecmp(args[0], "help", 4) == 0) {
	vos_cmdHelp (cmd);
	return (OK);
    }
    if (strncmp(cmd, "access", 6) == 0 && strncasecmp(args[0], "hub", 3) == 0) {
	int hub_connected = vos_sampInit ();

	printf ("%s\n", (hub_connected ? "yes" : "no"));
	if (hub_connected) 
	    vos_sampShutdown ();
	return ( (hub_connected ? OK : ERR) );
    }
    if (strcmp (cmd, "handle") == 0 || strcmp (cmd, "snoop") == 0)
	keep_alive = 0;


    /*  Sanity checks.
     */
    if (interact || argc <= 1)
	cmdfile = strdup ("-");
    if (to == NULL)
	to = strdup ("all");
    if (pattern == NULL)
	pattern = strdup ("async");


    /*  Initialize the SAMP interface.
    tm.tv_sec    = VOS_HUBWAIT;
    */
    tm.tv_sec    = timeout;
    tm.tv_usec   = 0;
    session_sock = 0;


    /*  Check for environment definitions.
     */
    if ((env_proxy = getenv (ENV_PROXY)))
	proxy = strdup (env_proxy);

    if (proxy) {
	/*  We've been asked to send the command to a particular proxy, just
	 *  send the command line and any required data.
	 */
	char  *cp = strchr (proxy, (int)':');

	vosDebug ("Using proxy '%s'\n", proxy);
	if (cp) {
	    proxy_port = vot_atoi (cp+1);
	    *cp = '\0';
	    strcpy (proxy_host, proxy);
	} else {
	    proxy_port = VOS_DEFPORT;
	    strcpy (proxy_host, proxy);
	}
	use_ipc++;
	have_dotfile++;
	ipc_sock = vos_openClientSocket (proxy_host, proxy_port, 1);

    } else {
	/*  See if we have a local proxy running we can use instead of starting 
	 *  a new connection.  Data references remain local unless being
	 *  passed on to the session manager service.
	 */
	FILE  *fd = (FILE *) NULL;

	if (keep_alive && access (dotfile , R_OK) == 0) {
	    if ((fd = fopen (dotfile, "r+")) != (FILE *) NULL) {
		time_t  now = time ((time_t) 0);
		int  test = 0;

		/*  Read the dotfile contents.
		 */
		fscanf (fd, "%s  %d  %d  %ld  %s", 
		    proxy_host, &proxy_port, &proxy_pid, &expiry, psession);
		fclose (fd);


		/*  Make sure we can reach the running proxy, otherwise
 		 *  assume it is a stale dotfile.
		 */
		if ((test = vos_testClientSocket(proxy_host, proxy_port)) < 0) {
		    if (verbose)
			fprintf (stderr, "Removing stale .vosamp file ...\n");
		    unlink (dotfile);
            	    goto samp_init;
		} else {
    		    vos_sockWriteHdr (test, 0, NULL, SAMP_TEST, 0, "proxy");
		    close (test);
		}

		/*  Check for a stale dotfile. */
		if (now <= (expiry - 2)) {
		    use_ipc++;
		    have_dotfile++;
		} else {
		    if (verbose)
			fprintf (stderr, "Removing stale .vosamp file ...\n");
		    unlink (dotfile);
            	    goto samp_init;
		}
	    }
	} else {
	    /*  First do some sanity checks on the command.  If we're asked
	     *  to quit, then simply don't connect.
	     */
	    if (strncasecmp ("quit", cmd, 4) == 0) {
                return (OK);
	    }

	    /*  No remote command specified, no local connection, so connect to
	     *  the Hub as a new application.
	     * 
    	     *  Start the keep-alive proxy server.
     	     */
samp_init:
    	    if (keep_alive) {
    	        vos_startProxy ();
	        use_ipc = 1;
	    } else {
                if (! (hub_connected = vos_sampInit ())) {
		    fprintf (stderr, "Error: cannot connect to Hub.\n");
		    return (ERR);
		}
	        use_ipc = 0;
	    }
	}
    }


    /*  Process a single command if we're not in keep_alive mode.
     */
    if (!keep_alive) {
	vos_procCmd (sampH, to, cmd, args, numargs);
	goto cleanup_;
    }

    /*  Open an IPC socket to the proxy.
     */
    while (in_parent && ipc_sock <= 0) {
	vosDebug ("opening ipc socket to %s:%d\n", proxy_host, proxy_port);
        ipc_sock = vos_openClientSocket (proxy_host, proxy_port, 1);
        if (ipc_sock < 0) {
            if (verbose)
	        fprintf (stderr, "Cannot open client ipc_sock\n");
	    sleep (1);
        }
    }
    vosDebug ("ipc socket %d\n", ipc_sock);


    /*  Process the messages in the named file, or from the cmdline.  Since
     *  there is some overhead in connecting to the hub, this is an efficient
     *  way to send multiple messages from a single connection.
     */

    if (cmdfile) {
	fd = (cmdfile[0] == '-' ? stdin : fopen (cmdfile, "r"));

	/*  Send the command string from the stdin or the cmd file.
	 */
	fprintf (stderr, "vosamp> ");
#ifdef FGETS
	while (fgets (cmdline, SZ_CMD, fd) && !feof (fd) && !ferror (fd)) {
#else
    	FD_ZERO (&allset);
    	FD_SET (fileno (stdin), &allset);
	while (1) {
	    memcpy (&fds, &allset, sizeof(allset));
            if (select (8, &fds, NULL, NULL, NULL) == 0) 
		continue;
 
	    if (fgets (cmdline, SZ_CMD, stdin) == NULL)
		continue;
#endif
	    len = strlen (cmdline);
	    if (cmdline[len-1] == '\n')
	        cmdline[len-1] = '\0';
	    
	    if (strncasecmp (cmdline, "quit", 4) == 0 || 
	        strncasecmp (cmdline, "logo", 4) == 0)
		    break;
	    else if (cmdline[0]) {
	        vos_sendToProxy (cmdline);
		fprintf (stderr, "vosamp> ");
	    }
	}

        if (fd != stdin)
	    fclose (fd);

    } else if (cmd[0]) {
	/*  Send the command string from the commandline.
	 */
        int  i, len = 0;

        memset (cmdline, 0, SZ_CMD);
        sprintf (cmdline, "%s %s ", cmd, args[0]);
        for (i=1; i < numargs; i++) {
            strcat (cmdline, args[i]);
            strcat (cmdline, " ");
        }
        len = strlen (cmdline); cmdline[len-1] = '\0';

	vos_sendToProxy (cmdline);
    }
    close (ipc_sock);


    /*  Close down and clean up.
     */
cleanup_:
    if (!keep_alive)
        vos_sampShutdown ();

    if (to)          free ((void *) to);
    if (proxy)       free ((void *) proxy);
    if (cmdfile)     free ((void *) cmdfile);
    if (pattern)     free ((void *) pattern);
    if (session)     free ((void *) session);
    if (filt_sender) free ((void *) filt_sender);

    return (return_stat);
}


/**
 *  VOS_SENDTOPROXY -- Send a cmdline to the proxy.
 */
static int
vos_sendToProxy (char *cmdline)
{
    /*  See if we need to join a session before processing the cmd.
     */
    if (session && psession[0] != '\0') {
        /*  Have session param and running a proxy. 
         */
        memset (cmdline, 0, SZ_CMD);
        sprintf (cmdline, "session %s", session);

    } else if (strcasecmp (cmd, "session") == 0 && 
        (psession[0] && strcasecmp (psession, "none") == 0)) {
    	    /*  Have session command and not running a proxy. 
    	     */
            memset (cmdline, 0, SZ_CMD);
	    sprintf (cmdline, "session %s", args[0]);
    }

    if (strcasecmp (proxy_host, vos_getLocalIP())) {
        int  nfiles = vos_uploadFiles (ipc_sock, cmdline);
        vosDebug ("....sent %d data files....\n", nfiles);
    }


    if (strncmp (cmd, "?", 1) == 0 || strncmp (cmd, "help", 4) == 0) {
	vos_cmdHelp (cmd);
	return (0);
    }

    /*  Send the command string.
     */
    vosDebug ("sending to proxy: '%s'\n", cmdline);
    if (vos_sendCmd (ipc_sock, cmdline) == 0) {
        vosDebug ("sending to proxy fails: '%s'\n", cmdline);
        if (verbose)
    	    fprintf (stderr, "Error writing command '%s'\n", cmdline);

	return (0);

    } else {
        short  nread, len;

        vosDebug ("reading ipc response ....\n");
        nread = vos_sockRead (ipc_sock, &len, sizeof (short));
        vosDebug ("ipc response len = %d....\n", len);
        if (len > 0) {
            char *res = calloc (1, (len+1));
            nread = vos_sockRead (ipc_sock, res, len);
            vosDebug ("ipc response nread = %d....res = '%s'\n", nread, res);
            if ((nread > 0 && strcmp (res, "OK") != 0) || verbose)
                printf ("%s\n", res);
            if (res) free ((void *) res);
        }
	return (1);
    }

    return (0);
}


/**
 *  VOS_STARTPROXY -- Start the VOSAMP proxy process.
 */
static int
vos_startProxy (void)
{
    /*  Start the keep-alive proxy server.
     */
    if (keep_alive && in_parent && !have_dotfile) {
	switch ( fork() ) {
	case 0:					/* start child proxy 	*/
	    vosDebug ("child pid\n");

	    /*  Open server socket, read future input from socket.
	     */
	    vosDebug ("child opening server port %d...\n", proxy_port);
	    if ((svr_sock = vos_openServerSocket (proxy_port)) < 0) {
		fprintf (stderr, "Cannot open VOSAMP port %d\n", proxy_port);
		if (have_dotfile) unlink (dotfile);
	    }

    	    FD_ZERO (&allset);
    	    FD_SET (svr_sock, &allset); vos_setNonBlock (svr_sock);
	    vosDebug ("child svr_sock %d...\n", svr_sock);

	    if (session) {
    		/*  Open a client connection to the VOSAMP Session Manager.
    		 */
	        vosDebug ("initializing session '%s' on %s:%d\n", session,
		    session_host, session_port);
    		if ((session_sock = vos_openSession (session_host, 
		     session_port, session)) < 2) {
		    	vosDebug ("session open fails...\n");
        	        session_sock = 0;
		        session = NULL;
    		} else {
		    vosDebug ("child session sock %d...\n", session_sock);
    	            FD_SET (session_sock, &allset);
    	            vos_setNonBlock (session_sock);
		    sess_connected = 1;
		}
	    }

	    /*  Now initialize the SAMP interface.
	     */
	    vosDebug ("initializing SAMP connection\n");
#ifdef HUB_BLOCKS
            while (vos_sampInit () == 0) {
		/*  Wait for a Hub connection.
		 */
		if (debug || verbose) {
		    fprintf (stderr, "Waiting for Hub ....");
		    sleep (VOS_HUBWAIT);
		}
	    }
#else
            hub_connected = vos_sampInit ();
#endif

	    in_parent = 0;
	    vos_handleCmdInput ();

	    /*  Child never returns from this point ..... 
	     */
	    exit (0);
	    break;

	case -1:			/* fork fails 			*/ 
	    if (verbose && in_parent)
	        fprintf (stderr, "%d  proxy fork failure.\n", (int)getpid());
	    break;

	default:			/* parent exits 		*/
	    vosDebug ("parent pid\n");
	    sleep (2);			/* give child a chance to start */
	}
    }

    return (0);
}


/**
 *  VOS_SENDCMD -- Send a SAMP_CMD to the specified socket.
 */
static int
vos_sendCmd (int sock, char *cmdline)
{
    char res[SZ_LINE];
    int  nread = 0, nwrite = 0, len = (cmdline ? strlen (cmdline) : 0);


    vosDebug ("sendCmd: '%s'\n", cmdline);
    memset (res, 0, SZ_LINE);
		    
    if (vos_sockWriteHdr (sock, len, NULL, SAMP_CMD, SAMP_NOTIFY, to)) {
	vosDebug ("writing cmdline '%s'  fd=%d\n", cmdline, sock);
	nwrite = vos_sockWrite (sock, cmdline, len);

	/*  See if we have a result returned to us.  If so, it will be
	 *  a character string we should just print out.
	 */
	if (session && sock != session_sock) {
	    vosDebug ("reading response  fd=%d\n", sock);
	    nread = vos_sockRead (sock, &len, sizeof (int));
	    if (len > 0) {
	        char *res = calloc (1, (len+1));	

	        nread = vos_sockRead (sock, res, len);
	        if ((nread > 0 && strcmp (res, "OK") != 0) || verbose)
	            printf ("%s\n", res);
	        if (res) free ((void *) res);
	    }
	} else
	    return (1);
    }

    return (nread);
}


/**
 *  VOS_UPLOADFILES -- Upload any files on the commandline to the remote
 *  host.  The remote client is responsible for rewriting the commandline
 *  as appropriate, our job here is to suss out which are the file arguments.
 */
int
vos_uploadFiles (int sock, char *cmdline)
{
    char *ip, *op, buf[SZ_BUF], fname[SZ_FNAME];
    int   nfiles = 0;


    for (ip=cmdline; *ip; ) {
	memset (buf, 0, SZ_BUF);
	memset (fname, 0, SZ_FNAME);
	for (op=buf; *ip && !isspace(*ip); )	/* get token		*/
	   *op++ = *ip++;

	/*  Upload the file.  If we have a file:// URL strip out the
	 *  local pathname.  Note that in this implementation we're 
	 *  limited to a single data file per-message.
	 */
	if (strncmp ("file://", buf, 7) == 0) {
	    strcpy (fname, &buf[7]);
	    if (access (fname, F_OK) < 0)	/* file doesn't exist	*/
	        return (-1);
	}
	if (access (buf, F_OK) == 0)
	    strcpy (fname, buf);

	if (fname[0]) {				/* upload the file	*/
	    vosDebug ("uploading file '%s'....\n", fname);
    	    if (vos_sendFile (sock, fname, vos_getFName (fname)) != OK)
		fprintf (stderr, "Error sending file '%s'\n", fname);
	    nfiles++;
	}

	while (*ip && isspace (*ip))		/* skip whitespace	*/
	    ip++;
    }

    return (nfiles);
}


/**
 *  VOS_SENDFILE -- Send a file to a remote host.
 */
int
vos_sendFile (int sock, char *path, char *fname)
{
    char  block[SZ_BLOCK];
    int   fd, nread = 0, nleft = 0, size = 0, nb = 0;
    struct stat fs;

    	    
    if (access (path, R_OK) != 0 || stat (path, &fs) != 0) 
	return (ERR);

    size = fs.st_size;
    nleft = size;

    if ((fd = open (path, O_RDONLY))) {
        /*  Send the header to pass the filename and size, then
         *  send the data itself.
         */
        vos_sockWriteHdr (sock, size, fname, SAMP_DATA, 0, to);
        while (nleft > 0) {
	    memset (block, 0, SZ_BLOCK);
	    nb = min (SZ_BLOCK, nleft);

    	    /* Read from the file, write it to the socket.
     	     */
    	    vos_fileRead (fd, block, nb);
	    vos_sockWrite (sock, block, nb);

       	    nleft -= nb;
	    nread += nb;
        }
        close (fd);
    }

    return (OK);
}


/**
 *  VOS_RECVFILE -- Receive a file of the specified size from a remote host.
 */
int
vos_recvFile (int sock, int size, char *fname)
{
    char  block[SZ_BLOCK];
    int   fd, nread = 0, nleft = size, nb = 0;


    /*  Read the file from the socket, write to the filename.
     */
    vosDebug ("receiving file '%s'....%d\n", fname, size);
    if ((fd = open (fname, O_WRONLY|O_CREAT, 0664))) {
        while (nleft > 0) {
            memset (block, 0, SZ_BLOCK);
	    nb = min (SZ_BLOCK, nleft);

	    /* Read from the socket, write it to the file.
	     */
            vos_sockRead (sock, block, nb);
	    vos_fileWrite (fd, block, nb);

            nleft -= nb;
            nread += nb;
        }
        close (fd);
    }

    return (OK);
}


/**
 *  VOS_HANDLECMDINPUT -- Handle input from an input source.
 */
static void
vos_handleCmdInput ()
{
    register int i, read_timeout = 0, rc;
    char  line[SZ_CMD];
    static char data_file[SZ_FNAME];


    memset (line, 0, SZ_CMD);
    memset (data_file, 0, SZ_FNAME);
    while (1) {				    	/* loop until timeout       */

	/*  Initialize the input file descriptor set and cmd buffer.
	 */
	memset (line, 0, SZ_CMD);
	memset (cmd, 0, SZ_CMD);
	for (i=0; i < MAX_ARGS; i++)
   	    memset (args[i], 0, SZ_LINE);

	memcpy (&fds, &allset, sizeof(allset));
      	if ((rc = select (8, &fds, NULL, NULL, (timeout ? &tm : NULL))) == 0) {
	    read_timeout++;			/* input timeout	    */
	    if (hub_connected && !samp_hubRunning()) {
		hub_connected = 0;
	    }
	    if (!hub_connected && samp_hubRunning()) {
		hub_connected = vos_sampInit();
#ifdef HUB_TIMEOUT
	    } else {
	        unlink (dotfile); 		/* clean up the dotfile     */
       	        vos_sampShutdown ();		/* shutdown Hub connection  */
	        return;
#endif
	    }
	    continue;

	} else if (rc < 0) { 			/*  Select error ....  */
	    vosDebug ("select error '%s'\n", strerror (errno));
	    continue;
	}


	if (FD_ISSET(svr_sock, &fds)) {
	    int  nbytes = 0, nread = 0, type = 0, mode = 0;
	    char fname[SZ_FNAME];


       	    /* Accept a connection on the proxy port from a cmdline client.
       	     */
	    vosDebug ("got input on server socket....\n");
	    memset (fname, 0, SZ_FNAME);

       	    if ((ipc_sock = accept (svr_sock, NULL, NULL)) < 0) {
       		fprintf (stderr, "accept() errno %d: %s", 
		    errno, strerror(errno));
		continue;
	    }

	    if (vos_sockReadHdr (ipc_sock, &nbytes, fname, &type, &mode)) {
		vosDebug ("server input: msgType = %s....\n", 
		    vos_typeName (type));

	        if (strcasecmp (proxy_host, senderIP) && type == SAMP_DATA) {
		    if (vos_recvFile (ipc_sock, nbytes, fname) != OK) {
		        if (verbose || debug)
			    fprintf (stderr, "Error in recvFile '%s'\n", fname);
		    }
		    strcpy (data_file, fname);

	            if (vos_sockReadHdr(ipc_sock, &nbytes, fname,&type,&mode)) {
	                nread = vos_sockRead (ipc_sock, line, nbytes);
		        vos_rewriteCmd (line, data_file);
		    }
    		    memset (data_file, 0, SZ_FNAME);

	        } else if (type == SAMP_CMD) {
	            nread = vos_sockRead (ipc_sock, line, nbytes);
		    vosDebug ("server cmd = '%s'....nr=%d nb=%d\n", 
			line, nread, nbytes);
		    if (data_file[0]) {
		        vos_rewriteCmd (line, data_file);
    		        memset (data_file, 0, SZ_FNAME);
    		    }
	        } else if (type == SAMP_RESULT) {
		    ; /*  Not yet implemented  */

	        } else if (type == SAMP_TEST) {
	    	    close (ipc_sock); 	/*  Test connection, just ignore.  */
	        }
	    }
	    input_sock = ipc_sock;

	} else if (FD_ISSET(session_sock, &fds)) {
	    int  nbytes = 0, nread = 0, type = 0, mode = 0;

	    /*  Accept a command from the session manager for forwarding.
	     */
	    if (vos_sockReadHdr (session_sock, &nbytes, fname, &type, &mode)) {
		vosDebug ("got input on session socket....\n");
	        nread = vos_sockRead (session_sock, line, nbytes);
		input_sock = session_sock;

	    } else {
		/*  Session manager closed the socket, shut it down and 
		 *  disconnect.
		 */
		vosDebug ("Closing session socket....\n");
    		FD_CLR (session_sock, &allset);
    		close (session_sock);
		if (session) free ((void *) session);
		session = NULL;
		session_sock = 0;
		continue;
	    }

	} else
	    vosDebug ("Ignoring input on other descriptor ...\n");


	/*  Kill any trailing spaces and make sure we have an actual cmdline.
	 */
	if (isspace(line[strlen(line)-1]))
	    line[strlen(line)-1] = '\0';   
	if (line[0] == '\0')
	    continue;


	/*  Check whether the command we want to process is directed to the
	 *  proxy process itself.  If so we process it locally rather than
	 *  as a SAMP message for other clients.
	 */
	vosDebug ("handleCmd line: '%s'\n", line);
	if (strncasecmp (line, "exec", 4) == 0 ||
	    strncasecmp (line, "echo", 4) == 0) {
	        /*  Special-case for exec/echo cmd to create single arg
	         */
	        strncpy (cmd, line, 4);
	        sprintf (args[0], "%s", &line[5]);
	        numargs = 1;
	        vos_procCmd (sampH, to, cmd, args, numargs);

	} else if (strncasecmp (line, "status", 4) == 0) {
    	    if (ipc_sock > 0) {
                short slen, nwrite;
		char  buf[SZ_LINE];

		memset (buf, 0, SZ_LINE);
	        sprintf (buf, "Status:  SAMP Hub: %s    ", 
		    (hub_connected ? "ON " : "OFF"));
	        if (sess_connected) {
	            strcat (buf, "Session Mgr: ON  (session='");
		    strcat (buf, session);
		    strcat (buf, "')");
	        } else 
	            strcat (buf, "Session Mgr: OFF");

		slen = strlen (buf);		/* return result string     */
        	if (vos_sockWrite (ipc_sock, &slen, sizeof (short)))
            	    nwrite = vos_sockWrite (ipc_sock, buf, slen);

		continue;
    	    }

	} else if (strncasecmp (line, "quit", 4) == 0) {
	    /*  Quit the proxy process and return.
	     */
	    if (access (vos_dotFile(), F_OK) == 0)
		unlink (vos_dotFile());
       	    vos_sampShutdown ();		/* shutdown Hub connection  */
	    exit (0);

	} else if (strncasecmp (line, "list", 4) == 0) {
	    /*  List the connected clients.
	     */
	    verbose++, numargs=1;
	    vos_procCmd (sampH, to, "listClients", args, numargs);

	} else if (strncasecmp (line, "session", 7) == 0) {
	    /*  Special commands for Session management.
	     */
       	    numargs = sscanf (line, "%s %s", cmd, args[0]);

	    if (strcmp (args[0], "list") == 0) {
		memset (args[0], 0, SZ_LINE);
		strcpy (args[0], "listNodes");

		    if (session_sock == 0) {
			fprintf (stderr, "Error: No session established\n");
			return;
		    } else {
	                verbose++;
		        numargs=1;
	                vos_procCmd (sampH, to, "listNodes", args, numargs);
		    }

	    } else if (strncmp (args[0], "leave", 5) == 0) {
		if (session_sock)
		    vos_closeSession (session_sock);

	    } else {
                /*  Open a client connection to the VOSAMP Session Manager.
                 */
		if (vos_testClientSocket (session_host, session_port) < 0) {
		    fprintf (stderr, 
			"Error: Session manager not available at %s:%d\n", 
			session_host, session_port);
		    return;
		}
                vosDebug ("initializing session '%s' on %s:%d\n", args[0],
                    session_host, session_port);
		session_sock = 0;
		session = strdup (args[0]);
                if ((session_sock = vos_openSession (session_host,
                     session_port, args[0])) < 2) {
                        vosDebug ("session open fails...\n");
                        session_sock = 0, session = NULL;
                } else {
                    vosDebug ("child session sock %d...\n", session_sock);
                    FD_SET (session_sock, &allset);
                    vos_setNonBlock (session_sock);
		    sess_connected = 1;
                }
	    }

	} else {
	    /*  Process the input command as a SAMP message.  Only process
	     *  the command if we're connected to a Hub
	     */
	    if (hub_connected) {
       	        numargs = sscanf (line, "%s %s %s %s %s %s %s %s %s", 
	            cmd, args[0], args[1], args[2], args[3], 
	               args[4], args[5], args[6], args[7]);

	        vos_procCmd (sampH, to, cmd, args, numargs);
	    }
	}
    }

#ifdef FOO
	if (ipc_sock)
	    close (ipc_sock); 		/*  close the socket descriptor  */
#endif
}


/**
 *  VOS_REWRITECMD --  Rewrite the command string with filename substitution.
 */
static void
vos_rewriteCmd (char *line, char *fname)
{
    char *ip, *op, buf[SZ_CMD], obuf[SZ_CMD];

    memset (obuf, 0, SZ_CMD);
    for (ip=line; *ip; ) {
        memset (buf, 0, SZ_CMD);
        for (op=buf; *ip && !isspace(*ip); )    /* get token            */
           *op++ = *ip++;

        /*  Replace file-URI and absolute paths with the replacement filename.
         */
        if (strncmp ("file://", buf, 7) == 0 || buf[0] == '/')
            strcat (obuf, fname);
	else
            strcat (obuf, buf);

        while (*ip && isspace (*ip))            /* skip whitespace      */
            strncat (obuf, ip++, 1);
    }

    memset (line, 0, SZ_CMD);
    strcpy (line, obuf);
}


/**
 *  VOS_REWRITEURL -- Rwrite a URL argument so it is compatible with the
 *  session manager.  Upload any local files if needed.
 */
static char *
vos_rewriteURL (char *in)
{
    static char out[SZ_URL], path[SZ_LINE], *fname;
    char   tmpfile[SZ_LINE];


    memset (path, 0, SZ_LINE);		/* initialize		*/
    memset (tmpfile, 0, SZ_LINE);
    memset (out, 0, SZ_URL);

    switch (vos_urlType (in)) {
    case VOS_LOCALURL:
	sprintf (tmpfile, "/tmp/%s", vos_getFName (in));
	vos_getURL (in, tmpfile);
	strcpy (path, tmpfile);
	break;

    case VOS_LOCALURI:
	strcpy (path, &in[7]);
	break;

    case VOS_LOCALFILE:
	strcpy (path, in);
	break;

    case VOS_REMOTE:
	/* Nothing to do, simply return the string.
	 */
	return (in);
    }

    /*  Get the filename from the path.  Format the new URL so that the 
     *  session manager can easily rewrite it to use the smgr data space.
     */
    fname = vos_getFName (path);
    sprintf (out, "SESSION_URL/%s", fname);

    /*  Send data to the session manager for storage as the 'fname'.  The
     *  session manager will rewrite the URL.
     */
    if (session_sock && vos_sendFile (session_sock, path, fname) != OK)
	fprintf (stderr, "Error uploading file '%s'\n", in);

    return (out);
}


/**
 *  USAGE --  Print a task usage summary.
 */
static void
Usage (void)
{ 
 fprintf (stderr,
 "  Usage:\n"
 "\n" 
 "    %% vosamp [-hvd] [-t to] [-p pattern] [-f file] <cmd> [args ...]\n" 
 "\n" 
 "    where:\n"
 "      <cmd>                           command to process\n"
 "      -%%,--test                       run unit tests\n"
 "      -h,--help                       print help summary\n"
 "      -d,--debug                      debug output\n"
 "      -v,--verbose                    verbose output\n"
 "      -q,--quiet                      suppress all output\n"
 "\n"
 "      -i,--interact                   interactive mode\n"
 "      -m,--many                       handle multiple messages\n"
 "      -s,--sender <sender>            handle only messages from <sender>\n"
 "\n"
 "      -t,--to <to>                    send to specified app (or all)\n"
 "      -p,--pattern <pattern>          message pattern: sync|async|notify\n"
 "      -f,--file <file>                send all commands in the file\n"
 "      -n,--nokeepalive                disable keep_alive feature\n"
 "\n"
 "      -P,--proxy <IP>                 use specfied proxy IP\n"
 "      -T,--timeout <N>                keepalive timeout\n"
 "\n"
 "      -r,--return                     return result to API\n"
 "\n" 
 "  Commands:\n" 
 "\n" 
 "  snoop                                 print all received messages\n"
 "  send <mtype> [<args> ...]             generalized <mtype> message send\n" 
 "\n" 
 "  status                                print Hub availability\n" 
 "  list                                  list all registered clients\n"
 "  access <appName>                      print <appName> availability\n" 
 "  handle <mtype>                        wait for <mtype> message\n" 
 "\n" 
 "  exec <cmd>                            execute a client command\n" 
 "  setenv  <name> <value>                set an environment value\n" 
 "  getenv  <name>                        get an environment value\n" 
 "  setparam <name> <value>               set a parameter value\n" 
 "  getparam <name>                       get a parameter value\n" 
 "\n" 
 "  load <url>                            load the image/table\n" 
 "  loadImage <url>                       load the named image\n" 
 "  loadVOTable <url>                     load the named VOTable\n" 
 "  loadFITS <url>                        load the named FITS bintable\n" 
 "  loadSpec <url>                        load the named spectrum\n" 
 "  loadResource <ivorn>                  load the named VO Resource\n" 
 "\n" 
 "  pointAt <ra> <dec>                    point at given coords\n" 
 "  showRow [<url>] [<tblId>] <row>       highlight specified row\n" 
 "  selectRows [<url>] [<tblId>] <rows>   select specified rows\n" 
 "  bibcode <bibcode>                     load the bibcode\n"
 "\n\n" 
 "  Examples:\n\n"
 "    1)  Load a VOTable to Topcat:\n\n"
 "          %% vosamp load /path/example.xml\n"
 "          %% vosamp load http://foo.edu/example.xml\n"
 "          %% vosamp load http://foo.edu/query?RA=0.0&DEC=0.0&SR=0.1\n"
 "\n"
 "    2)  Send a command string to IRAF:\n"
 "\n"
 "          %% vosamp -t iraf exec 'display dev$pix 1'\n"
 "\n"
 "    3)  List all clients in a SAMP desktop session:\n"
 "\n"
 "          %% vosamp list\n"
 "\n"
 "    4)  Check whether a Hub is available from a script:\n"
 "\n"
 "          set isHub = `vosamp access Hub`\n"
 "          if ($isHub == \"no\") then\n"
 "            echo \"No Hub available, quitting .....\"\n"
 "            exit 1\n"
 "          else\n"
 "            ....do something....\n"
 "          endif\n"
 "\n" 
 );
}



/**
 *  Tests -- Task unit tests.
 */
static void
Tests (char *input)
{
   Task *task  = &self;

   char *img   = "http://iraf.noao.edu/votest/ypix.fits";
   char *vot   = "http://iraf.noao.edu/votest/usno-b.xml";
   char *bin   = "http://iraf.noao.edu/votest/usno-b.fits";


   if (access (input, F_OK) != 0) {
	fprintf (stderr, "Error: cannot open input file '%s'\n", input);
	return;
   }

   vo_taskTest (task, "--help", NULL);

   vo_taskTest (task, "load", input, NULL);
   vo_taskTest (task, "load",        img, NULL);
   vo_taskTest (task, "loadImage",   img, NULL);
   vo_taskTest (task, "load",        vot, NULL);
   vo_taskTest (task, "loadVOTable", vot, NULL);
   vo_taskTest (task, "load",        bin, NULL);
   vo_taskTest (task, "loadFITS",    bin, NULL);

   vo_taskTest (task, "access", "hub", NULL);
   vo_taskTest (task, "list", NULL);
   vo_taskTest (task, "load", input, NULL);

   vo_taskTestReport (self);
}


/**
 *  VOS_DOTFILE -- Create a pathname to the .vosamp file indicating a 
 *  running local proxy server.
 */
static char *
vos_dotFile ()
{
    static char dotfile[SZ_FNAME];
    static int  initialized = 0;

    if (! initialized) {
        char   *home = NULL;

        if ((home = getenv ("HOME")) == NULL)
	    home = "./";
        memset (dotfile, 0, SZ_FNAME);
        sprintf (dotfile, "%s/.vosamp", home);
    }
    initialized++;

    return (dotfile);
}


/**
 *  VOS_SAMPINIT -- Initialize the SAMP/Proxy interface connection.
 */
static int
vos_sampInit (void)
{
    FILE *df;


    /*  If there's no Hub running, don't bother ....
     */
    if (!samp_hubRunning())
	return (0);

    /*  Initialize the SAMP interface.
    */
    sampH = sampInit ("vosamp", "VOClient SAMP Task");
/*
    if (samp_hubActive (sampH) == 0) {
	vosDebug ("sampInit failed to connect ....\n");
        vos_sampShutdown ();
        return (0);
    }
*/

    /*  Set alternative messaging pattern if requested. Valid values are
     *  'synch', 'asynch' or 'notify'.
     */
    if (pattern) {
	switch (tolower(pattern[0])) {
	case 's':  samp_setSyncMode (sampH);	break;	    /* default 	*/
	case 'a':  samp_setASyncMode (sampH);	break;
	case 'n':  samp_setNotifyMode (sampH);	break;
	default:
	    if (verbose)
		fprintf (stderr, "Warning: Invalid pattern '%s'\n", pattern);
	}
    } else {
        /*  Use Synchronous mode by default so we don't exit before receiving
         *  the reply.  Otherwise, we could cause an error in the recipient.
         */
        samp_setSyncMode (sampH);
    }

    /*  If we're not sending to a specific client, disable the name mapping
     *  to speed up the connection.
    if (strncmp (to, "all", 3) == 0)
	samp_setOpt (sampH, "mapClients", 0);
     */

    samp_Metadata (sampH, "author.name",   "Mike Fitzpatrick, NOAO");
    samp_Metadata (sampH, "author.email",  "fitz@noao.edu");
    samp_Metadata (sampH, "samp.description.html",
        	"http://iraf.noao.edu/voclient/vosamp.html");
    samp_Metadata (sampH, "samp.icon.url", 
		"http://iraf.noao.edu/voclient/vao_icon.gif");

    /*  If we're in session mode, subscribe to all message types.  The
     *  same handler is used to pass on the message to other clients in
     *  the session.
     */
    if (session)
        samp_Subscribe (sampH, "*",  vos_sessionHandler);

    /*  Register with the Hub and begin messaging.
     */
    sampStartup (sampH);


    /*  Write the dotfile.
     */
    if (!interact && (df = fopen (vos_dotFile(), "w+")) != (FILE *) NULL) {
	fprintf (df, "%s  %d  %d  %ld  %s\n", vos_getLocalIP(), proxy_port,
	    (int) getpid(), (long) (time((time_t)0) + timeout),
	    (session ? session : "none"));
	fclose (df);
    }

    return (sampH);
}


/**
 *  VOS_SAMPSHUTDOWN -- Shutdown the SAMP/Proxy interface connection.
 */
static void
vos_sampShutdown (void)
{
    if (sampShutdown (sampH) < 0) 			/*  clean up 	*/
	fprintf (stderr, "SAMP shutdown fails\n");
    sampClose (sampH);
}


/**
 *  VOS_MSGHANDLER -- Incoming message handler.
 */
static void
vos_msgHandler (char *sender, char *mtype, char *msg_id, int params)
{
    vosDebug ("in vos_msgHandler.....\n");
    if (filt_sender && strcasecmp (filt_sender, sender))
        return;

    /*  Either no filters were set, or the message is of the requested type,
     *  print the contents.
     */
    if (!quiet)
        vos_printMessage (mtype, samp_id2app (sampH, sender), params);

    if (!interact && !multiple) {       /*  Do a clean disconnect ....  */
        if (sampShutdown (sampH) < 0)
            fprintf (stderr, "SAMP shutdown fails\n");
        sampClose (sampH);
        exit (0);
    }
}


/**
 *  VOS_PRINTMESSAGE -- Print a received message to the stdout.
 */
static void
vos_printMessage (char *mtype, char *sender, Map map)
{
    int  i, npars = samp_getMapSize (map);
    char *key = NULL;
    Map   smap;


    if (verbose)
        printf ("sender: '%s'\t", sender);
    printf ("%s", mtype);

    for (i=0; i < npars; i++) {
        key = samp_getMapKey (map, i);
	if (strncmp (key, "meta", 4) == 0 || strncmp (key, "sub", 3) == 0) {
	    smap = samp_getMapFromMap (map, key);
            printf ("\n");
	    samp_printMap (key, smap);
	} else
            printf ("\t%s=%s", key, samp_getMapVal (map, i));
    }

    printf ("\n");
}


/**
 *  VOS_SESSIONHANDLER -- Generic session message-forwarding handler.
 */

#define MTYPE(s)        (strcasecmp(mtype,s)==0)

static void
vos_sessionHandler (char *sender, char *mtype, char *msg_id, Map map)
{
    char   sess_url[SZ_URL], sess_cmd[SZ_CMD], num[SZ_FNAME];
    char  *tid = NULL, *imid = NULL, *url = NULL, *sid = NULL;
    char  *name = NULL, *value = NULL, *cmd = NULL, *bibcode = NULL;
    char  *key = NULL, *val = NULL;
    int    i, row = 0, rows = 0, len, nelem;
    float  ra, dec;
    Map    meta, ids;


    if (!mtype)
	return;

    memset (sess_url, 0, SZ_URL);
    memset (sess_cmd, 0, SZ_CMD);

    /*
    vosDebug ("sender: '%s'  mtype: '%s'  id: '%s'\n", sender, mtype, msg_id);
    */

    /*  Process the mtype.
     */
    if (MTYPE ("samp.app.ping")) {
	;					/* FIXME -- no-op	*/

    } else if (MTYPE ("samp.app.status")) {
	;					/* FIXME -- no-op	*/

    } else if (MTYPE ("table.load.fits")) {
	tid  = strdup (samp_getStringFromMap (map, "table-id"));
	url  = strdup (samp_getStringFromMap (map, "url"));
	name = strdup (samp_getStringFromMap (map, "name"));

	sprintf (sess_cmd, "loadFITS %s %s", vos_rewriteURL(url), tid);


    } else if (MTYPE ("table.load.votable")) {
	/*  Get parameters from the message Map.
	 */
	tid  = strdup (samp_getStringFromMap (map, "table-id"));
	url  = strdup (samp_getStringFromMap (map, "url"));
	name = strdup (samp_getStringFromMap (map, "name"));

	sprintf (sess_cmd, "loadVOTable %s %s", vos_rewriteURL(url), tid);


    } else if (MTYPE ("table.highlight.row")) {
	tid = strdup (samp_getStringFromMap (map, "table-id"));
	url = strdup (samp_getStringFromMap (map, "url"));
	row = samp_getIntFromMap (map, "row");

	sprintf (sess_cmd, "showRow %s %d", vos_rewriteURL(url), row);

	
    } else if (MTYPE ("image.load.fits")) {
	imid = strdup (samp_getStringFromMap (map, "image-id"));
	url  = strdup (samp_getStringFromMap (map, "url"));
	name = strdup (samp_getStringFromMap (map, "name"));

	sprintf (sess_cmd, "loadImage %s %s", vos_rewriteURL(url), imid);


    } else if (MTYPE ("coord.pointAt.sky")) {
	ra  = samp_getFloatFromMap (map, "ra");
	dec = samp_getFloatFromMap (map, "dec");

	sprintf (sess_cmd, "pointAt %g %g", ra, dec);


    } else if (MTYPE ("client.cmd.exec")) {
	cmd = strdup (samp_getStringFromMap (map, "cmd"));

	sprintf (sess_cmd, "exec %s", cmd);


    } else if (MTYPE ("client.env.get")) { 			/**  NYI  **/
	name = strdup (samp_getStringFromMap (map, "name"));


    } else if (MTYPE ("client.env.set")) {
	name  = strdup (samp_getStringFromMap (map, "name"));
	value = strdup (samp_getStringFromMap (map, "value"));

	sprintf (sess_cmd, "setenv %s %s", name, value);


    } else if (MTYPE ("client.param.get")) { 			/**  NYI  **/
	name  = strdup (samp_getStringFromMap (map, "name"));


    } else if (MTYPE ("client.param.set")) {
	name  = strdup (samp_getStringFromMap (map, "name"));
	value = strdup (samp_getStringFromMap (map, "value"));

	sprintf (sess_cmd, "setparam %s %s", name, value);


    } else if (MTYPE ("bibcode.load")) {
	bibcode  = strdup (samp_getStringFromMap (map, "bibcode"));

	sprintf (sess_cmd, "bibcode %s", bibcode);


    } else if (MTYPE ("table.select.rowList")) {
	tid  = strdup (samp_getStringFromMap (map, "table-id"));
	url  = strdup (samp_getStringFromMap (map, "url"));
	rows = samp_getListFromMap (map, "row-list");

	sprintf (sess_cmd, "selectRows %s %s ", vos_rewriteURL(url), tid);
	len = samp_listLen (rows);
	for (i=0; i < len; i++) {
	    sprintf (num, "%d%c", samp_getIntFromList(rows,i), 
	        (i < (len - 1) ? ',' : ' '));
	    strcat (sess_cmd, num);
	}


    } else if (MTYPE ("spectrum.load.ssa-generic")) {
	url  = strdup (samp_getStringFromMap (map, "url"));
	sid  = strdup (samp_getStringFromMap (map, "spectrum-id"));
	name = strdup (samp_getStringFromMap (map, "name"));
	meta = samp_getMapFromMap (map, "meta");

	sprintf (sess_cmd, "loadSpec %s %s %s", 
	    vos_rewriteURL(url), (name ? name : " "), (sid ? sid : " "));


    } else if (MTYPE ("voresource.loadlist.*")) {		/*  NYI  */
	name = strdup (samp_getStringFromMap (map, "name"));
	ids  = samp_getMapFromMap (map, "ids");

	nelem = samp_getMapSize (ids);
	for (i=0; i < nelem; i++) {
	    key = samp_getMapKey (ids, i);
	    val = samp_getMapVal (ids, i);

	    /*  Forward the resources individually.
	     */
	    memset (sess_cmd, 0, SZ_CMD);
	    sprintf (sess_cmd, "loadResource %s", vos_rewriteURL(url));
    	    vos_forwardCmd (sess_cmd);
	}
	goto clean_up;


    } else if (MTYPE ("samp.hub.disconnect")) {
	; 	/* ignore hub ejections		*/
    } else if (MTYPE ("samp.hub.event.subscriptions")) {
	; 	/* ignore client subscriptions	*/
    } else if (MTYPE ("samp.hub.unregister") ||
    	       MTYPE ("samp.hub.event.unregister")) {
	; 	/* ignore hub closures	*/

    } else {
	if (verbose && debug) 
	    fprintf (stderr, "unknown mtype '%s'\n", mtype);
    }


    /*  Forward the session command.
     */
    vos_forwardCmd (sess_cmd);
	
    if (verbose && debug) 
	fprintf (stderr, "sessionHandler: cmdline = '%s'\n", sess_cmd);


    /*  Clean up.
     */
clean_up:
    if (tid)  free (tid); 
    if (url)  free (url); 
    if (cmd)  free (cmd);
    if (sid)  free (sid);
    if (imid)  free (imid); 
    if (name)  free (name);
    if (value)  free (value);
    if (bibcode)  free (bibcode);
}


/**
 *  VOS_FORWARDCMD -- Forward a session command.
 */
static void
vos_forwardCmd (char *sess_cmd)
{
    int   ssock = session_sock, len, nwrite;

    len = strlen (sess_cmd);
    if (vos_sockWriteHdr (ssock, len, NULL, SAMP_CMD, SAMP_NOTIFY, to))
	nwrite = vos_sockWrite (ssock, sess_cmd, len);
}


/**
 *  VOS_PROCCMD -- Process the command and its arguments.
 */
static void
vos_procCmd (int sampH, char *to, char *cmd, char *args[], int numargs)
{
    int   i, reslen = 0, stat = OK, ssock = session_sock;
    char *result = NULL;
    char buf[SZ_BUF];
    char sess_cmd[SZ_BUF];


    /*  Print command help.
     */
    if (!use_ipc) {
        if (strncmp (cmd, "?", 1) == 0) {
	    Usage ();
	    return;
        }
        if (strncmp (args[0], "?", 1) == 0) {
	    vos_cmdHelp (cmd);
	    return;
        }
    }


    /********************************************
     ***         VOSAMP Custom Commands        **
     ********************************************/
    memset (buf, 0, SZ_BUF);
    memset (sess_cmd, 0, SZ_BUF);

    if (MATCH ("status")) {
	stat = (sampH >= 0);

    } else if (MATCH ("handle")) {
	int timeout = (vot_atoi (args[1]) == 0 ? 999999 : vot_atoi (args[1]));

	if (verbose)
	    fprintf (stderr, "Waiting for '%s' ....\n", args[0]);
	if (*args[1])
	    filt_sender = args[1];

	samp_Subscribe (sampH, (filt_mtype = args[0]), vos_msgHandler);
    	samp_DeclareSubscriptions (sampH);
	sleep (timeout);

    } else if (MATCH ("snoop")) {
        /*  Subscribe to all message types and install the snoop handler.
         *  Sleep forever so the handler can print anything it receives.
         */
	multiple = 1;
        samp_Subscribe (sampH, "*",  vos_msgHandler);
    	samp_DeclareSubscriptions (sampH);
	sleep (9999999);

    } else if (MATCH ("send")) {
	if (strcasecmp (to, "all"))	/* no recipient, use broadcast */
	    samp_setASyncMode (sampH);
        stat = samp_sendGeneric (sampH, to, args[0], &args[1]);

    } else if (MATCH ("echo")) {     			/* ECHO 	      */
	if (!use_ipc) {
	    for (i=0; i < numargs; i++)
	        printf ("%s ", args[i]);
 	    printf ("\n");
	}

    } else if (MATCH ("start")) {     			/* START 	      */
	if (!use_ipc)
            printf ("Starting SAMP interface .....\n");
        sampStartup (sampH);

    } else if (MATCH ("stop")) {      			/* STOP 	      */
        if (!use_ipc)
	    printf ("Stopping SAMP interface .....\n");
        sampShutdown (sampH);

    } else if (MATCH ("help")) {      			/* HELP 	      */
        Usage ();

    } else if (MATCH ("quit")) {                   	/* QUIT 	      */
        if (sampShutdown (sampH) < 0) {
	    if (!use_ipc)
                fprintf (stderr, "Shutdown fails\n");
	}
        sampClose (sampH);
	unlink (vos_dotFile());
        exit (0);

    } else if (MATCH ("trace")) {          		/* TRACE 	      */
        xml_trace++;
        if (xml_trace % 2)
            setenv ("XMLRPC_TRACE_XML", "1", 1);
        else
            unsetenv ("XMLRPC_TRACE_XML");

    } else if (MATCH ("listNodes")) {         		/* LIST NODES	      */
	int  type, mode, nbytes, len, nwrite;
	char buf[SZ_BUF];

	len = strlen ("list");
        if (vos_sockWriteHdr (ssock, len, "list", SAMP_CMD, SAMP_NOTIFY, to)) {
	    nwrite = vos_sockWrite (ssock, "list", len);

	    memset (buf, 0, SZ_BUF);
            if (vos_sockReadHdr (session_sock, &nbytes, NULL, &type, &mode))
                reslen = vos_sockRead (session_sock, buf, nbytes);
	    result = strdup (buf);
	}

    } else if (MATCH ("listClients")) {         	/* LIST CLIENTS	      */
	char *clist = NULL;

	samp_mapClients (sampH);
        clist = samp_getClients (sampH);
	if (use_ipc) {
	    reslen = strlen (clist);
	    result = strdup (clist);
	} else {
	    printf ("\nClients:\n%s\n", clist);
	}
	


    /********************************************
     ***      Hub Administrative Messages     ***
     ********************************************/
    } else if (MATCH ("ping")) {		/* samp.app.ping 	      */
        stat = samp_Ping (sampH, "Hub");



    /********************************************
     ***    Client Administrative Messages    ***
     ********************************************/
    } else if (MATCH ("ping")) {		/* samp.app.ping 	      */
        stat = samp_Ping (sampH, to);

    } else if (MATCH ("access")) {		/* samp.app.ping 	      */
        stat = samp_Ping (sampH, (to=args[0]));
	if (use_ipc) {
	    result = strdup ((stat < 0 ? "no" : "yes"));
	    reslen = strlen (result);
	    return_stat = (stat < 0 ? ERR : OK);
	} else {
	    printf ("%s\n", (stat < 0 ? "no" : "yes"));
	    return_stat = (stat < 0 ? ERR : OK);
	}



    /********************************************
     ***    Table/Image Load Message Types    ***
     ********************************************/
						/* image.load.fits 	      */
    } else if (MATCH ("loadImage")) {
	if ((stat = vos_invalidArg (numargs, args, 1, 1))) {
	    return_stat = ERR;
	    goto cmd_exit_;
	}
        stat = samp_imageLoadFITS (sampH, to, 
		vos_toURL (args[0]),		/* URL/file 	*/
		vos_optArg (args[1]), 		/* imgId	*/
		vos_optArg (args[2]));		/* name		*/
	sprintf (sess_cmd, "loadImage %s %s", 
	    vos_rewriteURL(vos_toURL (args[0])), vos_optArg (args[1]));
	if (session && input_sock != session_sock)
	    vos_sendCmd (session_sock, sess_cmd);

    } else if (MATCH ("loadFITS")) {		/* table.load.fits 	      */
	if ((stat = vos_invalidArg (numargs, args, 1, 1))) {
	    return_stat = ERR;
	    goto cmd_exit_;
	}
        stat = samp_tableLoadFITS (sampH, to, 
		vos_toURL (args[0]),		/* URL/file 	*/
		vos_optArg (args[1]), 		/* tblId	*/
		vos_optArg (args[2]));		/* name		*/
	sprintf (sess_cmd, "loadFITS %s %s", 
	    vos_rewriteURL(vos_toURL (args[0])), vos_optArg (args[1]));
	if (session && input_sock != session_sock)
	    vos_sendCmd (session_sock, sess_cmd);


    } else if (MATCH ("loadVOTable")) {		/* table.load.votable 	      */
	if ((stat = vos_invalidArg (numargs, args, 1, 1))) {
	    return_stat = ERR;
	    goto cmd_exit_;
	}
        stat = samp_tableLoadVOTable (sampH, to, 
		vos_toURL (args[0]), 		/* URL/file 	*/
		vos_optArg (args[1]), 		/* tblId	*/
		vos_optArg (args[2]));		/* name		*/
	sprintf (sess_cmd, "loadVOTable %s %s", 
	    vos_rewriteURL(vos_toURL (args[0])), vos_optArg (args[1]));
	if (session && input_sock != session_sock)
	    vos_sendCmd (session_sock, sess_cmd);

    } else if (MATCH ("load")) {
	char   fname[SZ_LINE];
	extern int vot_fileType();

	memset (fname, 0, SZ_LINE);

	if (strncmp (args[0], "http://", 7) == 0) {
            strcpy (fname, "/tmp/vostmp");    /* temp download name    */
	    if (access (fname, F_OK) == 0)
		unlink (fname);
            if (vos_getURL (args[0], fname) <= 0)
                fprintf (stderr, "Error accessing url '%s'\n", args[0]);

	} else if (strncmp (args[0], "file://", 7) == 0) {
	    strcpy (fname, &args[0][7]);
	} else 
	    strcpy (fname, args[0]);

	if (access (fname, F_OK) == 0) {
	    switch (vot_fileType (fname)) {
	    case VOT_FITS:
                stat = samp_imageLoadFITS (sampH, to, 
		    vos_toURL (fname),		/* URL/file 	*/
		    vos_optArg (args[1]), 	/* imgId	*/
		    vos_optArg (args[2]));	/* name		*/
		sprintf (sess_cmd, "loadImage %s %s", 
		    vos_rewriteURL(vos_toURL (fname)), vos_optArg (args[1]));
		if (session && input_sock != session_sock)
		    vos_sendCmd (session_sock, sess_cmd);
	        break;
	    case VOT_VOTABLE:
                stat = samp_tableLoadVOTable (sampH, to, 
		    vos_toURL (fname), 		/* URL/file 	*/
		    vos_optArg (args[1]), 	/* tblId	*/
		    vos_optArg (args[2]));	/* name		*/
		sprintf (sess_cmd, "loadVOTable %s %s", 
		    vos_rewriteURL(vos_toURL (fname)), vos_optArg (args[1]));
		if (session && input_sock != session_sock)
		    vos_sendCmd (session_sock, sess_cmd);
	        break;
	    case VOT_FITS_SPEC:
	        break;
	    case VOT_VOTABLE_SPEC:
	        break;
	    default:
	        if (!use_ipc)
	            fprintf (stderr,
			"Error: cannot determine file type of '%s'.\n",
		        args[0]);
	    }
	} else
	    fprintf (stderr, "Error: cannot access '%s'.\n", fname);



    /********************************************
     ***        Resource Message Types        ***
     ********************************************/
    } else if (MATCH ("loadResource")) {	/* voresource.loadlist        */
	if ((stat = vos_invalidArg (numargs, args, 1, 1))) {
	    return_stat = ERR;
	    goto cmd_exit_;
	}
	/*  NYI  */



    /********************************************
     ***         Utility Message Types        ***
     ********************************************/
    } else if (MATCH ("showRow")) {		/* table.highlight.row        */
		/* showRow <url> <row> [<id>]	*/
        stat = samp_tableHighlightRow (sampH, to, 
		vos_optArg (args[2]),		/* table-id 	*/
		vos_toURL (args[0]),		/* URL/file 	*/
		vot_atoi(args[1]));		/* row 		*/
	
    } else if (MATCH ("selectRows")) {		/* table.select.rowList       */
	int  nrows = 0;
	int *rows = vos_toIntArray (args[2], &nrows);

	stat = samp_tableSelectRowList (sampH, to, 
		vos_optArg (args[0]),		/* table-id	*/
                vos_toURL (args[1]),		/* URL/file	*/
		rows,				/* rows[]	*/
		nrows);				/* row 		*/

    } else if (MATCH ("pointAt")) {		/* coord.pointAt.sky          */
        stat = samp_coordPointAtSky (sampH, to, 
		vot_atof (args[0]), 		/* RA		*/
		vot_atof (args[1]));		/* Dec		*/


    /********************************************
     ***     Spectrum Load Message Types      ***
     ********************************************/
    } else if (MATCH ("loadSpec")) {		/* spectrum.load.ssa-generic  */
	/*  FIXME -- meta map not implemented */
	stat = samp_specLoadSSAGeneric (sampH, to, 
		vos_toURL (args[0]),		/* URL		*/
		0, 				/* Map meta	*/  /* NYI */
		vos_optArg (args[2]), 		/* spectrumId	*/
		vos_optArg (args[3]));		/* name		*/


    /********************************************
     ***         Bibcode Message Types        ***
     ********************************************/
    } else if (MATCH ("bibcode")) {		/* bibcode.load	              */
        stat = samp_bibLoad (sampH, to, args[1]);


    /********************************************
     ***         VO/IRAF Message Types        ***
     ********************************************/
    } else if (MATCH ("exec")) {		/* client.cmd.exec    	      */
        samp_cmdExec (sampH, to, args[0]);

    } else if (MATCH ("getenv")) {		/* client.env.get     	      */
        char *v = samp_envGet (sampH, to, 
		args[0]); 			/* name		*/
	if (use_ipc) {
	    reslen = strlen (v);
	    result = strdup (v);
	} else
	    printf ("%s\n", v);
  	if (v) free ((void *) v);

    } else if (MATCH ("setenv")) {		/* client.env.set     	      */
        stat = samp_envSet (sampH, to, 
		args[0], 			/* name		*/
		args[1]);			/* value	*/

    } else if (MATCH ("getparam")) {		/* client.param.get   	      */
        char *v = samp_paramGet (sampH, to, 
		args[0]); 			/* name		*/
	if (use_ipc) {
	    reslen = strlen (v);
	    result = strdup (v);
	} else
	    printf ("%s\n", v);
  	if (v) free ((void *) v);

    } else if (MATCH ("setparam")) {		/* client.param.set           */
        stat = samp_paramSet (sampH, to, 
		args[0], 			/* name		*/
		args[1]);			/* value	*/


    /********************************************
     ***            Unknown Command           ***
     ********************************************/
    } else {
	if (use_ipc) {
	    memset (buf, 0, SZ_BUF);
	    sprintf (buf, "Error: unknown command '%s'\n", cmd);
	    result = strdup (buf);
	    reslen = strlen (buf);
	} else
	    fprintf (stderr, "Error: unknown command '%s'\n", cmd);
    }


    if (!quiet) {
	if (!reslen) {
	    result = strdup ((stat < 0 ? "Error" : "OK"));
	    reslen = strlen (result);
	    return_stat = (stat < 0 ? ERR : OK);
        }
    }
    vosDebug ("procCmd: reslen=%d  result='%s'\n", reslen, result);
    

    /*  Return result to the remote caller.
     */
cmd_exit_:
    if (ipc_sock > 0) {
	int  nwrite;
	short slen = reslen;

        if (vos_sockWrite (ipc_sock, &slen, sizeof (short)))
	    nwrite = vos_sockWrite (ipc_sock, result, slen);
    }
	    
    if (result)  free (result);
}


/**
 *  VOS_INVALIDARG -- Check arguments for validity.
 */
static int
vos_invalidArg (int argc, char *argv[], int nargs, int exist)
{
    int  stat = 0;

    /*  See if the number of args we have is less than what we require.
     */
    if (argc < nargs) {
	fprintf (stderr, "Error: invalid number of args for command '%s'\n",
	    cmd);
	return ((stat = -1));
    }

    /*  See whether we require the existance of the first argument.  If this
     *  is a local file it must exist, assume URLs are valid.
     */
    if (exist) {
        if (!argv[0] || !argv[0][0])
	    return ((stat = -1));
	if (strncmp ("http", argv[0], 4) && access (argv[0], R_OK)) {
	    fprintf (stderr, "Error: cannot access file '%s'\n", args[0]);
	    return ((stat = -1));
	}
    }

    return (stat);
}


/**
 *  VOS_CMDHELP -- Print a command help summary for interactive mode.
 */
static void
vos_cmdHelp (char *cmd)
{
    if (MATCH ("status")) {
	printf ("status\n");
    } else if (MATCH ("handle")) {
	;
    } else if (MATCH ("snoop")) {
	printf ("snoop\n");
    } else if (MATCH ("send")) {
	;
    } else if (MATCH ("start")) {
	printf ("start\n");
    } else if (MATCH ("stop")) {
	printf ("stop\n");
    } else if (MATCH ("echo")) {
	printf ("echo <text_string>\n");
    } else if (MATCH ("help")) {
	printf ("help <cmd>\n");
    } else if (MATCH ("quit")) {
	printf ("quit\n");
    } else if (MATCH ("trace")) {
	printf ("trace\n");
    } else if (MATCH ("listClients")) {
	printf ("listClients\n");
    } else if (MATCH ("ping")) {
	printf ("ping <appName>\n");
    } else if (MATCH ("access")) {
	printf ("access <appName>\n");

    } else if (MATCH ("loadImage") || MATCH ("loadFITS")) {
	printf ("loadImage <file | URL> [<table-id> [<name>]]\n");
    } else if (MATCH ("loadVOTable")) {
	printf ("loadVOTable <file | URL> [<table-id> [<name>]]\n");
    } else if (MATCH ("loadResource")) {
	;
    } else if (MATCH ("showRow")) {
	printf ("showRow <table-id> <file | URL> <row>\n");
    } else if (MATCH ("selectRows")) {
	printf ("selectRows <table-id> <file | URL> <rowList>\n");

    } else if (MATCH ("pointAt")) {
	printf ("pointAt <ra> <dec>\t# coords in decimal degrees\n");

    } else if (MATCH ("loadSpec")) {
	printf ("loadSpec <URL> <meta> [<spectrum-id> <name>]\n");
    } else if (MATCH ("bibcode")) {
	printf ("bibcode <bibcode>\n");

    } else if (MATCH ("exec")) {
	printf ("exec <cmd_str>\n");
    } else if (MATCH ("getenv")) {
	printf ("getenv <name>\n");
    } else if (MATCH ("setenv")) {
	printf ("setenv <name> <value>\n");
    } else if (MATCH ("getparam")) {
	printf ("getparam <name>\n");
    } else if (MATCH ("setparam")) {
	printf ("setparam <name> <value>\n");
    }
}


/**
 *  VOSDEBUG -- Utility to print debug messages.
 */
static void
vosDebug (char *format, ...)
{
    va_list  argp;
    char buf[1024];
    extern char *vo_encodeString ();


    if (!debug)
	return;

    /*  Format the message.
     */
    memset (buf, 0, 1024);
    va_start (argp,  format);
    vo_encodeString (buf, format, &argp);
    va_end (argp);

    if (buf[strlen(buf)-1] != '\n') 	/*  ensure a newline  	*/
        strcat (buf, "\n");

    fprintf (stderr, "[%d]  %s", (int)getpid(), buf);
}



/**
 *  VOS_OPENSESSION -- Open a connection to the VOSAMP Session Manager.
 *
 *  @brief      Open a connection to the VOSAMP Session Manager.
 *  @fn         int vos_openSession (char *host, int port, char *session_name)
 *
 *  @param  host        session manager host
 *  @param  port        session manager connection port
 *  @param  name        session name
 *  @return             fd to the session socket
 */
int
vos_openSession (char *host, int port, char *session_name)
{
    int   sock = 0, cb_sock = 0, cb_port = 0, nr, nw, len, ready = SESS_READY;
    char  cmd[SZ_LINE];


    /*  Open a socket to the connection port.
     */
    if ((sock = vos_openClientSocket (host, port, 1)) <= 0)
	return (1);

    /*  Read the callback port number.
     */
    if ((nr = vos_sockRead (sock, &cb_port, sizeof (short))) != sizeof (short))
	return (1);

    /*  Close the socket to the connection port.
     */
    close (sock); 		

    /*  Open a new connection to the callback port.
     */
    if ((cb_sock = vos_openClientSocket (host, cb_port, 1)) <= 0)
	return (1);

    /*  Write the 'ready' code to the callback prot.
     */
    nw = vos_sockWrite (cb_sock, &ready, sizeof (short));


    /*  Send the 'connect' message to connect to the named session.  The
     *  server will either create a new session or have us join an existing
     *  session created by another node.
     */
    memset (cmd, 0, SZ_LINE);
    sprintf (cmd, "connect %s", session_name);
    len = strlen (cmd);
    if (vos_sockWriteHdr (cb_sock, len, NULL, SAMP_CMD, SAMP_NOTIFY, "smgr"))
        nw = vos_sockWrite (cb_sock, cmd, len);

    /*  Return 0 if we can connect, 1 on err, or the callback descriptor.
     */
    return (cb_sock);
}


/**
 *  VOS_CLOSESESSION -- Close the specified session connection.
 *
 *  @brief      Close the specified session connection.
 *  @fn         int vos_closeSession (int sock)
 *
 *  @param  sock        session socket fd
 *  @return             0 if cmd was sent, 1 otherwise
 */
int
vos_closeSession (int sock)
{
    if (vos_sockWriteHdr (sock, 0, NULL, SAMP_QUIT, SAMP_NOTIFY, "smgr"))
	return (1);
    return (0);
}

