/**
 *  VOSESSION -- Inter-Desktop Session Manager for the VOSAMP task.
 *
 *  Usage:
 *
 *	vosession [<opts>]
 *
 *  Where	
 *  	-%,--test			run unit tests
 *  	-h,--help			print help summary
 *  	-d,--debug			debug output
 *  	-v,--verbose			verbose output
 *
 *  Subcommands:
 *
 *    status 				    print Hub availability
 *    list 				    list all registered clients
 *
 *
 *  @file       vosession.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/12
 *
 *  @brief      Session Manager for the VOSAMP task.
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


#include "samp.h"			/* LIBSAMP interface	        */
#include "voApps.h"			/* voApps interface	        */


#define	SZ_BUF		128
#define	SZ_MTYPE	 64
#define	SZ_HOSTIP	 16

#define	MAX_ARGS 	  8		/* Max args in a command	*/
#define MAX_SESSIONS     64             /* Max sessions to manage       */
#define MAX_PORTS       128             /* Max ports to manage          */

#define	SESS_SELWIDTH	 32		/* select fd width		*/
#define	SESS_TIMEOUT   3600		/* 1-hr timeout 	        */
#define	SESS_DISCONNECT 309             /* Session Mgr command timeout  */

#define MATCH(s)        (strcasecmp(cmd,s)==0)


/*  Connected session client.
 */
typedef struct {
    int    sock;                        /* socket descriptor            */
    int    port;                        /* connection port              */
    char   hostIP[SZ_HOSTIP];           /* host IP address              */
    char   session[SZ_LINE];            /* session name                 */

    void   *back;			/* linked-list back ptr		*/
    void   *next;			/* linked-list next ptr		*/
} Node, *NodeP;


/*  Session structure.
 */
typedef struct {
    int     nclients;                   /* number of clients            */
    Node   *clients;       		/* clients in session           */
    char    name[SZ_LINE];         	/* session name			*/
    char    dataCache[SZ_LINE];         /* path to local data cache     */

    int     ncmds;			/* Number of commands sent	*/
    int     nfiles;			/* Number of files uploaded	*/
    long    nbytes;			/* Number of files uploaded	*/
    time_t  start_time;         	/* session start time		*/
    time_t  end_time;         		/* session end time		*/
    time_t  last_cmd;         		/* session last command		*/

    void   *back;			/* linked-list back ptr		*/
    void   *next;			/* linked-list next ptr		*/
} Session, *SessionP;


static int  nSessions	  	= 0;
static Session* sessHead  	= NULL;
static Session* sessTail  	= NULL;

static int   nClients	  	= 0;
static Node* clientHead		= NULL;
static Node* clientTail  	= NULL;

static int  verbose	= FALSE;	/* verbose output		*/
static int  debug	= FALSE;	/* verbose output		*/
static int  interactive	= FALSE;	/* interactive mode		*/
static int  xml_trace 	= FALSE;	/* trace XML_RPC		*/
static int  timeout 	= SESS_TIMEOUT;	/* select() doesn't timeout	*/
static int  svr_port 	= SESS_DEFPORT;	/* connection port		*/
static int  selwidth	= SESS_SELWIDTH;/* select() width		*/

static int  keep_alive  = TRUE;		/* lingering connection		*/
static int  svr_sock 	= FALSE;	/* server socket descriptor	*/
static int  cb_sock 	= 0;		/* callback socket descriptor	*/
static int  cb_port 	= 0;		/* callback port number		*/
static int  socks[MAX_PORTS];		/* available sockets		*/


static char *to		= NULL;		/* message recipient		*/
static char *session	= NULL;		/* session name			*/
static char  cmd[SZ_CMD];		/* command name			*/
static char *args[MAX_ARGS];		/* command args buffer  	*/
static char  data_file[SZ_URL];		/* uploaded data file		*/


static fd_set allset, rfds;
static struct timeval tm;


#define NOAO
#ifdef NOAO
static	char *sessionData	= "/iraf/web/vosession";
static	char *sessionUrlBase	= "http://iraf.noao.edu/vosession";
#else
static	char *sessionData	= "/tmp/vosession";
static	char *sessionUrlBase	= "file:///tmp/vosession";
#endif
static char  logfile[SZ_LINE];
static char  statfile[SZ_LINE];



/*  Utility socket routines.
 */
extern int   vos_openServerSocket (int port);
extern int   vos_openClientSocket (char *host, int port, int retry);
extern int   vos_sockRead (int fd, void *vptr, int nbytes);
extern int   vos_sockWrite (int fd, void *vptr, int nbytes);
extern void  vos_setNonBlock (int sock);
extern int   vos_sockWriteHdr (int fd, int len, char *name, int type, 
				int mode, char *to);
extern int   vos_sockReadHdr (int fd, int *len, char *name, int *type, 
				int *mode);
extern int   vos_recvFile (int sock, int size, char *fname);
extern int   vot_atoi (char *v);
extern char *vo_logtime (void);
extern char *vos_typeName (int type);



/*  Task specific option declarations.
 */
static char  *opts      = "h%:dikp:tuvS:T:";
static struct option long_opts[] = {
        { "help",         2, 0,   'h'},         /* required             */
        { "test",         1, 0,   '%'},         /* required             */
        { "debug",        2, 0,   'd'},         /* debug		*/
        { "interactive",  2, 0,   'i'},         /* interactive mode	*/
        { "keepalive",    2, 0,   'k'},         /* keep connection 	*/
        { "port",         1, 0,   'p'},         /* server port		*/
        { "trace",        2, 0,   't'},         /* trace cmds		*/
        { "url",          2, 0,   'u'},         /* upload data base URL */
        { "verbose",      2, 0,   'v'},         /* verbose		*/
        { "session",      1, 0,   'S'},         /* session name		*/
        { "timeout",      1, 0,   'T'},         /* connection timeout  	*/
        { NULL,           0, 0,    0 }
};

static void Usage (void);
static void Tests (char *input);

		    
static int   sess_procCmd (int sock, int msgtype, int nbytes, char *fname);
static int   sess_forwardMessage (int sender, char *msg);
static int   sess_connectClient (int sock);
static int   sess_disconnectClient (int sock);
static int   sess_disconnectAllClients (void);
static int   sess_joinSession (int sock, char *session_name);
static int   sess_leaveSession (int sock, char *session_name);

static Session *sess_newSession (char *name);
static int   sess_freeSession (Session *s);

static Node *sess_newNode (int sock);
static int   sess_freeNode (Node *n);

static Session *sess_byName (char *name);
static Session *sess_bySock (int sock);
static Node *sess_clientBySock (int sock);

#ifdef SESSION_EXTRAS
static Session *sess_byPort (int port);
static Node *sess_clientByPort (int port);
static int   sess_portToSock (int port);
#endif

static int   sess_newPort (void);
static int   sess_freePort (int port);
static int   sess_sockToPort (int sock);

static char *sess_tok (char *str, int tok);
static void  sess_rewriteCmd (char *line, char *session, char *fname);
static void  sessLog (char *formtat, ...);
static void  sess_writeStats (Session *session);
static void  sess_printSessions (void);
static void  sess_printClients (void);




/****************************************************************************
 *  Program entry point.
 ***************************************************************************/
int
main (int argc, char **argv)
{
    char  **pargv, optval[SZ_FNAME], ch;
    int	    i, rc, pos = 0;



    /*  Initialize.
     */
    memset (logfile, 0, SZ_LINE);
    memset (data_file, 0, SZ_URL);
    svr_port = SESS_DEFPORT;
    for (i=0; i < MAX_ARGS; i++) 
	args[i] = calloc (1, SZ_LINE);


    /*  Parse the argument list.
     */
    pargv = vo_paramInit (argc, argv, opts, long_opts);
    i = 0;
    while ((ch = vo_paramNext (opts,long_opts,argc,pargv,optval,&pos)) != 0) {
	i++;
        if (ch > 0) {
            switch (ch) {
            case '%':  Tests (optval);                  return (OK);
            case 'h':  Usage ();                        return (OK);

            case 'd':  debug++;   			break;
            case 'v':  verbose++;			break;
            case 'k':  keep_alive=0;			break;

            case 'p':  svr_port = vot_atoi(optval);	break;
            case 't':  xml_trace=1;			break;

            case 'S':  session  = strdup (optval);  	break;
            case 'T':  timeout 	= vot_atoi (optval);	break;

            default:
                fprintf (stderr, "Invalid option '%s'\n", optval);
                return (1);
            }

        } else if (ch == PARG_ERR) {
            return (ERR);

        } else {
	    /*  There is no other cmdline arg.
	     */
            fprintf (stderr, "Invalid option '%s'\n", argv[i]);
            return (1);
        }
    }


    /*  Sanity checks.
     */
    if (to == NULL)
	to = strdup ("all");

    /*  Create the working session directory.
     */
    if (access (sessionData, F_OK) < 0) {
	if (mkdir (sessionData, 0755) < 0) {
	    fprintf (stderr, "Cannot create session directory '%s'\n",
		sessionData);
	    return (1);
	}
    }
    sprintf (logfile, "%s/Log", sessionData);
    sprintf (statfile, "%s/Stats", sessionData);


    /*  Initialize the socket interface and timeouts.
    */
    tm.tv_sec  = timeout;
    tm.tv_usec = 0;

    if ((svr_sock = vos_openServerSocket (svr_port)) < 0) {
        sessLog ("Cannot open server connection port %d\n", svr_port);
	return (1);
    } else if (debug) {
	sessLog ("=======================================\n");
	sessLog ("VOSession server started on port %4d\n", svr_port);
	sessLog ("=======================================\n");
    }

    FD_ZERO (&allset);
    FD_SET (svr_sock, &allset);
    vos_setNonBlock (svr_sock);
    if (interactive) {
        FD_SET (fileno(stdin), &allset);
	vos_setNonBlock (fileno(stdin));
    }


    /*  Begin processing.
     */
    while (1) {
        /*  Initialize the input file descriptor set.
         */
	memcpy (&rfds, &allset, sizeof(allset));
        rc = select (selwidth, &rfds, NULL, NULL, &tm);
        if (rc == 0) {
            time_t now = time (NULL);
            Session *s = (Session *) NULL;
            Node *n = (Node *) NULL;

            /*  Send a disconnect to all subscribed clients that have been
             *  inactive for more than SESS_DISCONNECT seconds (1-hr);
             */
            for (s=sessHead; s; s=s->next) {
                if ((now - s->last_cmd) >= SESS_DISCONNECT) {
                    sessLog ("Session timeout for '%s'\n", s->name);
		    for (n=s->clients; n; n=n->next) {
			sess_disconnectClient (n->sock);
			sess_leaveSession (n->sock, s->name);
		    }
                }
            }
            continue;
        }

	/*  Loop over the active descriptors to process input.
	 */
	for (i=0; i < selwidth+1; i++) {
	    if (FD_ISSET(i, &rfds)) {
		if (i == svr_sock) {
                    /* Accept a new connection on the port.
                     */
		    if (sess_connectClient (svr_sock) != 0)
                        sessLog ("Cannot connect Client: ", strerror(errno));

	        } else {
		    /*  Handle input from existing client-node connection.
		     */
		    int  nread=0, nbytes=0, type=0, mode=0;
		    char fname[SZ_FNAME];

		    memset (fname, 0, SZ_FNAME);
		    if (debug > 1) sessLog ("Input on fd %d ....\n", i);

		    /*  Read the length of the message and type.
		     */
           	    nread = vos_sockReadHdr (i, &nbytes, fname, &type, &mode);
		    if (nread <= 0) {
			/*  Error on the socket or client closed connection. 
			 */
			sess_disconnectClient (i);
		    } else {
			/*  Read the command.
			 */
			sess_procCmd (i, type, nbytes, fname);
		    }
	        }  /* end is server socket */
	    }  /* end FD_ISSET */
	}  /* end for */
    }  /* end while */


    /*  Clean up.  Note, we should never actually get here....
     */
    if (to)          free ((void *) to);
    if (session)     free ((void *) session);
    sess_disconnectAllClients ();

    /*  Close the the connection socket.
     */
    if (svr_sock)
	close (svr_sock);

    return (OK);
}




/***************************************************************************
****  		     Client Handling Procedures			  	****
***************************************************************************/

/**
 *  SESS_PROCCMD -- Process a command message.
 */
static int
sess_procCmd (int sock, int msgtype, int nbytes, char *fname)
{

    int   i, len, nsent, type=0, mode=0, nread=0;
    char  line[SZ_LINE], path[SZ_LINE];
    Session *s = (Session *) sess_bySock (sock);
    

    memset (line, 0, SZ_LINE);		/* initialize		*/
    memset (path, 0, SZ_LINE);

    /*  Read the command header.  This will either be a SAMP_DATA
     *  message in which case we get a filename, or SAMP_CMD where 
     *  we ignore the fname value and then read the command string.
     */
    switch (msgtype) {
    case SAMP_DATA:
	sprintf (path, "%s/%s", s->dataCache, fname);

        if (vos_recvFile (sock, nbytes, path) != OK) {
            if (debug)
                sessLog ("Error receiving file '%s'\n", fname);
        }
        strcpy (data_file, fname);

        vos_sockReadHdr (sock, &nbytes, fname, &type, &mode);
        nread = vos_sockRead (sock, line, nbytes);
	if (debug) sessLog ("SAMP_DATA: '%s' '%s'\n", line, fname);
        sess_rewriteCmd (line, s->name, data_file);
        memset (data_file, 0, SZ_FNAME);

	s->nfiles++;
	s->nbytes += nbytes;

	/*  Forward to other connected clients.
	 */
	s->ncmds++;
	s->last_cmd = time (NULL);
	nsent = sess_forwardMessage (sock, line);
	if (debug)
	    sessLog ("Forwarded to %d clients in '%s'\n", nsent, s->name);

        break;

    case SAMP_CMD:
    case SAMP_RELAY:
        nread = vos_sockRead (sock, line, nbytes);

        if (debug)
	    sessLog ("%s[%d][%s]: '%s'\n", vos_typeName (msgtype), nread,
		(s ? s->name : ""), line);
        if (s && line[0] && data_file[0]) {
            sess_rewriteCmd (line, s->name, data_file);
            memset (data_file, 0, SZ_FNAME);
        }

	if (strncasecmp (line, "quit", 4) == 0) {		/*  QUIT      */
	    sess_disconnectClient (sock);

	} else if (strncasecmp (line, "list", 4) == 0) {	/*  LIST      */
	    Node *n = (Node *) NULL;
	    char  buf[(4*SZ_LINE)], line[SZ_LINE];

	    memset (buf, 0, (4 * SZ_LINE));
	    sprintf (buf, "Clients in session '%s':\n", s->name);
	    for (i=0,n=clientHead; n; n=n->next,i++) {
	        memset (line, 0, SZ_LINE);
		sprintf (line, "    [%d]: host: %16.16s  port: %d\n",
		    i, n->hostIP, n->port);
		strcat (buf, line);
	    }

	    /*  Write the result string back to the client.
	     */
	    len = strlen (buf);
            if (vos_sockWriteHdr (sock, len, NULL, SAMP_RESULT, 
		SAMP_NOTIFY, "all"))
            	    nread = vos_sockWrite (sock, buf, strlen (buf));

	} else if (strncasecmp (line, "connect", 4) == 0) {	/*  JOIN      */
	    sess_joinSession (sock,  sess_tok (line, 2));

	} else if (strncasecmp (line, "leave", 4) == 0) {	/*  LEAVE     */
	    sess_leaveSession (sock, s->name);

	} else {						/*  ELSE .... */
	    if (line[0] && msgtype == SAMP_CMD) {
	        nsent = sess_forwardMessage (sock, line);
	        if (debug)
		    sessLog ("Forward to %d clients in '%s'\n", nsent, s->name);
	    }
	}
	if (s) {
	    s->ncmds++;
	    s->last_cmd = time (NULL);
	}
        break;

    case SAMP_RESULT:
	if (debug) 
	    sessLog ("%s[%s]: '%s'\n", vos_typeName (msgtype), 
		(s ? s->name : ""), " ");
        break; /*  Not yet implemented  */

    case SAMP_QUIT:
	sess_leaveSession (sock, NULL);
	sess_disconnectClient (sock);
        break;
		
    default:
	sessLog ("cmd: '%s'\n", cmd);
    }

    return (0);
}

	    
/**
 *  SESS_FORWARDMESSAGE -- Forward the message to all clients in the session
 *  other than the one which originated it.
 */
static int
sess_forwardMessage (int sender, char *msg)
{
    Session *s = sess_bySock (sender);
    Node *n = (Node *) NULL;
    int   len, nw, sock, nsent = 0;


    for (n=s->clients; n; n=n->next) {
	sock = n->sock;
	if (sock != sender) {
	    if (debug) fprintf (stderr, "forward to '%s'\n", n->hostIP);
            len = strlen (msg);
            if (vos_sockWriteHdr (sock, len, NULL, SAMP_RELAY, 0, "all"))
                nw = vos_sockWrite (sock, msg, len);
	    nsent++;
	}
    }

    return (nsent);
}


/**
 *  SESS_CONNECTCLIENT -- Connect a client on the named socket.
 */
static int
sess_connectClient (int sock) 
{
    int     new, nr;
    short   ready, sport;
    struct  sockaddr_in client;
    size_t  size = sizeof (client);
    char  sname[SZ_LINE];


    memset (sname, 0, SZ_LINE);

    if ((new = accept (sock, (struct sockaddr *) &client, 
	(socklen_t *) &size)) < 0) {
             sessLog ("svr_sock accept() errno %d: %s", errno, strerror(errno));

     } else {
	char *host = inet_ntoa (client.sin_addr);
	int   port = ntohs (client.sin_port);
	Node *node = (Node *) NULL;


	/*  Attach the client to the list of connected hosts.
	 */
	node = sess_newNode (new);
	node->port = port;
	node->sock = new;
	strcpy (node->hostIP, host);
	if (clientHead) {
	   clientTail->next = node;
	   clientTail = node;
	} else {
	   clientHead = clientTail = node;
	}
	nClients++;
	
	/*  Open new server connection and send the callback
	 *  port number over the connection.  After that, we
	 *  don't need the initial connection so release it.
	 */
	cb_port = sport = sess_newPort ();
	cb_sock = vos_openServerSocket (cb_port);
	if (debug)
	    sessLog ("Connect from host:%s, port:%d, fd:%d, cb_port: %d\n",
		host, port, new, cb_port);
	vos_sockWrite (new, &sport, sizeof(short));
	cb_port = sport;
	close (new);

	/*  Waiting for callback to connect.
	 */
        if ((new = accept (cb_sock, NULL, NULL)) < 0)
            sessLog ("cb_sock accept() errno %d: %s", errno, strerror(errno));
	vos_setNonBlock (new);

	/*  Wait for the callback ready message so we know
	 *  we are connected to the client.
	 */
	nr = vos_sockRead (new, &ready, sizeof(short));
	if (ready == SESS_READY) {
#ifdef READ_CONNECT
	    int  nbytes=0, type=0, mode=0;

	    /*  Read the session name.
	     */
            vos_sockReadHdr (new, &nbytes, NULL, &type, &mode);
            nr = vos_sockRead (new, sname, nbytes);

	    if (verbose)
	        sessLog ("Connect host %s:%d\n",
                    inet_ntoa(client.sin_addr), ntohs(client.sin_port), sname);
	    strcpy (node->session, sname);
#else
	    if (verbose)
	        sessLog ("Connect host %s:%d\n", 
                    inet_ntoa (client.sin_addr), ntohs (client.sin_port));
#endif
	}


	/*  Add the client socket fd to the master set of
	 *  active descriptors.
	 */
    	FD_SET (new, &allset);
    }

    return (0);
}


/**
 *  SESS_DISCONNECTCLIENT -- Shutdown client on the named socket.
 */
static int
sess_disconnectClient (int sock) 
{
    Node *client = sess_clientBySock (sock);

    sessLog ("Disconnecting client %d....\n", sock);
    FD_CLR (sock, &allset);
    close (sock);

    sess_freeNode (client);
    sess_freePort (client->port);

    return (0);
}


/**
 *  SESS_DISCONNECTALLCLIENTS -- Shutdown all connected clients   
 */
static int
sess_disconnectAllClients ()
{
    Session *s = (Session *) NULL;
    Node *n = (Node *) NULL;

    /*  Loop over all the session and their client lists and disconnect
     *  the client.  We do it here rather than following the client list
     *  since we'll also clean up the data cache when the session is closed.
     */
    for (s=sessHead; s; s=s->next) {
        for (n=s->clients; n; n=n->next) {
	    if (debug)
		sessLog ("Disconnecting host %s on port %d from '%s'\n",
		    n->hostIP, n->port, s->name);

	    sess_leaveSession (n->sock, s->name);
        }
    }
    return (0);
}




/***************************************************************************
****  			  Session Procedures			  	****
***************************************************************************/

/**
 *  SESS_JOINSESSION -- Join (or create) a session.
 */
static int   
sess_joinSession (int sock, char *session_name)
{
    Session *s = (Session *) NULL;
    Node    *n = (Node *) NULL;
    int      found = 0;
  

    if (sessHead) {
	s = sess_byName (session_name);
	if (s == (Session *) NULL) {
	    /*  Name not found, create a new session pointer.
	     */
	    s = sess_newSession (session_name);
	    sessTail->next = s;
	    sessTail = s;
            nSessions++;
	}

    } else {
	/*  No active sessions, create one.
	 */
	s = sessHead = sessTail = sess_newSession (session_name);

        /*  Create the working session directory.
	 */
	sprintf (s->dataCache, "%s/%s", sessionData, session_name);
        if (access (s->dataCache, F_OK) < 0) {
	    if (debug) sessLog ("Making session cache directory '%s'\n",
		s->dataCache);
	    if (mkdir (s->dataCache, 0755) < 0) {
	        fprintf (stderr, "Cannot create session cache directory '%s'\n",
		    s->dataCache);
	        return (1);
	    }
        }
        nSessions++;
    }

    /*  Now attach the node the session list of clients, both the master
     *  client list and the client list in each session.
     */
    for (n=clientHead; n; n=n->next) {	/* master client list		*/
	if (sock == n->sock)
	    strcpy (n->session, session_name);
    }
    found = 0;
    for (n=s->clients; n && n->next; n=n->next) {
	if (n->sock == sock) {		/* skip to end of client list	*/
	    /*  client is already in session. 
	     */
	    found++;
	}
    }

    /*  Add to client list if it isn't already found.
     */
    if (!found) {
        if (n) {
            n->next = sess_newNode (sock);
	    strcpy (n->session, session_name);
        } else {
            s->clients = sess_newNode (sock);
	    strcpy (s->clients->session, session_name);
        }
        s->nclients++;
    }

    if (debug > 1) { sess_printSessions (); sess_printClients (); }
    return (0);
}


/**
 *  SESS_LEAVESESSION -- Leave an open session.
 */
static int   
sess_leaveSession (int sock, char *session_name)
{
    Session *s = (Session *) NULL;
    int   port = sess_sockToPort (sock);


    s = (session_name ? sess_byName (session_name) : sess_bySock (sock));
    if (s) {
	Node *n = s->clients;

	while (n && n->port != port)
	    n = n->next;
    
	if (n) { 
            sessLog ("%s leaves session '%s'\n", n->hostIP, s->name);
	    sess_freeNode (n);
	    s->nclients--;
	}

	if (s->nclients == 0) {
            /*  Remove the working session directory and its contents.
	     */
            if (access (s->dataCache, F_OK) == 0) {
		char  cmd[SZ_LINE];

		memset (cmd, 0, SZ_LINE);
		sprintf (cmd, "/bin/rm -rf %s", s->dataCache);
	        if (debug) sessLog ("Removing session cache directory '%s'\n",
		    s->dataCache);

		system (cmd);		/* FIXME	*/
            }
	    sess_freeSession (s);
	}
    }

    if (debug > 1) sess_printSessions ();
    return (0);
}


/**
 *  SESS_BYNAME - Find a Session pointer by name.
 */
static Session *
sess_byName (char *name)
{
    Session *s = (Session *) NULL;

    for (s=sessHead; s; s=s->next)
	if (strcasecmp (name, s->name) == 0)
	    break;

    return (s);
}


/**
 *  SESS_BYSOCK - Find a Session socket descriptor.
 */
static Session *
sess_bySock (int sock)
{
    Session *s = (Session *) NULL;
    Node *n = (Node *) NULL;

    for (n=clientHead; n; n=n->next) {
	if (sock == n->sock)
	    return (sess_byName (n->session));
    }
    return (s);
}


/**
 *  SESS_CLIENTBYSOCK - Find a Node pointer by socket fd.
 */
static Node *
sess_clientBySock (int sock)
{
    Node *n = (Node *) NULL;

    for (n=clientHead; n; n=n->next) {
	if (n->sock == sock)
	    return (n);
    }

    return ((Node *) NULL);
}


/**
 *  SESS_SOCKTOPORT - Lookup a port number by the socket fd.
 */
static int 
sess_sockToPort (int sock)
{
    Node *n = (Node *) NULL;

    for (n=clientHead; n; n=n->next) {
	if (n->sock == sock)
	    return (n->port);
    }

    return (-1);
}


#ifdef SESSION_EXTRAS
/**
 *  SESS_CLIENTBYPORT - Find a Node pointer by port number.
 */
static Node *
sess_clientByPort (int port)
{
    Node *n = (Node *) NULL;

    for (n=clientHead; n; n=n->next) {
	if (n->port == port)
	    return (n);
    }

    return ((Node *) NULL);
}


/**
 *  SESS_BYPORT - Find a Session port number.
 */
static Session *
sess_byPort (int port)
{
    Session *s = (Session *) NULL;
    Node *n = (Node *) NULL;

    for (n=clientHead; n; n=n->next) {
	if (port == n->port)
	    return (sess_byName (n->session));
    }
    return (s);
}


/**
 *  SESS_PORTTOSOCK - Lookup a socket fd by the client port number.
 */
static int 
sess_portToSock (int port)
{
    Node *n = clientHead;

    for (n=clientHead; n; n=n->next)
	if (n->port == port)
	    return (n->sock);

    return (-1);
}
#endif


/**
 *  SESS_NEWSESSION - Create a new Session structure.
 */
static Session *
sess_newSession (char *name)
{
    Session *s = (Session *) calloc (1, sizeof (Session));

    strcpy (s->name, name);
    s->start_time = time (NULL);
    return (s);
}


/**
 *  SESS_FREESESSION - Release a Session structure.
 */
static int
sess_freeSession (Session *s)
{
    Session *back = (Session *) (s ? s->back : NULL);
    Session *next = (Session *) (s ? s->next : NULL);

    if (s == sessHead)
	sessHead = (Session *) next; 	/* head of the list 		*/
    else if (s->next == (Session *) NULL)
	sessHead = (Session *) back; 	/* tail of the list 		*/
    else
	back->next = (void *) s->next; 	/* middle of the list 		*/

    if (s->nclients == 0) {
	fprintf (stderr, "Free up session resources for '%s'\n", s->name);
    }
    nSessions--;
	    
    s->end_time = time (NULL);

    sess_writeStats (s);		/* log the session stats	*/

    free ((void *) s);
    if (debug > 1) sess_printSessions ();
    return (0);
}


/**
 *  SESS_NEWNODE - Create a new Node structure.
 */
static Node *
sess_newNode (int sock)
{
    Node *n = (Node *) calloc (1, sizeof (Node));
    Node *client = sess_clientBySock (sock);

    if (client) {
        n->sock = client->sock;
        n->port = client->port;
        strcpy (n->hostIP, client->hostIP);
    }

    return (n);
}


/**
 *  SESS_FREENODE - Free a Node structure.
 */
static int
sess_freeNode (Node *node)
{
    if (node->back) {
	Node *back = node->back;
	back->next = node->next;
    }
    if (node->next) {
	Node *next = node->next;
	next->back = node->back;
    }

    free ((void *) node);
    return (0);
}


/**
 *  SESS_PRINTSESSIONS - Utility to print the session list.
 */
static void
sess_printSessions (void)
{
    Session *s = sessHead;
    Node    *n = (Node *) NULL;
    int   i, j;

    if (!s) {
	fprintf (stderr, "No active sessions\n");

    } else {
	for (s=sessHead,i=0; s; s=s->next, i++) {
	    fprintf (stderr, "Session[%d]: session '%s' has %d client%c\n", 
		i, s->name, s->nclients, (s->nclients > 1 ? 's':' '));
	    for (n=sessHead->clients,j=0; n; n=n->next, j++) {
	        fprintf (stderr, "    Client[%d]: port: %d   host: %s\n", 
		    j, n->port, n->hostIP);
	    }
	}
    }
}


/**
 *  SESS_PRINTCLIENTS - Utility to print the client list.
 */
static void
sess_printClients (void)
{
    Node *n = (Node *) NULL;
    int   i;

    for (n=clientHead,i=0; n; n=n->next, i++) {
	fprintf (stderr, "Client[%d]: port:%d  sock:%d  host:%s  session:%s\n", 
	    i, n->port, n->sock, n->hostIP, n->session);
    }
}


/***************************************************************************
****  			   Utility Procedures			  	****
***************************************************************************/

/**
 *  SESS_WRITESTATS -- Log the sessions statistics.
 */
static void
sess_writeStats (Session *s)
{
    FILE   *fd;
    char    buf[SZ_LINE];
    double  nsec;


    if (s) {
        memset (buf, 0, SZ_LINE);
	nsec = (s->end_time - s->start_time);
        sprintf (buf, 
	    "%s %16.16s ncmds: %d  nfiles: %d  nbytes: %ld time: %6.1f min\n",
	    vo_logtime(), s->name, s->ncmds, s->nfiles, s->nbytes, (nsec/60.));

        if ((fd = fopen (statfile, "w+"))) {
	    if (debug)
	        sessLog ("%s", buf);
	    fprintf (fd, "%s", buf);
	    fflush (fd); fclose (fd);
        }
    } else
	sessLog ("Error: writeStats gets null session");
}


/**
 *  SESSLOG -- Print a message to the logfile.
 */
static void
sessLog (char *format, ...)
{
    FILE    *fd;
    va_list  argp;
    char *buf = calloc (1, 1024);
    char *tstr = NULL;
    int   len=0;
    extern char *vo_encodeString ();


    /*  Format the message.
     */
    va_start (argp,  format);
    vo_encodeString (buf, format, &argp);
    va_end (argp);

    len = strlen (buf);                 /* ensure a newline             */
    if (buf[len-1] != '\n')
        strcat (buf, "\n");


    /*  Log the message.
     */
    tstr = vo_logtime ();
    if (logfile[0] == '-' || strcasecmp (logfile, "stdout") == 0) {
	printf ("%s  %s", tstr, buf);

    } else {
        if ((fd = fopen (logfile, "w+"))) {
	    if (debug)
		fprintf (stderr, "%s  %s", tstr, buf);
	    fprintf (fd, "%s  %s", tstr, buf);
	    fflush (fd); fclose (fd);
        }
    }

    free ((void *) buf);
}


/**
 *  SESS_NEWPORT -- Get an open socket/port entry.
 */
static int
sess_newPort ()
{
    int  i, p;

    for (i=0; i < MAX_PORTS; i++) {
	p = svr_port + i + 3;
	if (socks[i] == 0) {
	    socks[i] = p;
	    return (p);
	}
    }

    sessLog ("Error: No available client ports.\n");
    return (-1);
}


/**
 *  SESS_FREEPORT -- Free a socket/port entry.
 */
static int
sess_freePort (int port)
{
    int  i, p;

    for (i=0; i < MAX_PORTS; i++) {
	p = svr_port + i + 3;
	if (p == port) {
	    close (socks[p]);
	    socks[i] = 0;
	}
    }

    return (0);
}


/**
 *  SESS_TOK -- Get the specified (1-indexed) w/s delimited token from str.
 */
static char *
sess_tok (char *str, int toknum)
{
    register int  i;
    static char tok[SZ_FNAME];
    char  *ip = str, *op = tok;


    memset (tok, 0, SZ_FNAME);			/* initialize		    */

    for (i=1; i < toknum; i++) { 		/* skip ahead to the token  */
	while (!isspace ((int) (*ip)) && *ip) ip++;
	while ( isspace ((int) (*ip)) && *ip) ip++;
    }

    do {
	*op++ = *ip++;
    } while (!isspace ((int) (*ip)) && *ip); 	/* copy the token 	    */

    return (tok);
}


/**
 *  SESS_REWRITECMD --  Rewrite the command string with filename substitution.
 */
static void
sess_rewriteCmd (char *line, char *session, char *fname)
{
    char *ip, *op, buf[SZ_LINE], obuf[SZ_LINE], name[SZ_FNAME];


    memset (obuf, 0, SZ_LINE);
    for (ip=line; *ip; ) {
        memset (buf, 0, SZ_LINE);
        memset (name, 0, SZ_FNAME);

        for (op=buf; *ip && !isspace(*ip); )    /* get token            */
           *op++ = *ip++;

#ifdef USE_FILE_URI
        /*  Replace file-URI and absolute paths with the replacement filename.
         */
        if (strncmp ("file://", buf, 7) == 0 || buf[0] == '/')
            strcat (obuf, fname);
        else
            strcat (obuf, buf);
#else
        /*  Replace 'SESSION_URL' with the session dataCache URL.
         */
        if (strncmp ("SESSION_URL", buf, 11) == 0) {
	    strcpy (name, (buf[11] ? &buf[12] : fname));
	    sprintf (buf, "%s/%s/%s", sessionUrlBase, session, name);
	}
        strcat (obuf, buf);
#endif

        while (*ip && isspace (*ip))            /* skip whitespace      */
            strncat (obuf, ip++, 1);
    }

    memset (line, 0, SZ_LINE);
    strcpy (line, obuf);
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
   "	%% vosession [-hvd] [-p <port>]\n" 
   "\n" 
   "  	where	<cmd>			command to process\n" 
   "  	     	-h			print help summary\n" 
   "  	     	-v			verbose output\n" 
   "  	     	-d			debug output\n" 
   "\n" 
   "  	     	-p <port>		server port\n"
   "\n" 
 );
}


/**
 *  Tests -- Task unit tests.
 */
static void
Tests (char *input)
{
   ; 	/*	 no-op		*/
}
