/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <setjmp.h>
#include <string.h>
#include <unistd.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <netdb.h>
#include <fcntl.h>
#include <time.h>
#include <pwd.h>

#include <termios.h>

#ifdef __APPLE__
#define	USE_RCMD 1
#include <unistd.h>
#endif

#define	import_kernel
#define	import_knames
#define	import_zfstat
#define	import_prtype
#define import_spp
#include <iraf.h>


/* ZFIOKS -- File i/o to a remote kernel server.  This driver is the network
 * interface for the kernel interface package (sys$ki).  The KS driver is
 * normally called directly by the KI routines, but is patterned after the
 * regular FIO drivers hence may be connected to FIO to provide a network
 * interface to the high level code.
 *
 *		zopcks		open kernel server on remote node
 *		zclcks		close kernel server
 *		zardks		read from the remote kernel server
 *		zawrks		write to the remote kernel server
 *		zawtks		wait for i/o
 *		zsttks		get channel/device status
 *
 * In the Berkeley UNIX environment the network interface is TCP/IP.  The
 * kernel server process irafks.e, running on the remote node, communicates
 * directly with the zfioks driver in the iraf client process via a socket.
 * Two protocols are supported for connecting to the irafks.e server.  The
 * default protocol uses RSH (or REMSH) as a bootstrap to start a daemon
 * irafks.e process (known as in.irafksd) on the remote node.  There is one
 * such daemon process per node per user.  Login authentication is performed
 * when the daemon process is started.  Once the daemon is running, each
 * instance of the irafks.e server is started by a simple request to the daemon
 * without repeating full login authentication, although an authorization code
 * is required.  The second, or alternate protocol uses REXEC to start the
 * irafks.e server directly, and requires that a password be supplied, either
 * in the irafhosts file or by an interactive query.
 *
 * The advantage of the default protocol is efficiency: starting a new server
 * is fast once the in.irafksd daemon is up and running on the server node.
 * All that is required is a connnect, an irafks.e fork, and another connect.
 * There are however some drawbacks (which result from the desire to make all
 * this work by boostrapping off of RSH so that no suid root processes are
 * needed).  First, for things to work correctly it must be possible to assign
 * each user a unique port number for the in.irafksd daemon.  This is done via
 * a uid based scheme.  There is however no guarantee that the port will not
 * already be in use, preventing the daemon from being bound to the port; if
 * this happens, a port is dynamically assigned by the daemon for temporary
 * use to set up the server.  If this happens a rsh request will be required
 * to spawn each server (this involves a total of half a dozen or so forks
 * and execs) but it should still work.
 *
 * The other complication has to do with security.  There is a potentially
 * serious security problem if the in.irafksd daemon simply responds to
 * every request to spawn a kernel server, because any user could issue such
 * a request (there is no way to check the uid of the client process over a
 * socket connection).  In an "open" environment this might be acceptable
 * since some effort is required to take advantage of this loophole, but to
 * avoid the loophole a simple authentication scheme is used.  This involves
 * a unique authentication integer which is set by the client when the daemon
 * is spawned - spawning the daemon via rsh is "secure" if rsh is secure.
 * Subsequent client requests must supply the same authentication number or
 * the request will be denied.  The authentication number may be specified
 * either in the user environment or in the .irafhosts file.  The driver will
 * automatically set protection 0600 (read-only) on the .irafhosts file when
 * accessed.
 *
 * The driver has a fall-back mode wherein a separate rsh call is used for
 * every server connection.  This is enabled by setting the in.irafksd port
 * number to zero in the irafhosts file.  In this mode, all port numbers are
 * dynamically assigned, eliminating the chance of collisions with reserved
 * or active ports.
 */

/*
 * PORTING NOTE -- A "SysV" system may use rsh instead of remsh.  This should
 * be checked when doing a port.  Also, on at least one system it was necessary
 * to change MAXCONN to 1 (maybe that is what it should be anyway).
 */

extern	int errno;
extern	int save_prtype;

#define	SZ_NAME		32		/* max size node, etc. name	  */
#define	SZ_CMD		256		/* max size rexec sh command	  */
#define	MAXCONN		1		/* for listen			  */
#define	MAX_UNAUTH	32		/* max unauthorized requests	  */
#define	ONEHOUR		(60*60)		/* number of seconds in one hour  */
#define	DEF_TIMEOUT	(1*ONEHOUR)	/* default in.irafksd idle timo   */
#define	MIN_TIMEOUT	60		/* minimum timeout (seconds)      */
#define	PRO_TIMEOUT	45		/* protocol (ks_geti) timeout     */
#define	DEF_HIPORT	47000		/* default in.irafksd port region */
#define	MAX_HIPORT	65535		/* top of port number range       */
#define	MIN_HIPORT	15000		/* bottom of port number range    */
#define REXEC_PORT	512		/* port for rexecd daemon	  */
#define	FNNODE_CHAR	'!'		/* node name delimiter		  */
#define	IRAFHOSTS	".irafhosts"	/* user host login file		  */
#define	HOSTLOGIN	"dev/irafhosts"	/* system host login file	  */
#define	USER		"<user>"	/* symbol for user account info   */
#define	UNAUTH		99		/* means auth did not match       */

#define	SELWIDTH	FD_SETSIZE	/* number of bits for select	  */

#define	KS_RETRY	"KS_RETRY"	/* number of connection attempts  */
#define	KS_NO_RETRY	"KS_NO_RETRY"	/* env to override rexec retry    */

#define	KSRSH		"KSRSH"		/* set in env to override RSH cmd */
#define	RSH		"rsh"		/* typical names are rsh, remsh   */

#define	IRAFKS_DIRECT		0	/* direct connection (via rexec)  */
#define	IRAFKS_CALLBACK		1	/* callback to client socket	  */
#define	IRAFKS_DAEMON		2	/* in.irafksd daemon process	  */
#define	IRAFKS_SERVER		3	/* irafks server process	  */
#define	IRAFKS_CLIENT		4	/* zfioks client process	  */

#define	C_RSH 			1	/* rsh connect protocol		  */
#define	C_REXEC			2	/* rexec connect protocol	  */
#define	C_REXEC_CALLBACK 	3	/* rexec-callback protocol	  */

struct	ksparam {
	int	auth;			/* user authorization code	  */
	int	port;			/* in.irafksd port		  */
	int	hiport;			/* in.irafksd port region	  */
	int	timeout;		/* in.irafksd idle timeout, sec   */
	int	protocol;		/* connect protocol		  */
};

int	debug_ks = 0;			/* print debug info on stderr	  */
char	debug_file[64] = "";		/* debug output file if nonnull   */
FILE	*debug_fp = NULL;		/* debugging output		  */

extern	uid_t getuid();
extern	char *getenv();
extern	char *strerror();
static	jmp_buf jmpbuf;
static	int jmpset = 0;
static	int recursion = 0;
static	int parent = -1;
static	SIGFUNC old_sigcld;
static  int ks_pchan[MAXOFILES];
static  int ks_achan[MAXOFILES];
static	int ks_getresvport(), ks_rexecport();
static	int ks_socket(), ks_geti(), ks_puti(), ks_getlogin();
static	void dbgsp(), dbgmsg(), dbgmsgs();
static  void dbgmsg1(), dbgmsg2(), dbgmsg3(), dbgmsg4();
static	char *ks_getpass();
static	void ks_onsig(), ks_reaper();

static int ks_getlogin (char *hostname, char *loginname, char *password,
  				struct ksparam *ks);
static char *ks_username (char *filename, char *pathname, char *username);
static char *ks_sysname (char *filename, char *pathname);
static struct irafhosts *ks_rhosts (char *filename);
static int   ks_getword (char **ipp, char *obuf);
static void  ks_whosts (struct irafhosts *hp, char *filename);
static char *ks_getpass (char *user, char *host);

void  pr_mask (char *str);

/* ZOPNKS -- Open a connected subprocess on a remote node.  Parse the "server"
 * argument to obtain the node name and the command to be issued to connect the
 * remote process.  Set up a socket to be used for communications with the
 * remote irafks kernel server.  The "server" string is implementation
 * dependent and normally comes from the file dev$hosts.  This file is read
 * by the high level VOS code before we are called.
 */
int
ZOPNKS (
  PKCHAR  *x_server,		/* type of connection */
  XINT	  *mode,		/* access mode (not used) */
  XINT	  *chan 		/* receives channel code (socket) */
)
{
	register char	*ip, *op;
	char	*server = (char *)x_server;
	char	host[SZ_NAME+1], username[SZ_NAME+1], password[SZ_NAME+1];
	int	proctype=0, port=0, auth=0, s_port=0, pid=0, s=0, i=0;
	struct  sockaddr_in  from;
	char	*hostp=NULL, *cmd=NULL;
	char	obuf[SZ_LINE];
	struct	ksparam ks;



	/* Initialize local arrays */
	host[0] = username[0] = password[0] = (char) 0;

	/* Parse the server specification.  We can be called to set up either
	 * the irafks daemon or to open a client connection.
	 *
	 *	(null)					direct via rexec
	 *      callback port@host			callback client
	 *      in.irafksd [port auth [timeout]]	start daemon
	 *      [-prot] [-log file] host!command	client connection
	 *
	 * where -prot is -rsh, -rex, or -rcb denoting the client connect
	 * protocols rsh, rexec, and rexec-callback.
	 */

	/* Eat any protocol specification strings.  The default connect
	 * protocol is rsh.
	 */
	for (ip = server;  isspace(*ip);  ip++)
	    ;
	ks.protocol = C_RSH;
	if (strncmp (ip, "-rsh", 4) == 0) {
	    ks.protocol = C_RSH;
	    ip += 4;
	} else if (strncmp (ip, "-rex", 4) == 0) {
	    ks.protocol = C_REXEC;
	    ip += 4;
	} else if (strncmp (ip, "-rcb", 4) == 0) {
	    ks.protocol = C_REXEC_CALLBACK;
	    ip += 4;
	}

	/* Check for the debug log flag. */
	for (  ;  isspace(*ip);  ip++)
	    ;
	if (strncmp (ip, "-log", 4) == 0) {
	    debug_ks++;
	    for (ip += 4;  isspace(*ip);  ip++)
		;
	    for (op=debug_file;  *ip && !isspace(*ip);  )
		*op++ = *ip++;
	    *op = EOS;
	}

	/* Determine connection type. */
	for (  ;  isspace(*ip);  ip++)
	    ;
	if (!*ip) {
	    proctype = IRAFKS_DIRECT;
	} else if (strncmp (ip, "callback ", 9) == 0) {
	    proctype = IRAFKS_CALLBACK;
	    ip += 9;
	} else if (strncmp (ip, "in.irafksd", 10) == 0) {
	    proctype = IRAFKS_DAEMON;
	    ip += 10;
	} else {
	    proctype = IRAFKS_CLIENT;
	    cmd = NULL;
	    for (op=host;  *ip != EOS;  ip++)
		if (*ip == FNNODE_CHAR) {
		    *op = EOS;
		    cmd = ++ip;
		    break;
		} else
		    *op++ = *ip;
	    if (cmd == NULL) {
		*chan = ERR;
		goto done;
	    }
	}


	/* Debug output.  If debug_ks is set (e.g. with adb) but no filename
	 * is given, debug output goes to stderr.
	 */
	if (debug_ks && !debug_fp) {
	    if (debug_file[0] != EOS) {
		if ((debug_fp = fopen (debug_file, "a")) == NULL)
		    debug_fp = stderr;
	    } else
		debug_fp = stderr;
	}


	/* Begin debug message log. */
	dbgmsg ("---------------------------------------------------------\n");
	dbgmsg1 ("zopnks (`%s')\n", server);
	dbgmsg4 ("kstype=%d, prot=%d, host=`%s', cmd=`%s')\n",
	    proctype, ks.protocol, host, ip);
	parent = getpid();

	/*
	 * SERVER side code.
	 * ---------------------
	 */

	if (proctype == IRAFKS_DIRECT) {
	    /* Kernel server was called by rexec and is already set up to
	     * communicate on the standard streams.
	     */
	    *chan = 0;
	    goto done;

	} else if (proctype == IRAFKS_CALLBACK) {
	    /* The kernel server was called by rexec using the rexec-callback
	     * protocol.  Connect to the client specified socket.
	     */
            char    *client_host;
            int     port, s;

            /* Parse "port@client_host". */
            for (port=0;  isdigit(*ip);  ip++)
                port = port * 10 + (*ip - '0');
            client_host = ip + 1;

	    dbgmsg2 ("S:callback client %s on port %d\n", client_host, port);
	    if ((s = ks_socket (client_host, NULL, port, "connect")) < 0)
		*chan = ERR;
	    else
		*chan = s;
	    goto done;

	} else if (proctype == IRAFKS_DAEMON) {
	    /* Handle the special case of spawning the in.irafksd daemon.  This
	     * happens when the zfioks driver is called by the irafks.e process
	     * which is started up on a remote server node by the networking
	     * system.  (via either rsh or rexec). To start the in.irafksd
	     * daemon we fork the irafks.e and exit the parent so that the
	     * rsh|remsh or rexec completes.  The daemon will run indefinitely
	     * or until the specified timeout interval passes without receiving
	     * any requests.  The daemon listens for connections on a global
	     * (per-user) socket; when a connection is made, the client passes
	     * in a socket address and authentication code, and provided the
	     * request is authenticated an irafks server is forked and
	     * connected to the client socket.  The irafks server then runs
	     * indefinitely, receiving and processing iraf kernel RPCs from the
	     * client until commanded to shutdown, the connection is broken, or
	     * an i/o error occurs.
	     */
	    struct  timeval timeout;
	    int     check, fromlen, req_port;
	    int     nsec, fd, sig;
	    fd_set  rfd;
	    int     once_only = 0;
	    int     detached = 0;
	    int     unauth = 0;
	    int     status = 0;

	    /* Get the server parameters.  These may be passed either via
	     * the client in the datastream, or on the command line.  The
	     * latter mode allows the daemon to be run standalone on a server
	     * node, without the need for a rsh call from a client to start
	     * the daemon.
	     */
	    while (*ip && isspace (*ip))
		ip++;
	    if (isdigit (*ip)) {
		/* Server parameters were passed on the command line. */
		char     *np;

		detached++;
		port = req_port = strtol (ip, &np, 10);
		if (np == NULL) {
		    status = 1;
		    goto d_err;
		} else
		    ip = np;
		auth = strtol (ip, &np, 10);
		if (np == NULL) {
		    status = 2;
		    goto d_err;
		} else
		    ip = np;
		nsec = strtol (ip, &np, 10);
		if (np == NULL) {
		    nsec = 0;   /* no timeout */
		} else
		    ip = np;
		dbgmsg3 ("S:detached in.irafksd, port=%d, auth=%d, timeout=%d\n",
		    port, auth, nsec);

	    } else {
		/* Get client data from the client datastream. */
		if ((req_port = port = ks_geti(0)) < 0)
		    { status = 1; goto d_err; }
		if ((auth = ks_geti(0)) < 0)
		    { status = 2; goto d_err; }
		if ((nsec = ks_geti(0)) < 0)
		    { status = 3; goto d_err; }
		dbgmsg2 ("S:client spawned in.irafksd, port=%d, timeout=%d\n",
		    port, nsec);
	    }

	    /* Attempt to bind the in.irafksd server socket to the client
	     * specified port.  If this fails a free socket is dynamically
	     * allocated.  If no port is specified (e.g. port=0) the port
	     * will always be dynamically allocated and a rsh call will be
	     * employed for every server connection.
	     */
	    if (port <= IPPORT_RESERVED)
		port = IPPORT_USERRESERVED - 1;
	    s = ks_getresvport (&port);
	    if (s < 0 || listen(s,MAXCONN) < 0) {
		status = 4;
		goto d_err;
	    } else if (port != req_port) {
		if (detached) {
		    status = 4;
		    goto d_err;
		}
		once_only++;
	    }

	    /* Fork daemon process and return if parent, exiting rsh. */
	    dbgmsg2 ("S:fork in.irafksd, port=%d, timeout=%d\n", port, nsec);
	    pid = fork();
	    if (pid < 0) {
		status = 4;
		goto d_err;
	    }

	    if (pid) {
d_err:		dbgmsg1 ("S:in.irafksd parent exit, status=%d\n", status);
		if (!detached) {
		    ks_puti (1, status);
		    ks_puti (1, port);
		}
		exit(0);
	    }

	    /*
	     * What follows is the code for the daemon process.  Close the
	     * stdio streams, which we won't need any more.  Create a socket
	     * and bind it to the given port.  Sit in a loop until timeout,
	     * listening for client connection requests and forking the irafks
	     * server in response to each such request.
	     */

	    dbgmsg3 ("S:in.irafksd started, pid=%d ppid=%d\n",
		getpid(), getppid());
	    old_sigcld = (SIGFUNC) signal (SIGCHLD, (SIGFUNC)ks_reaper);

	    /* Reset standard streams to console to record error messages. */
	    fd = open ("/dev/null", 0);     close(0);  dup(fd);  close(fd);
	    fd = open ("/dev/console", 1);  close(1);  dup(fd);  close(fd);
	    fd = open ("/dev/console", 2);  close(2);  dup(fd);  close(fd);

	    /* Loop forever or until the idle timeout expires, waiting for a
	     * client connection.
	     */
	    for (;;) {
		timeout.tv_sec = nsec;
		timeout.tv_usec = 0;
		FD_ZERO(&rfd);
		FD_SET(s,&rfd);
		status = 0;

		/* Wait until either we get a connection, or a previously
		 * started server exits.
		 */
		jmpset++;
		if ((sig = setjmp(jmpbuf))) {
	    	    if (sig == SIGCHLD) {
	    		dbgmsg ("S:in.irafksd sigchld return\n");
			while (waitpid ((pid_t)0, (int *)0, WNOHANG) > 0)
			        ;
		    } else
			exit (0);
		}
		if (select (SELWIDTH,&rfd,NULL,NULL, nsec ? &timeout : 0) <= 0)
		    exit (0);

		/* Accept the connection. */
		if ((fd = accept (s, (struct sockaddr *)0, 
			(socklen_t *)0)) < 0) {
		    	    fprintf (stderr,
			      "S:in.irafksd: accept on port %d failed\n", port);
		    exit (2);
		} else
		    dbgmsg ("S:in.irafksd: connection established\n");

		/* Find out where the connection is coming from. */
		fromlen = sizeof (from);
		if (getpeername (fd, (struct sockaddr *)&from, 
		    (socklen_t *)&fromlen) < 0) {
		    fprintf (stderr, "in.irafksd: getpeername failed\n");
		    exit (3);
		}

		/* Connection established.  Get client data. */
		if ((s_port = ks_geti(fd)) < 0 || (check = ks_geti(fd)) < 0) {
		    fprintf (stderr, "in.irafksd: protocol error\n");
		    status = 1;
		    goto s_err;
		}

		/* Verify authorization.  Shutdown if repeated unauthorized
		 * requests occur.
		 */
		if (auth && check != auth) {
		    if (unauth++ > MAX_UNAUTH) {
			fprintf (stderr,
			    "in.irafksd: unauthorized connection attempt\n");
			exit (4);
		    }
		    status = UNAUTH;
		    goto s_err;
		}

		/* Connection authorized if this message is output. */
		dbgmsg1 ("S:in.irafksd: client port = %d\n", s_port);

		/* Fork the iraf kernel server. */
		pid = fork();
		if (pid < 0) {
		    fprintf (stderr, "in.irafksd: process creation failed\n");
		    status = 3;
		    goto s_err;
		}

		if (pid) {					/** parent **/
s_err:		    dbgmsg1 ("S:in.irafksd fork complete, status=%d\n",
			status);
		    ks_puti (fd, status);
		    close (fd);
		    if (once_only)
			exit (0);
		    /* otherwise loop indefinitely */

		} else {					/** child  **/
		    /* Set up iraf kernel server. */
		    u_long  n_addr, addr;
		    unsigned char *ap = (unsigned char *)&n_addr;

		    dbgmsg2 ("S:irafks server started, pid=%d ppid=%d\n",
			getpid(), getppid());
		    signal (SIGCHLD, old_sigcld);
		    /*
		    old_sigcld = (SIGFUNC) signal (SIGCHLD, (SIGFUNC)ks_reaper);
		    */
		    close (fd); close (s);

		    n_addr = from.sin_addr.s_addr;
		    addr = ntohl(n_addr);
		    sprintf (obuf, "%d.%d.%d.%d", ap[0],ap[1],ap[2],ap[3]);
		    dbgmsg2 ("S:client address=%s port=%d\n", obuf, s_port);

		    if ((s = ks_socket (NULL, addr, s_port, "connect")) < 0) {
			dbgmsg1 ("S:irafks connect to port %d failed\n", s_port);
			fprintf (stderr, "irafks: cannot connect to client\n");
			exit (1);
		    } else
			dbgmsg1 ("S:irafks connected on port %d\n", s_port);

		    *chan = s;
		    goto done;
		}
	    }
	} /* else fall through to DAEMON_CLIENT code */

	/*
	 * CLIENT side code.
	 * ---------------------
	 */

	/* Attempt to fire up the kernel server process.  Get login name
	 * and password for the named host.  If a password is given attempt
	 * to connect via the rexec protocol, otherwise attempt the connection
	 * via the rsh/in.irafksd protocol.
	 */
	if (ks_getlogin (host, username, password, &ks) == ERR) {
	    *chan = ERR;

	} else if (ks.protocol == C_REXEC) {
	    /* Use rexec protocol.  We start the remote kernel server with
	     * rexec and communicate via the socket returned by rexec.
	     */
	    hostp = host;
	    dbgmsg2 ("C:rexec for host=%s, user=%s\n", host, username);
#ifdef USE_RCMD
	    *chan = rcmd (&hostp, ks_rexecport(),
		getlogin(), username, cmd, 0);
#else
	    *chan = rexec (&hostp, ks_rexecport(), username, password, cmd, 0);
#endif

	} else if (ks.protocol == C_REXEC_CALLBACK) {
	    /* Use rexec-callback protocol.  In this case the remote kernel
	     * server is started with rexec, but we have the remote server
	     * call us back on a private socket.  This guarantees a direct
	     * socket connection for use in cases where the standard i/o
	     * streams set up for rexec do not provide a direct connection.
	     */
	    char    localhost[SZ_FNAME];
	    char    callback_cmd[SZ_LINE];
	    struct  hostent *hp;
	    int     tfd=0, fd=0, ss=0;

	    /* Get reserved port for direct communications link. */
	    s_port = IPPORT_USERRESERVED - 1;
	    s = ks_getresvport (&s_port);
	    if (s < 0)
		goto r_err;

	    /* Ready to receive callback from server. */
	    if (listen (s, MAXCONN) < 0)
		goto r_err;

	    /* Compose rexec-callback command:  "cmd port@client-host". */
	    if (gethostname (localhost, SZ_FNAME) < 0)
		goto r_err;
	    if ((hp = gethostbyname (localhost)) == NULL)
		goto r_err;
	    sprintf (callback_cmd, "%s callback %d@%s",
		cmd, s_port, hp->h_name);
	    dbgmsg2 ("rexec to host %s: %s\n", host, callback_cmd);

	    hostp = host;
	    dbgmsg3 ("rexec for host=%s, user=%s, using client port %d\n",
		host, username, s_port);
#ifdef USE_RCMD
	    ss = rcmd (&hostp, ks_rexecport(),
		getlogin(), username, callback_cmd, 0);
#else
	    ss = rexec (&hostp,
		ks_rexecport(), username, password, callback_cmd, 0);
#endif

	    /* Wait for the server to call us back. */
	    dbgmsg1 ("waiting for connection on port %d\n", s_port);
	    if ((tfd = accept (s, (struct sockaddr *)0, (socklen_t *)0)) < 0) {
r_err:		dbgmsg ("rexec-callback connect failed\n");
		close(s);  close(ss);
		*chan = ERR;
	    } else {
		close(s);  fd = dup(tfd);  close(tfd);
		dbgmsg1 ("connected to irafks server on fd=%d\n", fd);
		*chan = fd;

		/* Mark the rexec channel for deletion at close time when
		 * the i/o socket is closed.
		 */
		for (i=0;  i < MAXOFILES;  i++)
		    if (!ks_pchan[i]) {
			ks_pchan[i] = fd;
			ks_achan[i] = ss;
			break;
		    }
	    }

	} else {
	    /* Use the default protocol, which avoids passwords.  This uses
	     * rsh to start up (once) the iraf networking daemon in.irafksd
	     * on the remote node, and thereafter merely places requests to
	     * in.irafksd to spawn each instance of the irafks.e server.
	     */
	    char    command[SZ_LINE], *nretryp;
	    int     pin[2], pout[2];
	    int     status = 0;
	    int     ntries = 0, nretries = 0;
	    char    *password;
	    int     fd, tfd;
	    int     t=0, s=0;

	    /* Get reserved port for client. */
	    s_port = IPPORT_USERRESERVED - 1;
	    s = ks_getresvport (&s_port);
	    if (s < 0) {
		status |= 01;
		goto c_err;
	    }
	    dbgmsg2 ("C:connect to in.irafksd host=%s client port=%d\n",
		host, s_port);

	    /* Ready to receive callback from server. */
	    if (listen (s, MAXCONN) < 0) {
		status |= 02;
		goto c_err;
	    }
			
	    /* Check for the number of connection attempts. */
	    if ((nretryp = getenv(KS_RETRY)))
		nretries = atoi(nretryp);

	    /* in.irafkd port. */
	    port = ks.port;
again:
	    /* Connect to in.irafksd daemon on server system and send request
	     * to start up a new irafks daemon on the port just created.  If
	     * the connection fails, fork an rsh and start up the in.irafksd.
	     */
	    if (!port || (t = ks_socket (host, NULL, port, "connect")) < 0) {
		dbgmsg ("C:no server, fork rsh to start in.irafksd\n");

		if (pipe(pin) < 0 || pipe(pout) < 0) {
		    status |= 04;
		    goto c_err;
		}
		pid = fork();
		if (pid < 0) {
		    status |= 010;
		    goto c_err;
		}

		if (pid) {
		    /* Pass target port and authorization code to in.irafksd.
		     * Server returns the actual port assigned.
		     */
		    close (pin[1]);
		    close (pout[0]);
retry:
		    dbgmsg2 ("C:send port=%d, timeout=%d to irafks.e\n",
			ks.port, ks.timeout);
		    if (ks_puti (pout[1], port) <= 0)
			status |= 0020;
		    if (ks_puti (pout[1], ks.auth) <= 0)
			status |= 0040;
		    if (ks_puti (pout[1], ks.timeout) <= 0)
			status |= 0100;
		    if (ks_geti(pin[0]))
			status |= 0200;

		    port = ks_geti (pin[0]);
		    dbgmsg1 ("C:irafks.e returns port=%d\n", port);

		    /* Wait for the rsh connection to shut down. */
		    while (read (pin[0], obuf, SZ_LINE) > 0)
			;
		    wait (NULL);
		    close (pin[0]);
		    if (pout[1] != pin[0])
			close (pout[1]);

		    /* If the rsh succeeded the in.irafksd daemon should be
		     * running now.  Attempt again to connect.  If this fails,
		     * most likely the rsh failed.  Try to use rexecd to start
		     * the daemon.
		     */
		    if (status ||
			(t = ks_socket (host, NULL, port, "connect")) < 0) {

			/* The KS_RETRY environment variable may be set to 
			 * the number of times we wish to try to reconnect.
			 * We'll sleep for 1-second between attempts before
			 * giving up.
			 */
			if (getenv (KS_RETRY) && nretries--) {
			    sleep (1);
			    goto again;
			}

			/* If KS_NO_RETRY is set then we won't try at all
			 * with an rexec.  These two variables give us a
			 * chance to retry using the rsh/KSRSH protocol some
			 * number of times before failing, and optionally
			 * trying with a different (rexec) before quitting
			 * entirely.  On most recent systems the rexec port
			 * isn't enabled anyway.
			 */
			if (getenv (KS_NO_RETRY) || ntries++) {
			    status |= 0400;
			    goto c_err;
			}

			dbgmsg ("C:rsh failed - try rexec\n");
			if (!(password = ks_getpass (username, host)))
			    { status |= 01000;  goto c_err; }

			sprintf (command, "%s in.irafksd", cmd);
			dbgmsg3 ("C:rexec %s@%s: %s\n", username, host, command);

			hostp = host;
#ifdef USE_RCMD
			fd = rcmd (&hostp, ks_rexecport(),
			    getlogin(), username, command, 0);
#else
			fd = rexec (&hostp, ks_rexecport(),
			    username, password, command, NULL);
#endif

			if (fd < 0) {
			    status |= 02000;
			    goto c_err;
			} else {
			    status = 0;
			    port = ks.port;
			    pin[0] = pout[1] = fd;
			    goto retry;
			}
		    }

		} else {
		    /* Call rsh to start up in.irafksd on server node.
		     */
		    char *s, *rshcmd;

		    close (pin[0]);  close (pout[1]);
		    close (0);  dup (pout[0]);  close (pout[0]);
		    close (1);  dup (pin[1]);   close (pin[1]);

		    rshcmd = (s = getenv(KSRSH)) ? s : RSH;

		    dbgmsg3 ("C:exec rsh %s -l %s `%s' in.irafksd\n",
			host, username, cmd);
		    execlp (rshcmd, rshcmd,
			host, "-l", username, cmd, "in.irafksd", NULL);
		    exit (1);
		}
	    }

	    /* Send command to start up irafks server.  This consists of the
	     * reserved port for the server connection followed by the
	     * authorization code.  The in.irafksd daemon returns a status
	     * byte which will be zero if the operation is successful.
	     */
	    dbgmsg1 ("C:request irafks server for client port %d\n", s_port);

	    if (ks_puti (t, s_port) <= 0)
		{ status |= 004000;  goto c_err; }
	    if (ks_puti (t, ks.auth) <= 0)
		{ status |= 010000;  goto c_err; }

	    /* Check for an authorization failure and connect on a dynamically
	     * allocated port if this happens (in.irafksd will allocate the
	     * port).  An authorization failure does not necessarily indicate
	     * an unauthorized connection attempt; it may mean instead that
	     * the user has not set up the same authorization code on two
	     * different nodes and iraf clients on both nodes, with different
	     * authorization codes, are trying to access the same server.
	     * If this happens the first client will get the in.irafksd daemon
	     * and the other client will have to do an rsh connect each time.
	     */
	    if ((status = ks_geti(t))) {
		if (port && status == UNAUTH) {
		    close(t);
		    port = 0;
		    dbgmsg ("C:authorization failed, retry with port=0\n");
		    status = 0;
		    goto again;
		} else {
		    status |= 020000;
		    goto c_err;
		}
	    }

	    /* Wait for the server to call us back. */
	    if ((tfd = accept (s, (struct sockaddr *)0, (socklen_t *)0)) < 0) {
c_err:		dbgmsg1 ("C:zfioks client status=%o\n", status);
		close(t);  close(s);
		kill (pid, SIGTERM);
		*chan = ERR;
	    } else {
		close(t);  close(s);  fd = dup(tfd);  close(tfd);
		dbgmsg1 ("C:connected to irafks server on fd=%d\n", fd);
		*chan = fd;
	    }
	}

done:
	jmpset = 0;
	if (*chan > 0) {
	    if (*chan < MAXOFILES)
		zfd[*chan].nbytes = 0;
	    else {
		close (*chan);
		*chan = ERR;
	    }
	}

	dbgmsg1 ("zopnks returns status=%d\n", *chan);

	return (*chan);
}


/* ZCLSKS -- Close a kernel server connection.
 */
int
ZCLSKS (
  XINT	*chan,			/* socket to kernel server	*/
  XINT	*status 		/* receives close status	*/
)
{
	int	i;

	/* Close the primary channel. */
	*status = close (*chan);

	/* Close any alternate channels associated with the primary. */
	for (i=0;  i < MAXOFILES;  i++) {
	    if (ks_pchan[i] == *chan) {
		close (ks_achan[i]);
		ks_pchan[i] = 0;
	    }
	}

	dbgmsg2 ("server [%d] terminated, status = %d\n", *chan, *status);

	return (*status);
}


/* ZARDKS -- Read from the kernel server channel.  No attempt is made to
 * impose a record structure upon the channel, as is the case with IPC.
 * In UNIX the channel is stream oriented and it is up to the caller to
 * unblock records from the input stream.  Data blocks are assumed to be
 * preceded by headers telling how much data to read, hence we read from
 * the channel until the specified number of bytes have been read or ERR
 * or EOF is seen on the stream.
 */
int
ZARDKS (
  XINT	*chan,			/* kernel server channel (socket)	*/
  XCHAR	*buf,			/* output buffer			*/
  XINT	*totbytes,		/* total number of bytes to read	*/
  XLONG	*loffset 		/* not used				*/
)
{
#ifdef ANSI
	volatile char	*op;
	volatile int	fd, nbytes;
#else
	char	*op;
	int	fd, nbytes;
#endif
	SIGFUNC	sigint, sigterm;
	int	status = ERR;

	fd = *chan;
	op = (char *)buf;
	zfd[fd].nbytes = nbytes = *totbytes;
	if (debug_ks > 1)
	    dbgmsg2 ("initiate read of %d bytes from KS channel %d\n",
		nbytes, fd);

	/* Now read exactly nbytes of data from channel into user buffer.
	 * Return actual byte count if EOF is seen.  If ERR is seen return
	 * ERR.  If necessary multiple read requests are issued to read the
	 * entire record.  Reads are interruptable but the interrupt is caught
	 * and returned as a read error on the server channel.
	 */
	sigint  = (SIGFUNC) signal (SIGINT,  (SIGFUNC)ks_onsig);
	sigterm = (SIGFUNC) signal (SIGTERM, (SIGFUNC)ks_onsig);

	while (nbytes > 0) {
	    jmpset++;
	    if (setjmp (jmpbuf) == 0)
		status = read (fd, op, nbytes);
	    else
		status = ERR;

	    switch (status) {
	    case 0:
		zfd[fd].nbytes -= nbytes;
		return (XERR);
	    case ERR:
		zfd[fd].nbytes = ERR;
		return (XERR);
	    default:
		nbytes -= status;
		op += status;
		break;
	    }
	}

	jmpset = 0;
	signal (SIGINT,  sigint);
	signal (SIGTERM, sigterm);
	if (debug_ks > 1)
	    dbgmsg2 ("read %d bytes from KS channel %d:\n", op-(char *)buf, fd);

	return (status);
}


/* ZAWRKS -- Write to a kernel server channel.
 */
int
ZAWRKS (
  XINT	*chan,			/* kernel server channel (socket)	*/
  XCHAR	*buf,			/* output buffer			*/
  XINT	*totbytes,		/* number of bytes to write		*/
  XLONG	*loffset 		/* not used				*/
)
{
	SIGFUNC	sigint, sigterm, sigpipe;
#ifdef ANSI
	volatile int fd, nbytes;
	volatile int ofd;
#else
	int	fd, nbytes;
	int	ofd;
#endif

	/* If chan=0 (the process standard input) then we really want to
	 * write to channel 1, the standard output.
	 */
	if ((ofd = fd = *chan) == 0)
	    ofd = 1;

	zfd[fd].nbytes = nbytes = *totbytes;
	if (debug_ks > 1)
	    dbgmsg2 ("initiate write of %d bytes to KS channel %d\n",
		nbytes, ofd);

	/* Write exactly nbytes of data to the channel from user buffer to
	 * the channel.  Block interrupt during the write to avoid corrupting
	 * the data stream protocol if the user interrupts the client task.
	 * Trap SIGPIPE and return it as a write error on the channel instead.
	 * Likewise, turn an interrupt into a write error on the channel.
	 */
	sigint  = (SIGFUNC) signal (SIGINT,  (SIGFUNC)ks_onsig);
	sigterm = (SIGFUNC) signal (SIGTERM, (SIGFUNC)ks_onsig);
	sigpipe = (SIGFUNC) signal (SIGPIPE, (SIGFUNC)ks_onsig);
	recursion = 0;

	jmpset++;
	if (setjmp (jmpbuf) == 0)
	    zfd[fd].nbytes = write (ofd, (char *)buf, nbytes);
	else
	    zfd[fd].nbytes = ERR;

	jmpset = 0;
	signal (SIGINT,  sigint);
	signal (SIGTERM, sigterm);
	signal (SIGPIPE, sigpipe);
	if (debug_ks > 1)
	    dbgmsg2 ("wrote %d bytes to KS channel %d:\n", zfd[fd].nbytes, ofd);

	return (XOK);
}


/* KS_ONSIG -- Catch a signal.
 */
static void
ks_onsig (
  int	sig,			/* signal which was trapped	*/
  int	*arg1,			/* not used */
  int	*arg2 			/* not used */
)
{
	/* If we get a SIGPIPE writing to a server the server has probably
	 * died.  Make it look like there was an i/o error on the channel.
	 */
	if (sig == SIGPIPE && recursion++ == 0)
	    fputs ("kernel server process has died\n", stderr);

	if (jmpset)
	    longjmp (jmpbuf, sig);
}


/* KS_REAPER -- Catch a SIGCHLD signal and reap all children.
 */
static void
ks_reaper (
  int     sig,                  /* signal which was trapped     */
  int     *arg1,                /* not used */
  int     *arg2                 /* not used */
)
{
        int status=0, pid=0;

        while ((pid = waitpid ((pid_t)0, (int *) &status, WNOHANG)) > 0)
	    dbgmsg2 ("ks_reaper -- pid=%d, status=%d\n", pid, status);

	if (jmpset)
	    longjmp (jmpbuf, sig);
}


/* ZAWTKS -- Wait for i/o to a KS channel.  Since UNIX i/o is not asynchronous
 * we do not really wait, rather we return the status value (byte count) from
 * the last read or write to the channel.
 */
int
ZAWTKS (XINT *chan, XINT *status)
{
	if ((*status = zfd[*chan].nbytes) == ERR)
	    *status = XERR;

	return (*status);
}


/* ZSTTKS -- Get binary file status for an KS channel.  A KS channel is a
 * streaming binary file.
 */
int
ZSTTKS (
  XINT	*chan,			/* not used; all KS channels have same status */
  XINT	*param,
  XLONG	*lvalue 
)
{
	switch (*param) {
	case FSTT_BLKSIZE:
	case FSTT_FILSIZE:
	    *lvalue = 0;
	    break;
	case FSTT_OPTBUFSIZE:
	    *lvalue = KS_OPTBUFSIZE;
	    break;
	case FSTT_MAXBUFSIZE:
	    *lvalue = KS_MAXBUFSIZE;
	    break;
	default:
	    *lvalue = XERR;
	}

	return (XOK);
}


/*
 * Internal routines.
 * -------------------
 */

/* KS_SOCKET -- Get a socket configured for the given host and port.  Either
 * bind the socket to the port and make it ready for connections, or connect
 * to the remote socket at the given address.
 */
static int
ks_socket (host, addr, port, mode)
char	*host;
u_long	addr;
int	port;
char	*mode;
{
	struct	sockaddr_in sockaddr;
	struct	hostent *hp;
	int	s;

	/* Create socket. */
	if ((s = socket (AF_INET, SOCK_STREAM, 0)) < 0)
	    return (ERR);

	/* Set socket address. */
	bzero ((char *)&sockaddr, sizeof(sockaddr));
	sockaddr.sin_family = AF_INET;
	sockaddr.sin_port = htons((short)port);

	/* Get address of server host. */
	if (addr) {
	    sockaddr.sin_addr.s_addr = htonl((long)addr);
	} else if (*host) {
	    if ((hp = gethostbyname (host)) == NULL)
		goto failed;
	    bcopy((char *)hp->h_addr,(char *)&sockaddr.sin_addr, hp->h_length);
	} else
	    sockaddr.sin_addr.s_addr = INADDR_ANY;

	/* Either bind and listen for connnections, or connect to a remote
	 * socket.
	 */
	if (strncmp (mode, "listen", 1) == 0) {
	    if (bind (s, (struct sockaddr *)&sockaddr, sizeof(sockaddr)) < 0)
		goto failed;
	    if (listen (s, MAXCONN) < 0)
		goto failed;
	} else if (strncmp (mode, "connect", 1) == 0) {
            if (connect(s,(struct sockaddr *)&sockaddr,sizeof(sockaddr)) < 0)
		goto failed;
	} else
	    goto failed;

	return (s);

failed:
	dbgmsg2 ("ks_socket: errno=%d (%s)\n", errno, strerror(errno));
	close (s);
	return (ERR);
}


/* KS_GETRESVPORT -- Open a socket and attempt to bind it to the given port.
 * Locate a usable port if this fails.  The actual port is returned in the
 * output argument.
 */
static int
ks_getresvport (alport)
int	*alport;
{
	struct	sockaddr_in sin;
	int	s;

	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	s = socket (AF_INET, SOCK_STREAM, 0);
	if (s < 0)
	    return (-1);

	for (;;) {
	    sin.sin_port = htons((u_short)*alport);
	    if (bind(s, (struct sockaddr *)&sin, sizeof(sin)) >= 0) {
		return (s);
	    }
	    if (errno != EADDRINUSE) {
		(void) close(s);
		return (-1);
	    }
	    dbgmsg4 ("ks_getresvport: decr errno=%d (%s) alport=%d -> %d\n", 
		errno, strerror(errno), *alport, *alport - 1);
	    (*alport)--;
	    if (*alport == IPPORT_RESERVED) {
		(void) close(s);
		errno = EAGAIN;		/* close */
		return (-1);
	    }
	}
}


/* KS_REXECPORT -- Return the port for the rexec system service.
 */
static int
ks_rexecport()
{
	register struct servent *sv;
	static	int port = 0;

	if (port)
	    return (port);

	if ((sv = getservbyname ("exec", "tcp")))
	    return (port = sv->s_port);
	else
	    return (port = REXEC_PORT);
}


/* KS_PUTI -- Write an integer value to the output stream as a null terminated
 * ascii string.
 */
static int
ks_puti (fd, ival)
int	fd;
int	ival;
{
	char	obuf[SZ_FNAME];

	sprintf (obuf, "%d", ival);
	return (write (fd, obuf, strlen(obuf)+1));
}


/* KS_GETI -- Read a positive integer value, passed as a null terminated ascii
 * string, base decimal, from the given stream.
 */
static int
ks_geti (fd)
int	fd;
{
	register int value = 0;
	struct  timeval timeout;
	int	stat, sig;
	fd_set  rfd;
	char	ch;

	jmpset++;
	if ((sig = setjmp(jmpbuf)))
	    if (sig == SIGCHLD)
		waitpid ((pid_t)0, (int *)0, WNOHANG);

	timeout.tv_sec = PRO_TIMEOUT;
	timeout.tv_usec = 0;
	FD_ZERO(&rfd);
	FD_SET(fd,&rfd);

	/* Read and accumulate a decimal integer.  Timeout if the client
	 * does not respond within a reasonable period.
	 */
	do {
	    if (select (SELWIDTH, &rfd, NULL, NULL, &timeout) <= 0) {
		dbgmsg ("ks_geti: timeout on read\n");
		jmpset = 0;
		return (ERR);
	    }

	    if ((stat = read (fd, &ch, 1)) <= 0) {
		dbgmsg3 ("ks_geti: read status=%d, errno=%d (%s)\n", 
		    stat, errno, strerror(errno));
		jmpset = 0;
		return (ERR);
	    }

	    if (ch) {
		if (isdigit(ch))
		    value = value * 10 + (ch - '0');
		else {
		    dbgmsg1 ("ks_geti: read char=%o\n", ch);
		    jmpset = 0;
		    return (ERR);
		}
	    }
	} while (ch);

	jmpset = 0;
	return (value);
}


/* KS_GETS -- Read a null terminated ascii string.
static int
ks_gets (fd, outstr)
int	fd;
char	*outstr;
{
	register char *op = outstr;
	int	stat;

	do {
	    if ((stat = read (fd, op, 1)) <= 0) {
		dbgmsg3 ("ks_gets: read status=%d, errno=%d (%s)\n", 
		    stat, errno, strerror(errno));
		return (ERR);
	    }
	} while (*op++);

	return (op - outstr - 1);
}
 */


/* KS_MSG -- Print debugging messages.
 */
static void dbgsp (pid)
int	pid;
{
	int i, nsp = ((parent > 0) ? (pid - parent) : 0);
	for (i=0; i < nsp; i++)
	    fprintf (debug_fp, "  ");
}

static void
dbgmsg (msg)
char    *msg;
{
	int pid;
	if (debug_ks) {
	    fprintf (debug_fp, "[%5d] ", (pid = getpid())); dbgsp(pid);
	    fprintf (debug_fp, "%s", msg);
	}
}
static void
dbgmsgs (fmt, arg)
char    *fmt;
char    *arg;
{
	int pid;
	if (debug_ks) {
	    fprintf (debug_fp, "[%5d] ", (pid = getpid())); dbgsp(pid);
	    fprintf (debug_fp, fmt, arg);
	    fflush (debug_fp);
	}
}
static void
dbgmsg1 (fmt, arg)
char    *fmt;
int     arg;
{
	int pid;
	if (debug_ks) {
	    fprintf (debug_fp, "[%5d] ", (pid = getpid())); dbgsp(pid);
	    fprintf (debug_fp, fmt, arg);
	    fflush (debug_fp);
	}
}
static void
dbgmsg2 (fmt, arg1, arg2)
char    *fmt;
int     arg1, arg2;
{
	int pid;
	if (debug_ks) {
	    fprintf (debug_fp, "[%5d] ", (pid = getpid())); dbgsp(pid);
	    fprintf (debug_fp, fmt, arg1, arg2);
	    fflush (debug_fp);
	}
}
static void
dbgmsg3 (fmt, arg1, arg2, arg3)
char    *fmt;
int     arg1, arg2, arg3;
{
	int pid;
	if (debug_ks) {
	    fprintf (debug_fp, "[%5d] ", (pid = getpid())); dbgsp(pid);
	    fprintf (debug_fp, fmt, arg1, arg2, arg3);
	    fflush (debug_fp);
	}
}
static void
dbgmsg4 (fmt, arg1, arg2, arg3, arg4)
char    *fmt;
int     arg1, arg2, arg3, arg4;
{
	int pid;
	if (debug_ks) {
	    fprintf (debug_fp, "[%5d] ", (pid = getpid())); dbgsp(pid);
	    fprintf (debug_fp, fmt, arg1, arg2, arg3, arg4);
	    fflush (debug_fp);
	}
}


/*
 * Stuff for processing the irafhosts file.
 * ----------------------------------------
 */

#define	MAX_HEADERLINES		128
#define	MAX_NODES		256
#define	SZ_SBUF			4096
#define	DEFAULT			(-1)
#define	KSAUTH			"KSAUTH"

struct irafhosts {
	int	port;
	int	auth;
	int	hiport;
	int	timeout;
	int	nheaderlines;
	int	nparams;
	int	mode;
	char	*header[MAX_HEADERLINES];
	int	nnodes;
	struct	nodelist {
		char	*name;
		char	*login;
		char	*password;
		int	port;
		int	auth;
		int	hiport;
		int	timeout;
		int	protocol;
	} node[MAX_NODES];
	int	sbuflen;
	char	sbuf[SZ_SBUF];
};


/* KS_GETLOGIN -- Read the irafhosts file to determine how to connect to
 * the indicated host.  If the user has a .irafhosts file read that, otherwise
 * read the system default irafhosts file in iraf$dev.  Fill in or correct
 * any networking parameters as necessary.  If the rsh protocol is enabled
 * and the user does not have a .irafhosts file, create one for them.  If
 * the user has a .irafhosts file but a unique authorization code has not
 * yet been assigned, assign one and write a new file.  Ensure that the
 * file has read-only access privileges.
 */
static int
ks_getlogin (
  char	 *hostname,		/* node we wish a login for */
  char	 *loginname,		/* receives the login name */
  char	 *password,		/* receives the login password */
  struct ksparam *ks 		/* networking parameters */
)
{
	register struct irafhosts *hp;
	register int i;
	char	userfile[SZ_PATHNAME];
	char	sysfile[SZ_PATHNAME];
	char	fname[SZ_PATHNAME];
	char	username[SZ_NAME];
	char	*namep, *authp;
	struct	nodelist *np;
	int	update = 0;
	int	auth;

	/* Get path to user irafhosts file. */
	if (ks_username (IRAFHOSTS, userfile, username) == NULL)
	    return (ERR);

	/* Read user irafhosts file if there is one.  Check for an old-style
	 * irafhosts file, and read the system file instead if the user file
	 * is the old obsolete version.
	 */
	if ((hp = ks_rhosts (userfile))) {
	    /* Old style irafhosts file? */
	    if (hp->nparams == 0) {
		/* Attempt to preserve old file with .OLD extension. */
		strcpy (fname, username);
		strcat (fname, ".OLD");
		unlink (fname);
		rename (username, fname);

		/* Read system file instead. */
		free ((char *)hp);
		if (ks_sysname (HOSTLOGIN, sysfile) == NULL)
		    return (ERR);
		if ((hp = ks_rhosts (sysfile)) == NULL)
		    return (ERR);
		update++;
	    }
	} else {
	    /* Use system default irafhosts. */
	    if (ks_sysname (HOSTLOGIN, sysfile) == NULL)
		return (ERR);
	    if ((hp = ks_rhosts (sysfile)) == NULL)
		return (ERR);
	    update++;
	}

	/* Search the node list for an entry for the named host.
	 */
	for (i=0, np=NULL;  i < hp->nnodes;  i++) {
	    namep = hp->node[i].name;
	    if (strcmp (hostname, namep) == 0 || strcmp (namep, "*") == 0) {
		np = &hp->node[i];
		break;
	    }
	}

	/* Get the login name.  If this is "disable" networking is disabled
	 * for the given node entry.
	 */
	if (np->login[0] && strcmp(np->login,"disable") == 0) {
	    free ((char *)hp);
	    return (ERR);
	} else if (np->login[0] && strcmp(np->login,USER) != 0) {
	    strcpy (loginname, np->login);
	} else
	    strcpy (loginname, username);

	/* Get the password. */
	if (np->password[0]) {
	    if (strcmp (np->password, USER) == 0) {
		if (ks->protocol == C_RSH)
		    password[0] = EOS;
		else
		    goto query;  /* need a valid password for rexec */
	    } else if (strcmp (np->password, "?") == 0) {
query:		if ((namep = ks_getpass (loginname, hostname)))
		    strcpy (password, namep);
		else
		    password[0] = EOS;
	    } else
		strcpy (password, np->password);
	} else
	    password[0] = EOS;

	/*
	 * Set up ksparam structure.  Check to see if any of the irafhosts
	 * parameter values are out of range; if so, set the default values,
	 * and mark the file for updating.
	 *
	 * NOTE -- If possible, the user should have the same port number and
	 * authorization code (e.g., same .irafhosts file) on all local nodes.
	 * All we can do here is manage the file on the local node.  It is up
	 * to the user to make all the files the same.
	 */

	/* The port number for the in.irafksd daemon should be unique for
	 * every user, as well as unique in the sense that no other network
	 * service uses the port.  This is impossible to guarantee, but we
	 * can come close by choosing port numbers in the high range for a
	 * short integer, using the user's UID to attempt to give each user
	 * a unique port.
	 */
	if (hp->hiport == DEFAULT) {
	    ks->hiport = DEF_HIPORT;
	} else if (hp->hiport > MAX_HIPORT || hp->hiport < MIN_HIPORT) {
	    ks->hiport = DEF_HIPORT;
	    hp->hiport = DEFAULT;
	    update++;
	} else
	    ks->hiport = hp->hiport;

	if (hp->port == 0)
	    ks->port = 0;
	else if (hp->port > MAX_HIPORT || hp->port < IPPORT_RESERVED)
	    ks->port = ks->hiport - (getuid() % 10000);
	else
	    ks->port = hp->port;

	/* Every user should also have a unique authorization code.  It should
	 * be next to impossible to predict apriori, so that user A cannot
	 * predict user B's authorization code.  The number is arbitary,
	 * and can be changed by the user by editing the irafhosts file.
	 * Any heuristic which produces a reasonable unique and unpredictable
	 * number will do.  We use the system clock time and a snapshot of
	 * the machine registers as saved in a setjmp.  Given that any iraf
	 * program can cause a network request which results in generation
	 * of an authorization code, and the point at which this request
	 * occurs during the execution of a task is arbtrary, the register
	 * set should be pretty unpredictable.
	 */
	if (hp->auth == DEFAULT) {
	    jmp_buf jmpbuf;
	    int     value;

	    setjmp (jmpbuf);
	    value = time(NULL);
	    for (i=0;  i < sizeof(jmpbuf)/sizeof(int);  i++)
		value ^= ((int *)jmpbuf)[i];
	    value = (value << 13) / 1000 * 1000;
	    if (value < 0)
		value = -value;
	    value += (getuid() % 1000);
	    ks->auth = hp->auth = value;
	    update++;
	} else
	    ks->auth = hp->auth;

	/* If KSAUTH is defined in the user environment this overrides the
	 * value given in the .irafhosts file.  This allows authorization
	 * codes to be dynamically allocated at login time if someone doesn't
	 * want to take the risk of having their authorization code in the
	 * irafhosts file.
	 */
	if ((authp = getenv(KSAUTH)) && (auth = atoi(authp)))
	    ks->auth = auth;

	/* The timeout value is the time in seconds after which the in.irafksd
	 * daemon will shutdown if idle.
	 */
	if (hp->timeout == DEFAULT) {
	    ks->timeout = DEF_TIMEOUT;
	} else if (hp->timeout < MIN_TIMEOUT) {
	    ks->timeout = DEF_TIMEOUT;
	    hp->timeout = DEFAULT;
	    update++;
	} else
	    ks->timeout = hp->timeout;

	/* Check for any node specific KS parameter overrides. */
	if (np->port)
	    ks->port = np->port;
	if (np->auth)
	    ks->auth = np->auth;
	if (np->hiport)
	    ks->hiport = np->hiport;
	if (np->timeout)
	    ks->timeout = np->timeout;
	if (np->protocol)
	    ks->protocol = np->protocol;

	dbgmsg1 ("ks.port = %d\n", ks->port);
	dbgmsg1 ("ks.hiport = %d\n", ks->hiport);
	dbgmsg1 ("ks.timeout = %d\n", ks->timeout);

	/* Update irafhosts if necessary. */
	if (update || (hp->mode & 077))
	    ks_whosts (hp, userfile);

	free ((char *)hp);
	return (0);
}


/* KS_USERNAME -- Convert the given filename into a user home directory
 * relative pathname.  A pointer to a buffer containing the pathname is
 * returned as the function value.  If the pointer "username" is nonnull
 * the user's name is returned as well.
 */
static char *
ks_username (char *filename, char *pathname, char *username)
{
	register struct passwd *pwd;

	pwd = getpwuid (getuid());
	if (pwd == NULL)
	    return (NULL);

	strcpy (pathname, pwd->pw_dir);
	strcat (pathname, "/");
	strcat (pathname, IRAFHOSTS);
	if (username)
	    strcpy (username, pwd->pw_name);

	endpwent();
	return (pathname);
}


/* KS_SYSNAME -- Convert the given filename into an iraf$dev pathname.
 * A pointer to a buffer containing the pathname is returned as the
 * function value.
 */
static char *
ks_sysname (char *filename, char *pathname)
{
	XCHAR	irafdir[SZ_PATHNAME+1];
	XINT	x_maxch=SZ_PATHNAME, x_nchars;
	extern  int  ZGTENV();


	ZGTENV ("iraf", irafdir, &x_maxch, &x_nchars);
	if (x_nchars <= 0)
	    return (NULL);

	strcpy (pathname, (char *)irafdir);
	strcat (pathname, HOSTLOGIN);

	return (pathname);
}


/* KS_RHOSTS -- Read the named irafhosts file into a descriptor, returning
 * the descriptor as the function value.
 */
static struct irafhosts *
ks_rhosts (char *filename)
{
	char	lbuf[SZ_LINE];
	char	word[SZ_LINE];
	struct	irafhosts *hp;
	struct	nodelist *np;
	struct	stat st;
	char	*ip, *op;
	int	value;
	FILE	*fp;

	dbgmsgs ("read %s\n", filename);

	/* Open irafhosts file. */
	if ((fp = fopen (filename, "r")) == NULL)
	    return (NULL);

	/* Get descriptor. */
	hp = (struct irafhosts *) malloc (sizeof(struct irafhosts));
	if (hp == NULL) {
	    fclose (fp);
	    return (NULL);
	}

	hp->port = DEFAULT;
	hp->auth = DEFAULT;
	hp->hiport = DEF_HIPORT;
	hp->timeout = DEF_TIMEOUT;
	hp->nheaderlines = 0;
	hp->nparams = 0;
	hp->nnodes = 0;
	hp->mode = 0;
	op = hp->sbuf;

	if (fstat (fileno(fp), &st) == 0)
	    hp->mode = st.st_mode;

	/* Get file header. */
	while (fgets (op, SZ_LINE, fp))
	    if (op[0] == '#' || isspace(op[0])) {
		hp->header[hp->nheaderlines++] = op;
		op += strlen(op) + 1;
	    } else {
		strcpy (lbuf, op);
		break;
	    }

	/* Everything else is a parameter assignment (param =), a node
	 * entry (node :), or ignored.
	 */
	do {
	    ip = lbuf;
	    if (*ip == '#' || isspace(*ip))
		continue;

	    ks_getword (&ip, word);
	    while (*ip && isspace(*ip))
		ip++;

	    if (*ip == '=') {
		for (ip++;  *ip && isspace(*ip);  ip++)
		    ;
		if (strncmp (ip, "default", 7) == 0)
		    value = DEFAULT;
		else if (isdigit (*ip))
		    value = atoi (ip);
		else
		    value = 0;

		if (strcmp (word, "port") == 0)
		    hp->port = value;
		else if (strcmp (word, "auth") == 0)
		    hp->auth = value;
		else if (strcmp (word, "hiport") == 0)
		    hp->hiport = value;
		else if (strcmp (word, "timeout") == 0)
		    hp->timeout = value;
		/* else disregard */

		hp->nparams++;

	    } else if (*ip == ':') {
		/* Node entry.
		 */
		np = &hp->node[hp->nnodes++];	/* nodename */
		strcpy (op, word);
		np->name = op;
		op += strlen(op) + 1;

		ip++;
		ks_getword (&ip, word);		/* loginname */
		strcpy (op, word);
		np->login = op;
		op += strlen(op) + 1;

		ks_getword (&ip, word);		/* password */
		strcpy (op, word);
		np->password = op;
		op += strlen(op) + 1;

		/* Process any optional networking paramaeter overrides.
		 * These are in the form param=value where param is port,
		 * auth, etc.
		 */
		np->port = 0;
		np->auth = 0;
		np->hiport = 0;
		np->timeout = 0;
		np->protocol = 0;

		while (ks_getword (&ip, word)) {
		    if (strncmp (word, "port=", 5) == 0) {
			np->port = atoi (word + 5);
		    } else if (strncmp (word, "auth=", 5) == 0) {
			np->auth = atoi (word + 5);
		    } else if (strncmp (word, "hiport=", 7) == 0) {
			np->hiport = atoi (word + 7);
		    } else if (strncmp (word, "timeout=", 8) == 0) {
			np->timeout = atoi (word + 8);
		    } else if (strncmp (word, "protocol=", 9) == 0) {
			if (strcmp (word + 9, "rsh") == 0)
			    np->protocol = C_RSH;
			else if (strcmp (word + 9, "rex") == 0)
			    np->protocol = C_REXEC;
			else if (strcmp (word + 9, "rcb") == 0)
			    np->protocol = C_REXEC_CALLBACK;
		    }
		}
	    }

	    hp->sbuflen = op - hp->sbuf;
	    if (hp->sbuflen + SZ_LINE > SZ_SBUF)
		break;

	} while (fgets (lbuf, SZ_LINE, fp));

	fclose (fp);
	return (hp);
}


/* KS_GETWORD -- Get a quoted or whitespace delimited word.
 */
static int
ks_getword (char **ipp, char *obuf)
{
	register char *ip = *ipp, *op = obuf;

	while (*ip && isspace(*ip))
	    ip++;

	if (*ip == '"') {
	    for (ip++;  *ip && *ip != '"';  )
		*op++ = *ip++;
	} else {
	    while (*ip && !isspace(*ip))
		*op++ = *ip++;
	}

	*op = EOS;
	*ipp = ip;
	return (op - obuf);
}


/* KS_WHOSTS -- Write out a hosts file from the internal descriptor to disk.
 */
static void
ks_whosts (
  struct irafhosts *hp,
  char 	*filename
)
{
	register char *ip;
	struct	nodelist *np;
	int	fd, q, i;
	FILE	*fp;

	dbgmsgs ("update %s\n", filename);

	/* Open new irafhosts file. */
	unlink (filename);
	if ((fd = creat (filename, 0600)) < 0)
	    return;
	if ((fp = fdopen(fd,"w")) == NULL) {
	    close (fd);
	    unlink (filename);
	    return;
	}

	/* Output any header comments. */
	for (i=0;  i < hp->nheaderlines;  i++)
	    fputs (hp->header[i], fp);

	/* Output the networking parameters. */
	if (hp->port == DEFAULT)
	    fprintf (fp, "port = default\n");
	else
	    fprintf (fp, "port = %d\n", hp->port);
	fprintf (fp, "auth = %d\n", hp->auth);
	if (hp->hiport == DEFAULT)
	    fprintf (fp, "hiport = default\n");
	else
	    fprintf (fp, "hiport = %d\n", hp->hiport);
	if (hp->timeout == DEFAULT)
	    fprintf (fp, "timeout = default\n");
	else
	    fprintf (fp, "timeout = %d\n", hp->timeout);
	fprintf (fp, "\n");

	/* Output each "node : login password" sequence, quoting the login
	 * and password strings if they contain any whitespace.
	 */
	for (i=0;  i < hp->nnodes;  i++) {
	    np = &hp->node[i];

	    /* Output username. */
	    fprintf (fp, "%s\t:", np->name);
	    for (q=0, ip=np->login;  *ip && !(q = isspace(*ip));  ip++)
		;
	    fprintf (fp, q ? " \"%s\"" : " %s", np->login);

	    /* Output password field. */
	    for (q=0, ip=np->password;  *ip && !(q = isspace(*ip));  ip++)
		;
	    fprintf (fp, q ? " \"%s\"" : " %s", np->password);

	    /* Add any optional parameter overrides given in the file when
	     * originally read.
	     */
	    if (np->port)
		fprintf (fp, " port=%d", np->port);
	    if (np->auth)
		fprintf (fp, " auth=%d", np->auth);
	    if (np->hiport)
		fprintf (fp, " hiport=%d", np->hiport);
	    if (np->timeout)
		fprintf (fp, " timeout=%d", np->timeout);
	    if (np->protocol) {
		fprintf (fp, " protocol=");
		switch (np->protocol) {
		case C_REXEC:
		    fprintf (fp, "rex");
		    break;
		case C_REXEC_CALLBACK:
		    fprintf (fp, "rcb");
		    break;
		default:
		    fprintf (fp, "rsh");
		    break;
		}
	    }

	    fprintf (fp, "\n");
	}

	fclose (fp);
}


/* KS_GETPASS -- Access the terminal in raw mode to get the user's
 * password.
 */
static char *ks_getpass (char *user, char *host)
{
	static	char password[SZ_NAME];
	char    prompt[80];
	int	tty, n;
	struct  termios tc, tc_save;

	if ((tty = open ("/dev/tty", 2)) == ERR)
	    return (NULL);

	sprintf (prompt, "Password (%s@%s): ", user, host);
	write (tty, prompt, strlen(prompt));

	tcgetattr (tty, &tc);
	tc_save = tc;
	 
	tc.c_lflag &=
	    ~(0 | ECHO | ECHOE | ECHOK | ECHONL);
	tc.c_oflag |=
	    (0 | TAB3 | OPOST | ONLCR);
	tc.c_oflag &=
	    ~(0 | OCRNL | ONOCR | ONLRET);

	tc.c_cc[VMIN] = 1;
	tc.c_cc[VTIME] = 0;
	tc.c_cc[VLNEXT] = 0;

	tcsetattr (tty, TCSADRAIN, &tc);

	n = read (tty, password, SZ_NAME);
	write (tty, "\n", 1);

	tcsetattr (tty, TCSADRAIN, &tc_save);

	close (tty);

	if (n <= 0)
	    return (NULL);
	else
	    password[n-1] = EOS;

	return (password);
}


/* PR_MASK -- Debug routine to print the current SIGCHLD mask.
 */
void pr_mask (char *str)
{
	sigset_t	sigset, pending;

	if (sigprocmask (0, NULL, &sigset) < 0)
	    dbgmsg ("sigprocmask error");

	dbgmsg (str);
	if (sigismember (&sigset, SIGCHLD))
	    dbgmsg ("pr_mask: SIGCHLD set\n");

	if (sigpending (&pending) < 0)
	    dbgmsg ("sigpending error");
	if (sigismember (&pending, SIGCHLD))
	    dbgmsg ("\tpr_mask: SIGCHLD pending\n"); 
}
