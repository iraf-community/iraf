/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <setjmp.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <pwd.h>

#ifdef SYSV
#include <termio.h>
#else
#include <sgtty.h>
#endif

#define	import_kernel
#define	import_knames
#define	import_zfstat
#define	import_prtype
#define import_spp
#include <iraf.h>

#ifdef AUX
#undef PFI
#define PFI sigfunc_t
#endif

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

extern	int errno;
extern	int save_prtype;

#define	SZ_NAME		32		/* max size node, etc. name	  */
#define	SZ_CMD		256		/* max size rexec sh command	  */
#define	MAXCONN		1		/* for listen (### DSUX=1)	  */
#define	SELWIDTH	32		/* number of bits for select	  */
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

#ifdef SYSV
#define	RSH		"remsh"		/* typical names are rsh, remsh   */
#else
#define	RSH		"rsh"		/* typical names are rsh, remsh   */
#endif

#define	IRAFKS_DIRECT	0		/* direct connection (via rexec)  */
#define	IRAFKS_DAEMON	1		/* in.irafksd daemon process	  */
#define	IRAFKS_SERVER	2		/* irafks server process	  */
#define	IRAFKS_CLIENT	3		/* zfioks client process	  */

struct	ksparam {
	int	auth;			/* user authorization code	  */
	int	port;			/* in.irafksd port		  */
	int	hiport;			/* in.irafksd port region	  */
	int	timeout;		/* in.irafksd idle timeout, sec   */
};

int	debug_ks = 0;			/* print debug info on stderr	  */
FILE	*debug_fp = NULL;		/* debugging output		  */
char	debug_tty[20] = "";		/* debug output file if nonnull   */

extern	int getuid();
extern	char *getenv();
static	jmp_buf jmpbuf;
static	int jmpset = 0;
static	int recursion = 0;
static	PFI old_sigcld;
static	int ks_getresvport(), ks_rexecport();
static	int ks_socket(), ks_geti(), ks_puti(), ks_getlogin();
static	dbgmsg(), dbgmsg1(), dbgmsg2(), dbgmsg3(), dbgmsg4();
static	char *ks_getpass();
static	ks_onsig();


/* ZOPNKS -- Open a connected subprocess on a remote node.  Parse the "server"
 * argument to obtain the node name and the command to be issued to connect the
 * remote process.  Set up a socket to be used for communications with the
 * remote irafks kernel server.  The "server" string is implementation
 * dependent and normally comes from the file dev$hosts.  This file is read
 * by the high level VOS code before we are called.
 */
ZOPNKS (server, mode, chan)
PKCHAR	*server;		/* type of connection */
XINT	*mode;			/* access mode (not used) */
XINT	*chan;			/* receives channel code (socket) */
{
	register char	*ip, *op;
	char	host[SZ_NAME+1], username[SZ_NAME+1], password[SZ_NAME+1];
	int	proctype, port, auth, s_port, pid, s;
	struct  sockaddr_in sockaddr, from;
	char	*hostp, *cmd;
	char	obuf[SZ_LINE];
	struct	ksparam ks;

	/* Debug output.  If debug_ks is set (e.g. with adb) but no filename
	 * is given, debug output goes to stderr.  If a filename is given it
	 * is interpreted as a tty device name unless a full pathname is given.
	 */
	if (debug_ks && !debug_fp) {
	    if (debug_tty[0] != EOS) {
		char  tty[30];

		if (debug_tty[0] == '/')
		    strcpy (tty, debug_tty);
		else
		    sprintf (tty, "/dev/%s", debug_tty);

		if ((debug_fp = fopen (tty, "a")) == NULL)
		    debug_fp = stderr;
	    } else
		debug_fp = stderr;
	}
	dbgmsg1 ("zopnks (`%s')\n", (char *)server);

	/* Parse the server specification.  There are two possible variations
	 * on this, depending upon whether we are setting up the irafks daemon
	 * or the zfioks client.
	 *
	 *	(null)				directly connected (via rexec)
	 *      in.irafksd			daemon process (via rsh)
	 *      host!irafks-command		zfioks client connection
	 */
	if (!*((char *)server))
	    proctype = IRAFKS_DIRECT;
	else if (strcmp ((char *)server, "in.irafksd") == 0)
	    proctype = IRAFKS_DAEMON;
	else {
	    proctype = IRAFKS_CLIENT;
	    cmd = NULL;
	    for (op=host, ip = (char *)server;  *ip != EOS;  ip++)
		if (*ip == FNNODE_CHAR) {
		    *op = EOS;
		    cmd = ip + 1;
		    break;
		} else
		    *op++ = *ip;
	    if (cmd == NULL) {
		*chan = ERR;
		goto done;
	    }
	}

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
	    int     nsec, rfd, fd, sig;
	    int     once_only = 0;
	    int     unauth = 0;
	    int     status = 0;

	    /* Get client data. */
	    if ((req_port = port = ks_geti(0)) < 0)
		{ status = 1; goto d_err; }
	    if ((auth = ks_geti(0)) < 0)
		{ status = 2; goto d_err; }
	    if ((nsec = ks_geti(0)) < 0)
		{ status = 3; goto d_err; }

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
	    } else if (port != req_port)
		once_only++;

	    /* Fork daemon process and return if parent, exiting rsh. */
	    dbgmsg2 ("fork in.irafksd, port=%d, timeout=%d\n", port, nsec);
	    pid = fork();
	    if (pid < 0) {
		status = 4;
		goto d_err;
	    }

	    if (pid) {
d_err:		dbgmsg1 ("in.irafksd parent exit, status=%d\n", status);
		ks_puti (1, status);
		ks_puti (1, port);
		exit(0);
	    }

	    /*
	     * What follows is the code for the daemon process.  Close the
	     * stdio streams, which we won't need any more.  Create a socket
	     * and bind it to the given port.  Sit in a loop until timeout,
	     * listening for client connection requests and forking the irafks
	     * server in response to each such request.
	     */

	    dbgmsg1 ("in.irafksd started, pid=%d\n", getpid());
	    old_sigcld = (PFI) signal (SIGCLD, (PFI)ks_onsig);

	    /* Reset standard streams to console to record error messages. */
	    fd = open ("/dev/null", 0);     close(0);  dup(fd);  close(fd);
	    fd = open ("/dev/console", 1);  close(1);  dup(fd);  close(fd);
	    fd = open ("/dev/console", 1);  close(2);  dup(fd);  close(fd);

	    /* Loop forever or until the idle timeout expires, waiting for a
	     * client connection.
	     */
	    for (;;) {
		timeout.tv_sec = nsec;
		timeout.tv_usec = 0;
		rfd = (1 << s); 
		status = 0;

		/* Wait until either we get a connection, or a previously
		 * started server exits.
		 */
		jmpset++;
		if (sig = setjmp(jmpbuf)) {
		    if (sig == SIGCLD)
			wait (NULL);
		    else
			exit (0);
		}
		if (select (SELWIDTH, &rfd, NULL, NULL, &timeout) <= 0)
		    exit (0);

		/* Accept the connection. */
		if ((fd = accept (s, (struct sockaddr *)0, (int *)0)) < 0) {
		    fprintf (stderr,
			"in.irafksd: accept on port %d failed\n", port);
		    exit (2);
		} else
		    dbgmsg ("in.irafksd: connection established\n");

		/* Find out where the connection is coming from. */
		fromlen = sizeof (from);
		if (getpeername (fd, &from, &fromlen) < 0) {
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
		dbgmsg1 ("in.irafksd: client port = %d\n", s_port);

		/* Fork the iraf kernel server. */
		pid = fork();
		if (pid < 0) {
		    fprintf (stderr, "in.irafksd: process creation failed\n");
		    status = 3;
		    goto s_err;
		}

		if (pid) {
s_err:		    dbgmsg1 ("in.irafksd fork complete, status=%d\n",
			status);
		    ks_puti (fd, status);
		    close (fd);
		    if (once_only)
			exit (0);
		    /* otherwise loop indefinitely */

		} else {
		    /* Set up iraf kernel server. */
		    u_long  n_addr, addr;
		    unsigned char *ap = (unsigned char *)&n_addr;

		    dbgmsg1 ("irafks server started, pid=%d\n", getpid());
		    signal (SIGCLD, old_sigcld);
		    close (fd); close (s);

		    n_addr = from.sin_addr.s_addr;
		    addr = ntohl(n_addr);
		    sprintf (obuf, "%d.%d.%d.%d", ap[0],ap[1],ap[2],ap[3]);
		    dbgmsg2 ("client address=%s port=%d\n", obuf, s_port);

		    if ((s = ks_socket (NULL, addr, s_port, "connect")) < 0) {
			dbgmsg1 ("irafks connect to port %d failed\n", s_port);
			fprintf (stderr, "irafks: cannot connect to client\n");
			exit (1);
		    } else
			dbgmsg1 ("irafks connected on port %d\n", s_port);

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

	} else if (*password) {
	    /* Password given; use rexec protocol. */
	    hostp = host;
	    dbgmsg2 ("rexec for host=%s, user=%s\n", host, username);
	    *chan = rexec (&hostp, ks_rexecport(), username, password, cmd, 0);

	} else {
	    /* Use the default protocol, which avoids passwords.  This uses
	     * rsh to start up (once) the iraf networking daemon in.irafksd
	     * on the remote node, and thereafter merely places requests to
	     * in.irafksd to spawn each instance of the irafks.e server.
	     */
	    struct  hostent *hp;
	    struct  sockaddr *ap;
	    char    command[SZ_LINE];
	    int     pin[2], pout[2];
	    int     status = 0;
	    int     ntries = 0;
	    char    *password;
	    int     fd, tfd;
	    int     t, s;

	    /* Get reserved port for client. */
	    s_port = IPPORT_USERRESERVED - 1;
	    s = ks_getresvport (&s_port);
	    if (s < 0) {
		status |= 01;
		goto c_err;
	    }
	    dbgmsg2 ("connect to in.irafksd host=%s client port=%d\n",
		host, s_port);

	    /* Ready to receive callback from server. */
	    if (listen (s, MAXCONN) < 0) {
		status |= 02;
		goto c_err;
	    }

	    /* in.irafkd port. */
	    port = ks.port;
again:
	    /* Connect to in.irafksd daemon on server system and send request
	     * to start up a new irafks daemon on the port just created.  If
	     * the connection fails, fork an rsh and start up the in.irafksd.
	     */
	    if (!port || (t = ks_socket (host, NULL, port, "connect")) < 0) {
		dbgmsg ("no server, fork rsh to start in.irafksd\n");

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
		    dbgmsg2 ("send port=%d, timeout=%d to irafks.e\n",
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
		    dbgmsg1 ("irafks.e returns port=%d\n", port);

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

			if (ntries++) {
			    status |= 0400;
			    goto c_err;
			}

			dbgmsg ("rsh failed - try rexec\n");
			if (!(password = ks_getpass (username, host)))
			    { status |= 01000;  goto c_err; }

			sprintf (command, "%s in.irafksd", cmd);
			dbgmsg3 ("rexec %s@%s: %s\n", username, host, command);

			hostp = host;
			fd = rexec (&hostp, ks_rexecport(),
			    username, password, command, NULL);

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
		    close (pin[0]);  close (pout[1]);
		    close (0);  dup (pout[0]);  close (pout[0]);
		    close (1);  dup (pin[1]);   close (pin[1]);

		    dbgmsg2 ("exec rsh %s %s in.irafksd\n", host, cmd);
		    execlp (RSH, RSH, host, cmd, "in.irafksd", NULL);
		    exit (1);
		}
	    }

	    /* Send command to start up irafks server.  This consists of the
	     * reserved port for the server connection followed by the
	     * authorization code.  The in.irafksd daemon returns a status
	     * byte which will be zero if the operation is successful.
	     */
	    dbgmsg1 ("request irafks server for client port %d\n", s_port);

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
	    if (status = ks_geti(t))
		if (port && status == UNAUTH) {
		    close(t);
		    port = 0;
		    dbgmsg ("authorization failed, retry with port=0\n");
		    status = 0;
		    goto again;
		} else {
		    status |= 020000;
		    goto c_err;
		}

	    /* Wait for the server to call us back. */
	    if ((tfd = accept (s, (struct sockaddr *)0, (int *)0)) < 0) {
c_err:		dbgmsg1 ("zfioks client status=%o\n", status);
		close(t);  close(s);
		kill (pid, SIGTERM);
		*chan = ERR;
	    } else {
		close(t);  close(s);  fd = dup(tfd);  close(tfd);
		dbgmsg1 ("connected to irafks server on fd=%d\n", fd);
		*chan = fd;
	    }
	}

done:
	jmpset = 0;
	if (*chan > 0)
	    if (*chan < MAXOFILES)
		zfd[*chan].nbytes = 0;
	    else {
		close (*chan);
		*chan = ERR;
	    }

	dbgmsg1 ("zopnks returns status=%d\n", *chan);
}


/* ZCLSKS -- Close a kernel server connection.
 */
ZCLSKS (chan, status)
XINT	*chan;				/* socket to kernel server	*/
XINT	*status;			/* receives close status	*/
{
	*status = close (*chan);
	dbgmsg2 ("server [%d] terminated, status = %d\n", *chan, *status);
}


/* ZARDKS -- Read from the kernel server channel.  No attempt is made to
 * impose a record structure upon the channel, as is the case with IPC.
 * In UNIX the channel is stream oriented and it is up to the caller to
 * unblock records from the input stream.  Data blocks are assumed to be
 * preceded by headers telling how much data to read, hence we read from
 * the channel until the specified number of bytes have been read or ERR
 * or EOF is seen on the stream.
 */
ZARDKS (chan, buf, totbytes, loffset)
XINT	*chan;			/* kernel server channel (socket)	*/
XCHAR	*buf;			/* output buffer			*/
XINT	*totbytes;		/* total number of bytes to read	*/
XLONG	*loffset;		/* not used				*/
{
#ifdef ANSI
	volatile char	*op;
	volatile int	fd, nbytes;
#else
	char	*op;
	int	fd, nbytes;
#endif
	PFI	sigint, sigterm;
	int	status;

	fd = *chan;
	op = (char *)buf;
	zfd[fd].nbytes = nbytes = *totbytes;
	dbgmsg2 ("initiate read of %d bytes from KS channel %d\n", nbytes, fd);

	/* Now read exactly nbytes of data from channel into user buffer.
	 * Return actual byte count if EOF is seen.  If ERR is seen return
	 * ERR.  If necessary multiple read requests are issued to read the
	 * entire record.  Reads are interruptable but the interrupt is caught
	 * and returned as a read error on the server channel.
	 */
	sigint  = (PFI) signal (SIGINT,  (PFI)ks_onsig);
	sigterm = (PFI) signal (SIGTERM, (PFI)ks_onsig);

	while (nbytes > 0) {
	    jmpset++;
	    if (setjmp (jmpbuf) == 0)
		status = read (fd, op, nbytes);
	    else
		status = ERR;

	    switch (status) {
	    case 0:
		zfd[fd].nbytes -= nbytes;
		return;
	    case ERR:
		zfd[fd].nbytes = ERR;
		return;
	    default:
		nbytes -= status;
		op += status;
		break;
	    }
	}

	jmpset = 0;
	signal (SIGINT,  sigint);
	signal (SIGTERM, sigterm);
	dbgmsg2 ("read %d bytes from KS channel %d:\n", op - (char *)buf, fd);
}


/* ZAWRKS -- Write to a kernel server channel.
 */
ZAWRKS (chan, buf, totbytes, loffset)
XINT	*chan;			/* kernel server channel (socket)	*/
XCHAR	*buf;			/* output buffer			*/
XINT	*totbytes;		/* number of bytes to write		*/
XLONG	*loffset;		/* not used				*/
{
	PFI	sigint, sigterm, sigpipe;
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
	dbgmsg2 ("initiate write of %d bytes to KS channel %d\n", nbytes, ofd);

	/* Write exactly nbytes of data to the channel from user buffer to
	 * the channel.  Block interrupt during the write to avoid corrupting
	 * the data stream protocol if the user interrupts the client task.
	 * Trap SIGPIPE and return it as a write error on the channel instead.
	 * Likewise, turn an interrupt into a write error on the channel.
	 */
	sigint  = (PFI) signal (SIGINT,  (PFI)ks_onsig);
	sigterm = (PFI) signal (SIGTERM, (PFI)ks_onsig);
	sigpipe = (PFI) signal (SIGPIPE, (PFI)ks_onsig);
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
	dbgmsg2 ("wrote %d bytes to KS channel %d:\n", zfd[fd].nbytes, ofd);
}


/* KS_ONSIG -- Catch a signal.
 */
static
ks_onsig (sig, code, scp)
int	sig;			/* signal which was trapped	*/
int	code;			/* subsignal code (vax)		*/
struct	sigcontext *scp;	/* not used			*/
{
	/* If we get a SIGPIPE writing to a server the server has probably
	 * died.  Make it look like there was an i/o error on the channel.
	 */
	if (sig == SIGPIPE && recursion++ == 0)
	    fputs ("kernel server process has died\n", stderr);

	if (jmpset)
	    longjmp (jmpbuf, sig);
}


/* ZAWTKS -- Wait for i/o to a KS channel.  Since UNIX i/o is not asynchronous
 * we do not really wait, rather we return the status value (byte count) from
 * the last read or write to the channel.
 */
ZAWTKS (chan, status)
XINT	*chan;
XINT	*status;
{
	if ((*status = zfd[*chan].nbytes) == ERR)
	    *status = XERR;
}


/* ZSTTKS -- Get binary file status for an KS channel.  A KS channel is a
 * streaming binary file.
 */
ZSTTKS (chan, param, lvalue)
XINT	*chan;			/* not used; all KS channels have same status */
XINT	*param;
XLONG	*lvalue;
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
}


/*
 * Internal routines.
 * -------------------
 */

/* KS_SOCKET -- Get a socket configured for the given host and port.  Either
 * bind the socket to the port and ready for connections, or connect to the
 * remote socket at the given address.
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
	dbgmsg1 ("ks_socket: errno=%d\n", errno);
	close (s);
	return (ERR);
}


/* KS_GETRESVPORT -- Open a socket and attempt to bind it to the given port.
 * Locate a usable port if this fails.  The actual report is returned in the
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
	    if (bind(s, (caddr_t)&sin, sizeof (sin)) >= 0)
		return (s);
	    if (errno != EADDRINUSE) {
		(void) close(s);
		return (-1);
	    }
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

	if (sv = getservbyname ("exec", "tcp"))
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
	int	stat, rfd, sig;
	char	ch;

	jmpset++;
	if (sig = setjmp(jmpbuf))
	    if (sig == SIGCLD)
		wait (NULL);

	timeout.tv_sec = PRO_TIMEOUT;
	timeout.tv_usec = 0;
	rfd = (1 << fd); 

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
		dbgmsg2 ("ks_geti: read status=%d, errno=%d\n", stat, errno);
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
		dbgmsg2 ("ks_gets: read status=%d, errno=%d\n", stat, errno);
		return (ERR);
	    }
	} while (*op++);

	return (op - outstr - 1);
}
 */


/* KS_MSG -- Print debugging messages.
 */
static
dbgmsg (msg)
char    *msg;
{
	if (debug_ks) {
	    fprintf (debug_fp, "[%5d] ", getpid());
	    fprintf (debug_fp, msg);
	}
}
static
dbgmsg1 (fmt, arg)
char    *fmt;
int     arg;
{
	if (debug_ks) {
	    fprintf (debug_fp, "[%5d] ", getpid());
	    fprintf (debug_fp, fmt, arg);
	}
}
static
dbgmsg2 (fmt, arg1, arg2)
char    *fmt;
int     arg1, arg2;
{
	if (debug_ks) {
	    fprintf (debug_fp, "[%5d] ", getpid());
	    fprintf (debug_fp, fmt, arg1, arg2);
	}
}
static
dbgmsg3 (fmt, arg1, arg2, arg3)
char    *fmt;
int     arg1, arg2, arg3;
{
	if (debug_ks) {
	    fprintf (debug_fp, "[%5d] ", getpid());
	    fprintf (debug_fp, fmt, arg1, arg2, arg3);
	}
}
static
dbgmsg4 (fmt, arg1, arg2, arg3, arg4)
char    *fmt;
int     arg1, arg2, arg3, arg4;
{
	if (debug_ks) {
	    fprintf (debug_fp, "[%5d] ", getpid());
	    fprintf (debug_fp, fmt, arg1, arg2, arg3, arg4);
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
	} node[MAX_NODES];
	int	sbuflen;
	char	sbuf[SZ_SBUF];
};

static	int ks_getword();
static	char *ks_username(), *ks_sysname();
static	struct irafhosts *ks_rhosts();
static	ks_whosts();


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
ks_getlogin (hostname, loginname, password, ks)
char	*hostname;		/* node we wish a login for */
char	*loginname;		/* receives the login name */
char	*password;		/* receives the login password */
struct	ksparam *ks;		/* networking parameters */
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
	if (hp = ks_rhosts (userfile)) {
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
		password[0] = EOS;
	    } else if (strcmp (np->password, "?") == 0) {
		if (namep = ks_getpass (loginname, hostname))
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
ks_username (filename, pathname, username)
char	*filename;
char	*pathname;
char	*username;
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
ks_sysname (filename, pathname)
char	*filename;
char	*pathname;
{
	XCHAR	irafdir[SZ_PATHNAME+1];
	int	x_maxch=SZ_PATHNAME, x_nchars;

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
ks_rhosts (filename)
char	*filename;
{
	char	lbuf[SZ_LINE];
	char	word[SZ_LINE];
	struct	irafhosts *hp;
	struct	nodelist *np;
	struct	stat st;
	char	*ip, *op;
	int	value;
	FILE	*fp;

	dbgmsg1 ("read %s\n", filename);

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
ks_getword (ipp, obuf)
char	**ipp;
char	*obuf;
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
static
ks_whosts (hp, filename)
struct	irafhosts *hp;
char	*filename;
{
	register char *ip;
	struct	nodelist *np;
	int	fd, q, i;
	FILE	*fp;

	dbgmsg1 ("update %s\n", filename);

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
	    fprintf (fp, "%s\t:", np->name);
	    for (q=0, ip=np->login;  *ip && !(q = isspace(*ip));  ip++)
		;
	    fprintf (fp, q ? " \"%s\"" : " %s", np->login);
	    for (q=0, ip=np->password;  *ip && !(q = isspace(*ip));  ip++)
		;
	    fprintf (fp, q ? " \"%s\"" : " %s", np->password);
	    fprintf (fp, "\n");
	}

	fclose (fp);
}


/* KS_GETPASS -- Access the terminal in raw mode to get the user's
 * password.
 */
static char *
ks_getpass (user, host)
char	*user;
char	*host;
{
	static	char password[SZ_NAME];
	char    prompt[80];
	int	tty, n;
#ifdef SYSV
	struct  termio tc, tc_save;
#else
	struct  sgttyb ttystat;
	int     sg_flags;
#endif

	if ((tty = open ("/dev/tty", 2)) == ERR)
	    return (NULL);

	sprintf (prompt, "Password (%s@%s): ", user, host);
	write (tty, prompt, strlen(prompt));

#ifdef SYSV
	ioctl (tty, TCGETA, &tc);
	tc_save = tc;
	tc.c_lflag &= ~ECHO;
	ioctl (tty, TCSETAF, &tc);
#else
	ioctl (tty, TIOCGETP, &ttystat);
	sg_flags = ttystat.sg_flags;
	ttystat.sg_flags &= ~ECHO;
	ioctl (tty, TIOCSETP, &ttystat);
#endif

	n = read (tty, password, SZ_NAME);
	write (tty, "\n", 1);

#ifdef SYSV
	ioctl (tty, TCSETAF, &tc_save);
#else
	ttystat.sg_flags = sg_flags;
	ioctl (tty, TIOCSETP, &ttystat);
#endif

	close (tty);

	if (n <= 0)
	    return (NULL);
	else
	    password[n-1] = EOS;

	return (password);
}
