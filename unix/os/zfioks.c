/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <setjmp.h>
#include <netdb.h>
#include <sgtty.h>
#include <pwd.h>

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
 * In the Berkeley UNIX environment the network interface is TCP/IP.  We use
 * the UNIX utility subroutine REXEC and associated daemon REXECD to spawn
 * the IRAF kernel server process on the remote node and to manage
 * communications with the remote server process.  Other ways of implementing
 * the interface are certainly possible; this method was selected because it
 * appeared to be the simplest given the UNIX environment, and tests show
 * that the bandwidth to be expected is reasonable (> 50 Kb / sec).
 */

extern	int save_prtype;

#define	SZ_NAME		32		/* max size node, etc. name	  */
#define	SZ_CMD		128		/* max size rexec sh command	  */
#define	FNNODE_CHAR	'!'		/* node name delimiter		  */
#define	IRAFHOSTS	".irafhosts"	/* user host login file		  */
#define	HOSTLOGIN	"dev/hostlogin"	/* dev$hostlogin file filename	  */
#define	USER		"<user>"	/* symbol for user's login name   */

int	debug_ks = 0;			/* print debug info on stderr	  */
FILE	*debug_fp = stderr;		/* debugging output		  */
char	debug_tty[10] = "";		/* debug output file if nonnull   */

static	jmp_buf jmpbuf;
static	int recursion = 0;


/* ZOPNKS -- Open a connected subprocess on a remote node.  Parse the "server"
 * argument to obtain the node name and the command to be issued to connect the
 * remote process.  Call REXEC to exec the remote process and set up a socket
 * to be used for CLIN, CLOUT to the remote process.  The "server" string is
 * implementation dependent and normally comes from the file "dev$hosts" on each
 * node.  This file is read by the high level code before we are called.
 */
ZOPNKS (server, mode, chan)
PKCHAR	*server;		/* node name ! command			*/
XINT	*mode;			/* access mode (not used)		*/
XINT	*chan;			/* receives channel code (socket)	*/
{
	register char	*ip;
	char	username[SZ_NAME+1], password[SZ_NAME+1];
	static	int ex_inport, sh_inport;
	char	*host, *cmd;
	struct	passwd *pwd;

	/* If a tty device number is given form the filename of the tty and
	 * direct debugging output there.
	 */
	if (debug_ks) {
	    if (debug_tty[0] != EOS) {
		char  tty[30];

		if (debug_tty[0] == '/')
		    strcpy (tty, debug_tty);
		else
		    sprintf (tty, "/dev/tty%s", debug_tty);

		if ((debug_fp = fopen (tty, "a")) == NULL)
		    debug_fp = stderr;
	    }

	    fprintf (debug_fp, "zopnks (`%s')", (char *)server);
	    fflush (debug_fp);
	}

	/* Extract the host name and remote process spawn command from the
	 * server string, format "host!cmd", e.g., "2!/iraf/lib/irafks.e".
	 * If the server is "host", we are being called from a server process
	 * to set up communications with the host process.  The UNIX rexec
	 * connects the host to the process standard input and output, hence
	 * if the server is "host" the channels are already active.
	 */
	if (strcmp ((char *)server, "host") == 0) {
	    *chan = 0;
	    return;
	}

	host = (char *)server;
	cmd  = NULL;

	for (ip = (char *)server;  *ip != EOS;  ip++)
	    if (*ip == FNNODE_CHAR) {
		*ip = EOS;
		cmd = ip + 1;
		break;
	    }
	if (cmd == NULL) {
	    *chan = ERR;
	    return;
	}

	/* Attempt to fire up the kernel server process.  An equivalent hosts
	 * connection (no login/password entry required) is best if permitted,
	 * otherwise we must do login authentication via rexec.  Unfortunately,
	 * superuser permission for the process calling us is required to make
	 * such a connection, hence this cannot be used.
	 *
	if (!sh_inport)
	    sh_inport = (getservbyname ("shell", "tcp")) -> s_port;
	pwd = getpwuid (getuid());
	*chan = rcmd (&host, ex_inport, pwd->pw_name, pwd->pw_name, cmd, 0);
	 */

	if (!ex_inport)
	    ex_inport = (getservbyname ("exec", "tcp")) -> s_port;

	/* Get login name and password and try to connect */
	if (ks_getlogin (host, username, password) == ERR)
	    *chan = ERR;
	else
	    *chan = rexec (&host, ex_inport, username, password, cmd, 0);

	if (*chan > 0)
	    if (*chan < MAXOFILES)
		zfd[*chan].nbytes = 0;
	    else {
		close (*chan);
		*chan = ERR;
	    }

	if (debug_ks) {
	    fprintf (debug_fp, " [%d]\n", *chan);
	    fflush (debug_fp);
	}
}


/* KS_GETLOGIN -- Get the user's login name and password, required for
 * authentication on the remote machine.  We could get these from the unix
 * password file on the local machine, but there is no guarantee that the
 * login name and password would be the same on a remote node as on the
 * local machine.  Instead we look in the user's unix login directory for
 * the file ".irafhosts".  If this file cannot be opened or if it does not
 * contain an entry for the named node we use a default public login.  The
 * public login provides sufficient priviledge for most operations but will
 * not provide write access to the user's files on the remote node.
 */
ks_getlogin (node, username, password)
char	*node;			/* node we wish a login for	*/
char	*username;		/* receives the login name	*/
char	*password;		/* receives the login password	*/
{
	XCHAR	irafdir[SZ_PATHNAME+1];
	char	fname[SZ_FNAME+1], user[SZ_FNAME+1];
	int	x_maxch=SZ_PATHNAME, x_nchars;
	struct	passwd *pwd;

	pwd = getpwuid (getuid());
	if (pwd != NULL) {
	    /* Scan user's .irafhosts file, if found.
	     */
	    register char *p;

	    /* Set the default user login name. */
	    strcpy (user, pwd->pw_name);

	    /* Get .irafhosts path in user's home directory. */
	    strcpy (fname, pwd->pw_dir);
	    endpwent();

	    for (p=fname;  *p != EOS;  p++)
		;
	    if (*(p-1) != '/') {
		*p++ = '/';
		*p = EOS;
	    }
	    strcat (fname, IRAFHOSTS);

	    if (ks_scanlogin (fname, node, user, username, password) == OK)
		return (OK);
	}

	/* Return a default public login on the remote node.
	 */
	ZGTENV ("iraf", irafdir, &x_maxch, &x_nchars);
	strcpy (fname, (char *)irafdir);
	strcat (fname, HOSTLOGIN);

	return (ks_scanlogin (fname, node, user, username, password));
}


/* KS_SCANLOGIN -- Open and scan a host login file, returning the login
 * name and password to be used on the named node.  The format of the table
 * is a series of lines of the form
 *
 *	alias1 alias2 ... aliasN : loginname password
 *
 * If the same login name and password are used on several nodes, a single
 * entry may be given for all.  If the alias "*" is encountered scanning stops
 * and the next login name and password are used.  The table file should of
 * course be protected from reading except by the owner.  If even this is
 * considered too dangerous, the password "?" may be given in the table and a 
 * runtime query will result - this will fail if one is no longer logged in.
 */
ks_scanlogin (fname, node, user, username, password)
char	*fname;			/* table file			*/
char	*node;			/* node name			*/
char	*user;			/* user's name on local node	*/
char	*username;		/* receives user login name	*/
char	*password;		/* receives user password	*/
{
	char	*ip;
	char	lbuf[SZ_LINE+1];
	char	wbuf[SZ_NAME+1];
	FILE	*fp;
	int	foundit;

	foundit = 0;
	if ((fp = fopen (fname, "r")) == NULL)
	    return (ERR);

	/* Scan file for line containing node name.
	 */
	while (!foundit && fgets (lbuf, SZ_LINE, fp) != NULL) {
	    /* Skip blank lines and comment lines */
	    for (ip=lbuf;  isspace(*ip);  ip++)
		;
	    if (*ip == '#' || *ip == EOS)
		continue;

	    /* Scan list of aliases */
	    while (ks_getword (&ip, wbuf) > 0) {
		if (strcmp (wbuf, ":") == 0) {
		    break;
		} else if (strcmp(wbuf,"*")==0 || strcmp(wbuf,node)==0) {
		    foundit++;
		    break;
		}
	    }
	}

	fclose (fp);
	if (!foundit)
	    return (ERR);

	/* Skip to end of alias list. */
	while (ks_getword (&ip, wbuf) > 0) {
	    if (strcmp (wbuf, ":") == 0) {
		/* Return login name and password.
		 */
		if (ks_getword (&ip, username) <= 0)
		    return (ERR);
		if (ks_getword (&ip, password) <= 0)
		    return (ERR);

		/* If the login name is the special string denoting the
		 * local user name, use the local name.
		 */
		if (strcmp (username, USER) == 0)
		    strcpy (username, user);

		/* If the password is given as "?", query the user for
		 * the actual password.
		 */
		if (strcmp (password, "?") == 0) {
		    char    prompt[80];
		    struct  sgttyb ttystat;
		    int	    tty, sg_flags, n;

		    if ((tty = open ("/dev/tty", 2)) == ERR)
			return (ERR);
		    else if (save_prtype == PR_DETACHED)
			return (ERR);

		    sprintf (prompt, "Password (%s@%s): ", username, node);
		    write (tty, prompt, strlen(prompt));

		    ioctl (tty, TIOCGETP, &ttystat);
		    sg_flags = ttystat.sg_flags;
		    ttystat.sg_flags &= ~ECHO;
		    ioctl (tty, TIOCSETP, &ttystat);

		    n = read (tty, password, SZ_NAME);
		    write (tty, "\n", 1);

		    ttystat.sg_flags = sg_flags;
		    ioctl ((int)tty, TIOCSETP, &ttystat);

		    close (tty);

		    if (n <= 0)
			return (ERR);
		    else
			password[n-1] = EOS;
		}

		return (OK);		/* SUCCESS */
	    }
	}

	return (ERR);
}


/* KS_GETWORD -- Get the next whitespace or : delimited word from the
 * input string.
 */
ks_getword (ip, obuf)
char	**ip;			/* pointer into input buffer	*/
char	*obuf;			/* receives name		*/
{
	register char	*cp, *op;
	register int	n;

	for (cp = *ip;  isspace(*cp);  cp++)
	    ;

	op = obuf;
	n  = 0;

	if (*cp == ':' || *cp == '*' || *cp == '?') {
	    *op++ = *cp++;
	    n++;
	} else {
	    while (*cp && !isspace(*cp) && !(*cp==':' || *cp=='*' || *cp=='?'))
		if (n++ >= SZ_NAME)
		    return (ERR);
		else
		    *op++ = *cp++;
	}

	*op = EOS;
	*ip = cp;

	return (n);
}


/* ZCLSKS -- Close a kernel server connection.
 */
ZCLSKS (chan, status)
XINT	*chan;				/* socket to kernel server	*/
XINT	*status;			/* receives close status	*/
{
	*status = close (*chan);

	if (debug_ks) {
	    fprintf (debug_fp, "server [%d] terminated, status = %d\n",
		*chan, *status);
	    fflush (debug_fp);
	}
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
	register char	*op;
	register int	fd, nbytes;
	PFI	sigint, sigterm;
	int	status;
	extern	pr_onsig();

	fd = *chan;
	op = (char *)buf;
	zfd[fd].nbytes = nbytes = *totbytes;

	if (debug_ks) {
	    fprintf (debug_fp,
		"initiate read for %d bytes from KS channel %d\n",
		nbytes, fd);
	    fflush (debug_fp);
	}

	/* Now read exactly nbytes of data from channel into user buffer.
	 * Return actual byte count if EOF is seen.  If ERR is seen return
	 * ERR.  If necessary multiple read requests are issued to read the
	 * entire record.  Reads are interruptable but the interrupt is caught
	 * and returned as a read error on the server channel.
	 */
	sigint  = (PFI) signal (SIGINT,  pr_onsig);
	sigterm = (PFI) signal (SIGTERM, pr_onsig);

	while (nbytes > 0) {
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

	signal (SIGINT,  sigint);
	signal (SIGTERM, sigterm);

	if (debug_ks) {
	    fprintf (debug_fp, "read %d bytes from KS channel %d:\n",
		op - (char *)buf, fd);
	    fflush (debug_fp);
	}
}


/* ZAWRKS -- Write to a kernel server channel.
 */
ZAWRKS (chan, buf, totbytes, loffset)
XINT	*chan;			/* kernel server channel (socket)	*/
XCHAR	*buf;			/* output buffer			*/
XINT	*totbytes;		/* number of bytes to write		*/
XLONG	*loffset;		/* not used				*/
{
	register int	fd, ofd, nbytes;
	PFI	sigint, sigterm, sigpipe;
	extern	pr_onsig();

	/* If chan=0 (the process standard input) then we really want to
	 * write to channel 1, the standard output.
	 */
	if ((ofd = fd = *chan) == 0)
	    ofd = 1;

	zfd[fd].nbytes = nbytes = *totbytes;

	if (debug_ks) {
	    fprintf (debug_fp, "initiate write of %d bytes to KS channel %d\n",
		nbytes, ofd);
	    fflush (debug_fp);
	}

	/* Write exactly nbytes of data to the channel from user buffer to
	 * the channel.  Block interrupt during the write to avoid corrupting
	 * the data stream protocol if the user interrupts the client task.
	 * Trap SIGPIPE and return it as a write error on the channel instead.
	 * Likewise, turn an interrupt into a write error on the channel.
	 */
	sigint  = (PFI) signal (SIGINT,  pr_onsig);
	sigterm = (PFI) signal (SIGTERM, pr_onsig);
	sigpipe = (PFI) signal (SIGPIPE, pr_onsig);
	recursion = 0;

	if (setjmp (jmpbuf) == 0)
	    zfd[fd].nbytes = write (ofd, (char *)buf, nbytes);
	else
	    zfd[fd].nbytes = ERR;

	signal (SIGINT,  sigint);
	signal (SIGTERM, sigterm);
	signal (SIGPIPE, sigpipe);

	if (debug_ks) {
	    fprintf (debug_fp, "wrote %d bytes to KS channel %d:\n",
		zfd[fd].nbytes, ofd);
	    fflush (debug_fp);
	}
}


/* PR_ONSIG -- Catch a signal and make it look like a write error on the
 * server i/o channel.
 */
pr_onsig (sig, code, scp)
int	sig;			/* signal which was trapped	*/
int	code;			/* subsignal code (vax)		*/
struct	sigcontext *scp;	/* not used			*/
{
	if (sig == SIGPIPE && recursion++ == 0)
	    fputs ("kernel server process has died\n", stderr);

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
