/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <setjmp.h>

#include "types.h"
#include "in.h"

#define	import_kernel
#define	import_knames
#define	import_zfstat
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
 * The network interface used is an emulation of the Berkeley UNIX function
 * REXEC on top of a standard TCP/IP interface.
 */

#define	SZ_NAME		32		/* max size node, etc. name	  */
#define	SZ_CMD		128		/* max size rexec sh command	  */
#define	FNNODE_CHAR	'!'		/* node name delimiter		  */
#define	HOSTLOGIN	"hostlogin"	/* user host login file		  */
#define	IRAFHOSTS	".irafhosts"	/* default host login file (dev$) */
#define	USER		"<user>"	/* symbol for user's login name   */

int	ks_ionbytes[MAXOFILES];		/* nbytes read|written on channel */
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
	char	*host, *cmd;
	int	ipport;

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

	/* Get login name and password and connect to the kernel server
	 * process.  TCP_REXEC is a portable version of the Berkeley UNIX
	 * REXEC facility (see ./net).
	 */
	if (ks_getlogin (host, username, password) == ERR)
	    *chan = ERR;
	else {
	    ipport = htons (IPPORT_EXECSERVER);
	    *chan = tcp_rexec (&host, ipport, username, password, cmd, 0);
	}

	if (*chan > 0)
	    ks_ionbytes[*chan] = 0;
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
int 
ks_getlogin (
    char *node,			/* node we wish a login for	*/
    char *username,		/* receives the login name	*/
    char *password		/* receives the login password	*/
)
{
	char	fname[SZ_FNAME+1];
	char	uname[SZ_FNAME+1];

	/* Get the user login name on the local node, used as the default
	 * login for remote nodes.  [MACHDEP - edit for local system]
	 */
	strcpy (uname, "USER");

	/* Try to open the .irafhosts file in the user's login directory.
	 */
	if (ku_mkfname ("home", "", IRAFHOSTS, fname, SZ_FNAME) != ERR)
	    if (ks_scanlogin (fname, node, uname, username, password) == OK)
		return (OK);

	/* Scan the dev$hostlogin file and return a default public login
	 * on the remote node.
	 */
	if (ku_mkfname ("iraf", "dev", HOSTLOGIN, fname, SZ_FNAME) != ERR)
	    return (ks_scanlogin (fname, node, uname, username, password));

	return (ERR);
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
int 
ks_scanlogin (
    char *fname,			/* table file			*/
    char *node,			/* node name			*/
    char *uname,			/* user login on local node	*/
    char *username,		/* receives user login name	*/
    char *password		/* receives user password	*/
)
{
	char	*ip;
	char	lbuf[SZ_LINE+1];
	char	wbuf[SZ_NAME+1];
	int	fp;
	int	foundit;
	char	*ku_fgets();

	foundit = 0;
	if ((fp = ku_fopen (fname, "r")) == ERR)
	    return (ERR);

	/* Scan file for line containing node name.
	 */
	while (!foundit && ku_fgets (lbuf, SZ_LINE, fp) != NULL) {
	    /* Skip blank lines and comment lines */
	    for (ip=lbuf;  *ip == ' ' || *ip == '\t';  ip++)
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

	ku_fclose (fp);
	if (!foundit)
	    return (ERR);

	/* Skip to end of alias list. */
	while (ks_getword (&ip, wbuf) > 0) {
	    if (strcmp (wbuf, ":") == 0) {
		/* Return login name and password.
		 */

		/* If the login name is given as the USER string, use the
		 * login name on the local node.  If the login name is given
		 * as "?", query the user for the actual login name.
		 */
		if (ks_getword (&ip, username) <= 0)
		    return (ERR);
		if (strcmp (username, USER) == 0)
		    strcpy (username, uname);
		else if (strcmp (username, "?") == 0) {
		    char    prompt[80];

		    sprintf (prompt, "Login name (%s@%s): ", username, node);
		    if (ku_gpasswd (prompt, username, SZ_NAME) == ERR)
			return (ERR);
		}

		/* If the password is given as "?", query the user for
		 * the actual password.
		 */
		if (ks_getword (&ip, password) <= 0)
		    return (ERR);
		if (strcmp (password, "?") == 0) {
		    char    prompt[80];

		    sprintf (prompt, "Password (%s@%s): ", username, node);
		    if (ku_gpasswd (prompt, password, SZ_NAME) == ERR)
			return (ERR);
		}

		return (OK);		/* SUCCESS */
	    }
	}

	return (ERR);
}


/* KS_GETWORD -- Get the next whitespace or : delimited word from the
 * input string.
 */
int 
ks_getword (
    char **ip,			/* pointer into input buffer	*/
    char *obuf			/* receives name		*/
)
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
	*status = tcp_close (*chan);
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
	int	(*sigint)(), (*sigterm)();
	int	status;
	extern	pr_onsig();

	fd = *chan;
	op = (char *)buf;
	ks_ionbytes[fd] = nbytes = *totbytes;

	/* Now read exactly nbytes of data from channel into user buffer.
	 * Return actual byte count if EOF is seen.  If ERR is seen return
	 * ERR.  If necessary multiple read requests are issued to read the
	 * entire record.  Reads are interruptable but the interrupt is caught
	 * and returned as a read error on the server channel.
	 */
	sigint  = signal (SIGINT,  pr_onsig);
	sigterm = signal (SIGTERM, pr_onsig);

	while (nbytes > 0) {
	    if (setjmp (jmpbuf) == 0)
		status = tcp_read (fd, op, nbytes);
	    else
		status = ERR;

	    switch (status) {
	    case 0:
		ks_ionbytes[fd] -= nbytes;
		signal (SIGINT,  sigint);
		signal (SIGTERM, sigterm);
		return;
	    case ERR:
		ks_ionbytes[fd] = ERR;
		signal (SIGINT,  sigint);
		signal (SIGTERM, sigterm);
		return;
	    default:
		nbytes -= status;
		op += status;
		break;
	    }
	}

	signal (SIGINT,  sigint);
	signal (SIGTERM, sigterm);
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
	int	(*sigint)(), (*sigterm)(), (*sigpipe)();
	extern	pr_onsig();

	/* If chan=0 (the process standard input) then we really want to
	 * write to channel 1, the standard output.
	 */
	if ((ofd = fd = *chan) == 0)
	    ofd = 1;

	ks_ionbytes[fd] = nbytes = *totbytes;

	/* Write exactly nbytes of data to the channel from user buffer to
	 * the channel.  Block interrupt during the write to avoid corrupting
	 * the data stream protocol if the user interrupts the client task.
	 * Trap SIGPIPE and return it as a write error on the channel instead.
	 * Likewise, turn an interrupt into a write error on the channel.
	 */
	sigint  = signal (SIGINT,  pr_onsig);
	sigterm = signal (SIGTERM, pr_onsig);
	sigpipe = signal (SIGPIPE, pr_onsig);
	recursion = 0;

	if (setjmp (jmpbuf) == 0)
	    ks_ionbytes[fd] = tcp_write (ofd, (char *)buf, nbytes);
	else
	    ks_ionbytes[fd] = ERR;

	signal (SIGINT,  sigint);
	signal (SIGTERM, sigterm);
	signal (SIGPIPE, sigpipe);
}


/* PR_ONSIG -- Catch a signal and make it look like a write error on the
 * server i/o channel.
 */
int 
pr_onsig (
    int sig,			/* signal which was trapped	*/
    int code,			/* subsignal code (vax)		*/
    struct sigcontext *scp	/* not used			*/
)
{
	if (sig == SIGPIPE && recursion++ == 0)
	    ku_error ("kernel server process has died");

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
	if ((*status = ks_ionbytes[*chan]) == ERR)
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
