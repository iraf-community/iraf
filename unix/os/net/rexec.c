/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "types.h"
#include "socket.h"
#include "in.h"
#include "netdb.h"

/* TCP_REXEC -- Execute a command on a remote node via the network.  This is
 * an implementation of the Berkeley UNIX procedure of the same name for a
 * machine independent TCP network interface.  Unlike the UNIX rexec,
 * however, we require that the user login name and password be given as
 * input arguments, in addition to the host name and port number and the
 * command to be executed.
 *
 * REXEC assumes that it is talking to an REXECD server on the remote node.
 * The TCP EXEC port on the remote node spawns the REXECD server which reads and
 * authenticates the user login and password, sets up the error socket if so
 * indicated, changes the current directory to the user's home directory, and
 * then executes the command.  The command syntax is determined by the shell
 * REXECD spawns to execute the command, and is implementation dependent.
 * Currently the REXECD daemons are either UNIX hosted or UNIX emulated, hence
 * the command syntax is the UNIX shell (Bourne shell usually).  The command
 * executes with its standard input and output (and error output if fd2p=0)
 * connected to the socket returned by REXEC.
 *
 * Note that the shell spawned by the REXEC daemon may be used to spawn a user
 * specified server process using a shell command, e.g. "run server.e arg arg".
 * In this case the daemon is used to login and set the current directory and
 * pass args to the user server, at the expense of one additional process spawn
 * for the shell.
 */
tcp_rexec (ahost, rport, name, pass, cmd, fd2p)
char	**ahost;		/* alias of server node		*/
int	rport;			/* IP port number (for EXEC)	*/
char	*name, *pass;		/* user login and password	*/
char	*cmd;			/* command to be executed	*/
int	*fd2p;			/* error channel		*/
{
	struct	hostent *tcp_gethostbyname();
	struct	sockaddr_in sin, sin2, from;
	struct	hostent *hp;
	int	timo = 1;
	u_sock	s, s3;
	char	c;
	short	port;
eprintf("rexec %s %s %s %s\n", *ahost, name, pass, cmd);

	/* Read host name table for the local network to get the internet
	 * address of the named host.
	 */
	hp = tcp_gethostbyname (*ahost);
	if (hp == 0) {
	    ku_error ("unknown network host");
	    return (-1);
	}

	/* Set up a full duplex TCP socket to the TCP/EXEC server process
	 * on the remote node.
	 */
retry:
	s = tcp_socket (AF_INET, SOCK_STREAM, 0);
	if (s < 0) {
	    ku_error ("rexec: cannot make socket");
	    return (-1);
	}

	sin.sin_family = hp->h_addrtype;
	sin.sin_port   = rport;
	ku_bcopy (hp->h_addr, (caddr_t)&sin.sin_addr, hp->h_length);

	if (tcp_connect (s, &sin, sizeof(sin)) < 0) {
	    if (timo <= 16) {
		tcp_close (s);
		ku_sleep (timo);
		timo *= 2;
		goto retry;
	    }
	    ku_error ("rexec: connect failure");
	    return (-1);
	}

	/* If no output error channel variable was given instruct the REXECD
	 * server to return error output on the data socket, else open a second
	 * socket to be used for error communications and signals.
	 */
	if (fd2p == 0) {
	    tcp_write (s, "", 1);
	    port = 0;

	} else {
	    char    *num, *ku_itoc();
	    int	    sin2len, len;
	    u_sock  s2;
	    
	    s2 = tcp_socket (AF_INET, SOCK_STREAM, 0);
	    if (s2 < 0) {
		tcp_close (s);
		return (-1);
	    }

	    tcp_listen (s2, 1);

	    sin2len = sizeof (sin2);
	    if (tcp_gsockname (s2, (char *)&sin2, &sin2len) < 0 ||
		sin2len != sizeof (sin2)) {

		ku_error ("rexec: getsockname failed");
		tcp_close (s2);
		goto bad;
	    }

	    port = htons ((u_short)sin2.sin_port);
	    num  = ku_itoc (port);
	    tcp_write (s, num, strlen(num)+1);
	    len  = sizeof (from);

	    s3 = tcp_accept (s2, &from, &len, 0);

	    tcp_close (s2);
	    if (s3 < 0) {
		ku_error ("rexec: accept failure");
		port = 0;
		goto bad;
	    }

	    *fd2p = s3;
	}

	tcp_write (s, name, strlen (name) + 1);
	tcp_write (s, pass, strlen (pass) + 1);
	tcp_write (s, cmd,  strlen (cmd)  + 1);

	if (tcp_read (s, &c, 1) != 1) {
	    ku_error ("rexec: cannot read server");
	    goto bad;
	}

	/* Read error message from server process.
	 */
	if (c != 0) {
	    char lbuf[80];
	    char *op;

	    for (op=lbuf;  (tcp_read (s, op, 1) == 1);  op++)
		if (*op == '\n')
		    break;
	    *op = '\0';
	    ku_error (lbuf);
	    goto bad;
	}

	return (s);
bad:
	if (port)
	    tcp_close (*fd2p);
	tcp_close (s);

	return (-1);
}
