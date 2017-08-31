/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/un.h>
#include <netdb.h>
#include <fcntl.h>
#include <ctype.h>
#include <signal.h>
#include <setjmp.h>

#include <errno.h>
#include <stdio.h>

#define	import_kernel
#define	import_knames
#define	import_zfstat
#define import_spp
#include <iraf.h>

/*
 * ZFIOND -- This driver provides a FIO-compatible interface to network or
 * IPC streaming devices such as Berkeley sockets, FIFOs, and the like.
 * Any connection-oriented stream type network interface can be supported.
 *
 * The type of connection desired is determined at device open time by the
 * "filename" and file access mode arguments.  The syntax for the filename
 * argument is as follows:
 *
 *	<domain> : <address> [ : flag ] [ : flag...]
 *
 * where <domain> is one of "inet" (internet tcp/ip socket), "unix" (unix
 * domain socket) or "fifo" (named pipe).  The form of the address depends
 * upon the domain, as illustrated in the examples below.
 *
 *  inet:5187			Server connection to port 5187 on the local
 *				host.  For a client, a connection to the
 *				given port on the local host.
 *
 *  inet:5187:foo.bar.edu	Client connection to port 5187 on internet
 *				host foo.bar.edu.  The dotted form of address
 *				may also be used.
 *
 *  unix:/tmp/.IMT212		Unix domain socket with the given pathname
 *				IPC method, local host only.
 *
 *  fifo:/dev/imt1i:/dev/imt1o	FIFO or named pipe with the given pathname.
 *				IPC method, local host only.  Two pathnames
 *				are required, one for input and one for
 *				output, since FIFOs are not bidirectional.
 *				For a client the first fifo listed will be
 *				the client's input fifo; for a server the
 *				first fifo will be the server's output fifo.
 *				This allows the same address to be used for
 *				both the client and the server, as for the
 *				other domains.
 *
 *  sock:5			Used by servers to accept a connection on
 *				a server socket opened in nonblocking mode
 *				on the given channel (5 in the example).
 *				The channel is returned in a previous call
 *				to open an INET or UNIX server port.
 *
 * The address field may contain up to two "%d" fields.  If present, the
 * user's UID will be substituted (e.g. "unix:/tmp/.IMT%d").
 *
 * The protocol flags currently supported are "text", "binary", "nonblock",
 * and "nodelay".  If "text" is specified the datastream is assumed to
 * consist only of byte packed ascii text and is automatically converted by
 * the driver to and from SPP chars during i/o.  The default is binary i/o
 * (no conversions).  The "nonblock" flag is used to specify nonblocking
 * mode.  The "nodelay" flag is used to return an error when opening a
 * "sock" connection in "nonblock" mode and for read when there is no
 * pending connection or data.
 *
 *
 * Client connections normally use mode READ_WRITE, although READ_ONLY and
 * WRITE_ONLY are permitted.  APPEND is the same as WRITE_ONLY.  A server
 * connection is indicated by the mode NEW_FILE.  The endpoints of the server
 * connection will be created if necessary.  A client connection will timeout
 * if no server responds.
 *
 * By default a server connection will block until a client connects, and
 * the channel returned will be the i/o channel for the client connection.
 * If however a server connection is opened in nonblocking mode then a server
 * socket will be opened which can be used for multiple client connections.
 * When a client makes a connection attempt, opening the server socket as
 * "sock:<chan>", where <chan> is the channel assigned to the socket, will
 * accept the client connection and open a new channel to be used for
 * bidirectional i/o to the client.  The open will block until a client
 * connects unless the socket is opened in nonblocking mode.  If opening the
 * channel under IRAF FIO and separate read and write streams are desired,
 * this can be achieved by using REOPEN to open a second stream on the same
 * channel).  The server sees an EOF on the input stream when the client
 * disconnects.
 *
 * The "nodelay" flag will poll for "sock" connections or for a pending
 * read.  This uses the SELECT call.  If there is no pending connection
 * then the open call will return an error through FIO.  The application
 * may trap the error and try again later.  If there is no pending data in a
 * read then the number of bytes read is set to XERR and FIO will return ERR.
 * The exception to this is if asynchronous reads are used with AREADB
 * and AWAITB.  In this case the application will see the number of bytes
 * read as zero for EOF (client disconnected) and ERR for no data.
 *
 * FIFO domain connection are slightly different.  When the server opens a
 * FIFO connection the open returns immediately.  When the server reads from
 * the input fifo the server will block until some data is written to the
 * fifo by a client.  The server connection will remain open over multiple
 * client connections until it is closed by the server.  This is done to
 * avoid a race condition that could otherwise occur at open time, with both
 * the client and the server blocked waiting for an open on the opposite stream.
 */

#define	SZ_NAME		256
#define	SZ_OBUF		4096
#define	MAXCONN		32
#define	MAXSEL		32

#define	INET		1
#define	UNIX		2
#define	FIFO		3
#define	SOCK		4

#define	F_SERVER	00001
#define	F_NONBLOCK	00002
#define	F_TEXT		00004
#define	F_DEL1		00010
#define	F_DEL2		00020
#define	F_NODELAY	00040

/* Network portal descriptor. */
struct portal {
	int channel;
	int domain;
	int flags;
	int datain;
	int dataout;
	int keepalive;
	char path1[SZ_NAME];
	char path2[SZ_NAME];
};

#define get_desc(fd)		((struct portal *)zfd[fd].fp)
#define set_desc(fd,np)		zfd[fd].fp = (FILE *)np
#define	min(a,b)		(((a)<(b))?(a):(b))

static	jmp_buf jmpbuf;
static	int jmpset = 0;
static	int recursion = 0;
extern	int errno;
static	int getstr(char **ipp, char *obuf, int maxch, int delim);

static void nd_onsig (int sig, int *arg1, int *arg2);



/* ZOPNND -- Open a network device.
 */
int
ZOPNND (
  PKCHAR  *pk_osfn,		/* UNIX name of file */
  XINT	  *mode,			/* file access mode */
  XINT	  *chan 			/* file number (output) */
)
{
	register int fd;
	    register struct portal *np, *s_np = (struct portal *) NULL;
	    unsigned short host_port = 0;
	    unsigned long host_addr = 0;
	    char osfn[SZ_NAME*2];
	    char flag[SZ_NAME];
	    char *ip;

	    /* Get network device descriptor. */
	    if (!(np = (struct portal *) calloc (1, sizeof(struct portal)))) {
		*chan = XERR;
		return (XERR);
	    }

	    /* Expand any %d fields in the network address to the UID. */
	    sprintf (osfn, (char *)pk_osfn, getuid(), getuid());

	    /* Parse the network filename to determine the domain type and
	     * network address.
	     */
	    if (strncmp (osfn, "inet:", 5) == 0) {
		/* Internet connection.
		 */
		char port_str[SZ_NAME];
		char host_str[SZ_NAME];
		unsigned short port;
		struct servent *sv;
		struct hostent *hp;

		/* Get port number.  This may be specified either as a service
		 * name or as a decimal port number.
		 */
		ip = osfn + 5;
		if (getstr (&ip, port_str, SZ_NAME, ':') <= 0)
		    goto err;
		if (isdigit (port_str[0])) {
		    port = atoi (port_str);
		    host_port = htons (port);
		} else if ((sv = getservbyname(port_str,"tcp"))) {
		    host_port = sv->s_port;
		} else
		    goto err;

		/* Get host address.  This may be specified either has a host
		 * name or as an Internet address in dot notation.  If no host
		 * name is specified default to the local host.
		 */
		if (getstr (&ip, host_str, SZ_NAME, ':') <= 0)
		    strcpy (host_str, "localhost");
		if (isdigit (host_str[0])) {
		    host_addr = inet_addr (host_str);
		    if ((int)host_addr == -1)
			goto err;
		} else if ((hp = gethostbyname(host_str))) {
		    bcopy (hp->h_addr, (char *)&host_addr, sizeof(host_addr));
		} else
		    goto err;

		np->domain = INET;

	    } else if (strncmp (osfn, "unix:", 5) == 0) {
		/* Unix domain socket connection.
		 */
		ip = osfn + 5;
		if (!getstr (&ip, np->path1, SZ_NAME, ':'))
		    goto err;
		np->domain = UNIX;

	    } else if (strncmp (osfn, "sock:", 5) == 0) {
		/* Open (accept) a client connection on an existing, open
		 * server socket.
		 */
		char chan_str[SZ_NAME];
		int channel;

		/* Get the channel of the server socket. */
		ip = osfn + 5;
		if (getstr (&ip, chan_str, SZ_NAME, ':') <= 0)
		    goto err;
		if (isdigit (chan_str[0]))
		    channel = atoi (chan_str);
		else
		    goto err;

		/* Get the server portal descriptor. */
		s_np = get_desc(channel);
	    if (!(s_np->flags & F_SERVER))
		goto err;

	    np->domain = SOCK;

	} else if (strncmp (osfn, "fifo:", 5) == 0) {
	    /* FIFO (named pipe) connection.
	     */
	    ip = osfn + 5;
	    if (*mode == NEW_FILE) {
		/* Server. */
		if (!getstr (&ip, np->path2, SZ_NAME, ':'))
		    goto err;
		if (!getstr (&ip, np->path1, SZ_NAME, ':'))
		    goto err;
	    } else {
		/* Client. */
		if (!getstr (&ip, np->path1, SZ_NAME, ':'))
		    goto err;
		if (!getstr (&ip, np->path2, SZ_NAME, ':'))
		    goto err;
	    }
	    np->domain = FIFO;

	} else
	    goto err;

	/* Process any optional protocol flags.
	 */
	while (getstr (&ip, flag, SZ_NAME, ':') > 0) {
	    /* Get content type (text or binary).  If the stream will be used
	     * only for byte-packed character data the content type can be
	     * specified as "text" and data will be automatically packed and
	     * unpacked during i/o.
	     */
	    if (strcmp (flag, "text") == 0)
		np->flags |= F_TEXT;
	    if (strcmp (flag, "binary") == 0)
		np->flags &= ~F_TEXT;

	    /* Check for nonblocking i/o or connections. */
	    if (strcmp (flag, "nonblock") == 0)
		np->flags |= F_NONBLOCK;

	    /* Check for no delay flag. */
	    if (strcmp (flag, "nodelay") == 0)
		np->flags |= F_NODELAY;
	}

	/* Open the network connection.
	 */
	switch (*mode) {
	case READ_ONLY:
	    /* Client side read only FIFO connection. */
	    if (np->domain == FIFO) {
		if ((fd = open (np->path1, O_RDONLY|O_NDELAY)) != ERR)
		    fcntl (fd, F_SETFL, O_RDONLY);
		np->datain = fd;
		np->dataout = -1;
		break;
	    }
	    /* fall through */

	case WRITE_ONLY:
	case APPEND:
	    /* Client side write only FIFO connection. */
	    if (np->domain == FIFO) {
		if ((fd = open (np->path2, O_WRONLY|O_NDELAY)) != ERR)
		    fcntl (fd, F_SETFL, O_WRONLY);
		np->datain = -1;
		np->dataout = fd;
		break;
	    }
	    /* fall through */

	case READ_WRITE:
	    if (np->domain == INET) {
		/* Client side Internet domain connection. */
		struct sockaddr_in sockaddr;

		/* Get socket. */
		if ((fd = socket (AF_INET, SOCK_STREAM, 0)) < 0)
		    goto err;

		/* Compose network address. */
		bzero ((char *)&sockaddr, sizeof(sockaddr));
		sockaddr.sin_family = AF_INET;
		sockaddr.sin_port = host_port;
		bcopy ((char *)&host_addr, (char *)&sockaddr.sin_addr,
		    sizeof(host_addr));

		/* Connect to server. */
		if (fd >= MAXOFILES || (connect (fd,
			(struct sockaddr *)&sockaddr, sizeof(sockaddr)) < 0)) {
		    close (fd);
		    fd = ERR;
		} else {
		    np->datain = fd;
		    np->dataout = fd;
		}

	    } else if (np->domain == UNIX) {
		/* Client side Unix domain socket connection. */
		struct sockaddr_un sockaddr;

		/* Get socket. */
		if ((fd = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
		    goto err;

		/* Compose network address. */
		bzero ((char *)&sockaddr, sizeof(sockaddr));
		sockaddr.sun_family = AF_UNIX;
		strncpy (sockaddr.sun_path,
		    np->path1, sizeof(sockaddr.sun_path));

		/* Connect to server. */
		if (fd >= MAXOFILES || (connect (fd,
			(struct sockaddr *)&sockaddr, sizeof(sockaddr)) < 0)) {
		    close (fd);
		    fd = ERR;
		} else {
		    np->datain = fd;
		    np->dataout = fd;
		}

	    } else if (np->domain == FIFO) {
		/* Client side FIFO connection. */
		int fd1, fd2;

		/* Open the fifos. */
		if ((fd1 = open (np->path1, O_RDONLY|O_NDELAY)) != ERR)
		    fcntl (fd1, F_SETFL, O_RDONLY);
		if ((fd2 = open (np->path2, O_WRONLY|O_NDELAY)) != ERR)
		    fcntl (fd2, F_SETFL, O_WRONLY);

		/* Clean up if there is an error. */
		if (fd1 < 0 || fd1 > MAXOFILES || fd2 < 0 || fd2 > MAXOFILES) {
		    if (fd1 > 0)
			close (fd1);
		    if (fd2 > 0)
			close (fd2);
		    fd = ERR;
		} else {
		    np->datain = fd1;
		    np->dataout = fd2;
		    fd = fd1;
		}
	    } else
		goto err;
	    break;

	case NEW_FILE:
	    /* Connect to a client. */
	    np->flags |= F_SERVER;

	    if (np->domain == INET) {
		/* Server side Internet domain connection. */
		struct sockaddr_in sockaddr;
		int s, reuse=1;

		/* Get socket. */
		if ((s = socket (AF_INET, SOCK_STREAM, 0)) < 0)
		    goto err;

		/* Bind server port to socket. */
		bzero ((char *)&sockaddr, sizeof(sockaddr));
		sockaddr.sin_family = AF_INET;
		sockaddr.sin_port = host_port;
		sockaddr.sin_addr.s_addr = htonl(INADDR_ANY);

		if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&reuse,
			sizeof(reuse)) < 0) {
		    close (s);
		    goto err;
		}

		if (bind (s,
			(struct sockaddr *)&sockaddr, sizeof(sockaddr)) < 0) {
		    close (s);
		    goto err;
		}

		/* Enable queuing of client connections. */
		if (listen (s, MAXCONN) < 0) {
		    close (s);
		    goto err;
		}

		/* If in blocking mode wait for a client connection, otherwise
		 * return the server socket on the channel.
		 */
		if (!(np->flags & F_NONBLOCK)) {
		    if ((fd = accept (s, (struct sockaddr *)0, 
			(socklen_t *)0)) < 0) {
			    close (s);
			    goto err;
		    } else
			close (s);
		} else
		    fd = s;

		np->datain = fd;
		np->dataout = fd;

	    } else if (np->domain == UNIX) {
		/* Server side Unix domain connection. */
		struct sockaddr_un sockaddr;
		int addrlen, s;

		/* Get socket. */
		if ((s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
		    goto err;

		/* Bind server port to socket. */
		bzero ((char *)&sockaddr, sizeof(sockaddr));
		sockaddr.sun_family = AF_UNIX;
		strncpy (sockaddr.sun_path,np->path1,sizeof(sockaddr.sun_path));
		addrlen = sizeof(sockaddr) - sizeof(sockaddr.sun_path)
		    + strlen(np->path1);

		unlink (np->path1);
		if (bind (s, (struct sockaddr *)&sockaddr, addrlen) < 0) {
		    close (s);
		    goto err;
		}

		/* Enable queuing of client connections. */
		if (listen (s, MAXCONN) < 0) {
		    close (s);
		    goto err;
		}

		/* If in blocking mode wait for a client connection, otherwise
		 * return the server socket on the channel.
		 */
		if (!(np->flags & F_NONBLOCK)) {
		    if ((fd = accept (s, (struct sockaddr *)0, 
			(socklen_t *)0)) < 0) {
			    close (s);
			    goto err;
		    } else
			close (s);
		} else
		    fd = s;

		np->datain = fd;
		np->dataout = fd;
		np->flags |= F_DEL1;

	    } else if (np->domain == SOCK) {
		/* Open (accept) a client connection on a server socket. */
		int s = s_np->channel;

                if (s_np->flags & F_NODELAY) {
		    struct timeval timeout;
		    fd_set readfds;
                    FD_ZERO (&readfds);
                    FD_SET (s, &readfds);
                    timeout.tv_sec = 0;
                    timeout.tv_usec = 0;
                    if (select (MAXSEL, &readfds, NULL, NULL, &timeout)) {
                        if ((fd = accept (s, (struct sockaddr *)0, 
			    (socklen_t *)0))<0)
                                goto err;
	            } else {
			goto err;
	            }
		} else {
		    if ((fd = accept (s, (struct sockaddr *)0, 
			(socklen_t *)0)) < 0)
		            goto err;
		}

		np->datain = fd;
		np->dataout = fd;
		np->flags = s_np->flags;

	    } else if (np->domain == FIFO) {
		/* Server side FIFO connection. */
		int fd1=0, fd2=0, keepalive=0;

		/* Create fifos if necessary. */
		if (access (np->path1, 0) < 0) {
		    if (mknod (np->path1, 010660, 0) < 0)
			goto err;
		    else
			np->flags |= F_DEL1;
		}
		if (access (np->path2, 0) < 0) {
		    if (mknod (np->path2, 010660, 0) < 0) {
			unlink (np->path1);
			goto err;
		    } else
			np->flags |= F_DEL2;
		}

		/* Open the output fifo (which is the client's input fifo).
		 * We have to open it ourselves first as a client to get
		 * around the fifo open-no-client error.
		 */
		if ((fd1 = open (np->path2, O_RDONLY|O_NDELAY)) != -1) {
		    if ((fd2 = open (np->path2, O_WRONLY|O_NDELAY)) != -1)
			fcntl (fd2, F_SETFL, O_WRONLY);
		    close (fd1);
		}

		/* Open the input fifo.  */
		if ((fd1 = open (np->path1, O_RDONLY|O_NDELAY)) == -1)
		    fprintf (stderr, "Warning: cannot open %s\n", np->path1);
		else {
		    /* Clear O_NDELAY for reading. */
		    fcntl (fd1, F_SETFL, O_RDONLY);

		    /* Open the client's output fifo as a pseudo-client to
		     * make it appear that a client is connected.
		     */
		    keepalive = open (np->path1, O_WRONLY);
		}

		/* Clean up if there is an error. */
		if (fd1 < 0 || fd1 > MAXOFILES || fd2 < 0 || fd2 > MAXOFILES) {
		    if (fd1 > 0) {
			close (fd1);
			close (keepalive);
		    }
		    if (fd2 > 0)
			close (fd2);
		    fd = ERR;
		} else {
		    np->datain = fd1;
		    np->dataout = fd2;
		    np->keepalive = keepalive;
		    fd = fd1;
		}

	    } else
		goto err;
	    break;

	default:
	    fd = ERR;
	}

	/* Initialize the kernel file descriptor.  Seeks are illegal for a
	 * network device; network devices are "streaming" files (blksize=1)
	 * which can only be accessed sequentially.
	 */
	if ((*chan = fd) == ERR) {
err:	    free (np);
	    *chan = XERR;
	} else if (fd >= MAXOFILES) {
	    free (np);
	    close (fd);
	    *chan = XERR;
	} else {
	    zfd[fd].fp     = NULL;
	    zfd[fd].fpos   = 0L;
	    zfd[fd].nbytes = 0;
	    zfd[fd].flags  = 0;
	    zfd[fd].filesize = 0;
	    set_desc(fd,np);
	    np->channel = fd;
	}

	return (*chan);
}


/* ZCLSND -- Close a network device.
 */
int
ZCLSND (XINT *fd, XINT *status)
{
	register struct portal *np = get_desc(*fd);
	register int flags;

	if (np) {
	    flags = np->flags;

	    if (np->datain > 0)
		close (np->datain);
	    if (np->dataout > 0 && np->dataout != np->datain)
		close (np->dataout);
	    if (np->keepalive > 0)
		close (np->keepalive);

	    if (flags & F_DEL1)
		unlink (np->path1);
	    if (flags & F_DEL2)
		unlink (np->path2);

	    free (np);
	    set_desc(*fd,NULL);
	    *status = XOK;

	} else
	    *status = XERR;

	return (*status);
}


/* ZARDND -- "Asynchronous" binary block read.  Initiate a read of at most
 * maxbytes bytes from the file FD into the buffer BUF.  Status is returned
 * in a subsequent call to ZAWTND.
 */
int
ZARDND (
  XINT	*chan,			/* UNIX file number */
  XCHAR	*buf,			/* output buffer */
  XINT	*maxbytes,		/* max bytes to read */
  XLONG	*offset 		/* 1-indexed file offset to read at */
)
{
	register int n;
	int fd = *chan;
	struct fiodes *kfp = &zfd[fd];
	register struct portal *np = get_desc (fd);
	register char *ip;
	register XCHAR *op;
	int nbytes, maxread;
	struct timeval timeout;
	fd_set readfds;
	FD_ZERO (&readfds);
	FD_SET (np->datain, &readfds);

	/* Determine maximum amount of data to be read. */
	maxread = (np->flags & F_TEXT) ? *maxbytes/sizeof(XCHAR) : *maxbytes;

	/* The following call to select shouldn't be necessary, but it
	 * appears that, due to the way we open a FIFO with O_NDELAY, read
	 * can return zero if read is called before the process on the other
	 * end writes any data.  This happens even though fcntl is called to
	 * restore blocking i/o after the open.
	 */
	FD_ZERO (&readfds);
	FD_SET (np->datain, &readfds);
        if ((np->flags & F_NODELAY) && np->datain < MAXSEL) {
	     timeout.tv_sec = 0;
	     timeout.tv_usec = 0;
	     if (select (MAXSEL, &readfds, NULL, NULL, &timeout))
	         nbytes = read (np->datain, (char *)buf, maxread);
	     else
		 nbytes = XERR;
	 } else {
	    if (np->domain == FIFO && np->datain < MAXSEL) {
	        select (MAXSEL, &readfds, NULL, NULL, NULL);
	        nbytes = read (np->datain, (char *)buf, maxread);
	    } else {
	        nbytes = read (np->datain, (char *)buf, maxread);
	    }
	}

	if ((n = nbytes) > 0 && (np->flags & F_TEXT)) {
	    op = (XCHAR *) buf;
	    op[n] = XEOS;
	    for (ip = (char *)buf;  --n >= 0;  )
		op[n] = ip[n];
	    nbytes *= sizeof(XCHAR);
	}

	kfp->nbytes = nbytes;

	return (nbytes);
}


/* ZAWRND -- "Asynchronous" binary block write.  Initiate a write of exactly
 * nbytes bytes from the buffer BUF to the file FD.  Status is returned in a
 * subsequent call to ZAWTND.
 */
int
ZAWRND (
  XINT	*chan,			/* UNIX file number */
  XCHAR	*buf,			/* buffer containing data */
  XINT	*nbytes,		/* nbytes to be written	 */
  XLONG	*offset 		/* 1-indexed file offset */
)
{
	register int fd = *chan;
	register struct	fiodes *kfp = &zfd[fd];
	register struct portal *np = get_desc (fd);
	int nwritten, maxbytes, n;
	char *text, *ip = (char *)buf;
	char obuf[SZ_OBUF];
	SIGFUNC sigpipe;


	/* Enable a signal mask to catch SIGPIPE when the server has died. 
	 */
	sigpipe = (SIGFUNC) signal (SIGPIPE, (SIGFUNC)nd_onsig);
	recursion = 0;

	maxbytes = (np->domain == FIFO || (np->flags & F_TEXT)) ? SZ_OBUF : 0;
	for (nwritten=0;  nwritten < *nbytes;  nwritten += n, ip+=n) {
	    n = *nbytes - nwritten;
	    if (maxbytes)
		n = min (maxbytes, n);

	    if (np->flags & F_TEXT) {
		register XCHAR *ipp = (XCHAR *)ip;
		register char *op = (char *)obuf;
		register int nbytes = n / sizeof(XCHAR);

		while (--nbytes >= 0)
		    *op++ = *ipp++;
		text = obuf;

		jmpset++;
		if (setjmp (jmpbuf) == 0) {
		    if ((n = write(np->dataout, text, n / sizeof(XCHAR))) < 0) {
		        nwritten = ERR;
		        break;
	            }
		} else {
		    nwritten = ERR;
		    break;
	        }

		n *= sizeof(XCHAR);

	    } else {
		text = ip;
		if ((n = write (np->dataout, text, n)) < 0) {
		    nwritten = ERR;
		    break;
	        }
	    }
	}

	/* Restore the signal mask. */
	jmpset = 0;
	signal (SIGPIPE, sigpipe);

	kfp->nbytes = nwritten;

	return (nwritten);
}


/* ND_ONSIG -- Catch a signal.
 *  */
static void
nd_onsig (
  int     sig,                    /* signal which was trapped     */
  int     *arg1,                  /* not used */
  int     *arg2                   /* not used */
)
{
        /* If we get a SIGPIPE writing to a server the server has probably
         * died.  Make it look like there was an i/o error on the channel.
         */
        if (sig == SIGPIPE && recursion++ == 0)
            ;

        if (jmpset)
            longjmp (jmpbuf, sig);
}


/* ZAWTND -- "Wait" for an "asynchronous" read or write to complete, and
 * return the number of bytes read or written, or ERR.
 */
int
ZAWTND (XINT *fd, XINT *status)
{
	if ((*status = zfd[*fd].nbytes) == ERR)
	    *status = XERR;

	return (*status);
}


/* ZSTTND -- Return file status information for a network device.
 */
int
ZSTTND (XINT *fd, XINT *param, XLONG *lvalue)
{
	switch (*param) {
	case FSTT_BLKSIZE:
	    (*lvalue) = 0L;
	    break;

	case FSTT_FILSIZE:
	    (*lvalue) = 0L;
	    break;

	case FSTT_OPTBUFSIZE:
	    /* On some systems this parameter may be device dependent in which
	     * case device dependent code should be substituted here.
	     */
	    (*lvalue) = ND_OPTBUFSIZE;
	    break;

	case FSTT_MAXBUFSIZE:
	    /* On some systems this parameter may be device dependent in which
	     * case device dependent code should be substituted here.
	     */
	    (*lvalue) = ND_MAXBUFSIZE;
	    break;

	default:
	    (*lvalue) = XERR;
	    break;
	}

	return (XOK);
}


/*
 * Internal routines.
 * ----------------------------
 */

/* GETSTR -- Internal routine to extract a metacharacter delimited substring
 * from a formatted string.  The metacharacter to be taken as the delimiter
 * is passed as an argument.  Any embedded whitespace between the tokens is
 * stripped.  The number of characters in the output token is returned as 
 * the function value, or zero if EOS or the delimiter is reached.
 */
static int
getstr (char **ipp, char *obuf, int maxch, int delim)
{
	register char *op, *ip = *ipp;
	register char *otop = obuf + maxch;

	while (*ip && isspace(*ip))
	    ip++;
	for (op=obuf;  *ip;  ip++) {
	    if (*ip == delim) {
		ip++;
		break;
	    } else if (op < otop && !isspace(*ip))
		*op++ = *ip;
	}

	*op = '\0';
	*ipp = ip;

	return (op - obuf);
}
