/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <netdb.h>
#include <fcntl.h>

#ifdef LINUX
#include <sys/time.h>
#endif

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
 *	<domain> : <address> [ : <flag ] [ : flag...]
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
 * The address field may contain up to two "%d" fields.  If present, the
 * user's UID will be substituted (e.g. "unix:/tmp/.IMT%d").
 *
 * The only protocol flags currently supported are "text" and "binary".
 * If "text" is specified the datastream is assumed to consist only of byte
 * packed ascii text and is automatically converted by the driver to and
 * from SPP chars during i/o.  The default is binary i/o (no conversions).
 *
 * Client connections normally use mode READ_WRITE, although READ_ONLY and
 * WRITE_ONLY are permitted.  APPEND is the same as WRITE_ONLY.  A server
 * connection is indicated by the mode NEW_FILE.  The endpoints of the server
 * connection will be created if necessary.  A client connection will timeout
 * if no server responds.
 *
 * An INET or UNIX domain server connection will block indefinitely until a
 * client connects.  Since connections are synchronous only a single client
 * can be supported.  The server sees an EOF on the input stream when the
 * client disconnects.
 *
 * FIFO domain connection are slightly different.  When the server opens a FIFO
 * connection the open returns immediately.  When the server reads from the
 * input fifo the server will block until some data is written to the fifo by a
 * client.  The server connection will remain open over multiple client
 * connections until it is closed by the server.  This is done to avoid a race
 * condition that could otherwise occur at open time, with both the client and
 * the server blocked waiting for an open on the opposite stream.
 */

#define	SZ_NAME		256
#define	SZ_OBUF		4096
#define	MAXCONN		5
#define	MAXSEL		32

#define	INET		1
#define	UNIX		2
#define	FIFO		3

#define	F_SERVER	00001
#define	F_DEL1		00002
#define	F_DEL2		00004
#define	F_TEXT		00010

/* Network portal descriptor. */
struct portal {
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

extern	int errno;
static	int getstr();


/* ZOPNND -- Open a network device.
 */
ZOPNND (pk_osfn, mode, chan)
PKCHAR	*pk_osfn;		/* UNIX name of file */
XINT	*mode;			/* file access mode */
XINT	*chan;			/* file number (output) */
{
	register int fd;
	register struct portal *np;
	unsigned short host_port;
	unsigned long host_addr;
	char osfn[SZ_NAME*2];
	char flag[SZ_NAME];
	char *ip;

	/* Get network device descriptor. */
	if (!(np = (struct portal *) calloc (1, sizeof(struct portal)))) {
	    *chan = XERR;
	    return;
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
	    if (getstr (&ip, port_str, SZ_NAME) <= 0)
		goto err;
	    if (isdigit (port_str[0])) {
		port = atoi (port_str);
		host_port = htons (port);
	    } else if (sv = getservbyname(port_str,"tcp")) {
		host_port = sv->s_port;
	    } else
		goto err;

	    /* Get host address.  This may be specified either has a host
	     * name or as an Internet address in dot notation.  If no host
	     * name is specified default to the local host.
	     */
	    if (getstr (&ip, host_str, SZ_NAME) <= 0)
		strcpy (host_str, "localhost");
	    if (isdigit (host_str[0])) {
		host_addr = inet_addr (host_str);
		if ((int)host_addr == -1)
		    goto err;
	    } else if (hp = gethostbyname(host_str)) {
		bcopy (hp->h_addr, (char *)&host_addr, sizeof(host_addr));
	    } else
		goto err;

	    np->domain = INET;

	} else if (strncmp (osfn, "unix:", 5) == 0) {
	    /* Unix domain socket connection.
	     */
	    ip = osfn + 5;
	    if (!getstr (&ip, np->path1, SZ_NAME))
		goto err;
	    np->domain = UNIX;

	} else if (strncmp (osfn, "fifo:", 5) == 0) {
	    /* FIFO (named pipe) connection.
	     */
	    ip = osfn + 5;
	    if (*mode == NEW_FILE) {
		/* Server. */
		if (!getstr (&ip, np->path2, SZ_NAME))
		    goto err;
		if (!getstr (&ip, np->path1, SZ_NAME))
		    goto err;
	    } else {
		/* Client. */
		if (!getstr (&ip, np->path1, SZ_NAME))
		    goto err;
		if (!getstr (&ip, np->path2, SZ_NAME))
		    goto err;
	    }
	    np->domain = FIFO;

	} else
	    goto err;

	/* Process any optional protocol flags.
	 */
	while (getstr (&ip, flag, SZ_NAME) > 0) {
	    /* Get content type (text or binary).  If the stream will be used
	     * only for byte-packed character data the content type can be
	     * specified as "text" and data will be automatically packed and
	     * unpacked during i/o.
	     */
	    if (strcmp (flag, "text") == 0)
		np->flags |= F_TEXT;
	    if (strcmp (flag, "binary") == 0)
		np->flags &= ~F_TEXT;
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
		if (fd >= MAXOFILES || connect (fd,
			(struct sockaddr *)&sockaddr, sizeof(sockaddr)) < 0) {
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
		if (fd >= MAXOFILES || connect (fd,
			(struct sockaddr *)&sockaddr, sizeof(sockaddr)) < 0) {
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

		/* Wait for client to connect. */
		if (listen (s, MAXCONN) < 0) {
		    close (s);
		    goto err;
		}
		if ((fd = accept (s, (struct sockaddr *)0, (int *)0)) < 0) {
		    close (s);
		    goto err;
		} else
		    close (s);

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

		/* Wait for client to connect. */
		if (listen (s, MAXCONN) < 0) {
		    close (s);
		    goto err;
		}
		if ((fd = accept (s, (struct sockaddr *)0, (int *)0)) < 0) {
		    close (s);
		    goto err;
		} else
		    close (s);

		np->datain = fd;
		np->dataout = fd;
		np->flags |= F_DEL1;

	    } else if (np->domain == FIFO) {
		/* Server side FIFO connection. */
		int fd1, fd2, keepalive;

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
	}
}


/* ZCLSND -- Close a network device.
 */
ZCLSND (fd, status)
XINT	*fd;
XINT	*status;
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
}


/* ZARDND -- "Asynchronous" binary block read.  Initiate a read of at most
 * maxbytes bytes from the file FD into the buffer BUF.  Status is returned
 * in a subsequent call to ZAWTND.
 */
ZARDND (chan, buf, maxbytes, offset)
XINT	*chan;			/* UNIX file number */
XCHAR	*buf;			/* output buffer */
XINT	*maxbytes;		/* max bytes to read */
XLONG	*offset;		/* 1-indexed file offset to read at */
{
	register int n;
	int fd = *chan;
	struct fiodes *kfp = &zfd[fd];
	register struct portal *np = get_desc (fd);
	register char *ip;
	register XCHAR *op;
	int nbytes, maxread;

	/* Determine maximum amount of data to be read. */
	maxread = (np->flags & F_TEXT) ? *maxbytes/sizeof(XCHAR) : *maxbytes;

	/* The following call to select shouldn't be necessary, but it
	 * appears that, due to the way we open a FIFO with O_NDELAY, read
	 * can return zero if read is called before the process on the other
	 * end writes any data.  This happens even though fcntl is called to
	 * restore blocking i/o after the open.
	 */
	if (np->domain == FIFO && np->datain < MAXSEL) {
#ifdef SOLARIS
	    fd_set readfds;
	    FD_ZERO (&readfds);
	    FD_SET (np->datain, &readfds);
#else
	    int readfds = (1 << np->datain);
#endif
	    select (MAXSEL, &readfds, NULL, NULL, NULL);
	    nbytes = read (np->datain, (char *)buf, maxread);
	} else
	    nbytes = read (np->datain, (char *)buf, maxread);

	if ((n = nbytes) && (np->flags & F_TEXT)) {
	    op = (XCHAR *) buf;
	    op[n] = XEOS;
	    for (ip = (char *)buf;  --n >= 0;  )
		op[n] = ip[n];
	    nbytes *= sizeof(XCHAR);
	}

	kfp->nbytes = nbytes;
}


/* ZAWRND -- "Asynchronous" binary block write.  Initiate a write of exactly
 * nbytes bytes from the buffer BUF to the file FD.  Status is returned in a
 * subsequent call to ZAWTND.
 */
ZAWRND (chan, buf, nbytes, offset)
XINT	*chan;			/* UNIX file number */
XCHAR	*buf;			/* buffer containing data */
XINT	*nbytes;		/* nbytes to be written	 */
XLONG	*offset;		/* 1-indexed file offset */
{
	register int fd = *chan;
	register struct	fiodes *kfp = &zfd[fd];
	register struct portal *np = get_desc (fd);
	int nwritten, maxbytes, n;
	char *text, *ip = (char *)buf;
	char obuf[SZ_OBUF];

	maxbytes = (np->domain == FIFO || (np->flags & F_TEXT)) ? SZ_OBUF : 0;
	for (nwritten=0;  nwritten < *nbytes;  nwritten += n, ip+=n) {
	    n = *nbytes - nwritten;
	    if (maxbytes)
		n = min (maxbytes, n);

	    if (np->flags & F_TEXT) {
		register XCHAR *ipp = (XCHAR *)ip;
		register char *op = (char *)obuf;
		register int nbytes = n;

		while (--nbytes >= 0)
		    *op++ = *ipp++;
		text = obuf;
		if ((n = write (np->dataout, text, n / sizeof(XCHAR))) < 0)
		    break;
		n *= sizeof(XCHAR);

	    } else {
		text = ip;
		if ((n = write (np->dataout, text, n)) < 0)
		    break;
	    }
	}

	kfp->nbytes = nwritten;
}


/* ZAWTND -- "Wait" for an "asynchronous" read or write to complete, and
 * return the number of bytes read or written, or ERR.
 */
ZAWTND (fd, status)
XINT	*fd;
XINT	*status;
{
	if ((*status = zfd[*fd].nbytes) == ERR)
	    *status = XERR;
}


/* ZSTTND -- Return file status information for a network device.
 */
ZSTTND (fd, param, lvalue)
XINT	*fd;
XINT	*param;
XLONG	*lvalue;
{
	register struct fiodes *kfp = &zfd[*fd];
	struct	stat filstat;

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
}


/*
 * Internal routines.
 * ----------------------------
 */

/* GETSTR -- Internal routine to extract a colon delimited string from a
 * network filename.
 */
static int
getstr (ipp, obuf, maxch)
char **ipp;
char *obuf;
int maxch;
{
	register char *ip = *ipp, *op = obuf;
	register char *otop = obuf + maxch;
	char *start;

	while (isspace(*ip))
	    ip++;
	for (start=ip;  *ip;  ip++) {
	    if (*ip == ':') {
		ip++;
		break;
	    } else if (op && op < otop)
		*op++ = *ip;
	}

	if (op)
	    *op = '\0';
	*ipp = ip;

	return (ip - start);
}
