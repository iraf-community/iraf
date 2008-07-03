/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <unistd.h>
#include <ctype.h>
#include <limits.h>

# ifndef O_NDELAY
#include <fcntl.h>
# endif

#include <errno.h>
#include <stdio.h>

#define import_kernel
#define import_knames
#define import_zfstat
#define import_spp
#include <iraf.h>

#include "zos.h"

/*
 * ZFIOBF -- FIO interface to UNIX 4.1BSD binary files.
 * This is the interface to general, random access disk resident binary
 * files (as opposed to text files).  The basic strategy is very simple.
 * FIO will request an asynchronous read or write of N device blocks at
 * a given offset.  The offset (one-indexed) is guaranteed by FIO to be
 * aligned on a device block boundary, and to be in bounds.  The size of
 * a device block and of a file are determined at open time by FIO, which
 * calls the zsttbf status routine.
 *
 * FIO ASSUMES that it can extend a file by writing at EOF in an ordinary
 * zawrbf call with the appropriate one-indexed byte offset.  If the last
 * block in the file is a partial block, FIO will write at some device
 * block offset within the file, after first reading the partial block
 * into the FIO buffer.  FIO will never write a partial block within
 * a file, but assumes it can do so at the end of the file.  If the OS
 * does not support irregular length files, the interface routines should
 * simulate it somehow.  The FIO buffer is an integral number of SPP chars
 * in size, and read requests (in units of bytes) will always be for an integral
 * number of chars.
 *
 * In UNIX 4.1BSD, there is no such thing as asynchronous i/o, so we have
 * to fake it.  Also, the UNIX i/o interface is sequential/seek, while the
 * FIO interface is absolute offset, so we have to keep track of the file
 * position to avoid a seek on every i/o access.
 */

static int vm_access ( const char *, int );
static int vm_directio ( int, int );
static int vm_largefile ( long );
static int vm_reservespace ( long );

/* ZOPNBF -- Open a binary file.  The file must exist for modes RO, WO, RW.
 * A new file will always be created for mode NF, and a file will be created
 * if it does not exist for mode AP.  Append mode is write-only at EOF.
 * It is also legal to open RW and append by seeking to EOF and writing,
 * if more generality is required.
 */
/* osfn : UNIX name of file		*/
/* mode : file access mode		*/
/* chan : file number (output)		*/
int ZOPNBF ( PKCHAR *osfn, XINT *mode, XINT *chan )
{
	int fd;
	struct	stat filstat;

	/* Open or create file with given access mode.
	 */
	switch (*mode) {
	case READ_ONLY:
	    /* The O_NDELAY is necessary for some types of special devices,
	     * e.g., a FIFO, and should be harmless for other file types.
	     */
	    if ((fd = open ((const char *)osfn, O_RDONLY|O_NDELAY)) != ERR)
		fcntl (fd, F_SETFL, O_RDONLY);
	    break;
	case WRITE_ONLY:
	    if ((fd = open ((const char *)osfn, O_WRONLY|O_NDELAY)) != ERR)
		fcntl (fd, F_SETFL, O_WRONLY);
	    break;

	case READ_WRITE:
	    fd = open ((const char *)osfn, O_RDWR);
	    break;

	case NEW_FILE:
	    /* Create file and then reopen for read-write access.
	     */
	    if ((fd = creat ((const char *)osfn, _u_fmode(FILE_MODEBITS))) != ERR) {
		close (fd);
		fd = open ((const char *)osfn, 2);
	    }
	    break;

	case APPEND:
	    /* It is legal to append to a nonexistent file.  We merely create
	     * a new, zero length file and append to it.  Read access is
	     * required on a binary file opened for appending, since FIO has
	     * to read the partial block at the end of the file before it can
	     * append to it.
	     */
	    if (access ((const char *)osfn, 0) == ERR)
		close (creat ((const char *)osfn, _u_fmode(FILE_MODEBITS)));
	    fd = open ((const char *)osfn, 2);
	    break;

	default:
	    fd = ERR;
	}

	/* Initialize the kernel file descriptor.  Seeks are illegal if the
	 * device is a character special device; the device is a "streaming"
	 * file (blksize=1) if it can only be accessed sequentially.
	 */
	if (fd != ERR && stat ((const char *)osfn, &filstat) == ERR) {
	    close (fd);
	    fd = ERR;
	}

	/* Don't set *chan until we have successfully finished opening the
	 * file, otherwise any error occuring during the open will try to
	 * close the partially opened file.
	 */
	if (fd == ERR) {
	    *chan = XERR;
	} else if (fd >= MAXOFILES) {
	    close (fd);
	    if (*mode == NEW_FILE)
		unlink ((const char *)osfn);
	    *chan = XERR;
	} else {
	    zfd[fd].fp = NULL;
	    zfd[fd].fpos = 0L;
	    zfd[fd].nbytes = 0;
	    zfd[fd].flags = (filstat.st_mode & S_IFCHR) ? KF_NOSEEK : 0;
	    zfd[fd].filesize = filstat.st_size;
	    if (!vm_access ((const char *)osfn, *mode))
		zfd[fd].flags |= KF_DIRECTIO;
	    *chan = fd;
	}

	if ( *chan == XERR ) return XERR;
	else return XOK;
}


/* ZCLSBF -- Close a binary file.
 */
int ZCLSBF ( XINT *fd, XINT *status )
{
	/* This is a bit of a kludge, but closing a FIFO pipe opened for
	 * reading (probably attempting the close before the writer has
	 * closed the connection) causes an EPERM error on the close.
	 * This is harmless and only causes the VOS task to report an
	 * error, so ignore the error.
	 */
	if ((*status = (close (*fd) == ERR) ? XERR : XOK) == XERR)
	    if (errno == EPERM)
		*status = XOK;

	return *status;
}


/* ZARDBF -- "Asynchronous" binary block read.  Initiate a read of at most
 * maxbytes bytes from the file FD into the buffer BUF.  Status is returned
 * in a subsequent call to ZAWTBF.
 */
/* chan     : UNIX file number			*/
/* buf      : output buffer			*/
/* maxbytes : max bytes to read			*/
/* offset   : 1-indexed file offset to read at	*/
int ZARDBF ( XINT *chan, XCHAR *buf, XSIZE_T *maxbytes, XLONG *offset )
{
	struct fiodes *kfp;
	off_t fileoffset;
	int aligned;
	int fd;

	fd = *chan;
	kfp = &zfd[fd];
	fileoffset = *offset - 1L;

	/* If reading from a device on which seeks are illegal, offset should
	 * be zero (as when called by ZARDCL).  Otherwise, we must seek to
	 * the desired position.
	 */
	if (*offset > 0 && kfp->fpos != fileoffset)
	    if ((kfp->fpos = lseek(fd,fileoffset,0)) == ERR) {
		kfp->nbytes = ERR;
		return XERR;
	    }

	/* Disable direct i/o if transfers are not block aligned. */
	aligned = (!(fileoffset % SZ_DISKBLOCK) && !(*maxbytes % SZ_DISKBLOCK));
	if ((kfp->flags & KF_DIRECTIO) && !aligned)
	    kfp->flags &= ~KF_DIRECTIO;

	if (kfp->flags & KF_DIRECTIO)
	    vm_directio (fd, 1);

	if ((kfp->nbytes = read (fd, (char *)buf, *maxbytes)) > 0)
	    kfp->fpos += kfp->nbytes;

	if (kfp->flags & KF_DIRECTIO && aligned)
	    vm_directio (fd, 0);

	return XOK;
}


/* ZAWRBF -- "Asynchronous" binary block write.  Initiate a write of exactly
 * nbytes bytes from the buffer BUF to the file FD.  Status is returned in a
 * subsequent call to ZAWTBF.
 */
/* chan   : UNIX file number		*/
/* buf    : buffer containing data	*/
/* nbytes : nbytes to be written	*/
/* offset : 1-indexed file offset	*/
int ZAWRBF ( XINT *chan, XCHAR *buf, XSIZE_T *nbytes, XLONG *offset )
{
	struct fiodes *kfp;
	off_t fileoffset;
	int aligned;
	int fd;

	fd = *chan;
	kfp = &zfd[fd];
	fileoffset = *offset - 1L;

	/* If writing to a device on which seeks are illegal, offset should
	 * be zero (as when called by ZAWRCL).  Otherwise, we must seek to
	 * the desired position.
	 */
	if (*offset > 0 && kfp->fpos != fileoffset)
	    if ((kfp->fpos = lseek(fd,fileoffset,0)) == ERR) {
		kfp->nbytes = ERR;
		return XERR;
	    }

	/* Disable direct i/o if transfers are not block aligned. */
	aligned = (!(fileoffset % SZ_DISKBLOCK) && !(*nbytes % SZ_DISKBLOCK));
	if ((kfp->flags & KF_DIRECTIO) && !aligned)
	    kfp->flags &= ~KF_DIRECTIO;

	if (kfp->flags & KF_DIRECTIO) {
	    vm_directio (fd, 1);
	} else if (vm_largefile(*offset) || vm_largefile(*nbytes)) {
	    /* Reserve VM space if writing at EOF. */
	    struct stat st;
	    if (!fstat(fd,&st) && fileoffset >= st.st_size)
		vm_reservespace (fileoffset + *nbytes - st.st_size);
	}

	if ((kfp->nbytes = write (fd, (const char *)buf, *nbytes)) > 0)
	    kfp->fpos += kfp->nbytes;

	if (kfp->flags & KF_DIRECTIO)
	    vm_directio (fd, 0);

	/* Invalidate cached file size, forcing a UNIX system call to determine
	 * the file size the next time ZSTTBF is called.
	 */
	kfp->filesize = -1;

	return XOK;
}


/* ZAWTBF -- "Wait" for an "asynchronous" read or write to complete, and
 * return the number of bytes read or written, or ERR.
 */
int ZAWTBF ( XINT *fd, XLONG *status )
{
	if ((*status = zfd[*fd].nbytes) == ERR)
	    *status = XERR;

	return *status;
}


/* ZSTTBF -- Return status on a binary file.  The same status routine is used
 * for both blocked (random access) and streaming (sequential) binary files.
 * All character special devices are considered to be streaming files, although
 * such is not necessarily the case.  Seeks are illegal on character special
 * devices.  The test for file type is made when the file is opened.
 */
int ZSTTBF ( XINT *fd, XINT *param, XLONG *lvalue )
{
	struct fiodes *kfp = &zfd[*fd];
	struct stat filstat;
	XINT x_status = XOK;

	switch (*param) {
	case FSTT_BLKSIZE:
	    /* If all disk devices do not have the same block size then
	     * device dependent code should be substituted for the reference
	     * to SZ_DISKBLOCK below.
	     */
	    if (kfp->flags & KF_NOSEEK)
		(*lvalue) = 1L;
	    else
		(*lvalue) = SZ_DISKBLOCK;
	    break;

	case FSTT_FILSIZE:
	    /* The file size is undefined if the file is a streaming file.
	     * For blocked files the file size is determined at open time
	     * and cached in the kernel file descriptor.  The cached value
	     * is updated when we are called and invalidated whenever the file
	     * is written to.  It is not worthwhile trying to keep track of
	     * the file size in the kernel because FIO only calls us to
	     * determine the filesize once, at open time.  Caching the size
	     * saves us one FSTAT system call at open time.
	     */
	    if (kfp->flags & KF_NOSEEK)
		(*lvalue) = 0L;
	    else if ((*lvalue = kfp->filesize) < 0) {
		if (fstat ((int)*fd, &filstat) == ERR) {
		    (*lvalue) = XERR;
		    x_status = XERR;
		}
		else
		    (*lvalue) = kfp->filesize = filstat.st_size;
	    }
	    break;

	case FSTT_OPTBUFSIZE:
	    /* On some systems this parameter may be device dependent in which
	     * case device dependent code should be substituted here.
	     */
	    (*lvalue) = BF_OPTBUFSIZE;
	    break;

	case FSTT_MAXBUFSIZE:
	    /* On some systems this parameter may be device dependent in which
	     * case device dependent code should be substituted here.
	     */
	    (*lvalue) = BF_MAXBUFSIZE;
	    break;

	default:
	    (*lvalue) = XERR;
	    x_status = XERR;
	    break;
	}

	return x_status;
}


/* _U_FMODE -- Compute the effective file mode, taking into account the
 * current process umask.  (A no-op at present).
 */
int _u_fmode ( int mode )
{
	return (mode);
}


/*
 * VMcache client interface
 * 
 *	      vm_access (fname, mode)
 *	vm_reservespace (nbytes)
 *	    vm_directio (fd, flag)
 *
 * This small interface implements a subset of the client commands provided
 * by the VMcache daemon (virtual memory cache controller).  The client
 * interface handles connection to the VMcache daemon (if any) transparently
 * within the interface.
 */
#include <signal.h>

#if (defined(LINUX) || defined(CYGWIN))
#define USE_SIGACTION
#endif

#define DEF_ACCESSVAL	1
#define ENV_VMPORT	"VMPORT"
#define ENV_VMCLIENT	"VMCLIENT"
#define DEF_VMTHRESH	(1024*1024*8)
#define DEF_DIOTHRESH	(1024*1024*8)
#define DEF_VMPORT	8677
#define SZ_CMDBUF	2048
#define SZ_CNAME	32

static int vm_debug = 0;
static int vm_dioenabled = 0;
static int vm_enabled = 1;
static int vm_initialized = 0;
static int vm_server = 0;
static int vm_threshold = DEF_VMTHRESH;
static int dio_threshold = DEF_DIOTHRESH;
static int vm_port = DEF_VMPORT;
static char vm_client[SZ_CNAME+1];

static void vm_initialize( void );
static void vm_shutdown( void );
static void vm_identify( void );
static int vm_write ( int, const char *, size_t );
static int vm_connect( void );
static int getstr ( const char **, char *, size_t, int );

/* VM_ACCESS -- Access a file via the VM subsystem.  A return value of 1
 * indicates that the file is (or will be) "cached" in virtual memory, i.e.,
 * that normal virtual memory file system (normal file i/o) should be used
 * to access the file.  A return value of 0 indicates that direct i/o should
 * be used to access the file, bypassing the virtual memory file system.
 */
static int vm_access ( const char *fname, int mode )
{
	struct stat st;
	const char *modestr;
	char buf[SZ_COMMAND];
	int status;
	/* for realpath() */
#ifdef LINUX
	char *pathname;
#else
	char pathname[PATH_MAX+1];
#endif
	const char *realpath_status;

	/* One-time process initialization. */
	if (!vm_initialized)
	    vm_initialize();

	if (stat (fname, &st) < 0) {
	    status = DEF_ACCESSVAL;
	    goto done;
	}

	/* If directio is enabled and the file exceeds the directio threshold
	 * use directio to access the file (access=0).  If vmcache is
	 * disabled use normal VM-based i/o to access the file (access=1).
	 * If VMcache is enabled we still only use it if the file size
	 * exceeds vm_threshold.
	 */
	if (vm_dioenabled) {
	    status = (st.st_size >= dio_threshold) ? 0 : 1;
	    goto done;
	} else if (!vm_enabled || st.st_size < vm_threshold) {
	    status = DEF_ACCESSVAL;
	    goto done;
	}

	/* Use of VMcache is enabled and the file equals or exceeds the 
	 * minimum size threshold.  Initialization has already been performed.
	 * Open a VMcache daemon server connection if we don't already have
	 * one.  If the server connection fails we are done, but we will try
	 * to open a connection again in the next file access.
	 */
	if (!vm_server)
	    if (vm_connect() < 0) {
		status = DEF_ACCESSVAL;
		goto done;
	    }

	/* Compute the mode string for the server request. */
	switch (mode) {
	case READ_ONLY:
	    modestr = "ro";
	    break;
	case NEW_FILE:
	case READ_WRITE:
	case APPEND:
	    modestr = "rw";
	    break;
	default:
	    modestr = "ro";
	    break;
	}

	/* Format and send the file access directive to the VMcache daemon.
	 * The status from the server is returned as an ascii integer value
	 * on the same socket.
	 */
#ifdef LINUX
	pathname = realpath(fname,NULL);
	realpath_status = pathname;
#else
	realpath_status = realpath(fname,pathname);
#endif
	if ( realpath_status == NULL ) 
	    sprintf (buf,"\n");
	else
	    snprintf (buf, SZ_COMMAND, "access %s %s\n", pathname, modestr);
#ifdef LINUX
	if (pathname) free(pathname);
#endif
	if (vm_write (vm_server, buf, strlen(buf)) < 0) {
	    vm_shutdown();
	    status = DEF_ACCESSVAL;
	    goto done;
	}
	if (read (vm_server, buf, SZ_CMDBUF) <= 0) {
	    if (vm_debug)
		fprintf (stderr,
		    "vmclient (%s): server not responding\n", vm_client);
	    vm_shutdown();
	    status = DEF_ACCESSVAL;
	    goto done;
	}

	status = atoi (buf);
done:
	if (vm_debug)
	    fprintf (stderr, "vmclient (%s): access `%s' -> %d\n",
		vm_client, fname, status);

	return (status < 0 ? DEF_ACCESSVAL : status);
}


/* VM_DELETE -- Delete any VM space used by a file, e.g., because the file
 * is being physically deleted.  This should be called before the file is
 * actually deleted so that the cache can determine its device and inode
 * values.
 */
int vm_delete ( const char *fname, int force )
{
	struct stat st;
	char buf[SZ_COMMAND];
	int status = 0;
	/* for realpath() */
#ifdef LINUX
	char *pathname;
#else
	char pathname[PATH_MAX+1];
#endif
	const char *realpath_status;

	/* One-time process initialization. */
	if (!vm_initialized)
	    vm_initialize();

	if (stat (fname, &st) < 0) {
	    status = -1;
	    goto done;
	}

	/* If VMcache is not being used we are done. */
	if (vm_dioenabled && (st.st_size >= dio_threshold))
	    goto done;
	else if (!vm_enabled || st.st_size < vm_threshold)
	    goto done;

	/* Don't delete the VM space used by the file if it has hard links
	 * and only a link is being deleted (force flag will override).
	 */
	if (st.st_nlink > 1 && !force)
	    goto done;

	/* Connect to the VMcache server if not already connected. */
	if (!vm_server)
	    if (vm_connect() < 0) {
		status = -1;
		goto done;
	    }

	/* Format and send the delete directive to the VMcache daemon.
	 * The status from the server is returned as an ascii integer value
	 * on the same socket.
	 */
#ifdef LINUX
	pathname = realpath(fname,NULL);
	realpath_status = pathname;
#else
	realpath_status = realpath(fname,pathname);
#endif
	if ( realpath_status == NULL ) 
	    sprintf (buf,"\n");
	else
	    snprintf (buf, SZ_COMMAND, "delete %s\n", pathname);
#ifdef LINUX
	if (pathname) free(pathname);
#endif
	if (vm_write (vm_server, buf, strlen(buf)) < 0) {
	    vm_shutdown();
	    status = -1;
	    goto done;
	}
	if (read (vm_server, buf, SZ_CMDBUF) <= 0) {
	    if (vm_debug)
		fprintf (stderr,
		    "vmclient (%s): server not responding\n", vm_client);
	    vm_shutdown();
	    status = -1;
	    goto done;
	}

	status = atoi (buf);
done:
	if (vm_debug)
	    fprintf (stderr, "vmclient (%s): delete `%s' -> %d\n",
		vm_client, fname, status);

	return (status < 0 ? -1 : status);
}


/* VM_RESERVESPACE -- Reserve VM space for file data.  This directive is
 * useful if VM is being used but the VM space could not be preallocated
 * at file access time, e.g., when opening a new file.
 */
static int vm_reservespace ( long nbytes )
{
	char buf[SZ_CMDBUF];
	int status;

	if (!vm_initialized)
	    vm_initialize();
	if (!vm_enabled || vm_dioenabled)
	    return (-1);
	if (vm_connect() < 0)
	    return (-1);

	/* Format and send the file access directive to the VMcache daemon.
	 * The status from the server is returned as an ascii integer value
	 * on the same socket.
	 */
	snprintf (buf, SZ_CMDBUF, "reservespace %ld\n", (long)nbytes);
	if (vm_debug)
	    fprintf (stderr, "vmclient (%s): %s", vm_client, buf);

	if (vm_write (vm_server, buf, strlen(buf)) < 0) {
	    vm_shutdown();
	    return (-1);
	}
	if (read (vm_server, buf, SZ_CMDBUF) <= 0) {
	    if (vm_debug)
		fprintf (stderr,
		    "vmclient (%s): server not responding\n", vm_client);
	    vm_shutdown();
	    return (-1);
	}

	status = atoi (buf);
	return (status);
}


/* VM_IDENTIFY -- Identify the current process to the VM cache server when
 * opening a new client connection.
 */
static void vm_identify( void )
{
	char buf[SZ_CMDBUF];
	/* int status; */

	if (vm_write (vm_server, vm_client, strlen(vm_client)) < 0)
	    vm_shutdown();

	if (read (vm_server, buf, SZ_CMDBUF) <= 0) {
	    if (vm_debug)
		fprintf (stderr,
		    "vmclient (%s): server not responding\n", vm_client);
	    vm_shutdown();
	}
}


/* VM_LARGEFILE -- Test if the given offset or file size exceeds the VMcache
 * threshold.  Zero (false) is returned if the offset is below the threshold
 * or if VMcache is disabled.
 */
static int vm_largefile ( long nbytes )
{
	return (vm_enabled && nbytes >= vm_threshold);
}


/* VM_DIRECTIO -- Turn direct i/o on or off for a file.  Direct i/o is raw
 * i/o from the device to process memory, bypassing system virtual memory.
 */
static int vm_directio ( int fd, int flag )
{
#ifdef SOLARIS
	/* Currently direct i/o is implemented only for Solaris. */
	if (vm_debug > 1)
	    fprintf (stderr, "vmclient (%s): directio=%d\n", vm_client, flag);
	return (directio (fd, flag));
#else
	return (-1);
#endif
}


/* VM_INITIALIZE -- Called once per process to open a connection to the
 * vmcache daemon.  The connection is kept open and is used for all
 * subsequent vmcache requests by the process.
 */
static void vm_initialize( void )
{
	const char *ip;
	char *op, *maxop;
	/* XINT acmode = READ_WRITE; */
	char token[SZ_FNAME], value[SZ_FNAME];
	char buf[SZ_FNAME];
	const char *argp;
	/* int fd; */

	/* Extract the process name minus the file path. */
	maxop = vm_client + SZ_CNAME+1 -1;
	op = vm_client;
	for ( ip=os_process_name ; (*ip) ; ip++ ) {
	    if ( *ip == '/' ) {
		op = vm_client;
	    }
	    else if ( op < maxop ) {
		*op = *ip;
		op++;
	    }
	}
	*op = EOS;

	/* Get the server socket port if set in the user environment. */
	if (argp = getenv (ENV_VMPORT))
	    vm_port = atoi (argp);

	/* Get the VM client parameters if an initialization string is
	 * defined in the user environment.
	 */
	if (argp = getenv (ENV_VMCLIENT)) {
	    while (getstr (&argp, buf, SZ_FNAME, ',') > 0) {
		char *modchar;
		const char *cp = buf;
		int haveval;

		/* Parse "token[=value]" */
		if (getstr (&cp, token, SZ_FNAME, '=') <= 0)
		    continue;
		haveval = (getstr (&cp, value, SZ_FNAME, ',') > 0);

		if (strcmp (token, "enable") == 0) {
		    vm_enabled = 1;
		} else if (strcmp (token, "disable") == 0) {
		    vm_enabled = 0;

		} else if (strcmp (token, "debug") == 0) {
		    vm_debug = 1;
		    if (haveval)
			vm_debug = strtol (value, &modchar, 10);

		} else if (strcmp (token, "threshold") == 0 && haveval) {
		    vm_threshold = strtol (value, &modchar, 10);
		    if (*modchar == 'k' || *modchar == 'K')
			vm_threshold *= 1024;
		    else if (*modchar == 'm' || *modchar == 'M')
			vm_threshold *= (1024 * 1024);

		} else if (strcmp (token, "directio") == 0) {
		    vm_dioenabled = 1;
		    if (haveval) {
			dio_threshold = strtol (value, &modchar, 10);
			if (*modchar == 'k' || *modchar == 'K')
			    dio_threshold *= 1024;
			else if (*modchar == 'm' || *modchar == 'M')
			    dio_threshold *= (1024 * 1024);
		    }
		}
	    }
	}

	if (vm_debug) {
	    fprintf (stderr, "vmclient (%s): vm=%d dio=%d ",
		vm_client, vm_enabled, vm_dioenabled);
	    fprintf (stderr, "vmth=%d dioth=%d port=%d\n",
		vm_threshold, dio_threshold, vm_port);
	}

	/* Attempt to open a connection to the VMcache server.  */
	if (vm_enabled && !vm_dioenabled)
	    vm_connect();

#ifdef SUNOS
	on_exit (vm_shutdown, NULL);
#else
	atexit (vm_shutdown);
#endif
	vm_initialized++;
}


/* VM_CONNECT -- Connect to the VMcache server.
 */
static int vm_connect( void )
{
	XINT acmode = READ_WRITE;
	PKCHAR osfn[SZ_FNAME];
	XINT fd;
	int status = 0;

	/* Already connected? */
	if (vm_server)
	    return (0);

	snprintf ((char *)osfn, SZ_FNAME, "inet:%d::", vm_port);
	if (vm_debug)
	    fprintf (stderr,
		"vmclient (%s): open server connection `%s' -> ",
		vm_client, (char *)osfn);

	ZOPNND (osfn, &acmode, &fd);
	if (fd == XERR) {
	    if (vm_debug)
		fprintf (stderr, "failed\n");
	    status = -1;
	} else {
	    vm_server = fd;
	    if (vm_debug)
		fprintf (stderr, "fd=%ld\n", (long)fd);
	    vm_identify();
	}

	return (status);
}


/* VM_SHUTDOWN -- Called at process exit to shutdown the VMcached server
 * connection.
 */
static void vm_shutdown( void )
{
	XINT status, x_vm_server;

	if (vm_server) {
	    if (vm_debug)
		fprintf (stderr,
		    "vmclient (%s): shutdown server connection\n", vm_client);
	    vm_write (vm_server, "bye\n", 4);
	    x_vm_server = vm_server;
	    ZCLSND (&x_vm_server, &status);
	}
	vm_server = 0;
}


/* VM_WRITE -- Write to the server.  We need to encapsulate write so that
 * SIGPIPE can be disabled for the duration of the write.  We don't want the
 * calling process to abort if the VMcache server goes away.
 */
static int vm_write ( int fd, const char *buf, size_t nbytes )
{
	int status;
#ifdef USE_SIGACTION
	struct sigaction oldact;
#else
	signal_handler_t oldact;
#endif

	if (vm_debug > 1) {
	    fprintf (stderr, "vmclient (%s):: %s", vm_client, buf);
	    if (buf[nbytes-1] != '\n')
		fprintf (stderr, "\n");
	}

#ifdef USE_SIGACTION
        sigaction (SIGPIPE, NULL, &oldact);
	status = write (fd, buf, nbytes);
        sigaction (SIGPIPE, &oldact, NULL);
#else
	oldact = signal (SIGPIPE, SIG_IGN);
	status = write (fd, buf, nbytes);
        signal (SIGPIPE, oldact);
#endif

	if (vm_debug && status < 0)
	    fprintf (stderr,
		"vmclient (%s): server not responding\n", vm_client);

	return (status);
}


/* GETSTR -- Internal routine to extract a metacharacter delimited substring
 * from a formatted string.  The metacharacter to be taken as the delimiter
 * is passed as an argument.  Any embedded whitespace between the tokens is
 * stripped.  The number of characters in the output token is returned as 
 * the function value, or zero if EOS or the delimiter is reached.
 */
static int getstr ( const char **ipp, char *obuf, size_t bufsize, int delim )
{
	const char *ip = *ipp;
	char *op;
	char *otop = obuf + bufsize - 1;

	while (*ip && isspace(*ip))
	    ip++;
	for (op=obuf;  *ip;  ip++) {
	    if (*ip == delim) {
		ip++;
		break;
	    } else if (op < otop && !isspace(*ip))
		*op++ = *ip;
	}

	if ( 0 < bufsize ) *op = '\0';
	*ipp = ip;

	return (op - obuf);
}
