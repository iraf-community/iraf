/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <unistd.h>

# ifndef O_NDELAY
#include <fcntl.h>
# endif

#include <errno.h>
#include <stdio.h>

#define	import_kernel
#define	import_knames
#define	import_zfstat
#define import_spp
#include <iraf.h>

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


/* ZOPNBF -- Open a binary file.  The file must exist for modes RO, WO, RW.
 * A new file will always be created for mode NF, and a file will be created
 * if it does not exist for mode AP.  Append mode is write-only at EOF.
 * It is also legal to open RW and append by seeking to EOF and writing,
 * if more generality is required.
 */
ZOPNBF (osfn, mode, chan)
PKCHAR	*osfn;			/* UNIX name of file		*/
XINT	*mode;			/* file access mode		*/
XINT	*chan;			/* file number (output)		*/
{
	register int fd;
	struct	stat filstat;

	/* Open or create file with given access mode.
	 */
	switch (*mode) {
	case READ_ONLY:
	    /* The O_NDELAY is necessary for some types of special devices,
	     * e.g., a FIFO, and should be harmless for other file types.
	     */
	    if ((fd = open ((char *)osfn, O_RDONLY|O_NDELAY)) != ERR)
		fcntl (fd, F_SETFL, O_RDONLY);
	    break;
	case WRITE_ONLY:
	    if ((fd = open ((char *)osfn, O_WRONLY|O_NDELAY)) != ERR)
		fcntl (fd, F_SETFL, O_WRONLY);
	    break;

	case READ_WRITE:
	    fd = open ((char *)osfn, O_RDWR);
	    break;

	case NEW_FILE:
	    /* Create file and then reopen for read-write access.
	     */
	    if ((fd = creat ((char *)osfn, _u_fmode(FILE_MODEBITS))) != ERR) {
		close (fd);
		fd = open ((char *)osfn, 2);
	    }
	    break;

	case APPEND:
	    /* It is legal to append to a nonexistent file.  We merely create
	     * a new, zero length file and append to it.  Read access is
	     * required on a binary file opened for appending, since FIO has
	     * to read the partial block at the end of the file before it can
	     * append to it.
	     */
	    if (access ((char *)osfn, 0) == ERR)
		close (creat ((char *)osfn, _u_fmode(FILE_MODEBITS)));
	    fd = open ((char *)osfn, 2);
	    break;

	default:
	    fd = ERR;
	}

	/* Initialize the kernel file descriptor.  Seeks are illegal if the
	 * device is a character special device; the device is a "streaming"
	 * file (blksize=1) if it can only be accessed sequentially.
	 */
	if (fd != ERR && stat ((char *)osfn, &filstat) == ERR) {
	    close (fd);
	    fd = ERR;
	}

	if ((*chan = fd) == ERR) {
	    *chan = XERR;
	} else if (fd >= MAXOFILES) {
	    close (fd);
	    if (*mode == NEW_FILE)
		unlink ((char *)osfn);
	    *chan = XERR;
	} else {
	    zfd[fd].fp     = NULL;
	    zfd[fd].fpos   = 0L;
	    zfd[fd].nbytes = 0;
	    zfd[fd].flags  = (filstat.st_mode & S_IFCHR) ? KF_NOSEEK : 0;
	    zfd[fd].filesize = filstat.st_size;
	}
}


/* ZCLSBF -- Close a binary file.
 */
ZCLSBF (fd, status)
XINT	*fd;
XINT	*status;
{
	extern	int errno;

	/* This is a bit of a kludge, but closing a FIFO pipe opened for
	 * reading (probably attempting the close before the writer has
	 * closed the connection) causes an EPERM error on the close.
	 * This is harmless and only causes the VOS task to report an
	 * error, so ignore the error.
	 */
	if ((*status = (close (*fd) == ERR) ? XERR : XOK) == XERR)
	    if (errno == EPERM)
		*status = XOK;
}


/* ZARDBF -- "Asynchronous" binary block read.  Initiate a read of at most
 * maxbytes bytes from the file FD into the buffer BUF.  Status is returned
 * in a subsequent call to ZAWTBF.
 */
ZARDBF (chan, buf, maxbytes, offset)
XINT	*chan;			/* UNIX file number			*/
XCHAR	*buf;			/* output buffer			*/
XINT	*maxbytes;		/* max bytes to read			*/
XLONG	*offset;		/* 1-indexed file offset to read at	*/
{
	register struct	fiodes *kfp;
	register int	fd;
	register long block_offset;
	off_t	lseek();

	fd = *chan;
	kfp = &zfd[fd];
	block_offset = *offset - 1L;

	/* If reading from a device on which seeks are illegal, offset should
	 * be zero (as when called by ZARDCL).  Otherwise, we must seek to
	 * the desired position.
	 */
	if (*offset > 0 && kfp->fpos != block_offset)
	    if ((kfp->fpos = lseek(fd,block_offset,0)) == ERR) {
		kfp->nbytes = ERR;
		return;
	    }

	if ((kfp->nbytes = read (fd, (char *)buf, *maxbytes)) > 0)
	    kfp->fpos += kfp->nbytes;
}


/* ZAWRBF -- "Asynchronous" binary block write.  Initiate a write of exactly
 * nbytes bytes from the buffer BUF to the file FD.  Status is returned in a
 * subsequent call to ZAWTBF.
 */
ZAWRBF (chan, buf, nbytes, offset)
XINT	*chan;			/* UNIX file number		*/
XCHAR	*buf;			/* buffer containing data	*/
XINT	*nbytes;		/* nbytes to be written		*/
XLONG	*offset;		/* 1-indexed file offset	*/
{
	register int fd;
	register struct	fiodes *kfp;
	register long block_offset;
	off_t	lseek();

	fd = *chan;
	kfp = &zfd[fd];
	block_offset = *offset - 1L;

	/* If writing to a device on which seeks are illegal, offset should
	 * be zero (as when called by ZAWRCL).  Otherwise, we must seek to
	 * the desired position.
	 */
	if (*offset > 0 && kfp->fpos != block_offset)
	    if ((kfp->fpos = lseek(fd,block_offset,0)) == ERR) {
		kfp->nbytes = ERR;
		return;
	    }

	if ((kfp->nbytes = write (fd, (char *)buf, *nbytes)) > 0)
	    kfp->fpos += kfp->nbytes;

	/* Invalidate cached file size, forcing a UNIX system call to determine
	 * the file size the next time ZSTTBF is called.
	 */
	kfp->filesize = -1;
}


/* ZAWTBF -- "Wait" for an "asynchronous" read or write to complete, and
 * return the number of bytes read or written, or ERR.
 */
ZAWTBF (fd, status)
XINT	*fd;
XINT	*status;
{
	if ((*status = zfd[*fd].nbytes) == ERR)
	    *status = XERR;
}


/* ZSTTBF -- Return status on a binary file.  The same status routine is used
 * for both blocked (random access) and streaming (sequential) binary files.
 * All character special devices are considered to be streaming files, although
 * such is not necessarily the case.  Seeks are illegal on character special
 * devices.  The test for file type is made when the file is opened.
 */
ZSTTBF (fd, param, lvalue)
XINT	*fd;
XINT	*param;
XLONG	*lvalue;
{
	register struct fiodes *kfp = &zfd[*fd];
	struct	stat filstat;

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
		if (fstat ((int)*fd, &filstat) == ERR)
		    (*lvalue) = XERR;
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
	    break;
	}
}


/* _U_FMODE -- Compute the effective file mode, taking into account the
 * current process umask.  (A no-op at present).
 */
_u_fmode (mode)
int	mode;
{
	return (mode);
}
