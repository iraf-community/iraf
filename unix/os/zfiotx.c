/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <setjmp.h>
#include <sgtty.h>
#include <stdio.h>
#include <errno.h>

# ifndef O_NDELAY
#include <fcntl.h>
# endif

#define	import_kernel
#define	import_knames
#define	import_zfstat
#define	import_fset
#define import_spp
#include <iraf.h>

#define	CTRLC	3
extern	int errno;		/* error code returned by the kernel	*/
static	jmp_buf jmpbuf;

/*
 * ZFIOTX -- File i/o to textfiles, for UNIX 4.1BSD.  This driver is used for
 * both terminals and ordinary disk text files.  I/O is via the C library
 * stdio routines, which provide buffering.
 *
 * On input, we must check for newline and terminate when it has been read,
 * including the newline character in the return buffer and in the character
 * count.  If the buffer limit is reached before newline is found, we return
 * the line without a newline, and return the rest of the line in the next
 * read request.  If a single character is requested character mode is
 * asserted, i.e., if the device is a terminal RAW is enabled and ECHO and
 * CRMOD (CR to LF mapping) are disabled.  A subsequent request to read or
 * write more than one character restores line mode.
 *
 * On output, we transmit the specified number of characters, period.
 * No checking for newline or EOS is performed.  Text may contain null
 * characters (note that EOS == NUL).  There is no guarantee that the
 * output line will contain a newline.  When flushing partial lines to a
 * terminal, this is often not the case.  When FIO writes a very long
 * line and the FIO buffer overflows, it will flush a partial line,
 * passing the rest of the line (with newline delimiter) in the next
 * write request.  The newline should be included in the data as stored
 * on disk, if possible.  It is not necessary to map newlines upon output
 * because UNIX does this already.
 *
 * N.B.: An SPP char is usually not a byte, so these routines usually
 * perform type conversion.  The actual SPP datatype is defined in language.h
 * An SPP char is a signed integer, but only values in the range 0-127 may
 * be written to a text file.  We assume that character set conversion (e.g.,
 * ASCII to EBCDIC) is not necessary since all 4.1BSD hosts we are aware of
 * are ASCII machines.
 */

#define	MAX_TRYS	2	/* max interrupted trys to read		*/
#define	NEWLINE	'\n'
static	int	tty_fd = -1;	/* FD of tty device if charmode set	*/
static	short	tty_flags = 0;	/* terminal driver mode flag bits	*/
static	char	tty_redraw = 0;	/* screen redraw control code		*/
static	int	tty_getraw = 0;	/* raw getc in progress			*/

static	PFI	sigint, sigterm;
static	PFI	sigtstp, sigcont;
static	int	tty_onsig(), tty_stop(), tty_continue();


/* ZOPNTX -- Open or create a text file.
 */
ZOPNTX (osfn, mode, chan)
PKCHAR	*osfn;			/* UNIX filename			*/
XINT	*mode;			/* file access mode			*/
XINT	*chan;			/* UNIX channel of file (output)	*/
{
	register int fd;
	register FILE *fp;
	struct	stat filestat;
	FILE	*fopen();
	char	*fmode;

	/* Map FIO access mode into UNIX/stdio access mode.
	 */
	switch (*mode) {
	case READ_ONLY:
	    fmode = "r";
	    break;
	case READ_WRITE:
	    fmode = "r+";		/* might not work on old systems */
	    break;
	case APPEND:
	    fmode = "a";
	    break;
	case WRITE_ONLY:
	    fmode = "w";
	    break;
	case NEW_FILE:			/* create for appending */
	    fmode = "w";
	    break;
	default:
	    goto error;
	}

	/* Open or create file.  If a new file is created it is necessary
	 * to explicitly set the file access permissions as indicated by
	 * FILE_MODEBITS in kernel.h.  This is done in such a way that the
	 * user's UMASK bits are preserved.
	 */
	if ((fp = fopen ((char *)osfn, fmode)) == NULL)
	    goto error;
	if (stat ((char *)osfn, &filestat) == ERR) {
	    fclose (fp);
	    goto error;
	} else if ((fd = fileno(fp)) >= MAXOFILES) {
	    fclose (fp);
	    goto error;
	}

	if ((filestat.st_mode & S_IFREG) &&
	    (*mode == WRITE_ONLY || *mode == NEW_FILE)) {

	    if (chmod ((char *)osfn, filestat.st_mode | FILE_MODEBITS) == ERR) {
		fclose (fp);
		goto error;
	    }
	}

	/* Set up kernel file descriptor.
	 */
	fd = fileno (fp); 
	zfd[fd].fp = fp;
	zfd[fd].fpos = 0;
	zfd[fd].nbytes = 0;
	zfd[fd].io_flags = 0;
	zfd[fd].sg_flags = 0;
	zfd[fd].flags = (filestat.st_mode & S_IFCHR) ? KF_NOSEEK : KF_NOSTTY;

	*chan = fd;
	return;

error:
	*chan = XERR;
	return;
}


/* ZCLSTX -- Close a text file.
 */
ZCLSTX (fd, status)
XINT	*fd;
XINT	*status;
{
	register struct	fiodes *kfp = &zfd[*fd];
	struct	sgttyb ttystat;

	/* Disable character mode if still in effect.  If this is necessary
	 * then we have already saved the old tty status flags in sg_flags.
	 */
	if (kfp->flags & KF_CHARMODE)
	    tty_reset ((int)*fd);

	/* Close the file.  Set file pointer field of kernel file descriptor
	 * to null to indicate that the file is closed.
	 *
	 * [NOTE] -- fclose errors are ignored if we are closing a terminal
	 * device.  This was necessary on the Suns and it was not clear why
	 * a close error was occuring (errno was EPERM - not owner).
	 */
	*status = (fclose(kfp->fp) == EOF && kfp->flags&KF_NOSTTY) ? XERR : XOK;
	kfp->fp = NULL;
}


/* ZFLSTX -- Flush any buffered textual output.
 */
ZFLSTX (fd, status)
XINT	*fd;
XINT	*status;
{
	*status = (fflush (zfd[*fd].fp) == EOF) ? XERR : XOK;
}


/* ZGETTX -- Get a line of text from a text file.  Unpack machine chars
 * into type XCHAR.  If output buffer is filled before newline is encountered,
 * the remainder of the line will be returned in the next call, and the
 * current line will NOT be newline terminated.  If maxchar==1 assert
 * character mode, otherwise assert line mode.
 */
ZGETTX (fd, buf, maxchars, status)
XINT	*fd;
XCHAR	*buf;
XINT	*maxchars;
XINT	*status;
{
	register FILE	*fp;
	register int	ch, maxch = *maxchars;
	register struct	fiodes *kfp;
	register XCHAR	*op;
	int	nbytes, ntrys;

	kfp = &zfd[*fd];
	fp = kfp->fp;

	/* If maxch=1 assert char mode if legal on device, otherwise clear
	 * char mode if set.  Ignore ioctl errors if ioctl is illegal on
	 * device.
	 */
	if (maxch <= 0) {
	    *status = 0;
	    return;
     	} else if (maxch == 1) {
	    if (!(kfp->flags & KF_CHARMODE))
		tty_rawon ((int)*fd, 0);
	} else if (kfp->flags & KF_CHARMODE) {
	    /* Disable character mode.  If this is necessary then we have
	     * already saved the old tty status flags in sg_flags.
	     */
	    tty_reset ((int)*fd);
	}

	/* Copy the next line of text into the user buffer.  Keep track of
	 * file offsets of current line and next line for ZNOTTX.  A call to
	 * ZNOTTX will return the value of kfp->fpos, the file offset of the
	 * next line to be read (unless newline has not yet been seen on the
	 * current line due to a partial read).  The following while loop is
	 * the inner loop of text file input, hence is heavily optimized at
	 * the expense of some clarity.  If the host is non-ASCII add a lookup
	 * table reference to this loop to map chars from the host character
	 * set to ASCII.
	 *
	 * N.B.: If an interrupt occurs during the read and the output buffer
	 * is still empty, try to complete the read.  This can happen when
	 * an interrupt occurs while waiting for input from the terminal, in
	 * which case recovery is probably safe.
	 */
	if (!(kfp->flags & KF_CHARMODE)) {
	    /* Read in line mode.
	     */
	    ntrys = MAX_TRYS;
	    do {
		clearerr (fp);
		op = buf;
		errno = 0;
		while (*op++ = ch = getc(fp), ch != EOF)
		    if (--maxch <= 0 || ch == NEWLINE)
			break;
	    } while (errno == EINTR && op-1 == buf && --ntrys >= 0);

	    *op = XEOS;
	    nbytes = *maxchars - maxch;

	} else {
	    /* Read a single character in raw mode.  Catch interrupts and
	     * return the interrupt character as an ordinary character.
	     * We map interrupts in this way, rather than use raw mode
	     * (which disables interrupts and all other input processing)
	     * because ctrl/s ctrl/q is disabled in raw mode, and that can
	     * cause sporadic protocol failures.
	     */
	    sigint  = (PFI) signal (SIGINT,  tty_onsig);
	    sigterm = (PFI) signal (SIGTERM, tty_onsig);
	    tty_getraw = 1;

	    op = buf;
	    if ((ch = setjmp (jmpbuf)) == 0) {
		clearerr (fp);
		ch = getc (fp);
	    }

	    signal (SIGINT,  sigint);
	    signal (SIGTERM, sigterm);
	    tty_getraw = 0;

	    /* Clear parity bit just in case raw mode is used.
	     */
	    if (ch == EOF) {
		*op++ = ch;
		nbytes = 0;
	    } else {
		*op++ = (ch & 0177);
		nbytes = 1;
	    }
	    *op = XEOS;
	}

	/* Check for errors and update offsets.  If the last char copied
	 * was EOF step on it with an EOS.  In nonblocking raw mode EOF is
	 * indistinguishable from a no-data read, but it doesn't matter
	 * especially since a ctrl/d, ctrl/z, etc. typed by the user is
	 * passed through as data.
	 */
	if (ferror (fp)) {
	    clearerr (fp);
	    *op = XEOS;
	    nbytes = (kfp->flags&KF_NDELAY && errno==EWOULDBLOCK) ? 0 : XERR;
	} else {
	    kfp->nbytes += nbytes;
	    switch (*--op) {
	    case NEWLINE:
		kfp->fpos += kfp->nbytes;
		kfp->nbytes = 0;
		break;
	    case EOF:
		*op = XEOS;
		break;
	    }
	}

	*status = nbytes;
}


/* ZNOTTX -- Return the seek offset of the beginning of the current line
 * of text (file offset of char following last newline seen).
 */
ZNOTTX (fd, offset)
XINT	*fd;
XLONG	*offset;
{
	*offset = zfd[*fd].fpos;
}


/* ZPUTTX -- Put "nchars" characters into the text file "fd".   The final
 * character will always be a newline, unless the FIO line buffer overflowed,
 * in which case the correct thing to do is to write out the line without
 * artificially adding a newline.  We do not check for newlines in the text,
 * hence ZNOTTX will return the offset of the next write, which will be the
 * offset of the beginning of a line of text only if we are called to write
 * full lines of text.
 */
ZPUTTX (fd, buf, nchars, status)
XINT	*fd;				/* file to be written to	*/
register XCHAR	*buf;			/* data to be output		*/
XINT	*nchars;			/* nchars to write to file	*/
XINT	*status;			/* return status		*/
{
	register FILE	*fp;
	register int	nbytes;
	register struct fiodes *kfp = &zfd[*fd];
	XCHAR	*ip;
	char	*cp;
	int	count, ch;

	count = nbytes = *nchars;
	fp = kfp->fp;

	/* Clear raw mode if raw mode is set, the output file is a tty, and
	 * more than one character is being written.  We must get an exact
	 * match to the RAWOFF string for the string to be recognized.
	 * The string is recognized and removed whether or not raw mode is
	 * in effect.  The RAWON sequence may also be issued to turn raw
	 * mode on.  The SETREDRAW sequence is used to permit an automatic
	 * screen redraw when a suspended process receives SIGCONT.
	 */
	if (*buf == '\033' && count == LEN_RAWCMD || count == LEN_SETREDRAW) {
	    /* Note that we cannot use strcmp since buf is XCHAR. */

	    /* The disable rawmode sequence. */
	    for (ip=buf, cp=RAWOFF;  *cp && (*ip == *cp);  ip++, cp++)
		;
	    if (*cp == EOS) {
		/* If no tty stream has been established, reset stdin. */
		tty_reset ((tty_fd >= 0) ? tty_fd : 0);
		*status = XOK;
		return;
	    }

	    /* The enable rawmode sequence.  The control sequence is followed
	     * by a single modifier character, `D' denoting a normal blocking
	     * character read, and `N' nonblocking character reads.
	     */
	    for (ip=buf, cp=RAWON;  *cp && (*ip == *cp);  ip++, cp++)
		;
	    if (*cp == EOS) {
		/* If writing to stdout|stderr, set tty_fd to stdin. */
		tty_rawon ((*fd > 2) ? *fd : 0, (*ip++ == 'N') ? KF_NDELAY : 0);
		*status = XOK;
		return;
	    }
	
	    /* The set-redraw control sequence.  If the tty_redraw code is
	     * set to a nonnull value, that value will be returned to the
	     * reader in the next GETC call following a process
	     * suspend/continue, as if the code had been typed by the user.
	     */
	    for (ip=buf, cp=SETREDRAW;  *cp && (*ip == *cp);  ip++, cp++)
		;
	    if (*cp == EOS) {
		tty_redraw = *ip;
		*status = XOK;
		return;
	    }
	}

	/* Do not check for EOS; merely output the number of characters
	 * requested.  Checking for EOS prevents output of nulls, used for
	 * padding to create delays on terminals.  We must pass all legal
	 * ASCII chars, i.e., all chars in the range 0-127.  The results of
	 * storing non-ASCII chars in a text file are system dependent.
	 * If the host is not ASCII then a lookup table reference should be
	 * added to this loop to map ASCII chars to the host character set.
	 *
	 * The uio_bwrite function moves the characters in a block and is
	 * more efficient than character at a time output with putc, if the
	 * amount of text to be moved is large.
	 */
	if (nbytes < 10) {
	    for (ip=buf;  --nbytes >= 0;  ) {
		ch = *ip++;
		putc (ch, fp);
	    }
	} else
	    uio_bwrite (fp, buf, nbytes);

	kfp->fpos += count;

	/* If an error occurred while writing to the file, clear the error
	 * on the host stream and report a file write error to the caller.
	 * Ignore the special case of an EBADF occuring while writing to the
	 * terminal, which occurs when a background job writes to the terminal
	 * after the user has logged out.
	 */
	if (ferror (fp)) {
	    clearerr (fp);
	    if (errno == EBADF && (fp == stdout || fp == stderr))
		*status = count;
	    else
		*status = XERR;
	} else
	    *status = count;
}


/* ZSEKTX -- Seek on a text file to the character offset given by a prior
 * call to ZNOTTX.  This offset should always refer to the beginning of a line.
 */
ZSEKTX (fd, znottx_offset, status)
XINT	*fd;
XLONG	*znottx_offset;
XINT	*status;
{
	register struct fiodes *kfp = &zfd[*fd];

	/* Clear the byte counter, used to keep track of offsets within
	 * a line of text when reading sequentially.
	 */
	kfp->nbytes = 0;

	/* Ignore seeks to the beginning or end of file on special devices
	 * (pipes or terminals).  A more specific seek on such a device
	 * is an error.
	 */
	if (kfp->flags & KF_NOSEEK) {
	    /* Seeks are illegal on this device.  Seeks to BOF or EOF are
	     * permitted but are ignored.
	     */
	    switch (*znottx_offset) {
	    case XBOF:
	    case XEOF:
		kfp->fpos = 0;
		break;
	    default:
		kfp->fpos = ERR;
	    }
	} else {
	    /* Seeks are permitted on this device.  The seek offset is BOF,
	     * EOF, or a value returned by ZNOTTX.
	     */
	    switch (*znottx_offset) {
	    case XBOF:
		kfp->fpos = fseek (kfp->fp, 0L, 0);
		break;
	    case XEOF:
		kfp->fpos = fseek (kfp->fp, 0L, 2);
		break;
	    default:
		kfp->fpos = fseek (kfp->fp, *znottx_offset, 0);
	    }
	}

	if (kfp->fpos == ERR)
	    *status = XERR;
	else {
	    kfp->fpos = ftell (kfp->fp);
	    *status = XOK;
	}
}


/* ZSTTTX -- Get file status for a text file.
 */
ZSTTTX (fd, param, value)
XINT	*fd;			/* file number				*/
XINT	*param;			/* status parameter to be returned	*/
XLONG	*value;			/* return value				*/
{
	struct	stat filestat;

	switch (*param) {
	case FSTT_BLKSIZE:
	    *value = 1L;
	    break;
	case FSTT_FILSIZE:
	    if (fstat ((int)*fd, &filestat) == ERR)
		*value = XERR;
	    else
		*value = filestat.st_size;
	    break;
	case FSTT_OPTBUFSIZE:
	    *value = TX_OPTBUFSIZE;
	    break;
	case FSTT_MAXBUFSIZE:
	    *value = TX_MAXBUFSIZE;
	    break;
	default:
	    *value = XERR;
	    break;
	}
}


/* TTY_RAWON -- Turn on rare mode and turn off echoing and all input and
 * output character processing.  Interrupts are caught and the interrupt
 * character is returned like any other character.  Save sg_flags for
 * subsequent restoration.  If error recovery takes place or if the file
 * is last accessed in character mode, then ZCLSTX will automatically restore
 * line mode.
 */
static
tty_rawon (fd, flags)
int	fd;			/* file descriptor */
int	flags;			/* file mode control flags */
{
	register struct	fiodes *kfp = &zfd[fd];
	struct	sgttyb ttystat;

	if (!(kfp->flags & (KF_CHARMODE|KF_NOSTTY))) {
	    ioctl (fd, TIOCGETP, &ttystat);
	    kfp->sg_flags = ttystat.sg_flags;
	    kfp->flags |= KF_CHARMODE;

	    /* Set raw mode in the terminal driver. */
	    ttystat.sg_flags |= CBREAK;
	    ttystat.sg_flags &= ~(ECHO|CRMOD);
	    ioctl (fd, TIOCSETN, &ttystat);

	    /* Save FD, SG_FLAGS of raw mode tty device. */
	    tty_fd = fd;
	    tty_flags = ttystat.sg_flags;

	    /* Post signal handlers to clear/restore raw mode if process is
	     * suspended.
	     */
	    sigtstp = (PFI) signal (SIGTSTP, tty_stop);
	    sigcont = (PFI) signal (SIGCONT, tty_continue);
	}

	/* Set any file descriptor flags, e.g., for nonblocking reads. */
	if ((flags & KF_NDELAY) && !(kfp->flags & KF_NDELAY)) {
	    kfp->io_flags = fcntl (fd, F_GETFL, 0);
	    fcntl (fd, F_SETFL, kfp->io_flags | O_NDELAY);
	    kfp->flags |= KF_NDELAY;
	} else if (!(flags & KF_NDELAY) && (kfp->flags & KF_NDELAY))
	    fcntl (fd, F_SETFL, kfp->io_flags);
}


/* TTY_RESET -- Clear character at a time mode on the terminal device, if in
 * effect.  This will restore normal line oriented terminal i/o, even if raw
 * mode i/o was set on the physical device when the ioctl status flags were
 * saved.
 */
static
tty_reset (fd)
int	fd;
{
	register struct	fiodes *kfp = &zfd[fd];
	struct	sgttyb ttystat;

	if (ioctl (fd, TIOCGETP, &ttystat) == -1)
	    return;

	if (!(kfp->flags & KF_CHARMODE))
	    kfp->sg_flags = ttystat.sg_flags;

	ttystat.sg_flags = (kfp->sg_flags | (ECHO|CRMOD)) & ~CBREAK;
	ioctl (fd, TIOCSETN, &ttystat);
	kfp->flags &= ~KF_CHARMODE;

	if (kfp->flags & KF_NDELAY) {
	    fcntl (fd, F_SETFL, kfp->io_flags & ~O_NDELAY);
	    kfp->flags &= ~KF_NDELAY;
	}

	signal (SIGTSTP, sigtstp);
	signal (SIGCONT, sigcont);
}


/* TTY_ONSIG -- Catch interrupt and return a nonzero status.  Active only while
 * we are reading from the terminal in raw mode.
 */
static
tty_onsig (sig, code, scp)
int	sig;			/* signal which was trapped	*/
int	code;			/* subsignal code (vax)		*/
struct	sigcontext *scp;	/* not used			*/
{
	longjmp (jmpbuf, CTRLC);
}


/* TTY_STOP -- Called when a process is suspended while the terminal is in raw
 * mode; our function is to restore the terminal to normal mode.
 */
static
tty_stop (sig, code, scp)
int	sig;			/* signal which was trapped	*/
int	code;			/* subsignal code (vax)		*/
struct	sigcontext *scp;	/* not used			*/
{
	struct	sgttyb ttystat;

	if (ioctl (tty_fd, TIOCGETP, &ttystat) != -1) {
	    ttystat.sg_flags = (tty_flags | (ECHO|CRMOD)) & ~CBREAK;
	    ioctl (tty_fd, TIOCSETN, &ttystat);
	}

	kill (getpid(), SIGSTOP);
}


/* TTY_CONTINUE -- Called when execution of a process which was suspended with
 * the terminal in raw mode is resumed; our function is to restore the terminal
 * to raw mode.
 */
static
tty_continue (sig, code, scp)
int	sig;			/* signal which was trapped	*/
int	code;			/* subsignal code (vax)		*/
struct	sigcontext *scp;	/* not used			*/
{
	struct	sgttyb ttystat;

	if (ioctl (tty_fd, TIOCGETP, &ttystat) != -1) {
	    ttystat.sg_flags = tty_flags;
	    ioctl (tty_fd, TIOCSETN, &ttystat);
	}

	if (tty_redraw && tty_getraw)
	    longjmp (jmpbuf, tty_redraw);
}


/* UIO_BWRITE -- Block write.  Pack xchars into chars and write the data out
 * as a block with fwrite.  The 4.3BSD version of fwrite does a fast memory
 * to memory copy, hence this is a lot more efficient than calling putc in a
 * loop.
 */
static
uio_bwrite (fp, buf, nbytes)
FILE	*fp;			/* output file		*/
XCHAR	*buf;			/* data buffer		*/
int	nbytes;			/* data size		*/
{
	register XCHAR	*ip = buf;
	register char	*op;
	register int	n;
	char	obuf[1024];
	int	chunk;

	while (nbytes > 0) {
	    chunk = (nbytes <= 1024) ? nbytes : 1024;
	    for (op=obuf, n=chunk;  --n >= 0;  )
		*op++ = *ip++;

	    fwrite (obuf, 1, chunk, fp);
	    nbytes -= chunk;
	}
}
