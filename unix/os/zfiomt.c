/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <sys/errno.h>
#include <stdio.h>

#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/*
 * ZFIOMT.C -- MTIO zz-routines for 4.XBSD Berkeley UNIX.  On this system
 * we can skip records and files in either direction, making it easy to
 * position the tape.  When a file opened for writing is closed, UNIX always
 * writes a new EOT and leaves us positioned between the tapemarks.
 * I/O is not asynchronous.  The mapping of logical magtape names to UNIX
 * device names is defined by the entries in the table "dev$devices", which
 * is read by the VOS before we are called.
 */

extern	int errno;

#define	MAX_ERRIGNORE	10		/* max errs before skiprec	*/
#define	MAX_ERRCNT	20		/* max errs before EOF		*/
#define	errcnt		io_flags	/* i/o error count		*/


/* ZZOPMT -- Open the indicated magtape device at the given density.
 * On output, "newfile" contains the number of the file actually opened,
 * which is less than what was requested if EOT is reached.  The density
 * parameter is ignored since the density was selected when the unix
 * device name was selected from the DEV$DEVICES table.
 */
ZZOPMT (dev, density, acmode, oldrec, oldfile, newfile, oschan)
PKCHAR	*dev;		/* unix device name, minus the /dev	*/
XINT	*density;	/* density; not used at this level	*/
XINT	*acmode;	/* read_only or write_only for tapes	*/
XINT	*oldrec;	/* record currently positioned to	*/
XINT	*oldfile;	/* file currently positioned to		*/
XINT	*newfile;	/* file to be opened or EOT		*/
XINT	*oschan;	/* OS channel of opened file		*/
{
	int	read_only = 0, write_only = 1;

	/* Position to the desired file.  Open the tape read-only for
	 * positioning, so that an interrupt occurring while seeking to EOT
	 * for writing will not result in truncation of the tape!
	 * BE SURE TO RETURN OSCHAN as soon as the device is physically
	 * opened, so that the error recovery code can close the file if we
	 * are interrupted.
	 */
	(*oschan) = zzopenmt ((char *)dev, read_only);
	if (*oschan == ERR) {
	    *oschan = XERR;
	    return;
	} else if (*oschan >= MAXOFILES) {
	    close ((int)*oschan);
	    *oschan = XERR;
	    return;
	} else if (zzposmt_ (oschan, oldrec, oldfile, newfile) == XERR) {
	    close ((int)*oschan);
	    *oschan = XERR;
	    return;
	}

	/* Open file with specified access mode and return OS channel.
	 */
	switch (*acmode) {
	case READ_ONLY:
	    break;
	case WRITE_ONLY:
	    close ((int)*oschan);
	    *oschan = zzopenmt ((char *)dev, write_only);
	    if (*oschan == ERR)
		*oschan = XERR;
	    break;
	default:
	    *oschan = XERR;
	}

	zfd[*oschan].errcnt = 0;
}


/* ZZCLMT -- Close magtape.  Write a new EOT mark at the current position
 * if so indicated.  UNIX always writes an EOT mark when a tape opened for
 * writing is closed, so we ignore the access mode argument here.
 */
ZZCLMT (oschan, access_mode, nrecords, nfiles, status)
XINT	*oschan;
XINT	*access_mode;
XINT	*nrecords;
XINT	*nfiles;
XINT	*status;
{
	*status = (close ((int)*oschan) == ERR) ? XERR : XOK;
	*nfiles = (*access_mode == WRITE_ONLY) ? 1 : 0;
	*nrecords = 0;
}


/* ZZRDMT -- Read next tape record.  We are supposed to be asynchronous,
 * so save read status for return by next call to ZZWTMT.  Read returns
 * zero byte count if EOF is seen, as required by the specs, so we need
 * do nothing special in that case.  Tape is left positioned just past the
 * tape mark.
 */
ZZRDMT (oschan, buf, maxbytes)
XINT	*oschan;
XCHAR	*buf;
XINT	*maxbytes;
{
	static	struct mtop mt_fwdskiprecord = { MTFSR, 1 };
	register int fd, status;

	fd = *oschan;

	/* If an error occurs on the read we assume that the tape has advanced
	 * beyond the bad record, and that the next read will return the next
	 * record on the tape.  If this is not true and a read error loop
	 * occurs, we try skipping a record forward.  If we continue to get
	 * read errors, we give up and return a premature EOF on the file.
	 */
	if ((status = read (fd, (char *)buf, (int)*maxbytes)) == ERR)
	    if ((zfd[*oschan].errcnt)++ >= MAX_ERRCNT)
		status = 0;			/* give up; return EOF */
	    else if (zfd[*oschan].errcnt >= MAX_ERRIGNORE)
		ioctl ((int)*oschan, MTIOCTOP, (char *)&mt_fwdskiprecord);

	zfd[fd].nbytes = status;
}


/* ZZWRMT -- Write next tape record.  We are supposed to be asynchronous,
 * so save write status for return by next call to ZZWTMT.  It is an error
 * if fewer than the given number of bytes are written.
 */
ZZWRMT (oschan, buf, nbytes)
XINT	*oschan;
XCHAR	*buf;
XINT	*nbytes;
{
	register int fd;
	register int nb;

	fd = *oschan;
	nb = *nbytes;
	if ((zfd[fd].nbytes = write (fd, (char *)buf, nb)) != nb)
	    zfd[fd].nbytes = ERR;
}


/* ZZWTMT -- "Wait" for i/o transfer to complete, and return the number of
 * bytes transferred or XERR.  On UNIX, a read at EOF returns a byte count
 * of zero, as required by the interface, so we do not have to do anything
 * special in that case.  Leave file positioned to just before EOF when EOF
 * is reached on a read.
 */
ZZWTMT (oschan, nrecords, nfiles, bytecount)
register XINT	*oschan;
XINT	*nrecords;
XINT	*nfiles;
register XINT	*bytecount;
{
	XINT	status;

	*nrecords = 0;
	*nfiles = 0;

	if ((*bytecount = zfd[*oschan].nbytes) == ERR)
	    *bytecount = XERR;
	else if (*bytecount == 0) {			/* saw EOF	*/
	    if (zzfbmt_ (oschan, &status) == XERR)
		*bytecount = XERR;
	} else						/* record rd/wr	*/
	    *nrecords = 1;
}


/* ZZRWMT -- Rewind the tape.  Return immediately if possible, i.e., do not
 * wait for the rewind to complete.  We assume that the OS driver will
 * provide synchronization if another tape operation is requested before the
 * rewind completes.
 */
ZZRWMT (dev, status)
PKCHAR	*dev;
XINT	*status;
{
	int	oschan, read_only=0;

	if ((oschan = zzopenmt ((char *)dev, read_only)) == ERR)
	    *status = XERR;
	else if ((*status = zzrewindmt (oschan)) == ERR)
	    *status = XERR;
	else
	    close (oschan);
}


/*
 * The remaining routines are used by ZZOPMT, but are
 * NOT FORMALLY PART OF THE INTERFACE.
 * The file and record skip primitives are particularly machine dependent.
 */

/* ZZOPENMT -- Convert the UNIX magtape device into a pathname and open
 * the drive.  Return OS channel to caller.  Do not move tape.
 */
zzopenmt (dev, acmode)
char	*dev;		/* device name or pathname		*/
int	acmode;		/* read_only or write_only for tapes	*/
{
	char	path[SZ_PATHNAME+1];

	/* If the device name is already a pathname leave it alone, else
	 * prepend the /dev/ prefix.
	 */
	strcpy (path, (*(char *)dev == '/') ? "" : "/dev/");
	strcat (path, (char *)dev);

	return (open (path, acmode));
}


/* ZZREWINDMT -- Rewind primitive.  Unfortunately this is not asynchronous
 * on our UNIX system (it should be if possible).  A different driver would
 * fix the problem.
 */
zzrewindmt (oschan)
int	oschan;
{
	static	struct mtop mt_rewind = { MTREW, 1 };

	/* NOSTRICT */
	return (ioctl (oschan, MTIOCTOP, (char *)&mt_rewind));
}


/* ZZPOSMT -- Position to the beginning of a file or to EOT.  Return the
 * actual file number in "newfile".
 */
zzposmt_ (oschan, oldrec, oldfile, newfile)
register XINT	*oschan;
XINT	*oldrec;
XINT	*oldfile;
XINT	*newfile;
{
	register int new, old, rec;
	XINT	status;

	new = *newfile;
	old = *oldfile;
	rec = *oldrec;

	if (new == 0) {					/* do not move tape */
	    status = XOK;

	} else if (new == 1) {				/* rewind tape	*/
	    status = (zzrewindmt ((int)*oschan) == ERR) ? XERR : XOK;

	} else if (new > 1 && new <= old) {		/* backspace	*/
	    while (new <= old--)
		if (zzfbmt_ (oschan, &status) == XERR)
		    break;
	    if (status != XERR)
		zzrfmt_ (oschan, &status);

	} else {					/* forward, EOT	*/
	    /* EOT is flagged as new < 0.
	     */
	    while (old < new || new < 0) {
		/* Skip forward to the next tape mark.  This has to be done
		 * carefully to prevent tape runaway.  We detect EOT by noting
		 * that if oldrec=1 and zzrfmt_ immediately passes a tapemark,
		 * we have a zero length file and have therefore passed the
		 * second tapemark of the EOT marker.
		 */
		zzrfmt_ (oschan, &status);
		if (status == XERR) {
		    /* If we get an error on the forward skip record it is
		     * probably harmless, since the next operation will likely
		     * be a skip file anyhow.
		     */
		    fprintf (stderr, "Warning [file=%d, record=%d, errno=%d]: ",
			old, rec, errno);
		    fprintf (stderr, "possible tape positioning error\n");
		    status = XOK;
		}

		if (status == XEOF && rec == 1) {
		    /* At EOT.  Skip back and then forward over the first
		     * tape mark to cancel the inter record gap between the
		     * two tape marks.
		     */
		    zzrbmt_ (oschan, &status);
		    zzrbmt_ (oschan, &status);
		    zzrfmt_ (oschan, &status);
		    break;
		} else if (status != XEOF) {
		    /* File skip the remainder of the records to avoid loading
		     * the system with skip record system calls.
		     */
		    zzffmt_ (oschan, &status);
		}

		if (status == XERR)
		    break;

		old++;
		rec = 1;
	    }
	    *newfile = old;
	}

	if (status == XERR)
	    *newfile = XERR;

	return (*newfile);
}


/* ZZRFMT -- Skip record forward.  Advance one record forward.  Return XOK if
 * an ordinary record is successfully skipped, XEOF if a tape mark is skipped,
 * or XERR if there is any error.  READ rather than IOCTL is used to skip a
 * record because all unix device drivers do not return EOF when the record
 * is a tape mark.  READ should always return 0 when a tapemark is seen.
 * We assume that if we read part of a record the drive physically reads the
 * entire record.
 *
 * NOTES - This routine has caused portability problems on various machines.
 * The mtio skip record function should not be used because it does not
 * reliably return -1 (EOF) when a tape mark is seen.  A read should always
 * return 0 at EOF since that is standard unix, but on some machines if the
 * buffer size requested is smaller than the tape block size, ERR is returned,
 * whereas on other machines the amount of data read is returned.  The current
 * solution is to use read() to read into a buffer larger than the maximum
 * tape block size; anything greater than 28800 (FITS blocked to 10) is a
 * nice number.  Unfortunately, this can also cause problems if we use automatic
 * storage for the buffer, but we want to use auto storage (rather than malloc)
 * for efficiency reasons.  The number 29184 (512 * 57) is used because the
 * more logical choice, 32768, causes a compile failure on at least one unix
 * host due to limitations on the internal compiler data structures.
 */
zzrfmt_ (oschan, status)
XINT	*oschan;
XINT	*status;
{
	register int	nb;
	char	buf[29184];

	/* The following subtlety was found to be necessary on the Sun-4.
	 * The first time the process is run it is likely that the hardware
	 * stack will not be large enough for the big buffer.  The first
	 * reference to BUF will cause an access violation, which is supposed
	 * to cause UNIX to quietly increase the stack size.  However, this
	 * fault was occuring during the read from the tape, and due to a bug
	 * in the tape driver would result in a read error returning errno 14,
	 * EFAULT or bad address (fault during execution of system call).
	 * The dummy assignment below causes this fault to occur during
	 * user process execution to workaround the bug, and is harmless if
	 * not needed.
	 */
	buf[0] = 0;

	nb = read ((int)*oschan, buf, 29184);
	if (nb < 0)
	    return (*status = XERR);
	else if (nb == 0)
	    return (*status = XEOF);
	else
	    return (*status = XOK);
}


/* ZZRBMT -- Skip record backward.
 */
zzrbmt_ (oschan, status)
XINT	*oschan;
XINT	*status;
{
	static	struct mtop mt_backskiprec = { MTBSR, 1 };

	/* Ignore i/o errors (errno=EIO); ERR may be returned when skipping
	 * back over a filemark.
	 */
	if (ioctl ((int)*oschan, MTIOCTOP, (char *)&mt_backskiprec) == ERR)
	    *status = ((errno == EIO) ? XOK : XERR);
	else
	    *status = XOK;

	return (*status);
}


/* ZZFBMT -- Skip file backward.
 */
zzfbmt_ (oschan, status)
XINT	*oschan;
XINT	*status;
{
	static	struct mtop mt_backskipfile = { MTBSF, 1 };

	if (ioctl ((int)*oschan, MTIOCTOP, (char *)&mt_backskipfile) == ERR)
	    *status = XERR;
	else
	    *status = XOK;

	return (*status);
}


/* ZZFFMT -- Skip file forward.
 */
zzffmt_ (oschan, status)
XINT	*oschan;
XINT	*status;
{
	static	struct mtop mt_fwdskipfile = { MTFSF, 1 };

	if (ioctl ((int)*oschan, MTIOCTOP, (char *)&mt_fwdskipfile) == ERR)
	    *status = XERR;
	else
	    *status = XOK;

	return (*status);
}
