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
 * ----------------------------------------------------------------------------
 * Hacked version of old unix/iraf magtape driver, for Exabyte on SunOS.
 * This is a "minimum-modification" version of the driver, used to support
 * the Exabyte while the new magtape driver is undergoing burn-in testing.
 * Prepared for V2.9.1 (dct-12Aug90)
 *
 * Note - this driver supports the following devices:
 *	9 track tapes interfaced via drivers other than ST
 *	Exabyte under Sun ST driver on 4.0.3, Sun-3 and Sun-4
 *	Exabyte under Sun ST driver on 4.1, Sun3 and sparcstation
 *	    (probably Sun-4 too but not tested)
 *	Exabyte under Ciprico RT driver on 4.0.3 running on Sun-3.
 *
 * 1/4inch cartridge tape and 9 track via the SCSI driver are not supported
 * in this driver.
 *
 * ----------------------------------------------------------------------------
 * Further changes to support Exabyte and HP88780 1/2inch tape drive on SCSI,
 * both under SunOS 4.1 on Sun 470 and 490.  490 fixes contributed by Skip
 * Schaller, Steward Obs.  All these "fixes" were required to work around
 * bugs in the Sun driver - our driver is getting to be quite a kludge as a
 * result.
 * ----------------------------------------------------------------------------
 * Further hacked for SunOS 4.1.1 (there were changes in the Sun magtape driver
 * between 4.1.0 and 4.1.1).  It is no longer known if this particular version
 * of the driver will work for other versions of SunOS, or other devices.
 * ----------------------------------------------------------------------------
 */

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
extern	char *getenv();
static	int sunos_version = 0;
static	int mtioctop, mtiocget;		/* need 4.0.3, 4.1 versions	*/

#define	MTDEBUG		"MTDEBUG"	/* define in env. to get debug msgs */
#define	MAX_ERRIGNORE	10		/* max errs before skiprec	*/
#define	MAX_ERRCNT	20		/* max errs before EOF		*/
#define	errcnt		io_flags	/* i/o error count		*/
#define	KF_ISEXB	001		/* device is Exabyte		*/
#define	KF_ISHPST	002		/* device is HP88780 on ST	*/
#define	KF_ISRT		004		/* Ciprico Rimfire driver	*/
#define	KF_ATEOT	010		/* positioned to EOT		*/

/* The following are used to store/retrieve the current file number in the
 * flags word.
 */
#define KF_FILENO(f)		((f)>>8)
#define	KF_SETFILE(f,fileno)	f=(((f)&0377)|((fileno)<<8))

#ifndef MTNBSF
#define	MTNBSF		11
#endif

/* MTGET structure for SunOS 4.1 - same as the 4.0.3 structure except for
 * two additional fields at the end.
 */
struct	x_mtget	{
	short	mt_type;	/* type of magtape device */
/* the following two registers are grossly device dependent */
	short	mt_dsreg;	/* ``drive status'' register */
	short	mt_erreg;	/* ``error'' register */
/* optional error info. */
	daddr_t	mt_resid;	/* residual count */
	daddr_t	mt_fileno;	/* file number of current position */
	daddr_t	mt_blkno;	/* block number of current position */
	u_short	mt_flags;
	short	mt_bf;		/* optimum blocking factor */
};


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
	int	flags;

	/* Get magic for ioctls. */
	mtsunos();

#ifdef sun
	/* Discard any saved status if the tape has been rewound, or if it
	 * has been repositioned to a different file than it was set to
	 * when the status was last saved.
	 */
	if (*oldfile <= 1 || (KF_FILENO(mt_getstat((char *)dev)) != *oldfile))
	    mt_savestat ((char *)dev, 0);

	/* The following is a special case kludge for the HP88780 used with
	 * the ST driver under SunOS 4.1.  This driver is brain-damaged when
	 * it comes to appending files and basically all you can do is
	 * open-write-close, open-write-close etc.  Any file positioning
	 * operations other than rewind, or any extra opens will mess things
	 * up, causing EOT to fail to be written or causing false EOTs to
	 * be written between files.  To avoid these problems merely open
	 * for writing and begin writing, if we are appending a file and
	 * are already positioned to EOT.
	 *
	 * In SunOS 4.1.1, the ST driver will runaway when the EOM ioctl is
	 * issued when the tape is opened already positoned to EOT, hence it
	 * is necessary to avoid any file positioning for the Exabyte under
	 * the ST driver as well.  (1/5/91)
	 */
	if ((flags = mt_getstat((char *)dev)) &&
	    (flags & KF_ISHPST) || ((flags & KF_ISEXB) && !(flags & KF_ISRT))) {

	    if (*acmode == WRITE_ONLY && *newfile < 0 && (flags & KF_ATEOT)) {
		if (getenv (MTDEBUG))
		    fprintf (stderr, "already at EOT\n");
		*oschan = zzopenmt ((char *)dev, write_only);
		if (*oschan == ERR)
		    *oschan = XERR;
		else {
		    *newfile = *oldfile;
		    zfd[*oschan].errcnt = 0;
		}
		KF_SETFILE(zfd[*oschan].flags,*newfile);
		return;
	    }
	}
#endif

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
	    KF_SETFILE(zfd[*oschan].flags,0);
	    zzclosemt ((int)*oschan);
	    *oschan = XERR;
	    return;
	}

	/* Physically position the tape. */
	if (zzposmt_ (oschan, oldrec, oldfile, newfile) == XERR) {
	    KF_SETFILE(zfd[*oschan].flags,0);
	    zzclosemt ((int)*oschan);
	    *oschan = XERR;
	    return;
	}

	/* Open file with specified access mode and return OS channel.
	 */
	switch (*acmode) {
	case READ_ONLY:
#ifdef sun
	    /* The following is a kludge for SunOS 4.1; evidently needed
	     * to synchronize after a rewind.
	     */
	    KF_SETFILE(zfd[*oschan].flags,*newfile);
	    zzclosemt ((int)*oschan);
	    *oschan = zzopenmt ((char *)dev, read_only);
#endif
	    break;
	case WRITE_ONLY:
	    KF_SETFILE(zfd[*oschan].flags,*newfile);
	    zzclosemt ((int)*oschan);
	    *oschan = zzopenmt ((char *)dev, write_only);
	    if (*oschan == ERR)
		*oschan = XERR;
	    break;
	default:
	    *oschan = XERR;
	}

	zfd[*oschan].errcnt = 0;
	KF_SETFILE(zfd[*oschan].flags,*newfile);
}


/* ZZCLMT -- Close magtape.  Write a new EOT mark at the current position
 * if so indicated.
 */
ZZCLMT (oschan, access_mode, nrecords, nfiles, status)
XINT	*oschan;
XINT	*access_mode;
XINT	*nrecords;
XINT	*nfiles;
XINT	*status;
{
	int	fd = *oschan;

	if (*access_mode == WRITE_ONLY) {
	    zfd[fd].flags |= KF_ATEOT;
	    KF_SETFILE(zfd[fd].flags,KF_FILENO(zfd[fd].flags)+1);
	}

	*status = (zzclosemt(fd) == ERR) ? XERR : XOK;
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
		ioctl ((int)*oschan, mtioctop, (char *)&mt_fwdskiprecord);

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
 * special in that case.
 */
ZZWTMT (oschan, nrecords, nfiles, bytecount)
register XINT	*oschan;
XINT	*nrecords;
XINT	*nfiles;
register XINT	*bytecount;
{
	register int nb, fd, flags;
	XINT	status;

	*nrecords = 0;
	*nfiles = 0;

	fd = *oschan;
	nb = zfd[fd].nbytes;
	flags = zfd[fd].flags;

	if ((flags & KF_ISEXB) && !(flags & KF_ISRT)) {
	    /* Sun ST driver for Exabyte won't position to past EOT,
	     * so we don't need to backspace over the second filemark.
	     */
	    if (nb < 0) {
		nb = XERR;
	    } else if (nb == 0) {			/* saw EOF */
		/* if fpos=0 we are at EOT. */
		if (zfd[fd].fpos > 0)
		    *nfiles = 1;
		else
		    zfd[fd].flags |= KF_ATEOT;
	    } else {					/* record rd/wr	*/
		*nrecords = 1;
		zfd[fd].fpos += nb;
	    }
	} else {
	    /* Backspace over filemark if we hit EOT on a read.
	     */
	    if (nb < 0) {
		nb = XERR;
	    } else if (nb == 0) {			/* saw EOF */
		int      nf = 1;
		if (zzfbmt_ (oschan, &nf, &status) == XERR)
		    nb = XERR;
		if (zfd[fd].fpos == 0)
		    zfd[fd].flags |= KF_ATEOT;
	    } else {					/* record rd/wr	*/
		*nrecords = 1;
		zfd[fd].fpos += nb;
	    }
	}

	*bytecount = nb;
	if (*nfiles)
	    KF_SETFILE(zfd[fd].flags,KF_FILENO(zfd[fd].flags)+1);
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

	/* Get magic for ioctls. */
	mtsunos();

	if ((oschan = zzopenmt ((char *)dev, read_only)) == ERR)
	    *status = XERR;
	else if ((*status = zzrewindmt (oschan)) == ERR)
	    *status = XERR;
	else
	    zzclosemt (oschan);
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
	register int fd, flags;
	char	path[SZ_PATHNAME+1];

	/* If the device name is already a pathname leave it alone, else
	 * prepend the /dev/ prefix.
	 */
	strcpy (path, (*(char *)dev == '/') ? "" : "/dev/");
	strcat (path, (char *)dev);

	if ((fd = open (path, acmode)) < 0)
	    return (fd);

	zfd[fd].flags = 0;
	zfd[fd].fpos = 0;
	zfd[fd].fp = (FILE *) malloc (strlen(path)+1);
	strcpy ((char *)zfd[fd].fp, path);

	/* Determine the tape type.  This version of the driver supports
	 * 1/2inch (Pertek style) drives, and Exabyte.  Sun/OS dependent.
	 */
#ifdef sun
	{   struct    x_mtget mt;
	    char      *dv, *ip;

	    if ((ioctl(fd,mtiocget,&mt) != -1) && mt.mt_type == MT_ISEXABYTE)
		zfd[fd].flags |= KF_ISEXB;
	    else if (mt.mt_type == MT_ISHP)
		zfd[fd].flags |= KF_ISHPST;

	    /* Test for Ciprico driver (/dev/nrrtN) instead of Sun SCSI
	     * driver (/dev/nrstN).  This kludge does not pretend to support
	     * any other Exabyte interfaces...
	     */
	    for (dv=ip=path;  *ip;  ip++)
		if (*ip == '/')
		    dv = ip + 1;
	    if (!strncmp (dv, "nrrt", 4) || !strncmp (dv, "nrt", 3))
		zfd[fd].flags |= KF_ISRT;
	}
#endif
	flags = mt_getstat ((char *)zfd[fd].fp);
	if (flags & KF_ATEOT)
	    zfd[fd].flags |= KF_ATEOT;
	KF_SETFILE(zfd[fd].flags,KF_FILENO(flags));

	return (fd);
}


/* ZZCLOSEMT -- Close the magtape device.
 */
zzclosemt (oschan)
int	oschan;
{
	if (getenv (MTDEBUG))
	    fprintf (stderr,
		"close at file %d\n", KF_FILENO(zfd[oschan].flags));

	mt_savestat ((char *)zfd[oschan].fp, zfd[oschan].flags);
	free ((char *)zfd[oschan].fp);
	return (close (oschan));
}


/* ZZREWINDMT -- Rewind primitive.  Unfortunately this is not asynchronous
 * on our UNIX system (it should be if possible).  A different driver would
 * fix the problem.
 */
zzrewindmt (oschan)
int	oschan;
{
	static	struct mtop mt_rewind = { MTREW, 1 };

	if (getenv (MTDEBUG))
	    fprintf (stderr, "rewind %s\n", (char *)zfd[oschan].fp);

	/* Keep track of where we are. */
	KF_SETFILE(zfd[oschan].flags,1);

	/* NOSTRICT */
	return (ioctl (oschan, mtioctop, (char *)&mt_rewind));
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
	int	flags = zfd[*oschan].flags;
	XINT	status = XOK;

	new = *newfile;
	old = *oldfile;
	rec = *oldrec;

	if (getenv (MTDEBUG)) {
	    if (new < 0)
		fprintf (stderr, "position to EOT\n");
	    else
		fprintf (stderr, "position to file %d\n", new);
	}

	if (new == 0) {					/* do not move tape */
	    status = XOK;

	} else if (new == 1) {				/* rewind tape */
	    status = (zzrewindmt ((int)*oschan) == ERR) ? XERR : XOK;
	    zfd[*oschan].flags &= ~KF_ATEOT;

	} else if (new > 1 && new <= old) {		/* backspace file */
	    if (new < old || rec > 1)
		zfd[*oschan].flags &= ~KF_ATEOT;
#ifdef sun
	    if ((flags & KF_ISEXB) && !(flags & KF_ISRT)) {
		/* Under SunOS 4.0.3, the Exabyte (actually the st driver)
		 * BSF positions to the the first record of a file, rather
		 * than spacing back over the file mark, so BSF-0 rewinds
		 * the current file, BSF-1 goes to the preceeding file, etc.
		 */
		if (sunos_version == 403) {
		    struct  x_mtget mt;
		    int     nf = old - new;

		    /* The Sun 4.0.3 st driver has a hard time keeping track
		     * of the number of files on the tape and the current tape
		     * position and won't backspace to the desired file if it
		     * thinks there aren't that many files on the tape.  The
		     * workaround is to rewind and space forward.
		     */
		    if (ioctl (*oschan, mtiocget, &mt) != ERR)
			if (mt.mt_fileno < nf || mt.mt_fileno == 1 && nf == 1) {
			    zzrewindmt ((int)*oschan);
			    old = rec = 1;
			    goto fwd;
			}

		    /* Backspace to the desired file. */
		    zzfbmt_ (oschan, &nf, &status);
		} else {
		    /* For SunOS 4.1 BSF reverts to its usual function, and the
		     * nfiles bug is fixed.
		     */
		    struct  mtop mt;
		    int     nf = old - new;

		    mt.mt_op = MTNBSF;
		    mt.mt_count = nf;
		    status = ((ioctl(*oschan,mtioctop,&mt)) == -1) ? XERR:XOK;
		}
	    } else if ((flags & KF_ISHPST) && (flags & KF_ATEOT)) {
		zzrewindmt (*oschan);
		zfd[*oschan].flags &= ~KF_ATEOT;
		old = rec = 1;
		goto fwd;
	    } else
#endif
	    {
		int     nf = 1;
		while (new <= old--)
		    if (zzfbmt_ (oschan, &nf, &status) == XERR)
			break;
		if (status != XERR)
		    zzrfmt_ (oschan, &status);
	    }
#ifdef sun
#ifdef SUNOS403
	} else if ((flags & (KF_ISEXB|KF_ISHPST)) && !(flags & KF_ISRT)
	    && new < 0) {
#else
	} else if ((flags & KF_ISHPST) && new < 0) {
#endif

	    /* Position to EOT on Exabyte or SCSI/HP using MTEOM ioctl. */
	    struct  x_mtget omt, nmt;
	    struct  mtop mteom;

	    ioctl (*oschan, mtiocget, &omt);
	    mteom.mt_op = MTEOM;
	    mteom.mt_count = 0;
	    if (ioctl ((int)*oschan, mtioctop, (char *)&mteom) == ERR)
		status = XERR;

	    ioctl (*oschan, mtiocget, &nmt);
	    *newfile = old + nmt.mt_fileno - omt.mt_fileno;
	    rec = 1;
#endif
	} else {					/* forward, EOT */
fwd:	    /* EOT is flagged as new < 0.
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
		    /* At EOT. */
		    if (flags & KF_ISEXB) {
			if (flags & KF_ISRT) {
			    /* The Ciprico Exabyte/RT driver uses two tape
			     * marks to mark EOT, and leaves the tape
			     * positioned to after the second tape mark.
			     */
			    zzrbmt_ (oschan, &status);
			} else {
			    /* Already at EOT, no need to do anything. */
			}
		    } else {
			/* Skip back and then forward over the first tape
			 * mark to cancel the inter record gap between the
			 * two tape marks.
			 */
			zzrbmt_ (oschan, &status);
			zzrbmt_ (oschan, &status);
			zzrfmt_ (oschan, &status);
		    }
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
done:
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
#ifdef sun
	/* The 4/490 st driver complains about odd byte DMA, so let's super
	 * align everything.  [fix contrib. by Skip Schaller, Steward Obs.]
	 */
	long	lbuf[65532/sizeof(long)];
	char	*buf = (char *)lbuf;
#else
	char	buf[29184];
#endif

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

#ifdef sun
	nb = read ((int)*oschan, buf, 65532);
#else
	nb = read ((int)*oschan, buf, 29184);
#endif
	if (nb < 0)
	    return (*status = (zfd[*oschan].flags&KF_ISEXB) ? XEOF :  XERR);
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
	if (ioctl ((int)*oschan, mtioctop, (char *)&mt_backskiprec) == ERR)
	    *status = ((errno == EIO) ? XOK : XERR);
	else
	    *status = XOK;

	return (*status);
}


/* ZZFBMT -- Skip file backward.
 */
zzfbmt_ (oschan, nfiles, status)
XINT	*oschan;
XINT	*nfiles;		/* nfiles == 0 rewinds current file. */
XINT	*status;
{
	struct	mtop mt_backskipfile;

	mt_backskipfile.mt_op = MTBSF;
	mt_backskipfile.mt_count = *nfiles;

	if (ioctl ((int)*oschan, mtioctop, (char *)&mt_backskipfile) == ERR)
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

	if (ioctl ((int)*oschan, mtioctop, (char *)&mt_fwdskipfile) == ERR)
	    *status = XERR;
	else
	    *status = XOK;

	return (*status);
}


/* MTSUNOS -- Get magic for ioctls.  This nonsense is necessary in order to
 * compile a binary on 4.0.3 that can be used with 4.1.  The cases are
 * necessary because the mtget structure changed between 4.0.3 and 4.1,
 * and because it contains a hole which is aligned * differently on a Sun-3
 * and sparc.
 */
mtsunos()
{
	switch (set_sunos_version()) {
	case 403:
#ifdef sparc
	    mtioctop = 0x80086d01;
	    mtiocget = 0x40146d02;
#else
	    mtioctop = 0x80066d01;
	    mtiocget = 0x40126d02;
#endif
	    break;
	case 41:
#ifdef sparc
	    mtioctop = 0x80086d01;
	    mtiocget = 0x40186d02;
#else
	    mtioctop = 0x80066d01;
	    mtiocget = 0x40166d02;
#endif
	    break;
	default:
	    mtioctop = MTIOCTOP;
	    mtiocget = MTIOCGET;
	    break;
	}
}


/* Try to determine whether we are running on SunOS 4.0.3 or 4.1.
 */
set_sunos_version()
{
	static	char *s1 = "SunOS Release 4.0.3";
	static	char *s2 = "SunOS Release 4.1";
	static	char *s3 = "SunOS 4.0.3";
	static	char *s4 = "SunOS 4.1";

	char	lbuf[SZ_LINE+1];
	FILE	*fp;

	if (sunos_version)
	    return (sunos_version);

	if ((fp = fopen ("/etc/motd", "r")) != NULL) {
	    fgets (lbuf, SZ_LINE, fp);
	    fclose (fp);

	    if (strncmp (lbuf, s1, strlen(s1)) == 0) {
		sunos_version = 403;
		goto done;
	    } else if (strncmp (lbuf, s2, strlen(s2)) == 0) {
		sunos_version = 41;
		goto done;
	    }
	}

	if ((fp = fopen ("/usr/etc/upgrade/TOC", "r")) != NULL) {
	    fgets (lbuf, SZ_LINE, fp);
	    fclose (fp);

	    if (strncmp (lbuf, s3, strlen(s3)) == 0) {
		sunos_version = 403;
		goto done;
	    } else if (strncmp (lbuf, s4, strlen(s4)) == 0) {
		sunos_version = 41;
		goto done;
	    }
	}
done:
	return (sunos_version);
}


#define	NSAVE	5
struct	sv {
	int	flags;
	char	sv_dev[SZ_FNAME];
};
static	struct sv svv[NSAVE];
static	int svnext;

/* MT_SAVESTAT, MT_GETSTAT -- This is a kludge to save the ATEOT flag
 * over a close, which frees the runtime descriptor.
 */
mt_getstat (device)
char	*device;
{
	register int	i;
	register struct sv *sp;
	char	path[SZ_FNAME];

	strcpy (path, (*device == '/') ? "" : "/dev/");
	strcat (path, device);

	for (i=0;  i < NSAVE;  i++) {
	    sp = &svv[i];
	    if (sp->sv_dev[0] && strcmp (path, sp->sv_dev) == 0)
		return (sp->flags);
	}

	return (0);
}

mt_savestat (device, flags)
char	*device;
int	flags;
{
	register int	i;
	register struct sv *sp;
	char	path[SZ_FNAME];

	strcpy (path, (*device == '/') ? "" : "/dev/");
	strcat (path, device);

	/* Update existing device entry if there is one. */
	for (i=0;  i < NSAVE;  i++) {
	    sp = &svv[i];
	    if (sp->sv_dev[0] && strcmp (path, sp->sv_dev) == 0) {
		sp->flags = flags;
		return;
	    }
	}

	/* Allocate a new slot. */
	if (++svnext >= NSAVE)
	    svnext = 0;
	sp = &svv[svnext];
	strcpy (sp->sv_dev, path);
	sp->flags = flags;
}


/* PSTAT -- Print file position (debug routine).
 */
Pstat(fd)
int	fd;
{
	struct  x_mtget mt;

	if (ioctl (fd, mtiocget, &mt) != ERR)
	    fprintf (stderr, "file=%d, record=%d\n",
		mt.mt_fileno, mt.mt_blkno);
}
