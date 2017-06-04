/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/errno.h>
#include <fcntl.h>
#include <ctype.h>
#include <pwd.h>

#ifdef __linux__
#include <sys/mtio.h>
#endif

/* Define if status logging to sockets is desired. */
#define TCPIP

#ifdef TCPIP
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#define	DEFPORT 5138
#endif

#define	import_kernel
#define	import_knames
#define	import_zfstat
#define	import_stdarg
#define import_spp
#include <iraf.h>

/*
 * ZFIOMT.C -- Programmable magtape kernel interface for UNIX/IRAF systems.
 * This file contains only the lowest level i/o routines.  Most of the
 * functionality of the iraf magtape i/o system is provided by the routines
 * in the VOS interfaces MTIO and ETC.  The dev$tapecap file is used to
 * describe the magtape devices present on the local host system, and to
 * characterize the behavior of each device.
 *
 * The behavior of this driver is controlled by parameters given in the
 * entry for a magtape device in the tapecap file.  The following parameters
 * are defined (not all of which are necessarily used by the driver).
 *
 *	CODE  TYPE  DEFAULT	DESCRIPTION
 *
 *	bs    i     0		device block size (0 if variable)
 *	dn    s     0		density
 *	dt    s     generic	drive type
 *	fb    i     1           default FITS blocking factor (recsize=fb*2880)
 *	fe    i     0		time to FSF equivalent in file Kb
 *	fs    i     0		approximate filemark size (bytes)
 *	mr    i     65535	maximum record size
 *	or    i     63360	optimum record size
 *	rs    i     0		approximate record gap size (bytes)
 *	ts    i     0		tape capacity (Mb)
 *	tt    s     unknown	tape type
 *
 *	al    s     none	device allocation info
 *	dv    s     required	i/o (no-rewind) device file
 *	lk    s     required    lock file root name (uparm$mt<lk>.lok)
 *	rd    s     none	rewind device file
 *	so    s     none	status output device file or socket
 *
 *	bo    b     no		BSF positions to BOF
 *	ce    b     no		ignore close status on CLRO
 *	eo    b     no		do not write double EOT on CLWO (VMS)
 *	fc    b     no		device does a FSF on CLRO
 *	ir    b     no		treat all read errors as EOF
 *	mf    b     no		enable multifile FSF for forward positioning
 *	nb    b     no		device cannot backspace
 *	nf    b/i   no/0	rewind and space forward to backspace file
 *	np    b     no		disable all positioning ioctls
 *	ow    b     no		backspace and overwrite EOT at append
 *	re    b     no		read at EOT returns ERR
 *	rf    b     no		use BSR,FSR to space over filemarks
 *	ro    b     no		rewind on every open to define position
 *	rr    b     no		rewind at close-readonly to define position
 *	se    b     no		device will position past EOT in a read
 *	sk    b     no		skip record forward after a read error
 *	ue    b     no		force update of EOT (search for EOT)
 *	wc    b     no		OPWR-CLWR at EOF writes null file
 *
 *	ct    i     builtin	MTIOCTOP code
 *	bf    i     builtin	BSF ioctl code
 *	br    i     builtin	BSR ioctl code
 *	ff    i     builtin	FSF ioctl code
 *	fr    i     builtin	FSR ioctl code
 *	ri    i     builtin	REW ioctl code
 *
 *	
 * Many of these parameters are optional.  Some are used by the high level MTIO
 * code rather than the host level driver.
 *
 * The externally callable driver routines are the following.
 *
 *	ZZOPMT (device, acmode, devcap, devpos, newfile, chan)
 *	ZZRDMT (chan, buf, maxbytes, offset)
 *	ZZWRMT (chan, buf, nbytes, offset)
 *	ZZWTMT (chan, devpos, status)
 *	ZZSTMT (chan, param, value)
 *	ZZCLMT (chan, devpos, status)
 *	ZZRWMT (device, devcap, status)
 *
 * Here, "device" is the name by which the device is known to the driver,
 * acmode is the access mode, devcap is the tapecap device entry, devpos is a
 * structure giving the current tape position, amount of tape used, etc, and
 * newfile is the requested file.  The driver will position to the indicated
 * file at open time.  The devpos structure is passed in to the driver at open
 * time and returned to the client at close time.  While i/o is in progress
 * the driver is responsible for keeping track of the device position.  The
 * client is responsible for maintaining the position information while the
 * device is closed.  If the position is uncertain devpos should be passed in
 * with a negative file number and the driver will rewind to reestablish a
 * known position.
 *
 * The devpos structure (struct _mtpos) has the following fields:
 *
 *	int	filno		file number
 *	int	recno		record number
 *	int	nfiles		number of files on tape
 *	int	tapeused	total amount of storage used (Kb)
 *	int	pflags		bitflags describing last i/o operation
 *
 * FILNO and RECNO are negative if the position is undefined (unknown).  File
 * number 1 is the first file.  NFILES is the total number of files on the
 * tape.  NFILES less than or equal to zero indicates that the number of files
 * is unknown; the driver will set nfiles when EOT is seen.  The tape can be
 * positioned to EOT by opening the device at file NFILES+1.  TAPEUSED refers
 * to the total amount of tape used at EOT, regardless of the current tape
 * position.  The driver will keep track of tape usage using the device
 * attributes specified in the tapecap entry.  TAPEUSED less than or equal to
 * zero indicates that the amount of tape used is unknown.  Both the tape
 * capacity and TAPEUSED are given in Kb (1024 byte blocks).
 *
 *
 * The following bitflags are defined:
 *
 *	MF_ERR		i/o error occurred in last operation
 *	MF_EOF		a tape mark was seen in the last operation
 *	MF_EOT		end of tape seen in the last operation
 *	MF_EOR		a record advance occurred in the last operation
 *
 * The PFLAGS field is output only, and is cleared at the beginning of each
 * i/o request.
 */

extern	int errno;
typedef unsigned int U_int;

#define	CONSOLE		"/dev/console"
#define	MAX_ERRIGNORE	10		/* max errs before skiprec */
#define	MAX_ERRCNT	20		/* max errs before EOF */
#define	MAXDEV		8		/* max open magtape devices */
#define	MAXREC		64512		/* default maximum record size */
#define	OPTREC		64512		/* default optimum record size */
#define	SHORTREC	20		/* short record for dummy files */
#define	RDONLY		0		/* read only */
#define	WRONLY		1		/* write only */

/* Tape position information (must agree with client struct).  This is input
 * by the client at open time and is only modified locally by the driver.
 */
struct _mtpos {
	int	filno;			/* current file (1=first) */
	int	recno;			/* current record (1=first) */
	int	nfiles;			/* number of files on tape */
	int	tapeused;		/* total tape used (Kb) */
	int	pflags;			/* i/o status bitflags (output) */
};

/* MTPOS bitflags. */
#define	MF_ERR	0001	/* i/o error occurred in last operation */
#define	MF_EOF	0002	/* a tape mark was seen in the last operation */
#define	MF_EOT	0004	/* end of tape seen in the last operation */
#define	MF_EOR	0010	/* a record advance occurred in the last operation */

/* General magtape device information, for status output. */
struct mtdev {
	FILE	*statusout;		/* status out or NULL */
	int	blksize;		/* device block size */
	int	recsize;		/* last record size */
	int	maxrec;			/* maximum record size */
	int	optrec;			/* optimum record size */
	int	tapesize;		/* tape capacity (Kb) */
	int	eofsize;		/* filemark size, bytes */
	int	gapsize;		/* interrecord gap size, bytes */
	int	maxbsf;			/* BSF vs rewind-FSF threshold */
	char	density[SZ_FNAME];	/* tape density, bpi */
	char	devtype[SZ_FNAME];	/* drive type */
	char	tapetype[SZ_FNAME];	/* tape type */
	char	statusdev[SZ_FNAME];	/* status output device */
};

/* Magtape device descriptor. */
#define	get_mtdesc(fd)		((struct mtdesc *)zfd[fd].fp)
#define	set_mtdesc(fd,mp)	zfd[fd].fp = (FILE *)mp
#define ateot(pp)  (pp->nfiles>0 && pp->filno==pp->nfiles+1 && pp->recno==1)
#define spaceused(pp) ((pp->nfiles==0 || pp->filno>pp->nfiles) && !ateot(pp))

struct mtdesc {
	XINT	*chan;			/* file descriptor open device */
	int	flags;			/* device characteristics */
	int	acmode;			/* access mode */
	int	errcnt;			/* i/o error count */
	int	nbytes;			/* status of last i/o transfer */
	int	tbytes;			/* byte portion of tapeused */
	int	mtrew;			/* REW ioctl code */
	int	mtbsr, mtfsr;		/* BSR,FSR ioctl codes */
	int	mtbsf, mtfsf;		/* BSF,FSF ioctl codes */
	U_int	mtioctop;		/* MTIOCTOP code */
	struct	_mtpos mtpos;		/* position information */
	struct	mtdev mtdev;		/* drive type information */
	char	iodev[SZ_FNAME];	/* i/o device */
	char	nr_device[SZ_FNAME];	/* no-rewind-on-close device */
	char	rw_device[SZ_FNAME];	/* rewind-on-close device */
};

/* Parameter codes. */
#define	P_AL	 1		/* allocation stuff */
#define	P_BF	 2		/* MTBSF */
#define	P_BO	 3		/* BSF positions to BOF */
#define	P_BR	 4		/* MTBSR */
#define	P_BS	 5		/* block size */
#define	P_CE	 6		/* ignore close status on CLRO */
#define	P_CT	 7		/* MTIOCTOP */
#define	P_DN	 8		/* density */
#define	P_DT	 9		/* drive type id string */
#define	P_DV	10		/* no rewind device */
#define	P_EO	11		/* do not write double EOT on CLWO (VMS) */
#define	P_FC	12		/* device does FSF on close readonly */
#define	P_FF	13		/* MTFSF */
#define	P_FR	14		/* MTFSR */
#define	P_FS	15		/* filemark size, Kb */
#define	P_IR	16		/* map read errors to EOF */
#define	P_MF	17		/* enable multifile FSF for fwd positioning */
#define	P_MR	18		/* max record (i/o transfer) size */
#define	P_NB	19		/* backspace not allowed */
#define	P_NF	20		/* rewind and space forward to posn back */
#define	P_NP	21		/* disable file positioning */
#define	P_OR	22		/* optimum record size */
#define	P_OW	23		/* optimize EOT (9tk drives) */
#define	P_RD	24		/* rewind device */
#define	P_RE	25		/* read at EOT returns ERR */
#define	P_RF	26		/* use record skip ioctls to skip filemarks */
#define	P_RI	27		/* MTREW */
#define	P_RO	28		/* rewind on every open to define position */
#define	P_RR	29		/* rewind after close-readonly */
#define	P_RS	30		/* interrecord gap size, bytes */
#define	P_SE	31		/* BSF needed after read at EOT */
#define	P_SK	32		/* skip record forward after read error */
#define	P_SO	33		/* status output device or socket */
#define	P_TS	34		/* tape size, Mb */
#define	P_TT	35		/* tape type id string */
#define	P_UE	36		/* force update of EOT (search for EOT) */
#define	P_WC	37		/* open-write/close creates dummy EOT */

/* Tapecap device characteristic bitflags. */
#define	BO	00000001	/* BSF positions to BOF */
#define	CE	00000002	/* ignore close status on CLRO */
#define	EO	00000004	/* do not write double EOT on CLWO (VMS) */
#define	FC	00000010	/* defines does a FSF on CLRO */
#define	IR	00000020	/* treat all read errors as EOF */
#define	MF	00000040	/* enable multifile FSF for fwd positioning */
#define	NB	00000100	/* device cannot backspace */
#define	NF	00000200	/* rewind and space forward to position back */
#define	NP	00000400	/* disable file positioning */
#define	OW	00001000	/* backspace and overwrite at append */
#define	RD	00002000	/* rewind-on-close device specified */
#define	RE	00004000	/* read at EOT can signal error */
#define	RF	00010000	/* use BSR,FSR to space over filemarks */
#define	RO	00020000	/* rewind on every open to define position */
#define	RR	00040000	/* rewind after close-readonly */
#define	SE	00100000	/* read at EOT leaves past tape mark */
#define	SK	00200000	/* skip record forward after a read error */
#define	UE	00400000	/* force update of EOT (search for EOT) */
#define	WC	01000000	/* CLWR at EOF writes null file */

/* Device characteristic codes. */
#define	PNAME(a,b)	((((int)(a))<<8)+(int)(b))

/* Tape drives aren't supported on Mac systems currently.
*/
#ifdef __linux__

/* Device flag table. */
static struct mtchar {
	int	pname;		/* 2 byte parameter name code */
	int	pcode;		/* parameter number */
	int	bitflag;	/* flag bit */
	int	valset;		/* value has been set */
} devpar[] = {
	{ PNAME('a','l'), P_AL,  0, 0 },
	{ PNAME('b','f'), P_BF,  0, 0 },
	{ PNAME('b','o'), P_BO, BO, 0 },
	{ PNAME('b','r'), P_BR,  0, 0 },
	{ PNAME('b','s'), P_BS,  0, 0 },
	{ PNAME('c','e'), P_CE, CE, 0 },
	{ PNAME('c','t'), P_CT,  0, 0 },
	{ PNAME('d','n'), P_DN,  0, 0 },
	{ PNAME('d','t'), P_DT,  0, 0 },
	{ PNAME('d','v'), P_DV,  0, 0 },
	{ PNAME('e','o'), P_EO, EO, 0 },
	{ PNAME('f','c'), P_FC, FC, 0 },
	{ PNAME('f','f'), P_FF,  0, 0 },
	{ PNAME('f','r'), P_FR,  0, 0 },
	{ PNAME('f','s'), P_FS,  0, 0 },
	{ PNAME('i','r'), P_IR, IR, 0 },
	{ PNAME('m','f'), P_MF, MF, 0 },
	{ PNAME('m','r'), P_MR,  0, 0 },
	{ PNAME('n','b'), P_NB, NB, 0 },
	{ PNAME('n','f'), P_NF, NF, 0 },
	{ PNAME('n','p'), P_NP, NP, 0 },
	{ PNAME('o','r'), P_OR,  0, 0 },
	{ PNAME('o','w'), P_OW, OW, 0 },
	{ PNAME('r','d'), P_RD,  0, 0 },
	{ PNAME('r','e'), P_RE, RE, 0 },
	{ PNAME('r','f'), P_RF, RF, 0 },
	{ PNAME('r','i'), P_RI,  0, 0 },
	{ PNAME('r','o'), P_RO, RO, 0 },
	{ PNAME('r','r'), P_RR, RR, 0 },
	{ PNAME('r','s'), P_RS,  0, 0 },
	{ PNAME('s','e'), P_SE, SE, 0 },
	{ PNAME('s','k'), P_SK, SK, 0 },
	{ PNAME('s','o'), P_SO,  0, 0 },
	{ PNAME('t','s'), P_TS,  0, 0 },
	{ PNAME('t','t'), P_TT,  0, 0 },
	{ PNAME('u','e'), P_UE, UE, 0 },
	{ PNAME('w','c'), P_WC, WC, 0 },
	{ 0, 0, 0, 0 },
};


static	int zmtgetfd();
static	int zmtbsr(), zmtbsf(), zmtfsr(), zmtfsf();
static	int zmtclose(), zmtfpos(), zmtrew();

static int zmtopen (char *dev, int u_acmode);
static int zmtclose (int fd);
static struct mtdesc *zmtdesc (char *device, int acmode, char *devcap,
				struct _mtpos *devpos);
static int zmtfpos (struct mtdesc *mp, int newfile);
static int zmtrew (int fd);
static void zmtfls (struct mtdesc *mp);
static void zmtfree (struct mtdesc *mp);
static int zmtfsf (int fd, int nfiles);
static int zmtbsf (int fd, int nfiles);
static int zmtfsr (int fd, int nrecords);
static int zmtbsr (int fd, int nrecords);

static void zmtdbgn (struct mtdesc *mp, const char *argsformat, ... );
static void zmtdbg (struct mtdesc *mp, char *msg);
static void zmtdbgopen (struct mtdesc *mp);
static void zmtdbgclose (struct mtdesc *mp);




/* ZZOPMT -- Open the named magtape device and position to the given file.
 * On output, "newfile" contains the number of the file actually opened,
 * which may be less than what was requested if EOT is reached.
 */
int
ZZOPMT (
  PKCHAR  *device,	/* device name */
  XINT	  *acmode,	/* access mode: read_only or write_only for tapes */
  PKCHAR  *devcap,	/* tapecap entry for device */
  XINT	  *devpos,	/* pointer to tape position info struct */
  XINT	  *newfile,	/* file to be opened or EOT */
  XINT	  *chan 	/* OS channel of opened file */
)
{
	register int fd;
	register struct	mtdesc *mp;
	struct	_mtpos *pp;

	/* Open the main device descriptor. */
	mp = zmtdesc ((char *)device, *acmode, (char *)devcap,
	    (struct _mtpos *)devpos);
	if (mp == NULL) {
	    *chan = XERR;
	    return (XERR);
	}

	zmtdbgn (mp, "open device %s\n", (char *)device);

	/* Save the channel pointer for the delayed open used for file
	 * positioning.  If file positioning is needed the device will
	 * be opened read-only, so that an interrupt occurring while seeking
	 * to EOT for writing will not result in truncation of the tape!
	 * BE SURE TO RETURN OSCHAN as soon as the device is physically
	 * opened, so that the error recovery code can close the file if
	 * we are interrupted.
	 */
	mp->chan = chan;
	*chan = 0;

	/* Initialize the descriptor. */
	mp->errcnt = 0;
	mp->tbytes = 0;
	pp = &mp->mtpos;

	/* Zero counters if position is undefined. */
	if (pp->filno < 1 || pp->recno < 1) {
	    pp->nfiles = 0;
	    pp->tapeused = 0;
	}

	/* Zero counters if new tape? */
	if (mp->acmode == WRITE_ONLY && (*newfile == 0 || *newfile == 1)) {
	    pp->nfiles = 0;
	    pp->tapeused = 0;
	}

	/* Zero tapeused counter if rewinding and nfiles is still unknown. */
	if (pp->nfiles == 0 && *newfile == 1)
	    pp->tapeused = 0;

	/* Status output. */
	zmtdbgn (mp, "devtype = %s", mp->mtdev.devtype);
	zmtdbgn (mp, "tapetype = %s", mp->mtdev.tapetype);
	zmtdbgn (mp, "tapesize = %d", mp->mtdev.tapesize);
	zmtdbgn (mp, "density = %s",
	    mp->mtdev.density[0] ? mp->mtdev.density : "na");
	zmtdbgn (mp, "blksize = %d", mp->mtdev.blksize);
	zmtdbgn (mp, "acmode = %s", mp->acmode == READ_ONLY ? "read" :
	    ((*newfile < 0) ? "append" : "write"));
	zmtdbgn (mp, "file = %d%s", pp->filno, ateot(pp) ? " (EOT)" : "");
	zmtdbgn (mp, "record = %d", pp->recno);
	zmtdbgn (mp, "nfiles = %d", pp->nfiles);
	zmtdbgn (mp, "tapeused = %d", pp->tapeused);
	zmtfls (mp);

	/* Position to the desired file.  Do not move the tape if newfile=0
	 * or if NP (no-position) is specified for the device.  Rewind the tape
	 * to get to a known position if current tape position is undefined.
	 */
	if (*newfile == 0 || (mp->flags & NP)) {
	    zmtdbg (mp, "file positioning is disabled\n");
	    zmtfls (mp);
	}
	if (*newfile) {
	    /* Rewind if current position uncertain. */
	    if ((mp->flags & RO) || pp->filno < 1 || pp->recno < 1) {
		if (!(mp->flags & NP))
		    if ((fd = zmtgetfd (mp)) == ERR || zmtrew(fd) == ERR)
			goto err;
		pp->filno = pp->recno = 1;
	    }

	    /* Position to the desired file.  NP disables file positioning,
	     * in which case we assume the user knows what they are doing
	     * and we are already there.
	     */
	    if (mp->flags & NP)
		*newfile = (*newfile < 0) ? pp->filno : *newfile;
	    else if ((*newfile = zmtfpos (mp, *newfile)) == XERR)
		goto err;
	}

	/* Reopen file with write permission if necessary.  */
	if (mp->acmode == WRITE_ONLY) {
	    if (*chan) {
		zmtclose (*chan);
		zmtdbg (mp, "reopen for writing\n");
	    }
	    (*chan) = fd = zmtopen (mp->iodev, WRONLY);
	    if (fd != ERR)
		zmtdbgn (mp,
		    "device %s opened on descriptor %d\n", mp->iodev, fd);
	} else
	    fd = zmtgetfd (mp);

	if (fd == ERR)
	    goto err;
	set_mtdesc(fd,mp);
	mp->errcnt = 0;

	zmtdbgn (mp, "file = %d%s", pp->filno, ateot(pp) ? " (EOT)" : "");
	zmtdbgn (mp, "record = %d", mp->mtpos.recno);
	zmtfls (mp);

	if (mp->acmode == WRITE_ONLY)
	    zmtdbg (mp, "writing...\n");
	else
	    zmtdbg (mp, "reading...\n");

	return (XOK);
err:
	/* Error exit. */
	zmtfree (mp);
	*chan = XERR;

	return (*chan);
}


/* ZZCLMT -- Close magtape.  Write a new EOT mark at the current position
 * if tape is open for writing, leaving tape positioned ready to write the
 * next file.
 */
int
ZZCLMT (XINT *chan, XINT *devpos, XINT *o_status)
{
	register int fd;
	register struct mtdesc *mp;
	register struct _mtpos *pp;
	int	status, eof_seen, eor_seen, eot_seen;

	/* Since open files are closed during error recovery and an interrupt
	 * can occur while closing a magtape file, it is possible that ZZCLMT
	 * twice, after the host file has been closed and the MP descriptor
	 * freed.  Watch out for a bad channel number or mp=NULL.
	 */
	*o_status = XERR;
	if ((fd = *chan) <= 0)
	    return (XERR);
	if ((mp = get_mtdesc(fd)) == NULL)
	    return (XERR);
	pp = &mp->mtpos;

	eof_seen = 0;
	eor_seen = 0;
	eot_seen = 0;
	status = OK;

	/* Close file and update tape position.
	 */
	if (mp->acmode == READ_ONLY) {
	    /* Rewind if the rewind-after-read flag is set.  This is used on
	     * devices that can leave the tape in an undefined position after
	     * a file read.
	     */
	    if (mp->flags & RR) {
		if (zmtrew(fd) == ERR)
		    status = ERR;
		else {
		    pp->filno = pp->recno = 1;
		    zmtdbgn (mp, "file = %d", pp->filno);
		    zmtdbgn (mp, "record = %d", pp->recno);
		}
	    }

	    /* Close device. */
	    status = zmtclose (fd);
	    if (mp->flags & CE)
		status = 0;

	    /* On some SysV systems closing a tape opened RO causes a skip
	     * forward to BOF of the next file on the tape.  This does not
	     * occur though when the device is closed after reading EOF on
	     * a file.
	     */
	    if ((mp->flags & FC) && pp->recno > 1)
		eof_seen = 1;

	} else if (pp->recno > 1) {
	    /* Close WRONLY other than at BOF always writes EOT, advancing
	     * the file position by one file.
	     */
	    status = zmtclose (fd);
	    eof_seen = 1;
	    eot_seen = 1;

	} else if (mp->flags & WC) {
	    /* If a tape is opened for writing and then closed without any
	     * data being written, a tape mark is written resulting in a
	     * dummy EOT in the middle of the tape if writing continues.
	     * Backspace over the the extra tape mark if possible, otherwise
	     * write a short record.  This will result in an extra dummy
	     * file being written to the tape but the only alternative is to
	     * rewind and space forward, or abort on an error.
	     */
	    register int flags = mp->flags;
	    int     blksize = mp->mtdev.blksize;
	    int     bufsize;
	    char    *bufp;

	    if ((flags & NB) || ((flags & BO) && !(flags & RF))) {
		bufsize = blksize ? blksize : SHORTREC;
		bufsize = max (bufsize, SHORTREC);
		if ((bufp = malloc(bufsize)) == NULL) {
		    zmtclose (fd);
		    status = ERR;
		} else {
		    zmtdbg (mp, "no data - null file written\n");
		    zmtfls (mp);
		    strcpy (bufp, "[NULLFILE]");
		    write (fd, bufp, bufsize);
		    free (bufp);
		    status = zmtclose (fd);
		    eof_seen = 1;
		}
	    } else {
		/* Close and write EOT, reopen RDONLY and backspace over it. */
		status = (zmtclose(fd) == ERR);
		if (status || (fd = zmtopen (mp->iodev, RDONLY)) == ERR)
		    status = ERR;
		else {
		    status = ((flags & RF) ? zmtbsr : zmtbsf)(fd, 1);
		    status = (zmtclose(fd) == ERR) ? ERR : status;
		}
		eof_seen = 1;
	    }
	    eot_seen = 1;
	} else {
	    status = zmtclose (fd);
	    eof_seen = 0;
	    eot_seen = 1;
	}

	/* Update position information and write status output. */
	pp->pflags = status ? MF_ERR : 0;
	if (eot_seen) {
	    pp->pflags |= MF_EOT;
	}
	if (eof_seen) {
	    pp->pflags |= MF_EOF;
	    pp->filno++;
	    pp->recno = 1;
	    if (mp->acmode == WRITE_ONLY) {
		pp->nfiles = pp->filno - 1;
		zmtdbgn (mp, "nfiles = %d", pp->nfiles);
	    }
	    if (mp->acmode == WRITE_ONLY || spaceused(pp))
		mp->tbytes += mp->mtdev.eofsize;
	    zmtdbgn (mp, "record = %d", pp->recno);
	    zmtdbgn (mp, "file = %d%s", pp->filno, ateot(pp) ? " (EOT)" : "");
	}
	if (eor_seen) {
	    pp->pflags |= MF_EOR;
	    pp->recno++;
	    if (mp->acmode == WRITE_ONLY || spaceused(pp))
		mp->tbytes += mp->mtdev.gapsize;
	    zmtdbgn (mp, "record = %d", pp->recno);
	}
	
	pp->tapeused += ((mp->tbytes + 512) / 1024);
	zmtdbgn (mp, "tapeused = %d", pp->tapeused);
	zmtfls (mp);

	*((struct _mtpos *)devpos) = *pp;
	*o_status = status ? XERR : XOK;
	zmtfree (mp);

	return (status);
}


/* ZZRDMT -- Read next tape record.  We are supposed to be asynchronous,
 * so save read status for return by next call to ZZWTMT.  Read returns
 * zero byte count if EOF is seen, as required by the specs, so we need
 * do nothing special in that case.  Tape is left positioned just past the
 * tape mark.
 */
int
ZZRDMT (
  XINT	*chan,
  XCHAR	*buf,
  XINT	*maxbytes,
  XLONG	*offset 		/* fixed block devices only */
)
{
	register int fd = *chan, mb = (int)*maxbytes;
	register struct mtdesc *mp = get_mtdesc(fd);
	register struct _mtpos *pp = &mp->mtpos;
	int	status;

	if (mp->mtdev.blksize && (mb % mp->mtdev.blksize)) {
	    zmtdbgn (mp,
		"read request %d not a multiple of device block size\n", mb);
	    zmtfls (mp);
	}

	/* Position to the desired record (fixed block devices only). */
/*
	if (mp->mtdev.blksize && *offset > 0) {
	    int    blkno, oldblk;
	    blkno = *offset / mp->mtdev.blksize + 1;
	    oldblk = mp->mtpos.recno;
	    if (blkno != oldblk) {
		zmtdbgn (mp, "position to block %d\n", blkno);
		if (blkno > oldblk) {
		    if (zmtfsr (fd, blkno - oldblk) == ERR) {
			status = ERR;
			goto done;
		    }
		} else {
		    if ((mp->flags & NB) || zmtbsr(fd,oldblk-blkno) == ERR) {
			status = ERR;
			goto done;
		    }
		}
		mp->mtpos.recno = blkno;
	    }
	}
 */

	/* Map read error to EOF if RE is set and we are reading the first
	 * record of a file (i.e. are positioned to EOT) or if IR is set.
	 */
	status = read (fd, (char *)buf, mb);
	if (status == ERR) {
	    if ((mp->flags & RE) && pp->recno == 1) {
		status = 0;
	    } else if (mp->flags & IR) {
		zmtdbg (mp, "read error converted to zero read (EOF)\n");
		zmtfls (mp);
		status = 0;
	    }
	}

	/* If an error occurs on the read we assume that the tape has advanced
	 * beyond the bad record, and that the next read will return the next
	 * record on the tape.  If this is not true and a read error loop
	 * occurs, we try skipping a record forward.  If we continue to get
	 * read errors, we give up and return a premature EOF on the file.
	 */
	if (status == ERR) {
	    zmtdbgn (mp, "read error, errno = %d\n", errno);
	    zmtfls (mp);
	    if ((mp->errcnt)++ >= MAX_ERRCNT)
		status = 0;			/* give up; return EOF */
	    else if ((mp->flags & SK) || mp->errcnt >= MAX_ERRIGNORE)
		zmtfsr (fd, 1);
	}

	mp->nbytes = status;
	if (status >= 0 && mp->mtdev.recsize != status)
	    zmtdbgn (mp, "recsize = %d", mp->mtdev.recsize = status);
	zmtfls (mp);

	return (status);
}


/* ZZWRMT -- Write next tape record.  We are supposed to be asynchronous,
 * so save write status for return by next call to ZZWTMT.
 */
int
ZZWRMT (
  XINT	*chan,
  XCHAR	*buf,
  XINT	*nbytes,
  XLONG	*offset 		/* ignored on a write */
)
{
	register int fd = *chan, nb = *nbytes;
	register struct mtdesc *mp = get_mtdesc(fd);
	int	blksize = mp->mtdev.blksize;

	/* If writing to a blocked device, promote partial blocks to a
	 * full device block.
	 */
	if (blksize > 0 && (nb % blksize)) {
	    nb += blksize - (nb % blksize);
	    zmtdbgn (mp, "partial record promoted from %d to %d bytes\n",
		*nbytes, nb);
	}

	if (mp->mtdev.recsize != nb)
	    zmtdbgn (mp, "recsize = %d", mp->mtdev.recsize = nb);
	if ((mp->nbytes = write (fd, (char *)buf, nb)) != nb) {
	    zmtdbgn (mp, "write error, status=%d, errno=%d\n",
		mp->nbytes, errno);
	    mp->nbytes = ERR;
	}
	zmtfls (mp);

	return (XOK);
}


/* ZZWTMT -- "Wait" for i/o transfer to complete, and return the number of
 * bytes transferred or XERR.  A read at EOF returns a byte count of zero.
 */
int
ZZWTMT (
  XINT	*chan,
  XINT	*devpos,
  XINT	*o_status 
)
{
	register int fd = *chan;
	register struct mtdesc *mp = get_mtdesc(fd);
	register struct _mtpos *pp = &mp->mtpos;
	register int flags = mp->flags;
	int	status, eof_seen, eor_seen, eot_seen;

	eof_seen = 0;
	eor_seen = 0;
	eot_seen = 0;
	status = OK;

	if ((status = mp->nbytes) == ERR) {		/* i/o error */
	    status = ERR;
	} else if (status == 0) {			/* saw EOF */
	    if (pp->recno <= 1) {
		/* A read of zero (EOF) at the beginning of a file signals
		 * EOT.  The file number does not change.
		 */
		pp->nfiles = pp->filno - 1;
		zmtdbgn (mp, "nfiles = %d", pp->nfiles);
		zmtdbgn (mp, "file = %d%s",
		    pp->filno, ateot(pp) ? " (EOT)" : "");
		zmtfls (mp);
		eot_seen = 1;

		/* If the device allows us to read past the second filemark
		 * (SE) we must backspace over the filemark or any further
		 * reads could result in tape runaway.
		 */
		if (flags & SE) {
		    if ((flags & NB) || ((flags & FC) && !(flags & RF))) {
			/* Cannot backspace; must rewind and space forward. */
			if (zmtrew(fd) == ERR)
			    status = ERR;
			else if (zmtfsf(fd,pp->filno-1) == ERR)
			    status = ERR;
		    } else {
			/* BSR is preferable if we can use it. */
			if ((((flags & RF) ? zmtbsr : zmtbsf)(fd, 1)) < 0)
			    status = ERR;
		    }
		}
	    } else
		eof_seen = 1;
	} else
	    eor_seen = 1;

	/* Update position records and output status info. */
	pp->pflags = (status < 0) ? MF_ERR : 0;
	if (eot_seen)
	    pp->pflags |= MF_EOT;
	if (eof_seen) {
	    pp->filno++;
	    pp->recno = 1;
	    pp->pflags |= MF_EOF;
	    zmtdbg (mp, "record = 1");
	    if (spaceused(pp))
		mp->tbytes += mp->mtdev.eofsize;
	    zmtdbgn (mp, "file = %d%s", pp->filno, ateot(pp) ? " (EOT)" : "");
	}
	if (eor_seen) {
	    pp->pflags |= MF_EOR;
	    if (mp->mtdev.blksize > 0)
		pp->recno += (status / mp->mtdev.blksize);
	    else
		pp->recno++;
	    if (spaceused(pp))
		mp->tbytes += mp->mtdev.gapsize;
	    zmtdbgn (mp, "record = %d", pp->recno);
	}

	if (status >= 0 && spaceused(pp)) {
	    mp->tbytes += status;
	    pp->tapeused += mp->tbytes / 1024;
	    mp->tbytes %= 1024;
	    zmtdbgn (mp, "tapeused = %d", pp->tapeused);
	}

	*((struct _mtpos *)devpos) = *pp;
	*o_status = (status < 0) ? XERR : status;
	zmtfls (mp);

	return (status);
}


/* ZZSTMT -- Query a device or device driver parameter.
 */
int
ZZSTMT (XINT *chan, XINT *param, XLONG *lvalue)
{
	register int fd = *chan;
	register struct mtdesc *mp = get_mtdesc(fd);
	/* register struct _mtpos *pp = &mp->mtpos; */


        switch (*param) {
        case FSTT_BLKSIZE:
	    /* Zero for variable size record devices, nonzero for fixed
	     * block size devices.
	     */
	    (*lvalue) = mp->mtdev.blksize;
	    break;
        case FSTT_FILSIZE:
	    /* When reading there is no way to know the file size, so set
	     * it to the largest possible value to make all reads in bounds.
	     * When appending a file the file starts out zero length, so
	     * set the file size to zero for a write access.
	     */
            if (mp->acmode == READ_ONLY)
		(*lvalue) = MAX_LONG;
	    else
		(*lvalue) = 0;
            break;
        case FSTT_OPTBUFSIZE:
            (*lvalue) = mp->mtdev.optrec;
            break;
        case FSTT_MAXBUFSIZE:
            (*lvalue) = mp->mtdev.maxrec;
            break;
        default:
            (*lvalue) = XERR;
        }

	return (*lvalue);
}


/* ZZRWMT -- Rewind the tape.  This routine is in principle asynchronous but
 * this is not the case for most unix systems (unless the host driver does
 * asynchronous rewind with synchronization internally).
 *
 * This routine is not part of the normal binary file driver.
 */
int
ZZRWMT (
  PKCHAR  *device,		/* device name */
  PKCHAR  *devcap,		/* tapecap entry for device */
  XINT	  *o_status 
)
{
	register struct	mtdesc *mp;
	register int fd;
	int	status;

	/* Open the main device descriptor. */
	mp = zmtdesc ((char *)device, READ_ONLY, (char *)devcap, NULL);
	if (mp == NULL) {
	    *o_status = ERR;
	    return (XERR);
	}

	/* If a rewind-on-close device is defined for this device use that
	 * to do the rewind, otherwise open the no-rewind device RDONLY and do
	 * an explicit rewind.  The RD device can also be used to avoid an
	 * error condition if the device does not support the MTREW ioctl.
	 */
	if (mp->flags & RD) {
	    if ((fd = zmtopen (mp->rw_device, RDONLY)) == ERR)
		status = ERR;
	    else
		status = zmtclose (fd);
	} else {
	    if ((fd = zmtopen (mp->iodev, RDONLY)) == ERR) {
		status = ERR;
	    } else if (mp->flags & FC) {
		/* Device does a FSF when closed read-only, making it
		 * impossible to leave the tape rewound after the close.
		 * Return ERR to cause MTIO to mark the position undefined,
		 * forcing a rewind the next time the tape is opened for i/o.
		 */
		static FILE *tty = NULL;
		if (!tty && (tty = fopen (CONSOLE, "a")) != NULL) {
		    fprintf (tty, "cannot rewind device %s: ", (char *)device);
		    fprintf (tty, "add RD capability to dev$devices entry\n");
		    fclose (tty);
		}
		status = ERR;
	    } else {
		/* Normal rewind. */
		status = zmtrew (fd);
		status = zmtclose(fd) ? ERR : status;
	    }
	}

	zmtdbg (mp, "file = 1");
	zmtdbg (mp, "record = 1");
	zmtfls (mp);
	*o_status = status ? XERR : XOK;
	zmtfree (mp);

	return (status);
}



/*
 * INTERNAL INTERFACE ROUTINES.
 * ----------------------------
 */

/* ZMTGETFD -- Open tape read-only, if not already open, and return the
 * file descriptor as the function value.  If the tape is already open
 * the only action is to return the file descriptor.  This routine is used
 * to delay the device open during file positioning operations, so that
 * it can be skipped if it is not necessary to move the tape.
 */
static int
zmtgetfd (mp)
register struct	mtdesc *mp;
{
	register int fd;

	if (*mp->chan > 0)
	    return (*mp->chan);

	*mp->chan = fd = zmtopen (mp->iodev, RDONLY);
	if (fd >= MAXOFILES) {
	    zmtclose (fd);
	    fd = ERR;
	}

	if (fd == ERR)
	    zmtdbgn (mp, "failed to open device %s\n", mp->iodev);
	else {
	    zmtdbgn (mp, "device %s opened on descriptor %d\n", mp->iodev, fd);
	    set_mtdesc (fd, mp);
	    mp->errcnt = 0;
	    mp->tbytes = 0;
	}

	return (fd);
}


/* ZMTOPEN -- Convert the magtape device name into a unix pathname and open
 * the drive.  Do not move the tape.
 *
 * Devices can be specified as
 *
 *	devname		system device name in /dev or /dev/rmt
 *	/devpath	full pathname of device
 *	~/devpath	user home directory relative pathname
 *
 * Returns the unix file descriptor or ERR.
 */
static int
zmtopen (
  char	*dev,		/* device name or pathname		*/
  int	u_acmode 	/* read only or write only for tapes	*/
)
{
	char	path[SZ_PATHNAME+1];
	int	fd = ERR;

	/* If the device name is already a pathname leave it alone, else
	 * prepend the /dev/ or /dev/rmt prefix.  The device file can be
	 * in the user's home directory if ~/dev is specified.
	 */
	if (dev[0] == '/') {
	    /* Full pathname. */
	    fd = open (dev, u_acmode);

	} else if (dev[0] == '~' && dev[1] == '/') {
	    /* User home directory relative pathname. */
	    struct  passwd *pwd;
	    pwd = getpwuid (getuid());
	    if (pwd != NULL) {
		strcpy (path, pwd->pw_dir);
		strcat (path, &dev[1]);
		endpwent();
		fd = open (path, u_acmode);
	    }
	} else {
	    /* System device. */
	    strcpy (path, "/dev/");
	    strcat (path, dev);
	    if ((fd = open (path, u_acmode)) == ERR) {
		/* If that fails take a look in /dev/rmt too, since this
		 * is where some SysV systems like to hide raw magtape device
		 * files.
		 */
		strcpy (path, "/dev/rmt/");
		strcat (path, dev);
		fd = open (path, u_acmode);
	    }
	}

	return (fd);
}


/* ZMTCLOSE -- Close a magtape device.
 */
static int zmtclose (int fd)
{
	register struct mtdesc *mp = get_mtdesc(fd);
	zmtdbg (get_mtdesc(fd), "close device\n");
	zmtfls (mp);
	return (close (fd));
}


/* ZMTDESC -- Allocate and initialize the main magtape device descriptor.
 */
static struct mtdesc *
zmtdesc (
  char	*device,		/* host device to be used for i/o */
  int	acmode,			/* iraf file access mode code */
  char	*devcap,		/* tapecap entry for device */
  struct _mtpos *devpos 	/* device position info (or NULL ptr) */
)
{
	register struct mtdesc *mp;
	register struct mtdev *dp;
	register struct mtchar *pp;
	register char *ip, *op;
	int	pname;

	/* Allocate and initialize the device descriptor. */
	if ((mp = (struct mtdesc *) calloc (1, sizeof(*mp))) == NULL)
	    return (NULL);

	dp = &mp->mtdev;
	dp->maxrec = MAXREC;
	dp->optrec = OPTREC;
	strcpy (dp->devtype, "generic");
	strcpy (dp->tapetype, "unknown");

	mp->acmode = acmode;
	strcpy (mp->iodev, device);
	mp->mtioctop = MTIOCTOP;
	mp->mtbsr = MTBSR;
	mp->mtbsf = MTBSF;
	mp->mtfsr = MTFSR;
	mp->mtfsf = MTFSF;
	mp->mtrew = MTREW;

	if (devpos) {
	    mp->mtpos = *devpos;
	    mp->mtpos.pflags = 0;
	}

	/* Prepare to scan tapecap entry. */
	for (pp=devpar;  pp->pname;  pp++)
	    pp->valset = 0;

	/* Process the tapecap entry.  This is a sequence of entries of the
	 * form "nn=value", where the NN is a two character name, the "=" is
	 * actually either `=' or `#', and where successive entries are
	 * delimited by colons.  For example, ":dv=nrst0:rd=rst0:bs#0:...".
	 */
	for (ip=devcap;  *ip;  ) {
	    while (*ip && *ip != ':')
		ip++;
	    if (*ip == ':')
		ip++;
	    else
		break;
	    pname = PNAME(ip[0],ip[1]);

	    ip += 2;
	    if (*ip == '=' || *ip == '#')
		ip++;

	    for (pp=devpar;  pp->pname;  pp++) {
		if (pp->pname == pname) {
		    /* If multiple entries are given for the parameter ignore
		     * all but the first.
		     */
		    if (pp->valset)
			continue;
		    else
			mp->flags |= pp->bitflag;

		    /* Check for a negated entry (e.g., ":ir@:"). */
		    if (*ip == '@') {
			mp->flags &= ~pp->bitflag;
			pp->valset++;
			continue;
		    }

		    /* Check for a string valued parameter. */
		    switch (pp->pcode) {
		    case P_DV:  op = mp->nr_device;  break;
		    case P_RD:  op = mp->rw_device;  break;
		    case P_DN:  op = mp->mtdev.density;    break;
		    case P_DT:  op = mp->mtdev.devtype;    break;
		    case P_TT:  op = mp->mtdev.tapetype;   break;
		    case P_SO:  op = mp->mtdev.statusdev;  break;
		    default:    op = NULL;
		    }

		    if (op != NULL) {
			int     nchars;

			/* String valued parameters. */
			for (nchars=0;  *ip && *ip != ':';  ip++, nchars++) {
			    if (*ip == '\\' && isdigit(*(ip+1))) {
				int     n, i;
				for (n=i=0;  i < 3;  i++)
				    n = n * 10 + (*(++ip) - '0');
				*op++ = n;
			    } else
				*op++ = *ip;
			}
			*op = EOS;
			pp->valset++;

			/* Default if no string value given but entry was
			 * found, e.g., ":so:".
			 */
			if (!nchars)
			    if (pp->pcode == P_SO)
				strcpy (mp->mtdev.statusdev, ",");
			break;

		    } else if (*ip != ':') {
			/* Numeric parameters. */
			int     n = 0;

			while (*ip && *ip != ':') {
			    if (isdigit (*ip))
				n = n * 10 + (*ip - '0');
			    ip++;
			}

			switch (pp->pcode) {
			case P_CT:  mp->mtioctop = n;	break;
			case P_BF:  mp->mtbsf = n;	break;
			case P_BR:  mp->mtbsr = n;	break;
			case P_FF:  mp->mtfsf = n;	break;
			case P_FR:  mp->mtfsr = n;	break;
			case P_RI:  mp->mtrew = n;	break;

			case P_BS:  dp->blksize = n;	break;
			case P_FS:  dp->eofsize = n;	break;
			case P_MR:  dp->maxrec = n;	break;
			case P_NF:  dp->maxbsf = n;	break;
			case P_OR:  dp->optrec = n;	break;
			case P_RS:  dp->gapsize = n;	break;
			case P_TS:  dp->tapesize = n;	break;
			default:    /* ignore (bitflags) */
			    ;
			}

			pp->valset++;
			break;
		    }
		}
	    }
	}

	/* Apply some obvious constraints. */
	if (dp->blksize) {
	    dp->maxrec = dp->maxrec / dp->blksize * dp->blksize;
	    dp->optrec = dp->optrec / dp->blksize * dp->blksize;
	}
	if (dp->maxrec > 0 && dp->optrec > dp->maxrec)
	    dp->optrec = dp->maxrec;

	zmtdbgopen (mp);
	return (mp);
}


/* ZMTFREE -- Free the magtape device descriptor.
 */
static void
zmtfree (struct mtdesc *mp)
{
	zmtdbgclose (mp);
	free (mp);
}


/* ZMTFPOS -- Position to the indicated file.  The first file is #1.
 * A negative newfile number signifies EOT.
 */
static int
zmtfpos (
  register struct mtdesc *mp,
  int	newfile 		/* file we want to position to */
)
{
	register struct _mtpos *pp = &mp->mtpos;
	register int flags = mp->flags;
	int	oldfile, oldrec, maxrec;
	char	*buf = NULL;
	int	fd, status, n;

	oldfile = pp->filno;
	oldrec  = pp->recno;

	if (newfile > 0)
	    zmtdbgn (mp, "position to file %d\n", newfile);
	else if (newfile < 0)
	    zmtdbg (mp, "position to end of tape\n");
	else
	    return (oldfile);

	/* If we are positioning to EOT and UE is not set to force a search
	 * for EOT, use the nfiles information in the position descriptor to
	 * position to just before the EOT marker.
	 */
	if (newfile < 0 && !(flags&UE) && pp->nfiles > 0) {
	    newfile = pp->nfiles + 1;
	    zmtdbgn (mp, "end of tape is file %d\n", newfile);
	}
	zmtfls (mp);

	/* Don't do anything if already positioned to desired file and no
	 * further positioning is necessary.
	 */
	if (newfile == oldfile && oldrec == 1 && (!(flags & OW) ||
	    newfile < pp->nfiles + 1 || mp->acmode == READ_ONLY))
	    return (newfile);

	/* It is necessary to move the tape.  Open the device if it has
	 * not already been opened.
	 */
	if ((fd = zmtgetfd(mp)) < 0)
	    return (ERR);

	/* Move the tape. */
	if (newfile == 1) {
	    if (zmtrew(fd) < 0)
		return (ERR);

	} else if (newfile <= oldfile && newfile > 0) {
	    /* Backspace to the desired file. */
	    if ((flags & NB) ||
	       ((flags & NF) && oldfile - newfile > mp->mtdev.maxbsf)) {

		/* Device cannot backspace or is slow to backspace; must
		 * rewind and space forward.
		 */
		if (zmtrew(fd) < 0)
		    return (ERR);
		oldfile = oldrec = 1;
		zmtdbgn (mp, "file = %d", oldfile);
		zmtfls (mp);
		goto fwd;
	    } else if (flags & BO) {
		/* BSF positions to BOF. */
		if (zmtbsf (fd, oldfile - newfile) < 0)
		    return (ERR);
	    } else {
		/* BSF positions to BOT side of filemark. */
		if (zmtbsf (fd, oldfile - newfile + 1) < 0)
		    return (ERR);
		else if (zmtfsf (fd, 1) < 0)
		    return (ERR);
	    }

	} else if (newfile < 0 && !(flags & UE) &&
	    (pp->nfiles > 0 && oldfile == pp->nfiles+1)) {

	    /* Already at EOT. */
	    newfile = oldfile;
	    if ((flags & OW) && mp->acmode == WRITE_ONLY)
		goto oweot;

	} else {
	    /* Space forward to desired file or EOT.
	     */
fwd:
	    /* Fast file skip forward to numbered file.  Used only when
	     * positioning to a numbered file, as opposed to positioning
	     * to EOT and the number of files on the tape is unknown.
	     * A multifile FSF is much faster on some devices than skipping
	     * a file at a time.  It is also an atomic, uninterruptable
	     * operation so may be undesirable on devices where file
	     * positioning takes a long time, and could result in tape
	     * runaway in an attempt to position beyond EOT (if the host
	     * device driver cannot detect this).  Fast skip is enabled if
	     * the  MF (multifile-file) flag is set.
	     */
	    if (newfile > oldfile && (flags & MF)) {
		if (zmtfsf (fd, newfile - oldfile) < 0)
		    return (ERR);

		oldfile = newfile;
		if ((flags & OW) && mp->acmode == WRITE_ONLY)
		    goto oweot;
		else
		    goto done;
	    }

	    /* Get a read buffer as large as the largest possible record,
	     * for variable record size devices, or the size of a device
	     * block for fixed block devices.
	     */
	    if (mp->mtdev.blksize)
		maxrec = mp->mtdev.blksize;
	    else {
		maxrec = mp->mtdev.maxrec;
		if (maxrec <= 0)
		    maxrec = MAXREC;
	    }
	    if (buf == NULL && !(buf = malloc(maxrec)))
		return (ERR);

	    /* Skip file forward one file at a time.  This is tricky as we
	     * must be able to detect EOT when spacing forward or we risk
	     * tape runaway.  Detecting EOT portably requires looking for
	     * a double EOT.  We FSF to the next file and then read the
	     * first record; a read of zero (or ERR on some devices) signals
	     * a tape mark and hence double EOF or EOT.
	     */
	    while (oldfile < newfile || newfile < 0) {
		/* Test if the next record is a data record or filemark. */
		n = read (fd, buf, maxrec);

		/* Check for EOT, signified by two consecutive logical
		 * filemarks, or equivalently, a zero length file.  On
		 * some systems a read at EOT might be an error, so treat
		 * a read error the same as EOF if the RE capability is set.
		 * (the IR flag causes all read errors to be treated as EOF
		 * and is intended only to try to workaround host driver bugs).
		 */
		if (n < 0 && !(flags & (RE|IR))) {
		    goto err;
		} else if (n <= 0 && oldrec == 1) {
		    /* At EOT.  Leave the tape between the filemarks if such
		     * a concept applies to the current device.  If SE is
		     * not specified for the device, we are already there.
		     */

		    pp->nfiles = (newfile=oldfile) - 1;
		    zmtdbgn (mp, "nfiles = %d", pp->nfiles);
		    zmtdbg (mp, "at end of tape\n");
		    zmtfls (mp);

		    if (flags & SE) {
			/* Cannot backspace? */
			if (flags & NB) {
			    newfile = oldfile;
			    if (zmtrew (fd) < 0)
				goto err;
			    if (zmtfsf (fd, newfile - 1) < 0)
				goto err;
			    oldrec = 1;
			    break;
			} else {
			    if ((((flags & RF) ? zmtbsr:zmtbsf)(fd, 1)) < 0)
				goto err;
			}
		    }
oweot:
		    /* On some devices, e.g., 1/2 inch reel tape, the space
		     * between the two filemarks marking EOT can be large and
		     * we can get more data on the tape if we back up over the
		     * first filemark and then space forward over it, leaving
		     * the tape just after the first filemark rather than just
		     * before the second one.  The OW (overwrite) capability
		     * enables this.
		     */
		    if ((flags & OW) && !(flags & NB) &&
			mp->acmode == WRITE_ONLY) {

			if (flags & RF) {
			    status = zmtbsr (fd, 1);
			    status = (zmtfsr (fd, 1) < 0) ? ERR : status;
			} else if (flags & BO) {
			    /* This may not actually do anything, depending
			     * upon the host driver... */
			    status = zmtbsf (fd, 0);
			} else {
			    status = zmtbsf (fd, 1);
			    status = (zmtfsf (fd, 1) < 0) ? ERR : status;
			}
			if (status < 0)
			    goto err;
		    }

		    break;

		} else if (n > 0) {
		    if (zmtfsf (fd, 1) < 0) {
err:			free (buf);
			return (ERR);
		    }
		}

		oldfile++;
		oldrec = 1;
		zmtdbgn (mp, "file = %d", oldfile);
		zmtfls (mp);
	    }

	    /* Set newfile to the file we actually ended up positioned to. */
	    newfile = oldfile;
	    free (buf);
	}
done:
	/* Update position descriptor */
	pp->filno = newfile;
	pp->recno = 1;

	return (newfile);
}


/* ZMTREW -- Rewind the tape.
 */
static int
zmtrew (int fd)
{
	register struct mtdesc *mp = get_mtdesc(fd);
	struct	mtop mt_rewind;
	int	status;

	mt_rewind.mt_op = mp->mtrew;
	mt_rewind.mt_count = 1;

	zmtdbg (mp, "rewinding...");
	zmtfls (mp);
	status = ioctl (fd, mp->mtioctop, (char *)&mt_rewind);
	zmtdbgn (mp, "%s\n", status < 0 ? "failed" : "done");
	zmtfls (mp);

	return (status);
}


/* ZMTFSF -- Skip file forward.
 */
static int
zmtfsf (int fd, int nfiles)
{
	register struct mtdesc *mp = get_mtdesc(fd);
	struct mtop mt_fwdskipfile;
	int	status;

	mt_fwdskipfile.mt_op = mp->mtfsf;
	mt_fwdskipfile.mt_count = nfiles;

	zmtdbgn (mp, "skip %d file%s forward...", nfiles,
	    nfiles > 1 ? "s" : "");
	zmtfls (mp);
	status = ioctl (fd, mp->mtioctop, (char *)&mt_fwdskipfile);
	zmtdbgn (mp, "%s\n", status < 0 ? "failed" : "done");
	zmtfls (mp);

	return (status);
}


/* ZMTBSF -- Skip file backward.
 */
static int
zmtbsf (int fd, int nfiles)
{
	register struct mtdesc *mp = get_mtdesc(fd);
	struct mtop mt_backskipfile;
	int	status;

	mt_backskipfile.mt_op = mp->mtbsf;
	mt_backskipfile.mt_count = nfiles;

	zmtdbgn (mp, "skip %d file%s backward...", nfiles,
	    nfiles > 1 ? "s" : "");
	zmtfls (mp);
	status = ioctl (fd, mp->mtioctop, (char *)&mt_backskipfile);
	zmtdbgn (mp, "%s\n", status < 0 ? "failed" : "done");
	zmtfls (mp);

	return (status);
}


/* ZMTFSR -- Skip record forward.
 */
static int
zmtfsr (int fd, int nrecords)
{
	register struct mtdesc *mp = get_mtdesc(fd);
	struct mtop mt_fwdskiprecord;
	int	status;

	mt_fwdskiprecord.mt_op = mp->mtfsr;
	mt_fwdskiprecord.mt_count = nrecords;

	zmtdbgn (mp, "skip %d record%s forward...", nrecords,
	    nrecords > 1 ? "s" : "");
	zmtfls (mp);
	status = ioctl (fd, mp->mtioctop, (char *)&mt_fwdskiprecord);
	zmtdbgn (mp, "%s\n", status < 0 ? "failed" : "done");
	zmtfls (mp);

	return (status);
}


/* ZMTBSR -- Skip record backward.
 */
static int
zmtbsr (int fd, int nrecords)
{
	register struct mtdesc *mp = get_mtdesc(fd);
	struct mtop mt_backskiprecord;
	int	status;

	mt_backskiprecord.mt_op = mp->mtbsr;
	mt_backskiprecord.mt_count = nrecords;

	zmtdbgn (mp, "skip %d record%s backward...", nrecords,
	    nrecords > 1 ? "s" : "");
	zmtfls (mp);
	status = ioctl (fd, mp->mtioctop, (char *)&mt_backskiprecord);
	zmtdbgn (mp, "%s\n", status < 0 ? "failed" : "done");
	zmtfls (mp);

	return (status);
}


/*
 * I/O logging routines.
 *
 *	     zmtdbgopen (mp)
 *	         zmtdbg (mp, msg)
 *	        zmtdbgn (mp, fmt, arg)
 *	        zmtdbgn (mp, fmt, arg1, arg2)
 *	        zmtdbg3 (mp, fmt, arg1, arg2, arg3)
 *		 zmtfls (mp)
 *	    zmtdbgclose (mp)
 *
 * Output may be written to either a file, if an absolute file pathname is
 * given, or to a socket specified as "host[,port]".
 */

#ifdef TCPIP
static	SIGFUNC sigpipe = NULL;
static	int nsockets = 0;
static	int s_port[MAXDEV];
static	FILE *s_fp[MAXDEV];
static	int s_fd[MAXDEV];
static	int s_checksum[MAXDEV];
#endif

/* ZMTDBGOPEN -- Attempt to open a file or socket for status logging.
 */
static void zmtdbgopen (struct mtdesc *mp)
{
#ifndef TCPIP
        if (!mp->mtdev.statusdev[0] || mp->mtdev.statusout)
	    return;
	if (mp->mtdev.statusdev[0] == '/')
	    mp->mtdev.statusout = fopen (mp->mtdev.statusdev, "a");
#else
	register char *ip, *op;
	struct	sockaddr_in sockaddr;
	int	port, isfile, sum, s, i;
	char	host[SZ_FNAME];
	struct	hostent *hp;
	FILE	*fp = (FILE *) NULL;


	/* Status logging disabled. */
        if (!mp->mtdev.statusdev[0])
	    return;

	/* Status device already open.  This is only possible in repeated
	 * calls to zmtdbgopen after a zzopmt.
	 */
        if (mp->mtdev.statusout) {
	    if (nsockets > 0 && !sigpipe)
		sigpipe = (SIGFUNC) signal (SIGPIPE, SIG_IGN);
	    return;
	}

	/* Compute statusdev checksum. */
	for (sum=0, ip = mp->mtdev.statusdev;  *ip;  ip++)
	    sum += (sum + *ip);

	/* Log status output to a file if a pathname is specified.
	 */
	for (isfile=0, ip=mp->mtdev.statusdev;  *ip;  ip++)
	    if ((isfile = (*ip == '/')))
		break;
	if (isfile) {
	    mp->mtdev.statusout = fopen (mp->mtdev.statusdev, "a");
	    if (mp->mtdev.statusout) {
		for (i=0;  i < MAXDEV;  i++)
		    if (!s_fp[i]) {
			s_fp[i] = mp->mtdev.statusout;
			s_port[i] = s_fd[i] = 0;
			s_checksum[i] = sum;
			break;
		    }
	    }
	    return;
	}

	/* If the entry is of the form "host" or "host,port" then status output
	 * is written to a socket connected to the specified host and port.
	 */
	for (ip=mp->mtdev.statusdev, op=host;  *ip && *ip != ',';  )
	    *op++ = *ip++;
	*op = EOS;
	if (!host[0])
	    strcpy (host, "localhost");
	if (*ip == ',')
	    ip++;
	port = (isdigit(*ip)) ? atoi(ip) : DEFPORT;

	/* Is port already open in cache? */
	s = 0;
	for (i=0;  i < MAXDEV;  i++)
	    if (s_port[i] == port) {
		if (s_checksum[i] != sum) {
		    fclose (s_fp[i]);
		    s_fd[i] = s_port[i] = s_checksum[i] = 0;
		    s_fp[i] = NULL;
		} else {
		    s = s_fd[i];
		    fp = s_fp[i];
		}
		break;
	    }

	if (!s) {
	    if ((hp = gethostbyname(host)) == NULL)
		return;
	    if ((s = socket (AF_INET, SOCK_STREAM, 0)) < 0)
		return;

	    bzero ((char *)&sockaddr, sizeof(sockaddr));
	    bcopy ((char *)hp->h_addr,(char *)&sockaddr.sin_addr, hp->h_length);
	    sockaddr.sin_family = AF_INET;
	    sockaddr.sin_port = htons((short)port);
	    if (connect (s,(struct sockaddr *)&sockaddr,sizeof(sockaddr)) < 0) {
		close (s);
		return;
	    }

	    fp = fdopen (s, "w");
	    for (i=0;  i < MAXDEV;  i++)
		if (!s_fp[i]) {
		    s_port[i] = port;
		    s_fd[i] = s;
		    s_fp[i] = fp;
		    s_checksum[i] = sum;
		    break;
		}
	}

	/* Ignore signal generated if server goes away unexpectedly. */
	nsockets++;
	if (!sigpipe)
	    sigpipe = signal (SIGPIPE, SIG_IGN);

	mp->mtdev.statusout = fp;
	zmtdbgn (mp, "iodev = %s", mp->iodev);
	if (gethostname (host, SZ_FNAME) == 0)
	    zmtdbgn (mp, "host = %s", host);
#endif
}


/* ZMTDBGCLOSE -- Close the status output.  Called at ZZCLMT time.  If the
 * status output is a socket merely flush the output and restore the original
 * sigpipe signal handler if the reference count for the process goes to zero.
 * If the output is a file always close the file.  If the debug output is
 * changed from a socket to a file during the execution of a process this
 * will leave a socket open, never to be closed, but this is not likely to
 * be worth fixing since the status output device, if used, should change
 * infrequently.
 */
static void zmtdbgclose (struct mtdesc *mp)
{
	register int i;

	if (mp->mtdev.statusout) {
	    fflush (mp->mtdev.statusout);
	    for (i=0;  i < MAXDEV;  i++) {
		if (s_fp[i] == mp->mtdev.statusout) {
		    if (s_port[i])
			nsockets--;
		    else {
			fclose (mp->mtdev.statusout);
			s_checksum[i] = 0;
			s_fp[i] = NULL;
		    }
		    break;
		}
	    }

	    if (sigpipe && nsockets <= 0) {
		signal (SIGPIPE, sigpipe);
		sigpipe = (SIGFUNC) NULL;
		nsockets = 0;
	    }
	}
}

static void zmtdbg (struct	mtdesc *mp, char *msg)
{
	register FILE *out;
	register char *ip;

	if ((out = mp->mtdev.statusout)) {
	    for (ip=msg;  *ip;  ip++) {
		if (*ip == '\n') {
		    putc ('\\', out);
		    putc ('n', out);
		} else
		    putc (*ip, out);
	    }
	    putc ('\n', out);
	}
}

static void zmtfls (struct mtdesc *mp)
{
	FILE	*out;
	if ((out = mp->mtdev.statusout))
	    fflush (out);
}

static void zmtdbgn ( struct mtdesc *mp, const char *argsformat, ... )
{
        va_list ap;
        char obuf[SZ_LINE];
        va_start (ap, argsformat);
        vsnprintf (obuf, SZ_LINE, argsformat, ap);
        va_end (ap);
        obuf[SZ_LINE-1]='\0';
        zmtdbg (mp, obuf);
}



#else

int ZZOPMT (
  PKCHAR  *device,	/* device name */
  XINT	  *acmode,	/* access mode: read_only or write_only for tapes */
  PKCHAR  *devcap,	/* tapecap entry for device */
  XINT	  *devpos,	/* pointer to tape position info struct */
  XINT	  *newfile,	/* file to be opened or EOT */
  XINT	  *chan 	/* OS channel of opened file */
)
{
	return (XERR);
}


int ZZCLMT (XINT *chan, XINT *devpos, XINT *o_status)
{
	return (XERR);
}


int ZZRDMT (
  XINT	*chan,
  XCHAR	*buf,
  XINT	*maxbytes,
  XLONG	*offset 		/* fixed block devices only */
)
{
	return (XERR);
}


int ZZWRMT (
  XINT	*chan,
  XCHAR	*buf,
  XINT	*nbytes,
  XLONG	*offset 		/* ignored on a write */
)
{
	return (XERR);
}


int ZZWTMT (
  XINT	*chan,
  XINT	*devpos,
  XINT	*o_status 
)
{
	return (XERR);
}


int ZZSTMT (XINT *chan, XINT *param, XLONG *lvalue)
{
	return (XERR);
}


int ZZRWMT (
  PKCHAR  *device,		/* device name */
  PKCHAR  *devcap,		/* tapecap entry for device */
  XINT	  *o_status 
)
{
	return (XERR);
}


#endif


