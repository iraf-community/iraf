/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>

#define NOKNET
#define import_spp
#define import_finfo
#define import_knames
#include <iraf.h>
#include "bootlib.h"

/*
 * TAPE.C -- Generalized binary file i/o to a tape drive or other devices.
 *
 *	fd = tape_open (fname, mode)
 *	    tape_close (fd)
 *	nb = tape_read (fd, buf, maxbytes)
 *     nb = tape_write (fd, buf, nbytes)
 *
 * Only one "tape" file can be open at a time (unless all open files are of
 * the same type).  Since we call ZZRDMT and ZZWRMT directly, only blocked
 * output is permitted (there is no internal buffering).  Only sequential
 * output is permitted to disk (there is no seek entry point).
 *
 * NOTE - For the IRAF V2.10 version of this utility, only host device names
 * are permitted.  The IRAF device names "mta", "mtb", etc are not supported
 * as the tapefile file is not read.
 */

#define TF_STDIN	0
#define TF_STDOUT	1
#define TF_BINARY	2
#define TF_TAPE		3

#define R	0
#define W	1
#define RW	2

/* Tape position structure (V?.??). */
struct mtpos {
        XLONG    filno;                  /* current file (1=first) */
        XLONG    recno;                  /* current record (1=first) */
        XLONG    nfiles;                 /* number of files on tape */
        XLONG    tapeused;               /* total tape used (Kb) */
        XLONG    pflags;                 /* i/o status bitflags (output) */
};

/* MTPOS bitflags. */
#define MF_ERR  0001    /* i/o error occurred in last operation */
#define MF_EOF  0002    /* a tape mark was seen in the last operation */
#define MF_EOT  0004    /* end of tape seen in the last operation */
#define MF_EOR  0010    /* a record advance occurred in the last operation */

static	int ftype;
static	int ateof;
static	XINT acmode;
static	XLONG offset = 0;

static int os_mtname ( const char *, char * );

/* TAPE_OPEN -- Open the named file, which need not actually be a tape device.
 */
int tape_open ( const char *fname, int mode )
/* fname : file or device to be opened	*/
/* mode  : access mode			*/
{
	PKCHAR	osfn[SZ_PATHNAME+1];
	XINT	chan;

	if (strcmp (fname, "stdin") == 0) {
	    ftype = TF_STDIN;
	    if (mode != R)
		chan = ERR;
	    else
		chan = 1;	/* arbitrary */

	} else if (strcmp (fname, "stdout") == 0) {
	    ftype = TF_STDOUT;
	    if (mode != W)
		chan = ERR;
	    else
		chan = 1;	/* arbitrary */

	} else if (os_mtname (fname, (char *)osfn)) {
	    /* Open a magtape device.  Only host device names are permitted.
	     * Try to open without moving the tape (newfile=0).
	     */
	    XINT *op;
	    struct mtpos devpos;
	    int nwords = sizeof(devpos) / sizeof(XINT);
	    XLONG newfile = 0;
	    const char *tapecap = ":np";

	    for ( op = (XINT *)&devpos ; nwords > 0 ; nwords-- )
		*op++ = 0;
	    ftype = TF_TAPE;
	    if (mode == R)
		acmode = READ_ONLY;
	    else
		acmode = WRITE_ONLY;

	    ZZOPMT (osfn, &acmode, (XCHAR *)tapecap, (XLONG *)(&devpos), &newfile, &chan);

	} else {
	    /* Open a binary disk file.
	     */
	    ftype = TF_BINARY;
	    offset = 1;

	    safe_strcpy ((char *)osfn, SZ_PATHNAME+1, vfn2osfn (fname, 0));
	    if (mode == R)
		acmode = READ_ONLY;
	    else if (mode == W)
		acmode = NEW_FILE;
	    else
		acmode = READ_WRITE;

	    ZOPNBF (osfn, &acmode, &chan);
	}

	ateof = 0;

	return (chan == XERR ? ERR : chan);
}


/* TAPE_CLOSE -- Close a file opened with tape_open.
 */
int tape_close ( int fd )
{
	struct	mtpos devpos;
	XINT	x_fd, status;

	x_fd = fd;
	if (ftype == TF_BINARY)
	    ZCLSBF (&x_fd, &status);
	else if (ftype == TF_TAPE)
	    ZZCLMT (&x_fd, (XLONG *)(&devpos), &status);
	else
	    status = XOK;

	return (status == XERR ? ERR : OK);
}


/* TAPE_READ -- Read from a file opened with tape_open.
 */
/* fd       : input file	*/
/* buf      : output buffer	*/
/* maxbytes : max bytes to read	*/
long tape_read ( int fd, char *buf, long maxbytes )
{
	struct	mtpos devpos;
	long status;
	XINT	x_fd;
	XLONG	x_status;
	XSIZE_T	x_maxbytes;

	if (ateof)
	    return (0);

	x_fd = fd;
	x_maxbytes = maxbytes;
	if (ftype == TF_STDIN) {
	    status = read (0, buf, maxbytes);
	} else if (ftype == TF_BINARY) {
	    ZARDBF (&x_fd, (XCHAR *)buf, &x_maxbytes, &offset);
	    ZAWTBF (&x_fd, &x_status);
	    if (x_status > 0)
		offset += x_status;
	    status = x_status;
	} else if (ftype == TF_TAPE){
	    ZZRDMT (&x_fd, (XCHAR *)buf, &x_maxbytes, &offset);
	    ZZWTMT (&x_fd, (XLONG *)(&devpos), &x_status);
	    if (devpos.pflags & MF_EOF)
		ateof++;
	    status = x_status;
	} else
	    status = XERR;

	return (status == XERR ? ERR : status);
}


/* TAPE_WRITE -- Write to a file opened with tape_open.
 */
/* fd     : output file		*/
/* buf    : input bufferr	*/
/* nbytes : nbytes to write	*/
long tape_write ( int fd, const char *buf, long nbytes )
{
	struct	mtpos devpos;
	long	status;
	XINT	x_fd;
	XLONG	x_status;
	XSIZE_T	x_nbytes;

	x_fd = fd;
	x_nbytes = nbytes;
	if (ftype == TF_STDOUT) {
	    status = write (1, buf, nbytes);
	} else if (ftype == TF_BINARY) {
	    ZAWRBF (&x_fd, (XCHAR *)buf, &x_nbytes, &offset);
	    ZAWTBF (&x_fd, &x_status);
	    if (x_status > 0)
		offset += x_status;
	    status = x_status;
	} else if (ftype == TF_TAPE) {
	    ZZWRMT (&x_fd, (XCHAR *)buf, &x_nbytes, &offset);
	    ZZWTMT (&x_fd, (XLONG *)(&devpos), &x_status);
	    status = x_status;
	} else
	    status = XERR;

	return (status == XERR ? ERR : status);
}


/* OS_MTNAME -- Parse a filename to determine if the file is a magtape
 * device or something else.  A nonzero return indicates that the device
 * is a tape.
 */
/* fname : filename e.g., "foo.tar" or "mua0:". */
/* osdev : receives host system drive name */
static int os_mtname ( const char *fname, char *osdev )
{
#ifdef VMS
	char *ip;
	char drive[SZ_FNAME+1];
#endif
	/* Ignore any "mt." prefix.  This is for backwards compatibility,
	 * to permit old-style names like "mt.MUA0:".
	 */
	if (!strncmp (fname, "mt.", 3) || !strncmp (fname, "MT.", 3))
	    fname += 3;

#ifdef VMS
	/* Resolve a possible logical device name. */
	if (strchr (fname, '['))
	    safe_strcpy (drive, SZ_FNAME+1, fname);
	else
	    _tranlog (fname, drive);

	/* If the resolved name ends with a colon it is a device name,
	 * which we assume to be a tape device.
	 */
	for (ip=drive;  *ip;  ip++)
	    ;
	if ( drive < ip && *(ip-1) == ':' ) {
	    safe_strcpy (osdev, SZ_PATHNAME+1, drive);
	    return (1);
	}
#else
	/* For unix systems we assume anything beginning with /dev is a
	 * tape device.
	 */
	if (strncmp (fname, "/dev/", 5) == 0) {
	    safe_strcpy (osdev, SZ_PATHNAME+1, fname);
	    return (1);
	}
#endif

	safe_strcpy (osdev, SZ_PATHNAME+1, fname);
	return (0);
}
