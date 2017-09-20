/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

#define	NOKNET
#define import_spp
#define import_finfo
#define import_knames
#include <iraf.h>

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

#define	TF_STDIN	0
#define	TF_STDOUT	1
#define	TF_BINARY	2
#define	TF_TAPE		3

#define	R	0
#define	W	1
#define	RW	2

/* Tape position structure (V2.10). */
struct mtpos {
        int     filno;                  /* current file (1=first) */
        int     recno;                  /* current record (1=first) */
        int     nfiles;                 /* number of files on tape */
        int     tapeused;               /* total tape used (Kb) */
        int     pflags;                 /* i/o status bitflags (output) */
};

/* MTPOS bitflags. */
#define MF_ERR  0001    /* i/o error occurred in last operation */
#define MF_EOF  0002    /* a tape mark was seen in the last operation */
#define MF_EOT  0004    /* end of tape seen in the last operation */
#define MF_EOR  0010    /* a record advance occurred in the last operation */

static	int ftype;
static	XINT acmode;
static	int ateof;
static	XLONG offset = 0;

static  int os_mtname (char *fname, char *osdev);

extern  int ZZOPMT(PKCHAR *device, XINT *acmode, PKCHAR *devcap, XINT *devpos, XINT *newfile, XINT *chan);
extern	int ZOPNBF(PKCHAR *osfn, XINT *mode, XINT *chan);
extern	int ZCLSBF(XINT *fd, XINT *status);
extern	int ZZCLMT(XINT *chan, XINT *devpos, XINT *o_status);
extern  int ZARDBF(XINT *chan, XCHAR *buf, XINT *maxbytes, XLONG *offset);
extern	int ZAWTBF(XINT *fd, XINT *status);
extern	int ZZRDMT(XINT *chan, XCHAR *buf, XINT *maxbytes, XLONG *offset);
extern	int ZZWTMT(XINT *chan, XINT *devpos, XINT *o_status);
extern	int ZAWRBF(XINT *chan, XCHAR *buf, XINT *nbytes, XLONG *offset);
extern	int ZZWRMT(XINT *chan, XCHAR *buf, XINT *nbytes, XLONG *offset);



/* TAPE_OPEN -- Open the named file, which need not actually be a tape device.
 */
int
tape_open (
  char	*fname,		/* file or device to be opened	*/
  int	mode 		/* access mode			*/
)
{
	PKCHAR	osfn[SZ_PATHNAME+1];
	XINT	chan;
	extern  char *vfn2osfn(char *vfn, int new);


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
	    register int *op;
	    struct  mtpos devpos;
	    int     nwords = sizeof(devpos) / sizeof(int);
	    XINT    newfile = 0;
	    char    *tapecap = ":np";

	    for (op = (int *)&devpos;  --nwords >= 0;  )
		*op++ = 0;
	    ftype = TF_TAPE;
	    if (mode == R)
		acmode = READ_ONLY;
	    else
		acmode = WRITE_ONLY;

	    ZZOPMT (osfn, &acmode, (PKCHAR *)tapecap, (XINT *)&devpos, 
		&newfile, &chan);

	} else {
	    /* Open a binary disk file.
	     */
	    ftype = TF_BINARY;
	    offset = 1;

	    strcpy ((char *)osfn, vfn2osfn (fname, 0));
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
int
tape_close (int fd)
{
	struct	mtpos devpos;
	XINT	x_fd=fd, status;

	if (ftype == TF_BINARY)
	    ZCLSBF (&x_fd, &status);
	else if (ftype == TF_TAPE)
	    ZZCLMT (&x_fd, (XINT *)&devpos, &status);
	else
	    status = XOK;

	return (status == XERR ? ERR : OK);
}


/* TAPE_READ -- Read from a file opened with tape_open.
 */
int
tape_read (
  int	fd,			/* input file		*/
  char	*buf,			/* output buffer	*/
  int	maxbytes 		/* max bytes to read	*/
)
{
	struct	mtpos devpos;
	XINT	x_fd=fd, x_maxbytes=maxbytes, status;

	if (ateof)
	    return (0);

	if (ftype == TF_STDIN) {
	    status = read (0, buf, maxbytes);
	} else if (ftype == TF_BINARY) {
	    ZARDBF (&x_fd, (XCHAR *)buf, &x_maxbytes, &offset);
	    ZAWTBF (&x_fd, &status);
	    if (status > 0)
		offset += status;
	} else if (ftype == TF_TAPE){
	    ZZRDMT (&x_fd, (XCHAR *)buf, &x_maxbytes, &offset);
	    ZZWTMT (&x_fd, (XINT *)&devpos, &status);
	    if (devpos.pflags & MF_EOF)
		ateof++;
	} else
	    status = XERR;

	return (status == XERR ? ERR : status);
}


/* TAPE_WRITE -- Write to a file opened with tape_open.
 */
int
tape_write (
  int	fd,			/* output file		*/
  char	*buf,			/* input bufferr	*/
  int	nbytes 			/* nbytes to write	*/
)
{
	struct	mtpos devpos;
	XINT	x_fd=fd, x_nbytes=nbytes, status;

	if (ftype == TF_STDOUT) {
	    status = write (1, buf, nbytes);
	} else if (ftype == TF_BINARY) {
	    ZAWRBF (&x_fd, (XCHAR *)buf, &x_nbytes, &offset);
	    ZAWTBF (&x_fd, &status);
	    if (status > 0)
		offset += status;
	} else if (ftype == TF_TAPE) {
	    ZZWRMT (&x_fd, (XCHAR *)buf, &x_nbytes, &offset);
	    ZZWTMT (&x_fd, (XINT *)&devpos, &status);
	} else
	    status = XERR;

	return (status == XERR ? ERR : status);
}


/* OS_MTNAME -- Parse a filename to determine if the file is a magtape
 * device or something else.  A nonzero return indicates that the device
 * is a tape.
 */
static int
os_mtname (
  char	*fname,		/* filename e.g., "foo.tar" or "mua0:". */
  char	*osdev 		/* receives host system drive name */
)
{
	/* Ignore any "mt." prefix.  This is for backwards compatibility,
	 * to permit old-style names like "mt.MUA0:".
	 */
	if (!strncmp (fname, "mt.", 3) || !strncmp (fname, "MT.", 3))
	    fname += 3;

	/* For unix systems we assume anything beginning with /dev is a
	 * tape device.
	 */
	if (strncmp (fname, "/dev/", 5) == 0) {
	    strcpy (osdev, fname);
	    return (1);
	}

	strcpy (osdev, fname);
	return (0);
}
