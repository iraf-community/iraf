/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
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
 *    bool = os_mtname (fname, drive, &density)
 *
 * Only one "tape" file can be open at a time (unless all open files are of
 * the same type).  Since we call ZARDBF and ZAWRBF directly, only blocked
 * output is permitted (there is no internal buffering).  Only sequential
 * output is permitted to disk (there is no seek entry point).
 */

#define	TF_STDIN	0
#define	TF_STDOUT	1
#define	TF_BINARY	2
#define	TF_TAPE		3
#define	DEVTABLE	"dev$devices"

#define	R	0
#define	W	1
#define	RW	2

static	int ftype;
static	int acmode;
static	int ateof;
static	long offset = 0;


/* TAPE_OPEN -- Open the named file, which need not actually be a tape device.
 */
tape_open (fname, mode)
char	*fname;		/* file or device to be opened	*/
int	mode;		/* access mode			*/
{
	PKCHAR	osfn[SZ_PATHNAME+1];
	int	chan, density;

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

	} else if (os_mtname (fname, (char *)osfn, &density)) {
	    /* Open a magtape device.  NOTE: names "mta", "mtb", etc. only.
	     * Try to open without moving the tape (newfile=0).
	     */
	    int     oldrec=0, oldfile=0, newfile=0;

	    ftype = TF_TAPE;

	    if (mode == R)
		acmode = READ_ONLY;
	    else
		acmode = WRITE_ONLY;

	    ZZOPMT (osfn, &density, &acmode,
		&oldrec, &oldfile, &newfile, &chan);

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
tape_close (fd)
int	fd;
{
	int	status;
	int	nrecords, nfiles;

	if (ftype == TF_BINARY)
	    ZCLSBF (&fd, &status);
	else if (ftype == TF_TAPE)
	    ZZCLMT (&fd, &acmode, &nrecords, &nfiles, &status);
	else
	    status = XOK;

	return (status == XERR ? ERR : OK);
}


/* TAPE_READ -- Read from a file opened with tape_open.
 */
tape_read (fd, buf, maxbytes)
int	fd;			/* input file		*/
char	*buf;			/* output buffer	*/
int	maxbytes;		/* max bytes to read	*/
{
	int	nrecords, nfiles;
	int	status;

	if (ateof)
	    return (0);

	if (ftype == TF_STDIN) {
	    status = read (0, buf, maxbytes);
	} else if (ftype == TF_BINARY) {
	    ZARDBF (&fd, (XCHAR *)buf, &maxbytes, &offset);
	    ZAWTBF (&fd, &status);
	    if (status > 0)
		offset += status;
	} else if (ftype == TF_TAPE){
	    ZZRDMT (&fd, (XCHAR *)buf, &maxbytes);
	    ZZWTMT (&fd, &nrecords, &nfiles, &status);
	    if (nfiles > 0)
		ateof++;
	} else
	    status = XERR;

	return (status == XERR ? ERR : status);
}


/* TAPE_WRITE -- Write to a file opened with tape_open.
 */
tape_write (fd, buf, nbytes)
int	fd;			/* output file		*/
char	*buf;			/* input bufferr	*/
int	nbytes;			/* nbytes to write	*/
{
	int	nrecords, nfiles;
	int	status;

	if (ftype == TF_STDOUT) {
	    status = write (1, buf, nbytes);
	} else if (ftype == TF_BINARY) {
	    ZAWRBF (&fd, (XCHAR *)buf, &nbytes, &offset);
	    ZAWTBF (&fd, &status);
	    if (status > 0)
		offset += status;
	} else if (ftype == TF_TAPE) {
	    ZZWRMT (&fd, (XCHAR *)buf, &nbytes);
	    ZZWTMT (&fd, &nrecords, &nfiles, &status);
	} else
	    status = XERR;

	return (status == XERR ? ERR : status);
}


/* OS_MTNAME -- Parse a magtape name.  Return the drive name and density
 * if the name is a magtape, returning true/false as the function value
 * indicating whether the filename is a tape.
 */
os_mtname (fname, osdev, density)
char	*fname;		/* filename e.g., "mta" or "mta1600"	*/
char	*osdev;		/* receives host system drive name	*/
int	*density;	/* receives density			*/
{
	register char	*ip, *op;
	register int	n;
	char	drive[SZ_FNAME+1];
	char	tabname[SZ_FNAME+1];
	char	lbuf[SZ_LINE];
	FILE	*fp;

	drive[0] = EOS;
	*density = 0;

	/* Must have "mt" prefix to be a tapefile.
	 */
	for (ip=fname;  *ip == ' ';  ip++)
	    ;
	if (*ip++ != 'm' || *ip++ != 't')
	    return (0);

	/* Extract logical drive name, e.g., the "a" in "mta".
	 */
	if (*ip == '.') {
	    for (ip++, op=drive;  (*op = *ip);  op++)
		if (*ip++ == '.')
		    break;
	} else {
	    for (op=drive;  (*op = *ip);  ip++, op++)
		if (isdigit(*ip) || *ip == '[')
		    break;
	}
	*op = EOS;

	/* Get density, if given.
	 */
	for (n=0;  isdigit (*ip);  ip++)
	    n = (n * 10) + *ip - '0';

	(*density) = n;

	/* Format drive name as given in the DEV$DEVICES file, and scan the
	 * file to get host system drive name.  If no entry is found for the
	 * named device assume we have a host system name, e.g., "mt.MUA0:".
	 */
	if (*density)
	    sprintf (tabname, "mt%s.%d", drive, *density);
	else
	    sprintf (tabname, "mt%s", drive);

	if ((fp = fopen (vfn2osfn(DEVTABLE,0), "r")) == NULL)
	    goto notfound_;

	n = strlen (tabname);
	while (fgets (lbuf, SZ_LINE, fp) != NULL)
	    if (strncmp (lbuf, tabname, n) == 0) {
		for (ip=lbuf+n;  isspace (*ip);  ip++)
		    ;
		for (op=osdev;  (*op = *ip) && !isspace(*op);  op++, ip++)
		    ;
		*op = EOS;
		fclose (fp);
		return (1);
	    }

notfound_:
	fclose (fp);

	/* If the logical device could not be found and a drive name was given,
	 * assume that a host drive is named (e.g., "mt.MUA1:") and just use
	 * that.
	 */
	if (*drive == EOS)
	    return (ERR);
	else {
	    strcpy (osdev, drive);
	    return (1);
	}
}
