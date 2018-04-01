/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>

#define	import_kernel
#define	import_knames
#define	import_zfstat
#define	import_prtype
#define import_spp
#include <iraf.h>

/*
 * ZFIOLP -- IRAF FIO interface to the line printer device.  The line printer
 * is opened as a streaming type (no seeks) write-only binary device.
 * On systems like UNIX in which the line printer is just another file,
 * the interface is trivial; we just call the FIOBF routines.  On other
 * systems it might be necessary to spool the output to a binary file
 * and dispose of the file to a queue when the printer file is closed,
 * or some such thing.
 *
 * Currently, the CL-callable LPRINT program is the only thing in IRAF which
 * writes to the printer.  Most programs are intended to write to their
 * standard output or a file, which is subsequently copied to the printer
 * by the user or by a CL level script.  LPRINT uses "dev$*.tty" descriptors
 * and the TTY interface to learn the characteristics of the printer (eject
 * is considered equivalent to a tty clear, for example).
 *
 * The system and device dependent information necessary to perform these
 * functions is contained in three strings passed as the "printer" parameter
 * to ZOPNLP.  The strings come from the TERMCAP entry for the device.
 * The format of such a string is
 *
 *	device D spoolfile D dispose_cmd EOS
 *
 * where DEVICE is the logical device name (not used herein), D is the field
 * delimiter character (the first nonalphnumeric character encountered after
 * the device field), SPOOLFILE is a UNIX pathname to be passed to MKTEMP
 * to create the spoolfile pathname, and DISPOSE_CMD is a fill-in-the-blanks
 * template for a UNIX shell command which will dispose of the spoolfile to
 * the printer device.
 */

extern	int save_prtype;

#define	SZ_OSCMD	512		/* buffer for dispose cmd	*/
#define	SZ_LPSTR	256		/* zopnlp plotter argument	*/

struct lprinter {
	char	*name;			/* logical gdevice name		*/
	char	*spoolfile;		/* spoolfile string		*/
	char	*dispose;		/* dispose format string	*/
};

struct oprinter {
	struct	lprinter *lp;		/* device code as above		*/
	long	wbytes;			/* nbytes written to device	*/
	char	spoolfile[SZ_PATHNAME+1];
};

struct	lprinter dpr;			/* device table			*/
struct	oprinter lpr;			/* printer descriptor		*/
int	lpr_inuse = NO;			/* set if printer is open	*/
char	lpstr[SZ_LPSTR+1];		/* save zopnlp argument		*/


extern  int ZOPNBF (PKCHAR *osfn, XINT *mode, XINT *chan);
extern	int ZCLSBF (XINT *fd, XINT *status);
extern	int ZOSCMD (PKCHAR *oscmd, PKCHAR *stdin_file, PKCHAR  *stdout_file, PKCHAR *stderr_file, XINT *status);
extern	int ZFDELE (PKCHAR *fname, XINT *status);
extern	int ZARDBF (XINT *chan, XCHAR *buf, XINT *maxbytes, XLONG *offset);
extern  int ZAWRBF (XINT *chan, XCHAR *buf, XINT *nbytes, XLONG *offset);
extern	int ZAWTBF (XINT *fd, XINT *status);
extern	int ZSTTBF (XINT *fd, XINT *param, XLONG *lvalue);


/* ZOPNLP -- Open a printer device for binary file i/o.  If we can talk
 * directly to the printer, do so, otherwise open a spoolfile which is
 * to be sent to the printer when ZCLSLP is later called.
 */
int
ZOPNLP (
  PKCHAR  *printer,		/* logical name of printer device	*/
  XINT	  *mode,		/* file access mode			*/
  XINT	  *chan 		/* UNIX file number (output)		*/
)
{
	register char *ip;
	static char delim;
	int fd;


	/* We do not see a need to have more than one printer open at
	 * a time, and it makes things simpler.  We can easily generalize
	 * to multiple open printer devices in the future if justified.
	 */
	if (lpr_inuse == YES) {
	    *chan = XERR;
	    return (XERR);
	} else
	    lpr_inuse = YES;
	
	/* Parse the printer string into the name, spoolfile, and dispose
	 * strings.
	 */
	strncpy (lpstr, (char *)printer, SZ_LPSTR);
	lpstr[SZ_LPSTR] = EOS;

	/* Locate NAME field. */
	dpr.name = lpstr;
	for (ip=lpstr;  isalnum(*ip);  ip++)
	    ;
	delim = *ip;
	*ip++ = EOS;

	/* Locate SPOOLFILE field. */
	for (dpr.spoolfile=ip;  *ip && *ip != delim;  ip++)
	    ;
	*ip++ = EOS;

	/* Locate DISPOSE field. */
	for (dpr.dispose=ip;  *ip && *ip != delim;  ip++)
	    ;
	*ip++ = EOS;

	/* Initialize the open printer descriptor.
	 */
	lpr.wbytes = 0L;
	lpr.lp = &dpr;
	strcpy (lpr.spoolfile, dpr.spoolfile);
	if (dpr.dispose[0] != EOS)
	    if ((fd = mkstemp (lpr.spoolfile)) >= 0) {
		fchmod (fd, 0644);
		close (fd);
	    }
	
	return ZOPNBF ((PKCHAR *)lpr.spoolfile, mode, chan);
}


/* ZCLSLP -- To close a printer we merely close the "spoolfile", and then
 * dispose of the spoolfile to the OS if so indicated.
 */
int
ZCLSLP (XINT *chan, XINT *status)
{
	static	PKCHAR	xnullstr[1] = { XEOS };
	register char *ip, *op, *f;
	PKCHAR	cmd[(SZ_LINE+1) / sizeof(PKCHAR)];
	XINT	junk;

	ZCLSBF (chan, status);
	lpr_inuse = NO;

	/* Dispose of the output file if so indicated.  Do not bother to
	 * check the status return, since we cannot return status to FIO
	 * from here anyhow.  Do not dispose of the file if it is empty.
	 * If the file is disposed of by the OS, we assume that it is also
	 * deleted after printing.  If file is not disposed to the OS, we
	 * delete it ourselves.
	 */
	if (*(lpr.lp->dispose) != EOS) {
	    if (lpr.wbytes > 0) {
		PKCHAR   out[SZ_FNAME+1];

		/* Build up command line by substituting the spoolfile name
		 * everywhere the macro "$F" appears in the "dispose" text.
		 */
		op = (char *)cmd;
		for (ip=lpr.lp->dispose;  (*op = *ip++) != EOS;  op++)
		    if (*op == '$' && *ip == 'F') {
			for (f=lpr.spoolfile;  (*op = *f++) != EOS;  op++)
			    ;
			/* Overwrite EOS, skip over 'F' */
			--op, ip++;
		    }
		strcpy ((char *)out,
		    save_prtype == PR_CONNECTED ? "/dev/tty" : "");
		ZOSCMD (cmd, xnullstr, out, out, &junk);
	    } else
		ZFDELE ((PKCHAR *)lpr.spoolfile, &junk);
	}

	return (*status);
}


/* ZARDLP -- Initiate a read from the line printer device.  For UNIX, the read
 * and write routines are just the binary file i/o routines.  Note that packing
 * of chars into bytes, mapping of escape sequences, etc. is done by the high
 * level code; our function is merely to move the data to the device.  The read
 * primitive is not likely to be needed for a printer, but you never know...
 */
int
ZARDLP (XINT *chan, XCHAR *buf, XINT *maxbytes, XLONG *offset)
{
	XLONG	dummy_offset = 0;

	return ZARDBF (chan, buf, maxbytes, &dummy_offset);
}


/* ZAWRLP -- Initiate a write to the line printer.  Keep track of the number
 * of bytes written so we know whether or not to dispose of the spoolfile
 * at close time.
 */
int
ZAWRLP (XINT *chan, XCHAR *buf, XINT *nbytes, XLONG *offset)
{
	XLONG	dummy_offset = 0;

	lpr.wbytes += *nbytes;
	return ZAWRBF (chan, buf, nbytes, &dummy_offset);
}


/* ZAWTLP -- Wait for i/o and return the status of the channel, i.e., the
 * number of bytes read or written or XERR.
 */
int
ZAWTLP (XINT *chan, XINT *status)
{
	return ZAWTBF (chan, status);
}


/* ZSTTLP -- Get status for the line printer output file.  We call ZSTTBF since
 * the output file was opened by ZOPNBF.  The actual output file may be either
 * a blocked or streaming file depending on whether the output is spooled.
 */
int
ZSTTLP (XINT *chan, XINT *param, XLONG *lvalue)
{
	switch (*param) {
	case FSTT_BLKSIZE:
	    *lvalue = 0L;		/* streaming device	*/
	    break;
	default:
	    return ZSTTBF (chan, param, lvalue);
	}
	return (*lvalue);
}
