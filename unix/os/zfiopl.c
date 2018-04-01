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
 * ZFIOPL -- IRAF FIO interface to plotter devices.  A plotter
 * is opened as a streaming type (no seeks) write-only binary device.
 * FIO writes NCAR metacode to the plotter.  This metacode is spooled
 * in a temporary file and then disposed of to the plotter by calling
 * an OS metacode translator to process the file.  The spoolfile is then
 * deleted.
 *
 * The system and device dependent information necessary to perform these
 * functions is contained in three strings passed as the "device" parameter
 * to ZOPNPL.  The strings come from the GRAPHCAP entry for the device.
 * The format of such a string is
 *
 *	device D spoolfile D dispose_cmd EOS
 *
 * where DEVICE is the logical device name (not used herein), D is the field
 * delimiter character (the first nonalphnumeric character encountered after
 * the device field), SPOOLFILE is a UNIX pathname to be passed to MKTEMP
 * to create the spoolfile pathname, and DISPOSE_CMD is a fill-in-the-blanks
 * template for a UNIX shell command which will dispose of the spoolfile to
 * the plotter device.
 */

extern	int save_prtype;

#define	SZ_OSCMD	512		/* buffer for dispose cmd	*/
#define	SZ_PLSTR	256		/* zopnpl plotter argument	*/

struct dplotter {
	char	*name;			/* logical gdevice name		*/
	char	*spoolfile;		/* spoolfile string		*/
	char	*dispose;		/* dispose format string	*/
};

struct oplotter {
	long	wbytes;			/* nbytes written to device	*/
	struct	dplotter *pl;		/* device code as above		*/
	int	status;			/* status of last write		*/
	char	spoolfile[SZ_PATHNAME+1];
};

struct	dplotter dpltr;			/* machdep plotter info		*/
struct	oplotter pltr;			/* open plotter descriptor	*/
char	plstr[SZ_PLSTR+1];		/* save zopnpl argument		*/
int	pltr_inuse = NO;		/* set if plotter is open	*/


extern  int ZOPNBF (PKCHAR *osfn, XINT *mode, XINT *chan);
extern	int ZCLSBF (XINT *fd, XINT *status);
extern	int ZOSCMD (PKCHAR *oscmd, PKCHAR *stdin_file, PKCHAR  *stdout_file, PKCHAR *stderr_file, XINT *status);
extern	int ZFDELE (PKCHAR *fname, XINT *status);
extern	int ZARDBF (XINT *chan, XCHAR *buf, XINT *maxbytes, XLONG *offset);
extern  int ZAWRBF (XINT *chan, XCHAR *buf, XINT *nbytes, XLONG *offset);
extern	int ZAWTBF (XINT *fd, XINT *status);
extern	int ZSTTBF (XINT *fd, XINT *param, XLONG *lvalue);


/* ZOPNPL -- Open a plotter device for binary file i/o.  If we can talk
 * directly to the plotter, do so, otherwise open a spoolfile which is
 * to be sent to the plotter when ZCLSPL is later called.
 */
int
ZOPNPL (
  PKCHAR  *plotter,		/* plotter device descriptor		*/
  XINT	  *mode,		/* file access mode			*/
  XINT	  *chan 		/* UNIX file number (output)		*/
)
{
	register char *ip;
	static char delim;
	int fd;

	/* We do not see a need to have more than one plotter open at
	 * a time, and it makes things simpler.  We can easily generalize
	 * to multiple open plotter devices in the future if justified.
	 */
	if (pltr_inuse == YES) {
	    *chan = XERR;
	    return (XERR);
	} else
	    pltr_inuse = YES;
	
	/* Parse the plotter string into the name, spoolfile, and dispose
	 * strings.
	 */
	strncpy (plstr, (char *)plotter, SZ_PLSTR);

	/* Locate NAME field. */
	dpltr.name = plstr;
	for (ip=plstr;  isalnum(*ip);  ip++)
	    ;
	delim = *ip;
	*ip++ = EOS;

	/* Locate SPOOLFILE field. */
	for (dpltr.spoolfile=ip;  *ip && *ip != delim;  ip++)
	    ;
	*ip++ = EOS;

	/* Locate DISPOSE field. */
	for (dpltr.dispose=ip;  *ip && *ip != delim;  ip++)
	    ;
	*ip++ = EOS;

	/* Initialize the open plotter descriptor.
	 */
	pltr.wbytes = 0L;
	pltr.pl = &dpltr;
	strcpy (pltr.spoolfile, dpltr.spoolfile);
	if (dpltr.dispose[0] != EOS)
	    if ((fd = mkstemp (pltr.spoolfile)) >= 0) {
		fchmod (fd, 0644);
		close (fd);
	    }
	
	return ZOPNBF ((PKCHAR *)pltr.spoolfile, mode, chan);
}


/* ZCLSPL -- To close a plotter we merely close the "spoolfile", and then
 * dispose of the spoolfile to the OS if so indicated.
 */
int
ZCLSPL (XINT *chan, XINT *status)
{
	static	PKCHAR	xnullstr[1] = { EOS };
	register char *ip, *op, *f;
	PKCHAR	cmd[(SZ_OSCMD+1) / sizeof(PKCHAR)];
	XINT	junk;

	ZCLSBF (chan, status);
	pltr_inuse = NO;

	/* Dispose of the output file if so indicated.  Do not bother to
	 * check the status return, since we cannot return status to FIO
	 * from here anyhow.  Do not dispose of the file if it is empty.
	 * If the file is disposed of by the OS, we assume that it is also
	 * deleted after printing.  If file is not disposed to the OS, we
	 * delete it ourselves.
	 */
	if (*(pltr.pl->dispose) != EOS) {
	    if (pltr.wbytes > 0) {
		PKCHAR   out[SZ_FNAME+1];

		/* Build up command line by substituting the spoolfile name
		 * everywhere the macro "$F" appears in the "dispose" text.
		 */
		op = (char *)cmd;
		for (ip=pltr.pl->dispose;  (*op = *ip++) != EOS;  op++)
		    if (*op == '$' && *ip == 'F') {
			for (f=pltr.spoolfile;  (*op = *f++) != EOS;  op++)
			    ;
			/* Overwrite EOS, skip over 'F' */
			--op, ip++;
		    }
		strcpy ((char *)out,
		    save_prtype == PR_CONNECTED ? "/dev/tty" : "");
		ZOSCMD (cmd, xnullstr, out, out, &junk);
	    } else
		ZFDELE ((PKCHAR *)pltr.spoolfile, &junk);
	}

	return (*status);
}


/* ZARDPL -- For UNIX, the read and write routines are just the binary file
 * i/o routines.  Note that packing of chars into bytes, mapping of escape
 * sequences, etc. is done by the high level code; our function is merely to
 * move the data to the device.  The read primitive is not likely to be needed
 * for a plotter, but you never know...
 */
int
ZARDPL (
  XINT	 *chan,
  XCHAR	 *buf,
  XINT	 *maxbytes,
  XLONG	 *offset
)
{
	return ZARDBF (chan, buf, maxbytes, offset);
}


/* ZAWRPL -- Write a metafile record to the plotter spoolfile.  We are always
 * called to write metacode; zfiopl is not used to send device codes to the
 * plotter.  Our job is to make the NSPP metacode record passed on to us by
 * WRITEB look like whatever the system metacode translators expect.  On the
 * KPNO system, the metacode translators are Fortran programs, expecting an
 * unformatted binary metacode file as input.  We simulate this file by
 * adding the integer byte count of the record to the beginning and end of
 * each record.
 *
 * N.B.: We ASSUME that the FIO buffer has been set to the size of a metafile
 * record, i.e., 1440 bytes or 720 chars on the VAX.
 */
int
ZAWRPL (
  XINT	 *chan,
  XCHAR	 *buf,
  XINT	 *nbytes,
  XLONG	 *offset 			/* not used		*/
)
{
	static	XINT hdrlen=sizeof(int);
	static	XLONG noffset=0L;
	XINT	status;
	int	reclen;

	/* Write out the integer record header.  Set the file offset to zero
	 * since the file is sequential, and the offsets do not include the
	 * record headers anyhow so are wrong.
	 */
	reclen = *nbytes;
	ZAWRBF (chan, (XCHAR *)&reclen, &hdrlen, &noffset);
	ZAWTBF (chan, &status);

	/* Write the metacode data.
	 */
	pltr.wbytes += *nbytes;
	ZAWRBF (chan, buf, nbytes, &noffset);
	ZAWTBF (chan, &status);
	pltr.status = status;

	/* Write out the integer record trailer.  Set the file offset to zero
	 * since the file is sequential, and the offsets do not include the
	 * record headers anyhow so are wrong.
	 */
	reclen = *nbytes;
	ZAWRBF (chan, (XCHAR *)&reclen, &hdrlen, &noffset);
	ZAWTBF (chan, &status);

	return (status);
}


/* ZAWTPL -- Return the status of the write (we do not read metafiles with
 * the plotter interface).  The status byte count does not include the
 * record header, since that was written with a separate write unbeknownst
 * to FIO, so the status value returned refers only to the metacode data.
 */
int
ZAWTPL (XINT *chan, XINT *status)
{
	ZAWTBF (chan, status);
	if (*status > 0)
	    *status = pltr.status;

	return (*status);
}


/* ZSTTPL -- Get status for the plotter output file.  Plotter output is
 * strictly sequential due to the way metacode records are packaged in
 * ZAWRPL.  Hence we must always return blksize=0 to indicate that the
 * device is a streaming file, regardless of whether or not the output
 * is spooled.
 */
int
ZSTTPL (XINT *chan, XINT *param, XLONG *lvalue)
{
	switch (*param) {
	case FSTT_BLKSIZE:
	    *lvalue = 0L;
	    break;
	default:
	    return ZSTTBF (chan, param, lvalue);
	}
	return (XOK);
}
