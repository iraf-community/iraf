/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>

#define import_kernel
#define import_knames
#define import_spp
#include <iraf.h>

#include "zos.h"

/*
 * ZFIOTY -- Device driver for terminals.  In the 4.1BSD UNIX kernel the same
 * driver is used for both terminals and ordinary text files, hence all we
 * need do to implement a TY routine is call the corresponding TX routine.
 * See "zfiotx.c" for the real driver.
 */

/* ZOPNTY -- Open or create a text file.  The special name "dev$tty" denotes
 * the user terminal and is passed to us via TTOPEN (in etc).  Direct access
 * to the terminal in this way (possibly from a subprocess) may not be possible
 * on all host systems.
 */
/* osfn : UNIX filename			*/
/* mode : file access mode		*/
/* chan : UNIX channel of file (output)	*/
int ZOPNTY ( PKCHAR *osfn, XINT *mode, XINT *chan )
{
	PKCHAR	ttyname[SZ_FNAME+1];

	if (strcmp ((const char *)osfn, "dev$tty") == 0)
	    strcpy ((char *)ttyname, TTYNAME);
	else
	    safe_strcpy ((char *)ttyname, SZ_FNAME+1, (const char *)osfn);

	return ZOPNTX (ttyname, mode, chan);
}


/* ZCLSTY -- Close a text file.
 */
int ZCLSTY ( XINT *fd, XINT *status )
{
	return ZCLSTX (fd, status);
}


/* ZFLSTY -- Flush any buffered textual output.
 */
int ZFLSTY ( XINT *fd, XINT *status )
{
	return ZFLSTX (fd, status);
}


/* ZGETTY -- Get a line of text from a text file.  Unpack machine chars
 * into type XCHAR.  If output buffer is filled before newline is encountered,
 * the remainder of the line will be returned in the next call, and the
 * current line will NOT be newline terminated.  If maxchar==1 assert
 * character mode, otherwise assert line mode.
 */
int ZGETTY ( XINT *fd, XCHAR *buf, XINT *maxchars, XINT *status )
{
	return ZGETTX (fd, buf, maxchars, status);
}


/* ZNOTTY -- Return the seek offset of the beginning of the current line
 * of text.
 */
int ZNOTTY ( XINT *fd, XLONG *offset )
{
	return ZNOTTX (fd, offset);
}


/* ZPUTTY -- Put "nchars" characters into the text file "fd".   The final
 * character will always be a newline, unless the FIO line buffer overflowed,
 * in which case the correct thing to do is to write out the line without
 * artificially adding a newline.  We do not check for newlines in the text,
 * hence ZNOTTY will return the offset of the next write, which will be the
 * offset of the beginning of a line of text only if we are called to write
 * full lines of text.
 */
/* fd     : file to be written to	*/
/* buf    : data to be output		*/
/* nchars : nchars to write to file	*/
/* status : return status		*/
int ZPUTTY ( XINT *fd, XCHAR *buf, XINT *nchars, XINT *status )
{
	return ZPUTTX (fd, buf, nchars, status);
}


/* ZSEKTY -- Seek on a text file to the character offset given by a prior
 * call to ZNOTTY.  This offset should always refer to the beginning of a line.
 */
int ZSEKTY ( XINT *fd, XLONG *znotty_offset, XINT *status )
{
	return ZSEKTX (fd, znotty_offset, status);
}


/* ZSTTTY -- Get file status for a text file.
 */
/* fd    : file number				*/
/* param : status parameter to be returned	*/
/* value : return value				*/
int ZSTTTY ( XINT *fd, XINT *param, XLONG *value )
{
	return ZSTTTX (fd, param, value);
}
