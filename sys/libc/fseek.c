/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#define	import_fset
#include <iraf.h>


/* FSEEK -- Seek on a file opened under stdio.  The seek functions do not
** completely emulate the UNIX seek functions.  The following restrictions
** apply:
**
**	- It is only permissible to seek to the beginning of a line if the
**	  actual file is a text file.  The seek offset must have been
**	  obtained by a prior call to FTELL, which returns the seek offset
**	  of the next line to be read or written.  Seek offsets cannot be
**	  manufactured, e.g., as physical char offsets in the file, as is
**	  the case for binary files.  These restrictions apply because text
**	  files are record structured on many systems, rather than simple
**	  byte stream files as in UNIX.
**
**	- It is permissible to randomly seek about on a binary file, but
**	  seeks must be aligned on XCHAR word boundaries in the file.  This
**	  can be guaranteed by structuring the application so that it always
**	  reads and writes binary data records that are an integral number
**	  of integers in size.  If this is done the program is portable to
**	  any IRAF machine as well as to UNIX.  Seek offsets are specified
**	  in units of bytes and are zero-indexed, as in C.
*/
int
fseek (
  FILE	*fp,			/* operand file				*/
  long	offset,			/* offset in file			*/
  int	mode			/* 0=absolute, 1=relative, 2=from EOF	*/
)
{
	register XINT	fd = fileno(fp);
	int	text_file, stat;
	long	c_note();


	text_file = (c_fstati (fd, F_TYPE) == TEXT_FILE);
	fp->_fflags &= ~_FEOF;

	if (text_file) {
	    switch (mode) {
	    case 0:
		if (offset == 0L)
		    stat = c_seek (fd, BOFL);
		else
		    stat = c_seek (fd, offset);
		break;
	    case 2:
		if (offset == 0L) {
		    stat = c_seek (fd, EOFL);
		    fp->_fflags |= _FEOF;
		} else
		    stat = ERR;
		break;
	    default:
		stat = ERR;
		break;
	    }

	} else {
	    /* Random seeks on (non-streaming) binary files are permitted,
	     * but the seek must be to an XCHAR offset.  This is checked
	     * by c_seek, which takes a zero-indexed byte offset as argument.
	     */
	    switch (mode) {
	    case 0:
		stat = c_seek (fd, offset);
		break;
	    case 1:
		stat = c_seek (fd, c_note(fd) + offset);
		break;
	    case 2:
		if ((stat = c_seek (fd, EOFL)) != ERR) {
		    if (offset == 0L)
			fp->_fflags |= _FEOF;
		    else
			stat = c_seek (fd, c_note(fd) - offset);
		}
		break;
	    default:
		stat = ERR;
	    }
	}

	return (stat);
}
