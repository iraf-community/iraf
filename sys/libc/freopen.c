/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>


/* FREOPEN -- Close a stream and reopen it upon the named file.  This is
** commonly used to redirect one of the standard streams stdin, stdout,
** or stderr to a named file.
*/
FILE *
freopen (
  char	*fname,			/* vfn of file to be opened	*/
  char	*modestr,		/* access mode [and type]	*/
  FILE	*fp			/* stream to be reopened	*/
)
{
	register XINT	fd = fileno(fp);
	register int	status, filetype;


	/* Determine the file type of the file to be opened.  This is given
	 * by an optional second character in the mode string.  Default is
	 * text file if absent.
	 */
	switch (modestr[1]) {
	case 't':
	case EOS:
	    filetype = TEXT_FILE;
	    break;
	case 'b':
	    filetype = BINARY_FILE;
	    break;
	default:
	    return (NULL);
	}

	switch (modestr[0]) {
	case 'r':
	    status = c_fredir (fd, fname, READ_ONLY, filetype);
	    break;
	case 'w':
	    status = c_fredir (fd, fname, NEW_FILE, filetype);
	    break;
	case 'a':
	    status = c_fredir (fd, fname, APPEND, filetype);
	    break;
	default:
	    return (NULL);
	}

	return (status == ERR ? NULL : fp);
}
