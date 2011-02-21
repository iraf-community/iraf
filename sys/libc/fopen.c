/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#define	import_stdio
#include <iraf.h>


/* FOPEN -- Open a file with the given access mode and file type.  The file type
** (text or binary) is specified with an optional, non UNIX standard character
** "t" or "b" in the modestring.  The default is text file if no type is given.
*/
FILE *
fopen (
  char	*fname,			/* vfn of file			*/
  char	*modestr		/* access mode [and type]	*/
)
{
	XINT	x_filetype, x_filemode;
	int 	fd;


	/* Get file type.
	 */
	switch (modestr[1]) {
	case 't':
	case EOS:
	    x_filetype = TEXT_FILE;
	    break;
	case 'b':
	    x_filetype = BINARY_FILE;
	    break;
	default:
	    return (NULL);
	}

	/* Determine file access mode.
	 */
	switch (modestr[0]) {
	case 'r':
	    x_filemode = READ_ONLY;
	    break;
	case 'w':
	    x_filemode = NEW_FILE;
	    break;
	case 'a':
	    x_filemode = APPEND;
	    break;
	default:
	    return (NULL);
	}

	/* Open file.
	 */
	iferr (fd = OPEN (c_sppstr(fname), &x_filemode, &x_filetype))
	    return (NULL);
	else
	    return (FDTOFP(fd));
}
