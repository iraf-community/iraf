/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#define	import_fset
#include <iraf.h>

	
extern int c_fstati();


/* FDOPEN -- Reopen a file for i/o with the STDIO package, after the file
** as already been opened by FIO.  It is an error if the access modes are
** incompatible.
*/
FILE *
fdopen (
  XINT	fd,			/* FIO file descriptor		*/
  char	*mode			/* STDIO access mode		*/
)
{
	register int	fio_mode = c_fstati (fd, F_MODE);
	register int	fio_type = c_fstati (fd, F_TYPE);


	/* Verify file access mode.  No mode checking is performed for the
	 * special file types.
	 */
	if (fio_type == TEXT_FILE || fio_type == BINARY_FILE)
	    switch (mode[0]) {
	    case 'r':
		if (fio_mode != READ_ONLY && fio_mode != READ_WRITE)
		    return (NULL);
		break;

	    case 'w':
		switch (fio_mode) {
		case NEW_FILE:
		case READ_WRITE:
		case WRITE_ONLY:
		    break;
		default:
		    return (NULL);
		}
		break;

	    case 'a':
		if (fio_mode != APPEND && fio_mode != NEW_FILE)
		    return (NULL);
		break;

	    default:
		return (NULL);
	    }

	/* Verify file type.  No checking is performed if no type is given.
	 */
	switch (mode[1]) {
	case EOS:
	    break;
	case 't':
	    if (fio_type != TEXT_FILE)
		return (NULL);
	    break;
	case 'b':
	    if (fio_type != BINARY_FILE)
		return (NULL);
	    break;
	default:
	    return (NULL);
	}

	return (FDTOFP(fd));
}
