/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZFALOC -- Preallocate space for a large file of known size, without having
 * to physically write zero blocks.  In UNIX this is done by seeking to the
 * desired end of file and writing some data.  Standard UNIX does not provide
 * any way to allocate a contiguous or near-contiguous file.
 */
int
ZFALOC (
  PKCHAR  *fname,
  XLONG	  *nbytes,
  XINT	  *status
)
{
	char	data = 0;
	char	*s;
	int	fd;
	extern  int  _u_fmode(int mode);


	if ((fd = creat ((char *)fname, _u_fmode(FILE_MODEBITS))) == ERR) {
	    *status = XERR;
	    return (XERR);
	}

	/* Fix size of file by seeking to the end of file minus one byte,
	 * and writing one byte of data.  UNIX will not allocate the remaining
	 * fileblocks until they are written into at run time; when referenced
	 * the blocks will be zero-fill on demand.
	 */
	if (*nbytes > 0) {
	    if (lseek (fd, (off_t)(*nbytes - 1), 0) == ERR) {
		close (fd);
		*status = XERR;
		return (XERR);
	    }
	    if (write (fd, &data, 1) == ERR) {
		close (fd);
		*status = XERR;
		return (XERR);
	    }
	    lseek (fd, (off_t)0, 0);
	}

	/* For efficiency reasons the above is all we normally do.  However,
	 * if ZFALOC is set in the environment we touch each file block at
	 * least once in order to preallocate all the space at zfaloc time.
	 * ZFALOC may optionally have a string value.  If no value is given
	 * all files are match (zfaloc is fully allocate all files).
	 * Otherwise, the string value is a comma delimited list of simple
	 * pattern strings.  A file is matched, and space preallocated, if
	 * the given substring appears anywhere in the file name.
	 */
	if ( (s = getenv ("ZFALOC")) ) {
	    register char *ip, *op;
	    char    patstr[SZ_PATHNAME];
	    int     match = (*s == '\0');
	    int     patlen, i;

	    while (!match && *s) {
		for (op=patstr;  *s && *s != ',';  )
		    *op++ = *s++;
		*op = '\0';
		patlen = strlen (patstr);
		if (*s == ',')
		    s++;

		for (ip=(char *)fname;  *ip;  ip++)
		    if (*ip == patstr[0] && !strncmp(ip,patstr,patlen)) {
			match++;
			break;
		    }
	    }

	    if (match)
		for (i=0;  i < *nbytes;  i += 512) {
		    lseek (fd, (off_t)i, 0);
		    if (write (fd, &data, 1) < 0) {
			*status = XERR;
			close (fd);
			return (XERR);
		    }
		}
	}

	close (fd);
	*status = XOK;

	return (XOK);
}
