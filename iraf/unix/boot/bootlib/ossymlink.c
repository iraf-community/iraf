/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <iraf.h>

#ifndef VMS
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif

/* OS_SYMLINK -- Determine if a file is a symbolic link.
 */
/* fname  : file to be tested				*/
/* valbuf : buffer to receive link path, else NULL	*/
int os_symlink ( const char *fname, char *valbuf, size_t bufsize )
{
#ifndef VMS
	struct	stat fi;
	int	n;

	if (lstat (fname, &fi) == 0)
	    if ((fi.st_mode & S_IFMT) == S_IFLNK) {
		if (valbuf && 0 < bufsize)
		    if ((n = readlink (fname, valbuf, bufsize-1)) > 0)
			valbuf[n] = '\0';
		return (1);
	    }
#endif

	return (0);
}
