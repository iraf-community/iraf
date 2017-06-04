/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <unistd.h>
#include <iraf.h>

#include <sys/types.h>
#include <sys/stat.h>

/* OS_SYMLINK -- Determine if a file is a symbolic link.
 */
int
os_symlink (
  char	*fname,			/* file to be tested */
  char	*valbuf,		/* buffer to receive link path, else NULL */
  int	maxch 
)
{
	struct	stat fi;
	int	n;

	if (lstat (fname, &fi) == 0)
	    if ((fi.st_mode & S_IFMT) == S_IFLNK) {
		if (valbuf && maxch)
		    if ((n = readlink (fname, valbuf, maxch)) > 0)
			valbuf[n] = '\0';
		return (1);
	    }

	return (0);
}
