/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZFALOC -- Preallocate space for a large file of known size, without having
 * to physically write zero blocks.  In UNIX this is done by seeking to the
 * desired end of file and writing some data.  Standard UNIX does not provide
 * any way to allocate a contiguous or near-contiguous file.
 */
ZFALOC (fname, nbytes, status)
PKCHAR	*fname;
XLONG	*nbytes;
XINT	*status;
{
	char	data = 0;
	int	fd;
	long	lseek();

	if ((fd = creat ((char *)fname, FILE_MODEBITS)) == ERR) {
	    *status = XERR;
	    return;
	}

	/* Fix size of file by seeking to the end of file minus one byte,
	 * and writing one byte of data.  UNIX will not allocate the remaining
	 * fileblocks until they are written into at run time; when referenced
	 * the blocks will be zero-fill on demand.
	 */
	if (*nbytes > 0) {
	    if (lseek (fd, (long)(*nbytes - 1), 0) == ERR) {
		close (fd);
		*status = XERR;
		return;
	    }
	    if (write (fd, &data, 1) == ERR) {
		close (fd);
		*status = XERR;
		return;
	    }
	    lseek (fd, 0L, 0);
	}

	close (fd);
	*status = XOK;
}
