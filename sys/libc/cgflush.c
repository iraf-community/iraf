/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_GFLUSH -- Flush any buffered graphics output.
 */
c_gflush (stream)
int	stream;			/* graphics stream		*/
{
	GTR_GFLUSH (&stream);
}
