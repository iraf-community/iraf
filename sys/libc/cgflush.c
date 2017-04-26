/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_GFLUSH -- Flush any buffered graphics output.
*/
void
c_gflush (
  int	stream			/* graphics stream		*/
)
{
	XINT  x_stream = stream;

	GTR_GFLUSH (&x_stream);
}
