/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_FREDIR -- FIO redirect an open i/o stream to a named file.  Most commonly
 * used to redirect one of the standard i/o streams to a file.  The named file
 * need not be of the same type as the old stream.
 */
/* fd    : stream to be redirected   */
/* fname : name of file to be opened */
/* mode  : access mode               */
/* type  : file type                 */
int c_fredir ( int fd, const char *fname, int mode, int type )
{
	XINT x_fd = fd;
	XINT x_mode = mode;
	XINT x_type = type;
	iferr (FREDIR (&x_fd, c_sppstr(fname), &x_mode, &x_type))
	    return (ERR);
	else
	    return (fd);
}
