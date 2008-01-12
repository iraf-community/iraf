/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_OPEN -- FIO file open.
 */
/* fname : name of file to be opened */
/* mode  : access mode               */
/* type  : file type                 */
int c_open ( const char *fname, mode_t mode, int type )
{
	XINT fd;
	XINT x_mode = mode;
	XINT x_type = type;

	iferr (fd = OPEN (c_sppstr(fname), &x_mode, &x_type))
	    return (ERR);
	else
	    return (fd);
}
