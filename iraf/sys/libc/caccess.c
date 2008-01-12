/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_ACCESS -- FIO file access.
 */
/* fname : name of file to be accessed */
/* mode  : access mode                 */
/* type  : file type                   */
int c_access ( const char *fname, int mode, int type )
{
	XINT x_mode = mode;
	XINT x_type = type;
	return (ACCESS (c_sppstr(fname), &x_mode, &x_type));
}
