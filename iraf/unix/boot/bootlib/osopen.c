/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <fcntl.h>
#include "bootlib.h"

/* OS_OPEN -- Open or create a host system file for reading or writing (text
 * and binary disk files only).
 */
/* vfn  : file to be opened		*/
/* mode : access mode 0=R, 1=W, 2=RW	*/
/* type : file type			*/
int os_open ( const char *vfn, int mode, int type )
{
	if (mode == 0) {
	    osfiletype = BINARY_FILE;
	    return (open (vfn2osfn (vfn, 0), 0));
	} else if (mode == 1) {
	    return (os_createfile (vfn, mode, type));
	} else
	    return (-1);
}
