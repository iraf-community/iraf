/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "bootlib.h"

/* OS_OPEN -- Open or create a host system file for reading or writing (text
 * and binary disk files only).
 */
os_open (vfn, mode, type)
char	*vfn;			/* file to be opened		*/
int	mode;			/* access mode 0=R, 1=W, 2=RW	*/
int	type;			/* file type			*/
{
	extern	char *vfn2osfn();

	if (mode == 0) {
	    osfiletype = BINARY_FILE;
	    return (open (vfn2osfn (vfn, 0), 0));
	} else if (mode == 1) {
	    return (os_createfile (vfn, mode, type));
	} else
	    return (-1);
}
