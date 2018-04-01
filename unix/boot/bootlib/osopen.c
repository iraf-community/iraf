/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <fcntl.h>
#include "bootlib.h"

extern 	int  os_createfile (char *fname, int mode, int type);


/* OS_OPEN -- Open or create a host system file for reading or writing (text
 * and binary disk files only).
 */
int
os_open (
  char	*vfn,			/* file to be opened		*/
  int	mode,			/* access mode 0=R, 1=W, 2=RW	*/
  int	type 			/* file type			*/
)
{
	extern	char *vfn2osfn(char *vfn, int new);

	if (mode == 0) {
	    osfiletype = BINARY_FILE;
	    return (open (vfn2osfn (vfn, 0), O_RDONLY));
	} else if (mode == 1) {
	    return (os_createfile (vfn, mode, type));
	} else
	    return (-1);
}
