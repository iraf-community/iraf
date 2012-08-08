/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/stat.h>
#include "bootlib.h"


/* OS_SETFMODE -- Set the file mode bits.  This is an important function on
 * any system and should be implemented.
 */
int
os_setfmode (
  char	*fname,
  int	mode
)
{
	return (chmod (vfn2osfn(fname,0), mode));
}
