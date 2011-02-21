/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_IMACCESS -- IMIO test if image can be accessed (exists).  1 is returned
** if the image exists and is unique, 0 if the image does not exist, and ERR
** if the image name ambiguous and multiple images exist matching that name.
*/
int
c_imaccess (
  char	*imname,		/* name of image to be accessed	*/
  int	mode			/* access mode			*/
)
{
	int  status;
	XINT  x_mode = mode;


	iferr (status = (int) IMACCESS (c_sppstr(imname), &x_mode))
	    return (ERR);
	else
	    return (status);
}
