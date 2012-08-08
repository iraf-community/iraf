/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>


/* C_NDOPEN -- Network driver FIO file open.
*/
int
c_ndopen (
  char  *fname,                 /* name of file to be opened    */
  int   mode                    /* access mode                  */
)
{
        int   fd;
	XINT  x_mode = mode;

        iferr (fd = NDOPEN (c_sppstr(fname), &x_mode))
            return (ERR);
        else
            return (fd);
}
