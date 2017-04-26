/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>


/* C_REOPEN -- Reopen a binary file.
*/
int
c_reopen (
  XINT  fd,                 	/* FIO file descriptor		*/
  int   mode                    /* access mode                  */
)
{
	XINT  x_fd = fd, x_mode = mode;
	int new_fd;


        iferr (new_fd = (int) REOPEN (&x_fd, &x_mode))
            return (ERR);
        else
            return (new_fd);
}

