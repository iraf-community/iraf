/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>


/* C_REOPEN -- Reopen a binary file.
 */
/* fd   : FIO file descriptor */
/* mode : access mode         */
int c_reopen ( int fd, mode_t mode )
{
	XINT new_fd;
	XINT x_fd = fd;
	XINT x_mode = mode;

        iferr (new_fd = REOPEN (&x_fd, &x_mode))
            return (ERR);
        else
            return (new_fd);
}

