/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>


/* C_REOPEN -- Reopen a binary file.
 */
c_reopen (fd, mode)
int     fd;                 	/* FIO file descriptor		*/
int     mode;                   /* access mode                  */
{
	int new_fd;

        iferr (new_fd = REOPEN (&fd, &mode))
            return (ERR);
        else
            return (new_fd);
}

