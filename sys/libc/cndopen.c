/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_NDOPEN -- Network driver FIO file open.
 */
c_ndopen (fname, mode)
char    *fname;                 /* name of file to be opened    */
int     mode;                   /* access mode                  */
{
        int     fd;

        iferr (fd = NDOPEN (c_sppstr(fname), &mode))
            return (ERR);
        else
            return (fd);
}

