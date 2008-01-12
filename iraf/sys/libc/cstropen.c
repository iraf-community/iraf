/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_STROPEN -- FIO string-file open.
 */
/* ??? SPP char type ??? */
/* ??? Where is c_strclose() ??? */
/* obuf  : string file-buffer	*/
/* maxch : max chars in string	*/
/* mode  : file access mode	*/
int c_stropen ( XCHAR *obuf, XINT maxch, mode_t mode )
{
	int fd;
	XINT x_mode = mode;

	iferr (fd = STROPEN (obuf, &maxch, &x_mode))
	    return (ERR);
	else
	    return (fd);
}
