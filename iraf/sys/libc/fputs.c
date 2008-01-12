/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

/* FPUTS -- Put a null terminated string to the output file.
 */
/* str : input string */
/* fp  : output file  */
int fputs ( const char *str, FILE *fp )
{
	const char *ip;

	for (ip=str;  *ip != EOS;  ip++)
	    putc (*ip, fp);

	return 0;
}
