/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#define import_stdio
#define import_error
#include <iraf.h>

#define SZ_OSERRMSG		80

int	sys_nerr;			/* UNIX standard	*/
char	*sys_errlist[1];		/* UNIX standard	*/
int	u_oserrcode;
char	u_oserrmsg[SZ_OSERRMSG+1];

/* PERROR -- Print a short error message on the standard output describing
 * the last system error (e.g., exception).  The prefix string supplied
 * as the argument is first printed, followed by an ":", followed by the
 * system dependent error message describing the error.
 */
/* prefix : prefix string */
void perror ( const char *prefix )
{
	u_oserrcode = c_errget (u_oserrmsg, SZ_OSERRMSG);
	sys_nerr = 0;
	sys_errlist[0] = u_oserrmsg;

	fputs (prefix, stderr);
	fputs (": ", stderr);
	fputs (u_oserrmsg, stderr);
	fputc ('\n', stderr);
}
