/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#define import_stdarg
#include <iraf.h>

/* EPRINTF -- Formatted print to the standard error output.
 */
int eprintf ( const char *format, ... )
{
	int ret = 0;
	va_list	argp;

	va_start (argp, format);
	ret = u_doprnt (format, &argp, stderr);
	va_end (argp);
	fflush (stderr);

	return ret;
}
