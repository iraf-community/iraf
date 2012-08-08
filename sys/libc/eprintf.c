/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#define	import_stdarg
#include <iraf.h>


/* EPRINTF -- Formatted print to the standard error output.
*/
void
eprintf (char *format, ...)
{
	va_list	argp;

	extern void u_doprnt();


	va_start (argp, format);
	u_doprnt (format, &argp, stderr);
	va_end (argp);
	(void) fflush (stderr);
}
