/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#include <stdarg.h>

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>

extern void u_doprnt (char *format, va_list *argp, FILE *fp);
	

/* EPRINTF -- Formatted print to the standard error output.
*/
void
eprintf (char *format, ...)
{
	va_list	argp;

	va_start (argp, format);
	u_doprnt (format, &argp, stderr);
	va_end (argp);
	(void) fflush (stderr);
}
