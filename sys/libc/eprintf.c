/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_stdio
#define	import_stdarg
#include <iraf.h>

/* EPRINTF -- Formatted print to the standard error output.
 */
#ifdef USE_STDARG
eprintf (char *format, ...)
#else
eprintf (va_alist)
va_dcl				/* pointer to arg list		*/
#endif
{
	va_list	argp;

#ifdef USE_STDARG
	va_start (argp, format);
#else
	char *format;
	va_start (argp);
	format = va_arg (argp, char *);
#endif
	u_doprnt (format, &argp, stderr);
	va_end (argp);
	fflush (stderr);
}
