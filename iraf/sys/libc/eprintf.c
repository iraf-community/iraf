/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#define import_stdarg
#include <iraf.h>

/* EPRINTF -- Formatted print to the standard error output.
 */
#ifdef USE_STDARG
int eprintf ( const char *format, ... )
#else
int eprintf ( va_alist )
va_dcl				/* pointer to arg list		*/
#endif
{
	int ret = 0;
	va_list	argp;

#ifdef USE_STDARG
	va_start (argp, format);
#else
	const char *format;
	va_start (argp);
	format = va_arg (argp, const char *);
#endif
	ret = u_doprnt (format, &argp, stderr);
	va_end (argp);
	fflush (stderr);

	return ret;
}
