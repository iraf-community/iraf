/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_stdio
#define	import_varargs
#include <iraf.h>

/* EPRINTF -- Formatted print to the standard error output.
 */
/* VARARGS */
eprintf (va_alist)
va_dcl				/* pointer to arg list		*/
{
	va_list	argp;
	char	*format;

	va_start (argp);
	format = va_arg (argp, char *);
	u_doprnt (format, &argp, stderr);
	va_end (argp);
	fflush (stderr);
}
