/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#define import_stdio
#define import_stdarg
#include <iraf.h>

#define SZ_OBUF		SZ_COMMAND	/* sz intermediate buffer	*/


/* SNPRINTF -- Formatted print to a string.  If char and XCHAR are the
 * same size we open the output string as a file and write directly into
 * it.  Otherwise we must write into an intermediate buffer, then pack
 * XCHAR into the char output string.  This is not as bad as it sounds
 * as the operation is negligible compared to the encoding operation.
 */
#ifdef USE_STDARG
int snprintf ( char *str, size_t bufsize, const char *format, ... )
#else
int snprintf ( va_alist )
va_dcl
#endif
{
	int ret;
	XCHAR *ip;
	char *op, *maxop;
	XCHAR obuf[SZ_OBUF+1], *fiobuf;
	XINT fd, maxch=SZ_OBUF, mode=NEW_FILE;
	va_list argp;

#ifdef USE_STDARG
	va_start (argp, format);
#else
	char *str;
	size_t bufsize;
	const char *format;
	va_start (argp);
	str = va_arg (argp, char *);
	bufsize = va_arg (argp, size_t);
	format = va_arg (argp, const char *);
#endif

	fiobuf = obuf;

	/* Make it the file buffer.  Call FIO to open the string as a file.
	 */
	fd = STROPEN (fiobuf, &maxch, &mode);

	/* Format the data into obuf.  */
	ret = u_doprnt (format, &argp, FDTOFP(fd));

	/* FIO does not write the EOS until the string file is closed.
	 * Move obuf to str if it is not already there.
	 */
	CLOSE (&fd);

	maxop = str + bufsize -1;
	for ( ip=obuf, op=str ; op < maxop && *ip != EOS ; op++, ip++ )
	    *op = *ip;
	if ( op <= maxop ) *op = EOS;

	va_end (argp);
	return (ret);
}

#ifdef USE_STDARG
int sprintf ( char *str, const char *format, ... )
#else
int sprintf ( va_alist )
va_dcl
#endif
{
	int ret;
	XCHAR *ip;
	char *op, *maxop;
	XCHAR obuf[SZ_OBUF+1], *fiobuf;
	XINT fd, maxch=SZ_OBUF, mode=NEW_FILE;
	va_list argp;

#ifdef USE_STDARG
	va_start (argp, format);
#else
	char *str;
	const char *format;
	va_start (argp);
	str = va_arg (argp, char *);
	format = va_arg (argp, const char *);
#endif

	fiobuf = obuf;

	/* Make it the file buffer.  Call FIO to open the string as a file.
	 */
	fd = STROPEN (fiobuf, &maxch, &mode);

	/* Format the data into obuf.  */
	ret = u_doprnt (format, &argp, FDTOFP(fd));

	/* FIO does not write the EOS until the string file is closed.
	 * Move obuf to str if it is not already there.
	 */
	CLOSE (&fd);

	maxop = str + SZ_OBUF+1 -1;
	for ( ip=obuf, op=str ; op < maxop && *ip != EOS ; op++, ip++ )
	    *op = *ip;
	if ( op <= maxop ) *op = EOS;

	va_end (argp);
	return (ret);
}

