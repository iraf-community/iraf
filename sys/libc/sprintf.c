/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#define	import_stdio
#define	import_stdarg
#include <iraf.h>

#define	SZ_OBUF		SZ_COMMAND	/* sz intermediate buffer	*/


/* SPRINTF -- Formatted print to a string.  If char and XCHAR are the
** same size we open the output string as a file and write directly into
** it.  Otherwise we must write into an intermediate buffer, then pack
** XCHAR into the char output string.  This is not as bad as it sounds
** as the operation is negligible compared to the encoding operation.
*/
char *
sprintf (char *str, char *format, ...)
{
	register XCHAR	*ip;
	register char	*op;
	XCHAR obuf[SZ_OBUF], *fiobuf;
	XINT x_fd, x_maxch = SZ_OBUF, x_mode = NEW_FILE;
	va_list	argp;

	extern int  u_doprnt();


	va_start (argp, format);

	/* Select output buffer. */
	if (sizeof (XCHAR) == sizeof (char))
	    fiobuf = (XCHAR *)str;
	else
	    fiobuf = obuf;

	/* Make it the file buffer.  Call FIO to open the string as a file.
	 */
	x_fd = STROPEN (fiobuf, &x_maxch, &x_mode);

	/* Format the data into obuf.  */
	u_doprnt (format, &argp, FDTOFP(x_fd));

	/* FIO does not write the EOS until the string file is closed.
	 * Move obuf to str if it is not already there.
	 */
	CLOSE (&x_fd);
	if (fiobuf == obuf)
	    for (ip=obuf, op=str;  (*op++ = *ip++) != EOS;  )
		;

	va_end (argp);

	return (str);
}
