#include <stdio.h>

/* EPRINTF -- Formatted print to the standard error output.
 */
/* VARARGS */
eprintf (format, argp)
char	*format;		/* format specification		*/
int	**argp;			/* pointer to arg list		*/
{
	_doprnt (format, &argp, stderr);
	fflush (stderr);
}
