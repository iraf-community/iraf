/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

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
