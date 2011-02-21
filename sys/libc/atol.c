/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_ctype
#include <iraf.h>


/* ATOL -- Ascii to long integer.  Convert a simple integer in decimal radix to
** a binary long integer value.
*/
long
atol (char *str)
{
	register char	*ip = str;
	register int	ch;
	register long	lval;
	int	neg;


	if (*str == EOS)
	    return (0);

	/* Skip leading whitespace. */
	while (isspace (*ip))
	    ip++;

	/* Check for indefinite. */
	if ((ch = *--ip) == 'I')
	    if (strncmp (ip, "INDEF", 5) == 0)
		if (! (isalnum (ch = *(ip+5)) || ch == '_'))
		    return (INDEFL);

	/* Check for leading + or - sign. */
	neg = 0;
	if (ch == '-') {
	    neg++;
	    ip++;
	} else if (ch == '+')
	    ip++;

	/* Accumulate sequence of digits. */
	lval = 0;
	while (isdigit (ch = *ip++))
	    lval = lval * 10 + tointeg(ch);

	return (neg ? -lval : lval);
}
