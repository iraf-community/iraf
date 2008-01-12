/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_ctype
#include <iraf.h>

/* ATOL -- Ascii to long integer.  Convert a simple integer in decimal radix to
 * a binary long integer value.
 */
long atol ( const char *str )
{
	const char *ip = str;
	int ch, neg;
	long lval;

	/* Skip leading whitespace. */
	while (isspace (*ip))
	    ip++;

	if (*ip == EOS)
	    return (0);

	/* Check for indefinite. */
	if ((ch = *--ip) == 'I') {
	    if (strncmp (ip, "INDEF", 5) == 0) {
		ch = *(ip+5);
		if (! (isalnum (ch) || ch == '_'))
		    return (INDEFL);
	    }
	}

	/* Check for leading + or - sign. */
	neg = 0;
	if (ch == '-') {
	    neg++;
	    ip++;
	} else if (ch == '+')
	    ip++;

	/* Accumulate sequence of digits. */
	lval = 0;
	while ( 1 ) {
	    ch = *ip++;
	    if ( ! isdigit (ch) ) break;
	    lval = lval * 10 + tointeg(ch);
	}

	return (neg ? -lval : lval);
}
