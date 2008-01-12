/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_ctype
#include <iraf.h>

/* ATOI -- Ascii to integer.  Convert a simple integer in decimal radix to
 * a binary integer value.
 */
int atoi ( const char *str )
{
	const char *ip = str;
	int ch, ival;
	int neg;

	/* Skip leading whitespace. */
	while (isspace (*ip))
	    ip++;

	if (*ip == EOS)
	    return (0);

	/* Check for indefinite. */
	if ((ch = *ip) == 'I') {
	    if (strncmp (ip, "INDEF", 5) == 0) {
		ch = *(ip+5);
		if (! (isalnum (ch) || ch == '_'))
		    return (INDEFI);
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
	ival = 0;
	while ( 1 ) {
	    ch = *ip++;
	    if ( ! isdigit(ch) ) break;
	    ival = ival * 10 + tointeg(ch);
	}

	return (neg ? -ival : ival);
}
