/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define import_libc
#define	import_ctype
#define	import_lexnum
#define	import_xnames
#include <iraf.h>


/* LEXNUM -- Lexically analyze a string to determine if it is a legal IRAF
** style number.  Returns one of the following tokens (lexnum.h):
**
**	LEX_OCTAL		octal number (e.g. B suffix)
**	LEX_HEX			hex number (e.g. X suffix)
**	LEX_DECIMAL		decimal number
**	LEX_REAL		real number (incl sexagesimal)
**	LEX_NONNUM		nonnumeric
**
** A numeric token is returned if any (prefix) portion of the field is
** numeric.  The total number of numeric characters is also returned so
** that the application may verify that the entire field was numeric,
** if desired.
*/
int
c_lexnum (
  char	*str,			/* input string			*/
  int	*toklen			/* nchars in token		*/
)
{
	register char *ip;
	register XCHAR *op, ch, ndigits;
	PKCHAR	 numbuf[SZ_FNAME];
	XINT	 ip_start = 1, x_toklen = *toklen;
	int      status;


	/* Convert number to XCHAR for lexnum.  In the process check to see
	 * if we have a simple decimal integer constant to save a scan.
	 */
	for (ip=str, op=numbuf, ndigits=0;  (*op++ = ch = *ip++);  )
	    if (isdigit (ch))
		ndigits++;

	if (ndigits == (ip - str - 1)) {
	    *toklen = ndigits;
	    return (LEX_DECIMAL);
	} else {
	    status = LEXNUM (numbuf, &ip_start, &x_toklen);
	    *toklen = (int) x_toklen;
	    return (status);
	}
}
