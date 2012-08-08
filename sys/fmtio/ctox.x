# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# CTOX -- Convert a character string into a complex number.  The complex
# number must have the form (r,i), with no embedded whitespace (GCTOX is
# cabable of accepting numbers in other formats).

int procedure ctox (str, ip, xval)

char	str[ARB]
int	ip, ip_save
double	dval1, dval2
complex	xval
int	ctod()
define	notanumber_ 99

begin
	while (IS_WHITE (str[ip]))
	    ip = ip + 1
	ip_save = ip
	dval2 = 0.0d0

	if (str[ip] == '(') {				# x = (r1,r2)
	    ip = ip + 1
	    if (ctod (str, ip, dval1) <= 0)
		goto notanumber_
	    if (str[ip] != ',')
		goto notanumber_
	    ip = ip + 1
	    if (ctod (str, ip, dval2) <= 0)
		goto notanumber_
	    if (str[ip] != ')')
		goto notanumber_
	    ip = ip + 1
	} else
	    goto notanumber_

	if (IS_INDEFD(dval1) || IS_INDEFD(dval2))
	    xval = INDEFX
	else
	    xval = complex (dval1, dval2)
	return (ip - ip_save)

notanumber_
	ip = ip_save
	return (0)
end
