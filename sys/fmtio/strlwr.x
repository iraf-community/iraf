# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# STRLWR -- Convert string to lower case.

procedure strlwr (a)

char	a[ARB]
int	ip

begin
	do ip = 1, ARB
	    if (a[ip] == EOS)
		break
	    else if (IS_UPPER(a[ip]))
		a[ip] = TO_LOWER (a[ip])
end
