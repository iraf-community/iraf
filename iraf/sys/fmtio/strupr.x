# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# STRUPR -- Convert string to upper case.

procedure strupr (str)

char	str[ARB]
int	ip

begin
	do ip = 1, ARB
	    if (str[ip] == EOS)
		return
	    else if (IS_LOWER (str[ip]))
		str[ip] = TO_UPPER (str[ip])
end
