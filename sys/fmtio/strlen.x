# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STRLEN -- Return length of string (EOS not included).

int procedure strlen (str)

char	str[ARB]
int	ip

begin
	do ip = 1, ARB
	    if (str[ip] == EOS)
		return (ip - 1)
end
