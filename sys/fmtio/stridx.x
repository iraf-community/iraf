# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STRIDX -- Return the index of the first occurrence of a character in a
# string.

int procedure stridx (ch, str)

char	ch, str[ARB]
int	ip

begin
	do ip = 1, ARB
	    if (str[ip] == EOS)
		return (0)
	    else if (str[ip] == ch)
		return (ip)
end
