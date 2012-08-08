# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STRLDX -- Return the index of the last occurrence of a character in a
# string.

int procedure strldx (ch, str)

char	ch, str[ARB]
int	ip, offset

begin
	offset = 0
	do ip = 1, ARB
	    if (str[ip] == EOS)
		break
	    else if (str[ip] == ch)
		offset = ip

	return (offset)
end
