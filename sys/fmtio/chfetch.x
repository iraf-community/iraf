# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CHFETCH -- Return the next character from a string, bump pointer.

char procedure chfetch (str, ip, ch)

char	str[ARB], ch
int	ip

begin
	ch = str[ip]
	if (ch != EOS)
	    ip = ip + 1

	return (ch)
end
