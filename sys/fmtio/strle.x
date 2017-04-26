# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STRLE -- Is S1 <= S2.

bool procedure strle (s1, s2)

char	s1[ARB], s2[ARB]
int	ip

begin
	do ip = 1, ARB
	    if (s1[ip] == EOS)
		return (true)
	    else if (s1[ip] != s2[ip])
		return (s1[ip] < s2[ip])
end
