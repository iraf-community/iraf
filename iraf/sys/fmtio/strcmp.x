# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STRCMP -- Compare two strings.  -N is returned if S1 < S2, 0 if S1 == S2,
# and +N if S1 > S2.

int procedure strcmp (s1, s2)

char	s1[ARB], s2[ARB]		# strings to be compared
int	i

begin
	do i = 1, ARB
	    if (s1[i] != s2[i])
		return (s1[i] - s2[i])
	    else if (s1[i] == EOS)
		return (0)
end
