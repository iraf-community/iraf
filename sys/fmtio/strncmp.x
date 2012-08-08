# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STRNCMP -- Compare the first N characters of two strings.  A negative value
# is returned if S1 < S2, 0 if S1 == S2, and a positive value if S1 > S2.

int procedure strncmp (s1, s2, n)

char	s1[ARB], s2[ARB]		# strings to be compared
int	n				# number of chars to compare
int	i

begin
	do i = 1, n
	    if (s1[i] != s2[i])
		return (s1[i] - s2[i])
	    else if (s1[i] == EOS)
		return (0)

	return (0)
end
