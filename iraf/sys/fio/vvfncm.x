# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# VVFN_CHECKSUM -- Compute the integer checksum of a char array.

int procedure vvfn_checksum (a, nchars)

char	a[nchars]		# array to be summed
int	nchars			# length of array
int	i, sum

begin
	sum = 0
	do i = 1, nchars
	    sum = sum + a[i]

	return (sum)
end
