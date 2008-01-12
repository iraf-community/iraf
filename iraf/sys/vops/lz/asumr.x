# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ASUM -- Vector sum.  Take care to prevent integer overflow by returning
# a floating point sum.

real procedure asumr (a, npix)

real	a[ARB]
int	npix
int	i

real	sum

begin
	sum = 0.0
	do i = 1, npix
	    sum = sum + a[i]
	
	return (sum)
end
