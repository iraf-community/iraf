# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ASUM -- Vector sum.  Take care to prevent integer overflow by returning
# a floating point sum.

complex procedure asumx (a, npix)

complex	a[ARB]
int	npix
int	i

complex	sum

begin
	sum = (0.0,0.0)
	do i = 1, npix
	    sum = sum + a[i]
	
	return (sum)
end
