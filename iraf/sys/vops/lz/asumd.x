# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ASUM -- Vector sum.  Take care to prevent integer overflow by returning
# a floating point sum.

double procedure asumd (a, npix)

double	a[ARB]
size_t	npix
size_t	i

double	sum

begin
	sum = 0.0D0
	do i = 1, npix
	    sum = sum + a[i]
	
	return (sum)
end
