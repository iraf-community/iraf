# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ASUM -- Vector sum.  Take care to prevent integer overflow by returning
# a floating point sum.

double procedure asuml (a, npix)

long	a[ARB]
int	npix
int	i

double	sum

begin
	sum = 0
	do i = 1, npix
	    sum = sum + a[i]
	
	return (sum)
end
