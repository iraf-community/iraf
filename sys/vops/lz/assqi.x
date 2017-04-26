# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ASSQ -- Vector sum of squares.

real procedure assqi (a, npix)
real	sum

int	a[ARB]
int	npix
int	i

begin
	sum = 0
	do i = 1, npix
	    sum = sum + (a[i] ** 2)
	
	return (sum)
end
