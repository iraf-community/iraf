# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ASSQ -- Vector sum of squares.

complex procedure assqx (a, npix)
complex	sum

complex	a[ARB]
int	npix
int	i

begin
	sum = (0.0,0.0)
	do i = 1, npix
	    sum = sum + (a[i] ** 2)
	
	return (sum)
end
