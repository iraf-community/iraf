# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ABLE -- Vector boolean less than or equals.  C[i], type INT, is set to 1 if
# A[i] is less than or equal to B[i], else C[i] is set to zero.

procedure abler (a, b, c, npix)

real	a[ARB], b[ARB]
int	c[ARB]
int	npix
int	i

begin
	do i = 1, npix
	    if (a[i] <= b[i])
		c[i] = 1
	    else
		c[i] = 0
end
