# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ABGT -- Vector boolean greater than.  C[i], type INT, is set to 1 if
# A[i] is greater than B[i], else C[i] is set to zero.

procedure abgtx (a, b, c, npix)

complex	a[ARB], b[ARB]
int	c[ARB]
int	npix
int	i

begin
	do i = 1, npix
	    if (abs (a[i]) > abs (b[i]))
		c[i] = 1
	    else
		c[i] = 0
end
