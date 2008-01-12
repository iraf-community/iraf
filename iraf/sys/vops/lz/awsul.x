# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AWSU -- Vector weighted sum.  C = A * k1 + B * k2

procedure awsul (a, b, c, npix, k1, k2)

long	a[ARB], b[ARB], c[ARB]
real	k1, k2
int	npix, i

begin
	do i = 1, npix
	    c[i] = a[i] * k1 + b[i] * k2
end
