# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMOVK -- Copy a constant into a vector (generic).

procedure amovkr (a, b, npix)

real	a
real	b[ARB]
int	npix, i

begin
	do i = 1, npix
	    b[i] = a
end
