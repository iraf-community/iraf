# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACJGX -- Complex conjugate of a complex vector.

procedure acjgx (a, b, npix)

complex	a[ARB], b[ARB]
int	npix
int	i

begin
	do i = 1, npix
	    b[i] = conjg (a[i])
end
