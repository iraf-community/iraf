# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMOD -- Compute the modulus of two vectors (generic).

procedure amodi (a, b, c, npix)

int	a[ARB], b[ARB], c[ARB]
size_t	npix, i
int	modi()

begin
	do i = 1, npix
	    c[i] = modi (a[i], b[i])
end
