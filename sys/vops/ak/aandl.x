# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AAND -- Compute the bitwise boolean 'and' of two vectors (generic).

procedure aandl (a, b, c, npix)

long	a[ARB], b[ARB], c[ARB]
int	npix, i
long	andl()

begin
	do i = 1, npix {
		c[i] = andl (a[i], b[i])
	}
end
