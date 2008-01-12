# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ANOT -- Compute the bitwise boolean complement of a vector (generic).

procedure anotl (a, b, npix)

long	a[ARB], b[ARB]
int	npix, i
long	notl()

begin
	do i = 1, npix {
		b[i] = notl (a[i])
	}
end
