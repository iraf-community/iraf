# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ANOT -- Compute the bitwise boolean complement of a vector (generic).

procedure anots (a, b, npix)

short	a[ARB], b[ARB]
int	npix, i
short	nots()

begin
	do i = 1, npix {
		b[i] = nots (a[i])
	}
end
