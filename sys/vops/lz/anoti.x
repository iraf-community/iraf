# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ANOT -- Compute the bitwise boolean complement of a vector (generic).

procedure anoti (a, b, npix)

int	a[ARB], b[ARB]
int	npix, i
int	not()

begin
	do i = 1, npix {
		b[i] = not (a[i])
	}
end
