# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AAND -- Compute the bitwise boolean 'and' of two vectors (generic).

procedure aandi (a, b, c, npix)

int	a[ARB], b[ARB], c[ARB]
int	npix, i
int	and()

begin
	do i = 1, npix {
		c[i] = and (a[i], b[i])
	}
end
