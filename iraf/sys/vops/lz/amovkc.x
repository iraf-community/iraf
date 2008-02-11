# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMOVK -- Copy a constant into a vector (generic).

procedure amovkc (a, b, npix)

char	a
char	b[ARB]
size_t	npix, i

begin
	do i = 1, npix
	    b[i] = a
end
