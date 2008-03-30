# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AMOVK -- Copy a constant into a vector (generic).

procedure amovkp (a, b, npix)

pointer	a
pointer	b[ARB]
size_t	npix, i

begin
	do i = 1, npix
	    b[i] = a
end
